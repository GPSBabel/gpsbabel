/* pdb.c
 *
 * Functions for dealing with Palm databases and such.
 *
 *	Copyright (C) 1999-2001, Andrew Arensburger.
 *	You may distribute this file under the terms of the Artistic
 *	License, as specified in the README file.
 *
 * $Id: pdb.c,v 1.11 2006-05-07 02:14:35 robertl Exp $
 */
/* XXX - The way zero-length records are handled is a bit of a kludge. They
 * shouldn't normally exist, with the exception of expunged records. But,
 * of course, a malformed conduit or something can create them.
 * The half-assed way they're handled here is to a) not upload zero-length
 * records to the Palm, b) warn the user if they're written to a file, c)
 * provide a utility (in the p5-Palm package) to delete zero-length
 * records.
 */
/* XXX - This is a library. It shouldn't print error messages.
 * Add 'int pdb_errno'; define error numbers and error messages that go
 * with them.
 * Debugging messages should go to 'FILE *pdb_logfile'.
 */
#include "config.h"
#if PDBFMTS_ENABLED

#include "cs-config.h"
#include <stdio.h>
#include <fcntl.h>		/* For open() */
#include <sys/types.h>
/*
 * Unistd.h (indeed, read, write, and lseek) are not part of ISO C.
 * Systems may not have unistd.h.  While the below is tacky, Windows
 * is the only system that we care about that has lseek and friends 
 * but doesn't have it prototyped.  Systems with 64-bit file I/O but
 * based on LP64 model (i.e. OS/X) _require_ the prototype for lseek.
 */
#if defined (__WIN32__)
#include <io.h>
#define lseek _lseek
#define write _write
#define read _read
#define close _close
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <time.h>

#if STDC_HEADERS
# include <string.h>		/* For strncat(), memcpy() et al. */
#else	/* STDC_HEADERS */
# ifndef HAVE_STRCHR
#  define strchr index
#  define strrchr rindex
# endif	/* HAVE_STRCHR */
# ifndef HAVE_MEMCPY
#  define memcpy(d,s,n)		bcopy ((s), (d), (n))
#  define memmove(d,s,n)	bcopy ((s), (d), (n))
# endif	/* HAVE_MEMCPY */
#endif	/* STDC_HEADERS */

/* XXX - Is this right? Should this be in the "else" clause, above? */
#if HAVE_STRINGS_H
#  include <strings.h>		/* For bzero() */
#endif	/* HAVE_STRINGS_H */

#if HAVE_LIBINTL_H
#  include <libintl.h>		/* For i18n */
#endif	/* HAVE_LIBINTL_H */

#include <palm.h>
#include "pdb.h"

/* XXX - The functions declared INLINE, below, really ought to be inline
 * functions. I'm not sure how to do this portably, though.
 */
#ifdef __GNUC__
#  define INLINE __inline__
#else
#  define INLINE
#endif	/* __GNUC__ */

/* Functions for extracting values from an array of ubytes */
extern INLINE ubyte get_ubyte(const ubyte **buf);
extern INLINE uword get_uword(const ubyte **buf);
extern INLINE udword get_udword(const ubyte **buf);

/* Functions for writing values to an array of ubytes */
extern INLINE void put_ubyte(ubyte **buf, const ubyte value);
extern INLINE void put_uword(ubyte **buf, const uword value);
extern INLINE void put_udword(ubyte **buf, const udword value);
extern void debug_dump(FILE *outfile, const char *prefix,
		       const ubyte *buf, const udword len);

int pdb_trace = 0;		/* Debugging level for PDB stuff */
#define PDB_TRACE(n)	if (pdb_trace >= (n))

/* Helper functions */
static uword get_file_length(int fd);
int pdb_LoadHeader(int fd, struct pdb *db);
			/* pdb_LoadHeader() is visible to other files */
static int pdb_LoadRecListHeader(int fd, struct pdb *db);
static int pdb_LoadRsrcIndex(int fd, struct pdb *db);
static int pdb_LoadRecIndex(int fd, struct pdb *db);
static int pdb_LoadAppBlock(int fd, struct pdb *db);
static int pdb_LoadSortBlock(int fd, struct pdb *db);
static int pdb_LoadResources(int fd, struct pdb *db);
static int pdb_LoadRecords(int fd, struct pdb *db);

/* merge_attributes
 * Takes a record's flags and category, and merges them into a single byte,
 * with the flags in the top nybble and the category in the bottom one
 */
static inline ubyte
merge_attributes(const ubyte flags,
		 const ubyte category)
{
	/* The PDB_REC_ARCHIVED flag is troublesome, since it overlaps the
	 * category field. The idea here is that if the record was deleted,
	 * then it doesn't have a category anymore, so the category part
	 * gets set to 0.
	 */
	if ((flags & PDB_REC_DELETED) == 0)
		return (flags & 0xf0) |
			(category & 0x0f);
	else
		return (flags & 0xf8);
}

/* split_attributes
 * The converse of merge_attributes(). Takes the combined field attributes
 * and writes its contents to *flags and *category, using the same rules as
 * merge_attributes(), above.
 */
static inline void
split_attributes(const ubyte attributes,
		 ubyte *flags,
		 ubyte *category)
{
	if ((attributes & PDB_REC_DELETED) == 0)
	{
		*flags = (attributes & 0xf0);
		*category = (attributes & 0x0f);
	} else {
		*flags = (attributes & 0xf8);
		*category = 0;
	}
	PDB_TRACE(6)
		fprintf(stderr, "split 0x%02x into 0x%02x, 0x%02x\n",
			attributes, *flags, *category);
}

/* new_pdb
 * struct pdb constructor.
 */
struct pdb *
new_pdb()
{
	struct pdb *retval;

	/* Allocate the new pdb */
	if ((retval = (struct pdb *) malloc(sizeof(struct pdb))) == NULL)
		/* Out of memory */
		return NULL;

	/* Write zeros all over it, just for safety */
	memset((void *) retval, 0, sizeof(struct pdb));

	return retval;
}

/* pdb_FreeRecord
 * Free a previously-allocated 'pdb_record'. This function wouldn't really
 * be necessary, except that pdb_CopyRecord() returns a 'pdb_record'.
 */
void
pdb_FreeRecord(struct pdb_record *rec)
{
	if (rec->data != NULL)
		free(rec->data);
	free(rec);
}

/* pdb_FreeResource
 * Free a previously-allocated 'pdb_resource'. This function wouldn't
 * really be necessary, except that pdb_CopyResource() returns a
 * 'pdb_resource'.
 */
void
pdb_FreeResource(struct pdb_resource *rsrc)
{
	if (rsrc->data != NULL)
		free(rsrc->data);
	free(rsrc);
}

/* free_pdb
 * Cleanly free a struct pdb, and all of its subparts (destructor).
 */
void
free_pdb(struct pdb *db)
{
	PDB_TRACE(7)
		fprintf(stderr, "Inside free_pdb(%p)\n", (void *) db);

	if (db == NULL)
		/* Trivial case */
		return;

	/* Free the array of records/resources */
	if (IS_RSRC_DB(db))
	{
		/* It's a resource database */
		struct pdb_resource *rsrc;
		struct pdb_resource *next;

		PDB_TRACE(8)
			fprintf(stderr, "Freeing resource list\n");

		/* Walk the linked list, freeing as we go along */
		for (rsrc = db->rec_index.rsrc;
		     rsrc != NULL;
		     rsrc = next)
		{
			next = rsrc->next;	/* Remember the next
						 * element on the list. We
						 * won't have a chance to
						 * look it up after this
						 * one has been free()d.
						 */

			/* Free this element */
			pdb_FreeResource(rsrc);
		}
	} else {
		/* It's a record database */
		struct pdb_record *rec;
		struct pdb_record *next;

		PDB_TRACE(8)
			fprintf(stderr, "Freeing record list\n");

		/* Walk the linked list, freeing as we go along */
		for (rec = db->rec_index.rec;
		     rec != NULL;
		     rec = next)
		{
			next = rec->next;	/* Remember the next
						 * element on the list. We
						 * won't have a chance to
						 * look it up after this
						 * one has been free()d.
						 */

			/* Free this element */
			pdb_FreeRecord(rec);
		}
	}

	/* Free the sort block */
	if (db->sortinfo != NULL)
		free(db->sortinfo);

	/* Free the app info block */
	if (db->appinfo != NULL)
		free(db->appinfo);

	free(db);
}

/* pdb_Read
 * Read a PDB from the file descriptor 'fd'. This must already have been
 * opened for reading and/or writing.
 *
 * Note: this function does not to any locking. The caller is responsible
 * for that.
 */
struct pdb *
pdb_Read(int fd)
{
	int err;
	struct pdb *retval;

	/* Create a new pdb to return */
	if ((retval = new_pdb()) == NULL)
	{
		return NULL;
	}

	/* Find out how long the file is */
	retval->file_size = get_file_length(fd);
	if (retval->file_size == ~0)
	{
		/* The file isn't seekable */
		fprintf(stderr, _("File isn't seekable.\n"));
		free_pdb(retval);
		return NULL;
	}

	/* Load the header */
	if ((err = pdb_LoadHeader(fd, retval)) < 0)
	{
		fprintf(stderr, _("Can't load header.\n"));
		free_pdb(retval);
		return NULL;
	}

	/* Load the record list header */
	if ((err = pdb_LoadRecListHeader(fd, retval)) < 0)
	{
		fprintf(stderr, _("Can't load record list header for "
				  "\"%.*s\".\n"),
			PDB_DBNAMELEN, retval->name);
		free_pdb(retval);
		return NULL;
	}

	/* Read the record/resource list */
	if (IS_RSRC_DB(retval))
	{
		/* Read the resource index */
		if ((err = pdb_LoadRsrcIndex(fd, retval)) < 0)
		{
			fprintf(stderr, _("Can't read resource index for "
					  "\"%.*s\".\n"),
				PDB_DBNAMELEN, retval->name);
			free_pdb(retval);
			return NULL;
		}
	} else {
		/* Read the record index */
		if ((err = pdb_LoadRecIndex(fd, retval)) < 0)
		{
			fprintf(stderr, _("Can't read record index for "
					  "\"%.*s\".\n"),
				PDB_DBNAMELEN, retval->name);
			free_pdb(retval);
			return NULL;
		}
	}

	/* In most PDBs, there are two NUL bytes here. They are allowed by
	 * the spec, but not mandated, and some PDBs don't have them. We'll
	 * ignore them for now, and have the appropriate pdb_Load*()
	 * function lseek() to the proper position.
	 */

	/* Load the AppInfo block, if any */
	if ((err = pdb_LoadAppBlock(fd, retval)) < 0)
	{
		fprintf(stderr, _("Can't read AppInfo block for "
				  "\"%.*s\".\n"),
			PDB_DBNAMELEN, retval->name);
		free_pdb(retval);
		return NULL;
	}

	/* Load the sort block, if any */
	if ((err = pdb_LoadSortBlock(fd, retval)) < 0)
	{
		fprintf(stderr, _("Can't read sort block for "
				  "\"%.*s\".\n"),
			PDB_DBNAMELEN, retval->name);
		free_pdb(retval);
		return NULL;
	}

	/* Load the records themselves */
	if (IS_RSRC_DB(retval))
	{
		/* Read the resources */
		if ((err = pdb_LoadResources(fd, retval)) < 0)
		{
			fprintf(stderr, _("Can't read resources for "
					  "\"%.*s\".\n"),
				PDB_DBNAMELEN, retval->name);
			free_pdb(retval);
			return NULL;
		}
	} else {
		/* Read the records */
		if ((err = pdb_LoadRecords(fd, retval)) < 0)
		{
			fprintf(stderr, _("Can't read records for "
					  "\"%.*s\".\n"),
				PDB_DBNAMELEN, retval->name);
			free_pdb(retval);
			return NULL;
		}
	}

	return retval;			/* Success */
}

/* pdb_Write
 * Write 'db' to the file descriptor 'fd'. This must already have been
 * opened for writing.
 *
 * Note that while you can open the backup file for reading and writing,
 * read from it with pdb_Read() and save it with pdb_Write(), this is not
 * recommended: if anything should go wrong at the wrong time (e.g., the
 * disk fills up just as you're about to write the database back to disk),
 * you will lose the entire backup.
 * A better approach is to use a staging file: read from the backup file,
 * write to a temporary file, then use rename() to move the temporary file
 * onto the real one. Alternately, you can copy the original file to a
 * temporary one, then open the temporary for both reading and writing.
 * This might have some advantages, in that it allows you to lock a single
 * file for the duration of the sync.
 *
 * Note: this function does not lock the file. The caller is responsible
 * for that.
 */
int
pdb_Write(const struct pdb *db,
	  int fd)
{
	static ubyte header_buf[PDB_HEADER_LEN];
				/* Buffer for writing database header */
	static ubyte rlheader_buf[PDB_RECORDLIST_LEN];
				/* Buffer for writing the record list header */
	static ubyte nul_buf[2];
				/* Buffer for writing the two useless NULs */
	ubyte *wptr;		/* Pointer into buffers, for writing */
	udword offset;		/* The next offset we're interested in */

	/* Initialize 'offset': the next variable-sized item will go after
	 * the header, after the index header, after the index, after the
	 * two useless NULs.
	 */
	offset = PDB_HEADER_LEN + PDB_RECORDLIST_LEN;
	if (IS_RSRC_DB(db))
		offset += db->numrecs * PDB_RESOURCEIX_LEN;
	else
		offset += db->numrecs * PDB_RECORDIX_LEN;
	offset += 2;		/* Those two useless NUL bytes */

	/** Write the database header **/

	/* Construct the header in 'header_buf' */
	wptr = header_buf;
	memcpy(wptr, db->name, PDB_DBNAMELEN);
	wptr += PDB_DBNAMELEN;
	put_uword(&wptr, (uword) (db->attributes & ~PDB_ATTR_OPEN));
				/* Clear the 'open' flag before writing */
	put_uword(&wptr, db->version);
	put_udword(&wptr, db->ctime);
	put_udword(&wptr, db->mtime);
	put_udword(&wptr, db->baktime);
	put_udword(&wptr, db->modnum);
	if (db->appinfo == NULL)	/* Write the AppInfo block, if any */
		/* This database doesn't have an AppInfo block */
		put_udword(&wptr, 0L);
	else {
		/* This database has an AppInfo block */
		put_udword(&wptr, offset);
		offset += db->appinfo_len;
	}
	if (db->sortinfo == NULL)	/* Write the sort block, if any */
		/* This database doesn't have a sort block */
		put_udword(&wptr, 0L);
	else {
		put_udword(&wptr, offset);
		offset += db->sortinfo_len;
	}
	put_udword(&wptr, db->type);
	put_udword(&wptr, db->creator);
	put_udword(&wptr, db->uniqueIDseed);

	/* Write the database header */
	if (write(fd, header_buf, PDB_HEADER_LEN) != PDB_HEADER_LEN)
	{
		fprintf(stderr, _("%s: can't write database header for "
				  "\"%.*s\".\n"),
			"pdb_Write",
			PDB_DBNAMELEN, db->name);
		perror("write");
		close(fd);
		return -1;
	}

	/** Write the record/resource index header **/
	/* Construct the record list header */
	wptr = rlheader_buf;
	put_udword(&wptr, 0L);	/* nextID */
			/* XXX - What is this? Should this be something
			 * other than 0? */
	put_uword(&wptr, db->numrecs);

	/* Write the record list header */
	if (write(fd, rlheader_buf, PDB_RECORDLIST_LEN) != PDB_RECORDLIST_LEN)
	{
		fprintf(stderr, _("%s: can't write record list header for "
				  "\"%.*s\".\n"),
			"pdb_Write",
			PDB_DBNAMELEN, db->name);
		perror("write");
		return -1;
	}

	/* Write the record/resource index */
	if (IS_RSRC_DB(db))
	{
		/* It's a resource database */
		struct pdb_resource *rsrc;	/* Current resource */

		/* Go through the list of resources, writing each one */
		for (rsrc = db->rec_index.rsrc;
		     rsrc != NULL;
		     rsrc = rsrc->next)
		{
			static ubyte rsrcbuf[PDB_RESOURCEIX_LEN];
					/* Buffer to hold the resource
					 * index entry.
					 */

			/* Construct the resource index entry */
			wptr = rsrcbuf;
			put_udword(&wptr, rsrc->type);
			put_uword(&wptr, rsrc->id);
			put_udword(&wptr, offset);

			/* Write the resource index entry */
			if (write(fd, rsrcbuf, PDB_RESOURCEIX_LEN) !=
			    PDB_RESOURCEIX_LEN)
			{
				fprintf(stderr, _("%s: Can't write resource "
						  "index entry for "
						  "\"%.*s\".\n"),
					"pdb_Write",
					PDB_DBNAMELEN, db->name);
				perror("write");
				return -1;
			}

			/* Bump 'offset' up to point to the offset of the
			 * next variable-sized thing in the file.
			 */
			offset += rsrc->data_len;
		}
	} else {
		/* It's a record database */
		struct pdb_record *rec;		/* Current record */

		/* Go through the list of records, writing each one */
		for (rec = db->rec_index.rec; rec != NULL; rec = rec->next)
		{
			static ubyte recbuf[PDB_RECORDIX_LEN];
					/* Buffer to hold the record index
					 * entry.
					 */

			/* Construct the record index entry */
			wptr = recbuf;

			/* Sanity check */
			if (rec->data_len == 0)
			{
				fprintf(stderr,
					_("\"%.*s\" record 0x%08lx has "
					  "length 0.\n"),
					PDB_DBNAMELEN, db->name,
					rec->id);
			}

			put_udword(&wptr, offset);
			put_ubyte(&wptr, merge_attributes(
				rec->flags,
				rec->category));
			put_ubyte(&wptr, (char) ((rec->id >> 16) & 0xff));
			put_ubyte(&wptr, (char) ((rec->id >> 8) & 0xff));
			put_ubyte(&wptr, (char) (rec->id & 0xff));

			/* Write the resource index entry */
			if (write(fd, recbuf, PDB_RECORDIX_LEN) !=
			    PDB_RECORDIX_LEN)
			{
				fprintf(stderr, _("%s: Can't write record "
						  "index entry for "
						  "\"%.*s\".\n"),
					"pdb_Write",
					PDB_DBNAMELEN, db->name);
				perror("write");
				return -1;
			}

			/* Bump 'offset' up to point to the offset of the
			 * next variable-sized thing in the file.
			 */
			offset += rec->data_len;
		}
	}

	/* Write the two useless NUL bytes */
	nul_buf[0] = nul_buf[1] = '\0';
	if (write(fd, nul_buf, 2) != 2)
	{
		fprintf(stderr, _("%s: Can't write the two useless NULs to "
				  "\"%.*s\".\n"),
			"pdb_Write",
			PDB_DBNAMELEN, db->name);
		perror("write");
		return -1;
	}

	/* Write the AppInfo block, if any */
	if (db->appinfo != NULL)
	{
		if (write(fd, db->appinfo, db->appinfo_len) !=
		    db->appinfo_len)
		{
			fprintf(stderr, _("%s: Can't write AppInfo block for "
					  "\"%.*s\".\n"),
				"pdb_Write",
				PDB_DBNAMELEN, db->name);
			perror("write");
			return -1;
		}
	}

	/* Write the sort block, if any */
	if (db->sortinfo != NULL)
	{
		if (write(fd, db->sortinfo, db->sortinfo_len) !=
		    db->sortinfo_len)
		{
			fprintf(stderr, _("%s: Can't write sort block for "
					  "\"%.*s\".\n"),
				"pdb_Write",
				PDB_DBNAMELEN, db->name);
			perror("write");
			return -1;
		}
	}

	/* Write the record/resource data */
	if (IS_RSRC_DB(db))
	{
		/* It's a resource database */
		struct pdb_resource *rsrc;

		/* Go through the list of resources, writing each one's
		 * data.
		 */
		for (rsrc = db->rec_index.rsrc;
		     rsrc != NULL;
		     rsrc = rsrc->next)
		{
			/* Write the data */
			if (write(fd, rsrc->data, rsrc->data_len) !=
			    rsrc->data_len)
			{
				fprintf(stderr, _("%s: Can't write resource "
						  "data for \"%.*s\".\n"),
					"pdb_Write",
					PDB_DBNAMELEN, db->name);
				perror("write");
				return -1;
			}
		}
	} else {
		/* It's a record database */
		struct pdb_record *rec;

		/* Go through the list of records, writing each one's data. */
		for (rec = db->rec_index.rec; rec != NULL; rec = rec->next)
		{
			/* Write the data */
			if (write(fd, rec->data, rec->data_len) !=
			    rec->data_len)
			{
				fprintf(stderr,
					_("%s: Can't write record data for "
					  "\"%.*s\".\n"),
					"pdb_Write",
					PDB_DBNAMELEN, db->name);
				perror("write");
				return -1;
			}
		}
	}

	return 0;		/* Success */
}

#if 0
/* pdb_FindRecordByID
 * Find the record in 'db' whose ID is 'id'. Return a pointer to it. If no
 * such record exists, or in case of error, returns NULL.
 */
struct pdb_record *
pdb_FindRecordByID(
	const struct pdb *db,
	const udword id)
{
	struct pdb_record *rec;

	/* Walk the list of records, comparing IDs */
	for (rec = db->rec_index.rec; rec != NULL; rec = rec->next)
	{
		if (rec->id == id)
			return rec;
	}

	return NULL;		/* Couldn't find it */
}

/* pdb_FindRecordByIndex
 * Find the 'index'th record in 'db', and return a pointer to it. If no
 * such record exists, or in case of error, return NULL.
 */
struct pdb_record *
pdb_FindRecordByIndex(
	const struct pdb *db,	/* Database to look in */
	const uword index)	/* Index of the record to look for */
{
	struct pdb_record *rec;
	int i;

	/* Walk the list, decrementing the count as we go along. If it
	 * reaches 0, we've found the record.
	 */
	rec = db->rec_index.rec;
	for (i = index; i > 0; i--)
	{
		if (rec == NULL)
			/* Oops! We've fallen off the end of the list */
			return NULL;
		rec = rec->next;
	}

	return rec;		/* Success */
}

/* pdb_NextRecord
 * Find the next record after 'rec' in 'db', and return a pointer to it. If
 * 'rec' is the last record in the list, return NULL.
 */
struct pdb_record *
pdb_NextRecord(const struct pdb *db,	/* Database to look in */
	       const struct pdb_record *rec)
					/* Return 'rec's successor */
{
	return rec->next;
}

/* pdb_DeleteRecordByID
 * Find the record whose unique ID is 'id' and delete it from 'db'. If the
 * record isn't found, well, that's okay; we wanted to delete it anyway.
 * Returns 0 if successful, -1 in case of error.
 */
int
pdb_DeleteRecordByID(
	struct pdb *db,
	const udword id)
{
	struct pdb_record *rec;		/* Record we're looking at */
	struct pdb_record *last;	/* Last record we saw */

	if (IS_RSRC_DB(db))
		/* This only works with record databases */
		return -1;

	/* Look through the list of records */
	last = NULL;		/* Haven't seen any records yet */
	for (rec = db->rec_index.rec; rec != NULL; rec = rec->next)
	{
		/* See if the ID matches */
		if (rec->id == id)
		{
			/* Found it */

			/* XXX - Presumably better to use pdb_FreeRecord() */
			/* Free 'rec's data */
			if (rec->data != NULL)
				free(rec->data);

			/* Cut 'rec' out of the list. The first element of
			 * the list is a special case.
			 */
			if (last == NULL)
				db->rec_index.rec = rec->next;
			else
				last->next = rec->next;

			free(rec);		/* Free it */
			db->numrecs--;		/* Decrement record count */
			
			return 0;	/* Success */
		}

		last = rec;	/* Remember what we just saw */
	}

	/* Couldn't find it. Oh, well. Call it a success anyway. */
	return 0;
}
#endif

/* pdb_AppendRecord
 * Append a new record to 'db's record list. 'newrec' is not copied, so it
 * is important that the caller not free it afterwards.
 */
/* XXX - Ought to make sure that the ID is unique */
int
pdb_AppendRecord(struct pdb *db,
		 struct pdb_record *newrec)
{
	struct pdb_record *rec;

	/* Sanity check */
	if (IS_RSRC_DB(db))
		/* This only works with record databases */
		return -1;

	/* Check to see if the list is empty */
	if (db->rec_index.rec == NULL)
	{
		db->rec_index.rec = newrec;
		newrec->next = NULL;

		db->numrecs++;		/* Bump record counter */

		return 0;		/* Success */
	}

	/* Walk the list to find its end */
	for (rec = db->rec_index.rec; rec->next != NULL; rec = rec->next)
		;
	rec->next = newrec;
	newrec->next = NULL;

	db->numrecs++;			/* Bump record counter */

	return 0;			/* Success */
}

/* pdb_AppendResource
 * Append a new resource to 'db's resource list. 'newrsrc' is not copied,
 * so it is important that the caller not free it afterwards.
 */
int
pdb_AppendResource(struct pdb *db,
		   struct pdb_resource *newrsrc)
{
	struct pdb_resource *rsrc;

	/* Sanity check */
	if (!IS_RSRC_DB(db))
		/* This only works with resource databases */
		return -1;

	/* Check to see if the list is empty */
	if (db->rec_index.rsrc == NULL)
	{
		db->rec_index.rsrc = newrsrc;
		newrsrc->next = NULL;

		db->numrecs++;		/* Bump resource counter */

		return 0;		/* Success */
	}

	/* Walk the list to find its end */
	for (rsrc = db->rec_index.rsrc; rsrc->next != NULL; rsrc = rsrc->next)
		;
	rsrc->next = newrsrc;
	newrsrc->next = NULL;

	db->numrecs++;			/* Bump resource counter */

	return 0;			/* Success */
}

/* pdb_InsertRecord
 * Insert 'newrec' into 'db', just after 'prev'. If 'prev' is NULL,
 * 'newrec' is inserted at the beginning of the list.
 * Returns 0 if successful, -1 otherwise.
 * 'newrec' is not copied, so it is important that the caller not free it.
 */
int
pdb_InsertRecord(struct pdb *db,	/* The database to insert into */
		 struct pdb_record *prev,
					/* Insert after this record */
		 struct pdb_record *newrec)
					/* The record to insert */
{
	/* If 'prev' is NULL, insert at the beginning of the list */
	if (prev == NULL)
	{
		newrec->next = db->rec_index.rec;
		db->rec_index.rec = newrec;
		db->numrecs++;		/* Increment record count */

		return 0;		/* Success */
	}

	/* XXX - This function doesn't actually check to make sure that
	 * 'prev' is in 'db'. You could really fuck yourself over with
	 * this.
	 * So make it a documented requirement.
	 */
	/* The new record goes in the middle of the list. Insert it. */
	newrec->next = prev->next;
	prev->next = newrec;
	db->numrecs++;			/* Increment record count */

	return 0;			/* Success */
}

#if 0
/* pdb_InsertResource
 * Insert 'newrsrc' into 'db', just after 'prev'. If 'prev' is NULL, 'newrsrc'
 * is inserted at the beginning of the list.
 * Returns 0 if successful, -1 otherwise.
 * 'newrec' is not copied, so it is important that the caller not free it.
 */
int
pdb_InsertResource(struct pdb *db,	/* The database to insert into */
		   struct pdb_resource *prev,
					/* Insert after this resource */
		   struct pdb_resource *newrsrc)
					/* The resource to insert */
{
	/* If 'prev' is NULL, insert at the beginning of the list */
	if (prev == NULL)
	{
		newrsrc->next = db->rec_index.rsrc;
		db->rec_index.rsrc = newrsrc;
		db->numrecs++;		/* Increment record count */

		return 0;		/* Success */
	}

	/* XXX - This function doesn't actually check to make sure that
	 * 'prev' is in 'db'. You could really fuck yourself over with
	 * this.
	 * So make it a documented requirement.
	 */
	/* The new resource goes in the middle of the list. Insert it. */
	newrsrc->next = prev->next;
	prev->next = newrsrc;
	db->numrecs++;			/* Increment record count */

	return 0;			/* Success */
}
#endif

/* new_Record
 * Create a new record from the given arguments, and return a pointer to
 * it. Returns NULL in case of error.
 * The record data is copied, so the caller needs to take care of freeing
 * 'data'.
 *
 * The 'attributes' and 'category' arguments are combined into one field:
 * if the new record is deleted, then the category is silently dropped.
 * Otherwise, the category occupies the bottom 4 bits of the
 * attributes/category field.
 */
struct pdb_record *
new_Record(const ubyte flags,
	   const ubyte category,
	   const udword id,
	   const uword len,
	   const ubyte *data)
{
	struct pdb_record *retval;

	PDB_TRACE(6)
	{
		fprintf(stderr, "new_Record: Creating new record:\n");
		fprintf(stderr, "\tflags == 0x%02x\n", flags);
		fprintf(stderr, "\tcategory == 0x%02x\n", category);
		fprintf(stderr, "\tid == 0x%08lx\n", id);
		fprintf(stderr, "\tlen == %d\n", len);
		debug_dump(stderr, "NEW", data, len);
	}

	/* Allocate the record to be returned */
	if ((retval = (struct pdb_record *) malloc(sizeof(struct pdb_record)))
	    == NULL)
	{
		fprintf(stderr, _("%s: Out of memory.\n"),
			"new_Record");
		return NULL;
	}

	/* Initialize the new record */
	retval->next = NULL;
	retval->offset = 0L;
	retval->flags = flags;
	retval->category = category;
	retval->id = id;

	/* Allocate space to put the record data */
	if (len == 0)
	{
		/* Special case: the record has no data (e.g., this is an
		 * expunged record).
		 */
		retval->data_len = len;
		retval->data = NULL;
		return retval;
	}

	if ((retval->data = (ubyte *) malloc(len)) == NULL)
	{
		/* Couldn't allocate data portion of record */
		fprintf(stderr, _("%s: can't allocate data.\n"),
			"new_Record");
		free(retval);
		return NULL;
	}

	/* Copy the data to the new record */
	retval->data_len = len;
	memcpy(retval->data, data, len);

	return retval;		/* Success */
}

/* new_Resource
 * Create a new resource from the given arguments, and return a pointer to
 * it. Returns NULL in case of error.
 * The resource data is copied, so the caller needs to take care of freeing
 * 'data'.
 */
struct pdb_resource *
new_Resource(const udword type,
	     const uword id,
	     const uword len,
	     const ubyte *data)
{
	struct pdb_resource *retval;

	PDB_TRACE(6)
	{
		fprintf(stderr, "new_Resource: Creating new resource:\n");
		fprintf(stderr, "\ttype == 0x%08lx (%c%c%c%c)\n",
			type,
			(int) ((type >> 24) & 0xff),
			(int) ((type >> 16) & 0xff),
			(int) ((type >>  8) & 0xff),
			(int)  (type        & 0xff));
		fprintf(stderr, "\tid == 0x%04x\n", id);
		fprintf(stderr, "\tlen == %d\n", len);
		debug_dump(stderr, "NEW", data, len);
	}

	/* Allocate the resource to be returned */
	if ((retval = (struct pdb_resource *)
	     malloc(sizeof(struct pdb_resource))) == NULL)
	{
		fprintf(stderr, _("%s: Out of memory.\n"),
			"new_Resource");
		return NULL;
	}

	/* Initialize the new resource */
	retval->next = NULL;
	retval->offset = 0L;
	retval->type = type;
	retval->id = id;

	/* Allocate space to put the resource data */
	if (len == 0)
	{
		/* Special case: zero-length resource (dunno if this should
		 * ever happen, but this way we avoid malloc(0).
		 */
		retval->data_len = len;
		retval->data = NULL;
		return retval;
	}

	if ((retval->data = (ubyte *) malloc(len)) == NULL)
	{
		/* Couldn't allocate data portion of resource */
		fprintf(stderr, _("%s: can't allocate data.\n"),
			"new_Resource");
		free(retval);
		return NULL;
	}

	/* Copy the data to the new resource */
	retval->data_len = len;
	memcpy(retval->data, data, len);

	return retval;		/* Success */
}

/* pdb_CopyRecord
 * Make a copy of record 'rec' in database 'db' (and its data), and return
 * it. The new record is allocated by pdb_CopyRecord(), so the caller has
 * to take care of freeing it.
 * Returns a pointer to the new copy, or NULL in case of error.
 */
struct pdb_record *pdb_CopyRecord(
	const struct pdb *db,
	const struct pdb_record *rec)
{
	struct pdb_record *retval;

	/* Allocate the record to be returned */
	if ((retval = (struct pdb_record *) malloc(sizeof(struct pdb_record)))
	    == NULL)
	{
		fprintf(stderr, _("%s: Out of memory.\n"),
			"pdb_CopyRecord");
		return NULL;
	}

	retval->next = NULL;		/* For cleanliness */

	/* Copy the old record to the new copy */
	retval->offset	= rec->offset;
	retval->flags	= rec->flags;
	retval->category = rec->category;
	retval->id	= rec->id;

	/* Allocate space for the record data itself */
	if ((retval->data = (ubyte *) malloc(rec->data_len)) == NULL)
	{
		fprintf(stderr, _("%s: can't allocate record data for "
				  "\"%.*s\".\n"),
			"pdb_CopyRecord",
			PDB_DBNAMELEN, db->name);
		free(retval);
		return NULL;
	}

	/* Copy the record data */
	retval->data_len = rec->data_len;
	memcpy(retval->data, rec->data, retval->data_len);

	return retval;		/* Success */
}

/* pdb_CopyResource
 * Make a copy of resource 'rsrc' in database 'db' (and its data), and
 * return it. The new record is allocated by pdb_CopyResource(), so the
 * caller has to take care of freeing it.
 * Returns a pointer to the new copy, or NULL in case of error.
 */
struct pdb_resource *pdb_CopyResource(
	const struct pdb *db,
	const struct pdb_resource *rsrc)
{
	struct pdb_resource *retval;

	/* Allocate the resource to be returned */
	if ((retval = (struct pdb_resource *)
	     malloc(sizeof(struct pdb_resource))) == NULL)
	{
		fprintf(stderr, _("%s: Out of memory.\n"),
			"pdb_CopyResource");
		return NULL;
	}

	retval->next = NULL;		/* For cleanliness */

	/* Copy the old resource to the new copy */
	retval->type = rsrc->type;
	retval->id = rsrc->id;
	retval->offset = rsrc->offset;

	/* Allocate space for the record data itself */
	if ((retval->data = (ubyte *) malloc(rsrc->data_len)) == NULL)
	{
		fprintf(stderr, _("%s: can't allocate resource data for "
				  "\"%.*s\".\n"),
			"pdb_CopyResource",
			PDB_DBNAMELEN, db->name);
		free(retval);
		return NULL;
	}

	/* Copy the resource data */
	retval->data_len = rsrc->data_len;
	memcpy(retval->data, rsrc->data, retval->data_len);

	return retval;		/* Success */
}

/*** Helper functions ***/

/* get_file_length
 * Return the length of a file, in bytes. In case of error, returns ~0.
 */
static uword
get_file_length(int fd)
{
	off_t here;
	off_t eof;

	/* Get the current position within the file */
	here = lseek(fd, 0L, SEEK_CUR);
	if (here < 0)
		/* The file isn't seekable, presumably either because it
		 * isn't open, or because it's a pipe/socket/FIFO/tty.
		 */
		return ~0;

	/* Go to the end of the file */
	eof = lseek(fd, 0L, SEEK_END);

	/* And return to where we were before */
	lseek(fd, here, SEEK_SET);

	return (uword) (eof - here);
}

/* pdb_LoadHeader
 * Read the header of a pdb file, and fill in the appropriate fields in
 * 'db'.
 */
int
pdb_LoadHeader(int fd,
	       struct pdb *db)
{
	int err;
	static ubyte buf[PDB_HEADER_LEN];
				/* Buffer to hold the file header */
	const ubyte *rptr;	/* Pointer into buffers, for reading */

	/* Read the header */
	if ((err = read(fd, buf, PDB_HEADER_LEN)) != PDB_HEADER_LEN)
	{
		perror("pdb_LoadHeader: read");
		return -1;
	}

	/* Parse the database header */
	rptr = buf;
	memcpy(db->name, buf, PDB_DBNAMELEN);
	rptr += PDB_DBNAMELEN;
	db->attributes = get_uword(&rptr);
	db->version = get_uword(&rptr);
	db->ctime = get_udword(&rptr);
	db->mtime = get_udword(&rptr);
	db->baktime = get_udword(&rptr);
	db->modnum = get_udword(&rptr);
	db->appinfo_offset = get_udword(&rptr);
	db->sortinfo_offset = get_udword(&rptr);
	db->type = get_udword(&rptr);
	db->creator = get_udword(&rptr);
	db->uniqueIDseed = get_udword(&rptr);

	PDB_TRACE(5)
	{
		time_t t;

		fprintf(stderr, "\tname: \"%s\"\n", db->name);
		fprintf(stderr, "\tattributes: 0x%04x", db->attributes);
		if (db->attributes & PDB_ATTR_RESDB)
			fprintf(stderr, " RESDB");
		if (db->attributes & PDB_ATTR_RO) fprintf(stderr, " RO");
		if (db->attributes & PDB_ATTR_APPINFODIRTY)
			fprintf(stderr, " APPINFODIRTY");
		if (db->attributes & PDB_ATTR_BACKUP)
			fprintf(stderr, " BACKUP");
		if (db->attributes & PDB_ATTR_OKNEWER)
			fprintf(stderr, " OKNEWER");
		if (db->attributes & PDB_ATTR_RESET) fprintf(stderr, " RESET");
		if (db->attributes & PDB_ATTR_NOCOPY)
			fprintf(stderr, " NOCOPY");
		if (db->attributes & PDB_ATTR_STREAM)
			fprintf(stderr, " STREAM");
		if (db->attributes & PDB_ATTR_OPEN)
			fprintf(stderr, " OPEN");
		fprintf(stderr, "\n");
		fprintf(stderr, "\tversion: %u\n", db->version);
		t = db->ctime - EPOCH_1904;
		fprintf(stderr, "\tctime: %lu %s", db->ctime,
			ctime(&t));
		t = db->mtime - EPOCH_1904;
		fprintf(stderr, "\tmtime: %lu %s", db->mtime,
			ctime(&t));
		t = db->baktime - EPOCH_1904;
		fprintf(stderr, "\tbaktime: %lu %s", db->baktime,
			ctime(&t));
		fprintf(stderr, "\tmodnum: %ld\n", db->modnum);
		fprintf(stderr, "\tappinfo_offset: 0x%08lx\n",
			db->appinfo_offset);
		fprintf(stderr, "\tsortinfo_offset: 0x%08lx\n",
			db->sortinfo_offset);
		fprintf(stderr, "\ttype: '%c%c%c%c' (0x%08lx)\n",
			(char) (db->type >> 24) & 0xff,
			(char) (db->type >> 16) & 0xff,
			(char) (db->type >> 8) & 0xff,
			(char) db->type & 0xff,
			db->type);
		fprintf(stderr, "\tcreator: '%c%c%c%c' (0x%08lx)\n",
			(char) (db->creator >> 24) & 0xff,
			(char) (db->creator >> 16) & 0xff,
			(char) (db->creator >> 8) & 0xff,
			(char) db->creator & 0xff,
			db->creator);
		fprintf(stderr, "\tuniqueIDseed: %ld\n", db->uniqueIDseed);
	}

	return 0;		/* Success */
}

/* pdb_LoadRecListHeader
 * Load the record list header from a pdb file, and fill in the appropriate
 * fields in 'db'.
 */
static int
pdb_LoadRecListHeader(int fd,
		      struct pdb *db)
{
	int err;
	static ubyte buf[PDB_RECORDLIST_LEN];
	const ubyte *rptr;	/* Pointer into buffers, for reading */

	/* Read the record list header */
	if ((err = read(fd, buf, PDB_RECORDLIST_LEN)) != PDB_RECORDLIST_LEN)
	{
		perror("pdb_LoadRecListHeader: read2");
		return -1;
	}

	/* Parse the record list */
	rptr = buf;
	db->next_reclistID = get_udword(&rptr);
	db->numrecs = get_uword(&rptr);

	PDB_TRACE(6)
	{
		fprintf(stderr, "\tnextID: %ld\n", db->next_reclistID);
		fprintf(stderr, "\tlen: %u\n", db->numrecs);
	}

	return 0;
}

/* pdb_LoadRsrcIndex
 * Read the resource index from a resource database file, and fill in the
 * appropriate fields in 'db'.
 */
static int
pdb_LoadRsrcIndex(int fd,
		  struct pdb *db)
{
	int i;
	int err;
	uword totalrsrcs;	/* The real number of resources in the
				 * database.
				 */

	totalrsrcs = db->numrecs;	/* Get the number of resources in
					 * the database. It is necessary to
					 * remember this here because
					 * pdb_AppendResource() increments
					 * db->numrecs in the name of
					 * convenience.
					 */

	if (totalrsrcs == 0)
	{
		/* There are no resources in this file */
		db->rec_index.rsrc = NULL;
		return 0;
	}

	/* Read the resource index */
	for (i = 0; i < totalrsrcs; i++)
	{
		static ubyte inbuf[PDB_RESOURCEIX_LEN];
					/* Input buffer */
		const ubyte *rptr;	/* Pointer into buffers, for reading */
		struct pdb_resource *rsrc;
					/* New resource entry */

		/* Allocate the resource entry */
		if ((rsrc = (struct pdb_resource *)
		     malloc(sizeof(struct pdb_resource)))
		    == NULL)
			return -1;
		/* Scribble zeros all over it, just in case */
		memset((void *) rsrc, 0, sizeof(struct pdb_resource));

		/* Read the next resource index entry */
		if ((err = read(fd, inbuf, PDB_RESOURCEIX_LEN)) !=
		    PDB_RESOURCEIX_LEN)
			return -1;

		/* Parse it */
		rptr = inbuf;
		rsrc->type = get_udword(&rptr);
		rsrc->id = get_uword(&rptr);
		rsrc->offset = get_udword(&rptr);

		PDB_TRACE(6)
		{
			fprintf(stderr,
				"\tResource %d: type '%c%c%c%c' (0x%08lx), "
				"id %u, offset 0x%08lx\n",
				i,
				(char) (rsrc->type >> 24) & 0xff,
				(char) (rsrc->type >> 16) & 0xff,
				(char) (rsrc->type >> 8) & 0xff,
				(char) rsrc->type & 0xff,
				rsrc->type,
				rsrc->id,
				rsrc->offset);
		}

		/* Append the new resource to the list */
		pdb_AppendResource(db, rsrc);	/* XXX - Error-checking */
		db->numrecs = totalrsrcs;	/* Kludge */
	}

	return 0;
}

/* pdb_LoadRecIndex
 * Read the record index from a record database file, and fill in the
 * appropriate fields in 'db'.
 */
static int
pdb_LoadRecIndex(int fd,
		 struct pdb *db)
{
	int i;
	int err;
	uword totalrecs;	/* The real number of records in the
				 * database.
				 */

	totalrecs = db->numrecs;	/* Get the number of records in the
					 * database. It is necessary to
					 * remember this here because
					 * pdb_AppendResource() increments
					 * db->numrecs in the name of
					 * convenience.
					 */

	if (totalrecs == 0)
	{
		/* There are no records in this file */
		db->rec_index.rec = NULL;
		return 0;
	}

	/* Read the record index */
	/* XXX - It would be a Good Thing to check for zero-length records
	 * here. They've been known to appear as a result of a broken
	 * conduit.
	 */
	for (i = 0; i < totalrecs; i++)
	{
		static ubyte inbuf[PDB_RECORDIX_LEN];
					/* Input buffer */
		const ubyte *rptr;	/* Pointer into buffers, for reading */
		struct pdb_record *rec;
					/* New record entry */
		ubyte attributes;	/* Combined flags+category field */

		/* Allocate the record entry */
		if ((rec = (struct pdb_record *)
		     malloc(sizeof(struct pdb_record)))
		    == NULL)
		{
			fprintf(stderr, _("%s: Out of memory.\n"),
				"pdb_LoadRecIndex");
			return -1;
		}

		/* Scribble zeros all over it, just in case */
		memset((void *) rec, 0, sizeof(struct pdb_record));

		/* Read the next record index entry */
		if ((err = read(fd, inbuf, PDB_RECORDIX_LEN)) !=
		    PDB_RECORDIX_LEN)
		{
			fprintf(stderr, _("%s: error reading record index "
					  "entry for \"%.*s\" (%d bytes): "
					  "%d.\n"),
				"LoadRecIndex",
				PDB_DBNAMELEN, db->name,
				PDB_RECORDIX_LEN,
				err);
			perror("read");
			free(rec);
			return -1;
		}

		/* Parse it */
		rptr = inbuf;
		rec->offset = get_udword(&rptr);
		attributes = get_ubyte(&rptr);
		split_attributes(attributes, &(rec->flags), &(rec->category));

		rec->id =
			((udword) (get_ubyte(&rptr) << 16)) |
			((udword) (get_ubyte(&rptr) << 8)) |
			((udword) get_ubyte(&rptr));

		PDB_TRACE(6)
			fprintf(stderr,
				"\tRecord %d: offset 0x%08lx, flags 0x%02x, "
				" category 0x%02x, ID 0x%08lx\n",
				i,
				rec->offset,
				rec->flags,
				rec->category,
				rec->id);

		/* Append the new record to the database */
		pdb_AppendRecord(db, rec); 	/* XXX - Error-checking */
		db->numrecs = totalrecs;	/* Kludge */
	}

	return 0;
}

/* pdb_LoadAppBlock
 * Read the AppInfo block from a database file, and fill in the appropriate
 * fields in 'db'. If the file doesn't have an AppInfo block, set it to
 * NULL.
 */
static int
pdb_LoadAppBlock(int fd,
		 struct pdb *db)
{
	int err;
	udword next_off;		/* Offset of the next thing in the file
				 * after the AppInfo block */
	off_t offset;		/* Offset into file, for checking */

	/* Check to see if there even *is* an AppInfo block */
	if (db->appinfo_offset == 0L)
	{
		/* Nope */
		db->appinfo_len = 0L;
		db->appinfo = NULL;
		return 0;
	}

	/* Figure out how long the AppInfo block is, by comparing its
	 * offset to that of the next thing in the file.
	 */
	if (db->sortinfo_offset > 0L)
		/* There's a sort block */
		next_off = db->sortinfo_offset;
	else if (db->numrecs > 0)
	{
		/* There's no sort block, but there are records. Get the
		 * offset of the first one.
		 */
		if (IS_RSRC_DB(db))
			next_off = db->rec_index.rsrc->offset;
		else
			next_off = db->rec_index.rec->offset;
	} else
		/* There is neither sort block nor records, so the AppInfo
		 * block must go to the end of the file.
		 */
		next_off = db->file_size;

	/* Subtract the AppInfo block's offset from that of the next thing
	 * in the file to get the AppInfo block's length.
	 */
	db->appinfo_len = next_off - db->appinfo_offset;

	/* This is probably paranoid, but what the hell */
	if (db->appinfo_len == 0L)
	{
		/* An effective no-op */
		db->appinfo = NULL;
		return 0;
	}

	/* Now that we know the length of the AppInfo block, allocate space
	 * for it and read it.
	 */
	if ((db->appinfo = (ubyte *) malloc(db->appinfo_len)) == NULL)
	{
		fprintf(stderr, _("%s: Out of memory.\n"),
			"pdb_LoadAppBlock");
		return -1;
	}

	/* Just out of paranoia, make sure we're at the correct offset in
	 * the file. Since the two NULs may or may not have appeared in the
	 * file, the only thing that it makes sense to check is whether
	 * we've already passed the beginning of the AppInfo block, as
	 * given by its offset in the header.
	 */
	offset = lseek(fd, 0L, SEEK_CUR);	/* Find out where we are */
	if (offset != db->appinfo_offset)
	{
		if (offset > (off_t) db->appinfo_offset)
		{
			/* Oops! We're in the wrong place */
			fprintf(stderr, _("Warning: AppInfo block in \"%.*s\" "
					  "isn't where I thought it would "
					  "be.\n"
					  "Expected 0x%lx, but we're at "
					  "0x%lx.\n"),
				PDB_DBNAMELEN, db->name,
				db->appinfo_offset, (long) offset);
		}

		/* Try to recover */
		offset = lseek(fd, db->appinfo_offset, SEEK_SET);
					/* Go to where the AppInfo block
					 * ought to be */
		if (offset < 0)
		{
			/* Something's wrong */
			fprintf(stderr, _("Can't find the AppInfo block in "
					  "\"%.*s\"!\n"),
				PDB_DBNAMELEN, db->name);
			return -1;
		}
	}

	/* Read the AppInfo block */
	if ((err = read(fd, db->appinfo, db->appinfo_len)) != db->appinfo_len)
	{
		perror("pdb_LoadAppBlock: read");
		return -1;
	}
	PDB_TRACE(6)
		debug_dump(stderr, "<APP", db->appinfo, db->appinfo_len);

	return 0; 
}

/* pdb_LoadSortBlock
 * Read the sort block from a database file, and fill in the appropriate
 * fields in 'db'. If the file doesn't have a sort block, set it to NULL.
 *
 * XXX - Largely untested, since not that many databases have sort blocks.
 * But it's basically just a clone of pdb_LoadAppBlock(), so it should be
 * okay.
 */
static int
pdb_LoadSortBlock(int fd,
		 struct pdb *db)
{
	int err;
	localID next_off;		/* Offset of the next thing in the file
				 * after the sort block */
	off_t offset;		/* Offset into file, for checking */

	/* Check to see if there even *is* a sort block */
	if (db->sortinfo_offset == 0L)
	{
		/* Nope */
		db->sortinfo_len = 0L;
		db->sortinfo = NULL;
		return 0;
	}

	/* Figure out how long the sort block is, by comparing its
	 * offset to that of the next thing in the file.
	 */
	if (db->numrecs > 0)
	{
		/* There are records. Get the offset of the first one.
		 */
		if (IS_RSRC_DB(db))
			next_off = db->rec_index.rsrc->offset;
		else
			next_off = db->rec_index.rec->offset;
	} else
		/* There are no records, so the sort block must go to the
		 * end of the file.
		 */
		next_off = db->file_size;

	/* Subtract the sort block's offset from that of the next thing
	 * in the file to get the sort block's length.
	 */
	db->sortinfo_len = next_off - db->sortinfo_offset;

	/* This is probably paranoid, but what the hell */
	if (db->sortinfo_len == 0L)
	{
		/* An effective no-op */
		db->sortinfo = NULL;
		return 0;
	}

	/* Now that we know the length of the sort block, allocate space
	 * for it and read it.
	 */
	if ((db->sortinfo = (ubyte *) malloc(db->sortinfo_len)) == NULL)
	{
		fprintf(stderr, _("%s: Out of memory.\n"),
			"pdb_LoadSortBlock");
		return -1;
	}

	/* Just out of paranoia, make sure we're at the correct offset in
	 * the file. Since the two NULs may or may not have appeared in the
	 * file, the only thing that it makes sense to check is whether
	 * we've already passed the beginning of the sort block, as given
	 * by its offset in the header.
	 */
	offset = lseek(fd, 0L, SEEK_CUR);	/* Find out where we are */
	if (offset != db->sortinfo_offset)
	{
		if (offset > (off_t) db->sortinfo_offset)
		{
			/* Oops! We're in the wrong place */
			fprintf(stderr, _("Warning: sort block in \"%.*s\" "
					  "isn't where I thought it would "
					  "be.\n"
					  "Expected 0x%lx, but we're at "
					  "0x%lx.\n"),
				PDB_DBNAMELEN, db->name,
				db->sortinfo_offset, (long) offset);
		}

		/* Try to recover */
		offset = lseek(fd, db->sortinfo_offset, SEEK_SET);
					/* Go to where the sort block
					 * ought to be */
		if (offset < 0)
		{
			/* Something's wrong */
			fprintf(stderr, _("Can't find the sort block in "
					  "\"%.*s\"!\n"),	
				PDB_DBNAMELEN, db->name);
			return -1;
		}
	}

	/* Read the sort block */
	if ((err = read(fd, db->sortinfo, db->sortinfo_len)) !=
	    db->sortinfo_len)
	{
		perror("pdb_LoadSortBlock: read");
		return -1;
	}
	PDB_TRACE(6)
		debug_dump(stderr, "<SORT", db->sortinfo, db->sortinfo_len); 

	return 0; 
}

/* pdb_LoadResources
 * Read each resource in turn from a resource database file.
 */
static int
pdb_LoadResources(int fd,
		  struct pdb *db)
{
	int i;
	int err;
	struct pdb_resource *rsrc;

	/* This assumes that the resource list has already been created by
	 * 'pdb_LoadRsrcIndex()'.
	 */
	for (i = 0, rsrc = db->rec_index.rsrc;
	     i < db->numrecs;
	     i++, rsrc = rsrc->next)
	{
		off_t offset;		/* Current offset, for checking */
		udword next_off;	/* Offset of next resource in file */

		/* Sanity check: make sure we haven't stepped off the end
		 * of the list.
		 */
		if (rsrc == NULL)
		{
			fprintf(stderr, _("Hey! I can't find the %dth "
					  "resource in \"%.*s\"!\n"),
				i,
				PDB_DBNAMELEN, db->name);
			return -1;
		}

		PDB_TRACE(5)
			fprintf(stderr,
				"Reading resource %d (type '%c%c%c%c')\n",
				i,
				(char) (rsrc->type >> 24) & 0xff,
				(char) (rsrc->type >> 16) & 0xff,
				(char) (rsrc->type >> 8) & 0xff,
				(char) rsrc->type & 0xff);

		/* Out of paranoia, make sure we're in the right place.
		 * Since the two NULs may or may not have appeared in the
		 * file, the only thing that it makes sense to check is
		 * whether we've already passed the beginning of the
		 * resource, as given by its offset in the resource index.
		 */
		offset = lseek(fd, 0L, SEEK_CUR);
					/* Find out where we are now */
		if (offset != rsrc->offset)
		{
			if (offset > (off_t) rsrc->offset)
			{
				fprintf(stderr, _("Warning: resource %d in "
						  "\"%.*s\" isn't where "
						  "I thought it would be.\n"
						  "Expected 0x%lx, but we're "
						  "at 0x%lx.\n"),
					i,
					PDB_DBNAMELEN, db->name,
					rsrc->offset, (long) offset);
			}

			/* Try to recover */
			offset = lseek(fd, rsrc->offset, SEEK_SET);
						/* Go to where this
						 * resource ought to be.
						 */
			if (offset < 0)
			{
				/* Something's wrong */
				fprintf(stderr, _("Can't find resource %d in "
						  "\"%.*s\".\n"),
					i,
					PDB_DBNAMELEN, db->name);
				return -1;
			}
		}

		/* Okay, now that we're in the right place, find out what
		 * the next thing in the file is: its offset will tell us
		 * how much to read.
		 * It's debatable whether 'i' or 'rsrc' should be
		 * authoritative for determining the offset of the next
		 * resource. I'm going to choose 'rsrc', since I think
		 * that's more likely to be immune to fencepost errors. The
		 * two should, however, be equivalent. In fact, it might be
		 * a Good Thing to add a check to make sure.
		 */
		if (rsrc->next == NULL)
		{
			/* This is the last resource in the file, so it
			 * goes to the end of the file.
			 */
			next_off = db->file_size;
		} else {
			/* This isn't the last resource. Find the next
			 * one's offset.
			 */
			next_off = rsrc->next->offset;
		}

		/* Subtract this resource's index from that of the next
		 * thing, to get the size of this resource.
		 */
		rsrc->data_len = (uword) (next_off - rsrc->offset);

		/* Allocate space for this resource */
		if ((rsrc->data = (ubyte *) malloc(rsrc->data_len)) == NULL)
		{
			fprintf(stderr, _("%s: Out of memory.\n"),
				"pdb_LoadResources");
			return -1;
		}

		/* Read the resource */
		if ((err = read(fd, rsrc->data, rsrc->data_len)) !=
		    rsrc->data_len)
		{
			fprintf(stderr, _("Can't read resource %d in "
					  "\"%.*s\".\n"),
				i,
				PDB_DBNAMELEN, db->name);
			perror("pdb_LoadResources: read");
			return -1;
		}
		PDB_TRACE(6)
		{
			fprintf(stderr, "Contents of resource %d:\n", i);
			debug_dump(stderr, "<RSRC", rsrc->data,
				   rsrc->data_len);
		}
	}

	return 0;		/* Success */
}

/* pdb_LoadRecords
 * Read each record in turn from a record database file.
 */
static int
pdb_LoadRecords(int fd,
		struct pdb *db)
{
	int i;
	int err;
	struct pdb_record *rec;

	/* This assumes that the record list has already been created by
	 * 'pdb_LoadRecIndex()'.
	 */
	for (i = 0, rec = db->rec_index.rec;
	     i < db->numrecs;
	     i++, rec = rec->next)
	{
		off_t offset;		/* Current offset, for checking */
		localID next_off;	/* Offset of next resource in file */

		/* Sanity check: make sure we haven't stepped off the end
		 * of the list.
		 */
		if (rec == NULL)
		{
			fprintf(stderr, _("Hey! I can't find the %dth "
					  "record in \"%.*s\"!\n"),
				i,
				PDB_DBNAMELEN, db->name);
			return -1;
		}

		PDB_TRACE(5)
			fprintf(stderr, "Reading record %d (id 0x%08lx)\n",
				i, rec->id);

		/* Out of paranoia, make sure we're in the right place.
		 * Since the two NULs may or may not have appeared in the
		 * file, the only thing that it makes sense to check is
		 * whether we've already passed the beginning of the
		 * record, as given by its offset in the record index.
		 */
		offset = lseek(fd, 0L, SEEK_CUR);
					/* Find out where we are now */
		if (offset != rec->offset)
		{
			if (offset > (off_t) rec->offset)
			{
				fprintf(stderr, _("Warning: record %d in "
						  "\"%.*s\" isn't where "
						  "I thought it would be.\n"
						  "Expected 0x%lx, but we're "
						  "at 0x%lx.\n"),
					i,
					PDB_DBNAMELEN, db->name,
					rec->offset, (long) offset);
			}

			/* Try to recover */
			offset = lseek(fd, rec->offset, SEEK_SET);
						/* Go to where this record
						 * ought to be. */
			if (offset < 0)
			{
				/* Something's wrong */
				fprintf(stderr, _("Can't find record %d in "
						  "\"%.*s\".\n"),
					i,
					PDB_DBNAMELEN, db->name);
				return -1;
			}
		}

		/* Okay, now that we're in the right place, find out what
		 * the next thing in the file is: its offset will tell us
		 * how much to read.
		 * It's debatable whether 'i' or 'rec' should be
		 * authoritative for determining the offset of the next
		 * resource. I'm going to choose 'rec', since I think
		 * that's more likely to be immune from fencepost errors.
		 * The two should, however, be equivalent. In fact, it
		 * might be a Good Thing to add a check to make sure.
		 */
		if (rec->next == NULL)
		{
			/* This is the last record in the file, so it goes
			 * to the end of the file.
			 */
			next_off = db->file_size;
		} else {
			/* This isn't the last record. Find the next one's
			 * offset.
			 */
			next_off = rec->next->offset;
		}

		/* Subtract this record's index from that of the next one,
		 * to get the size of this record.
		 */
		rec->data_len = (uword) (next_off - rec->offset);

		/* Allocate space for this record
		 * If there's a record with length zero, don't pass that to
		 * malloc(). This is most likely due to a broken conduit.
		 * XXX - The Right Thing to do would be not to read
		 * zero-length records, but that would involve fixing the
		 * record index.
		 */
		if (rec->data_len > 0)
		{
			if ((rec->data = (ubyte *) malloc(rec->data_len)) ==
			    NULL)
			{
				fprintf(stderr, _("%s: Out of memory.\n"),
					"pdb_LoadRecords");
				return -1;
			}

			/* Read the record */
			if ((err = read(fd, rec->data, rec->data_len)) !=
			    rec->data_len)
			{
				fprintf(stderr, _("Can't read record %d in "
						  "\"%.*s\".\n"),
					i,
					PDB_DBNAMELEN, db->name);
				perror("pdb_LoadRecords: read");
				return -1;
			}

			PDB_TRACE(6)
			{
				fprintf(stderr, "Contents of record %d:\n", i);
				debug_dump(stderr, "<REC", rec->data,
					   rec->data_len);
			}
		}
	}

	return 0;		/* Success */
}

/* This is for Emacs's benefit:
 * Local Variables: ***
 * fill-column:	75 ***
 * End: ***
 */
#endif /* PDBFMTS_DISABLED */
