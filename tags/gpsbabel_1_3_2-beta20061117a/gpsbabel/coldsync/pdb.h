/* pdb.h
 *
 * Definitions and such for Palm databases.
 *
 *	Copyright (C) 1999-2000, Andrew Arensburger.
 *	You may distribute this file under the terms of the Artistic
 *	License, as specified in the README file.
 *
 * $Id: pdb.h,v 1.1 2002-08-16 06:13:10 robertl Exp $
 */
#ifndef _pdb_h_
#define _pdb_h_

/* XXX - Add a type (and support functions) for those ubitquitous
 * 4-character IDs.
 */

#define EPOCH_1904	2082844800L	/* Difference, in seconds, between
					 * Palm's epoch (Jan. 1, 1904) and
					 * Unix's epoch (Jan. 1, 1970).
					 */

#define PDB_DBNAMELEN	32		/* Length of name field in database
					 * header */

/* Database attribute flags */
#define PDB_ATTR_RESDB		0x0001	/* This is a resource database.
					 * Resource databases are usually
					 * saved in files with ".prc"
					 * extensions. Other databases are
					 * saved with a ".pdb" extension.
					 */
#define PDB_ATTR_RO		0x0002	/* Read-only database */
#define PDB_ATTR_APPINFODIRTY	0x0004	/* App info block is dirty */
#define PDB_ATTR_BACKUP		0x0008	/* Back up the database if no
					 * app-specific conduit exists */
#define PDB_ATTR_OKNEWER	0x0010	/* Tells the backup conduit that
					 * it's okay to install a newer
					 * version of this database with a
					 * different name if this one is
					 * open. Usually used for the
					 * Graffiti Shortcuts database.
					 */
#define PDB_ATTR_RESET		0x0020	/* Reset the Palm after the
					 * database is installed */
#define PDB_ATTR_NOCOPY		0x0040	/* Database should not be copied(?) */
#define PDB_ATTR_STREAM		0x0080	/* Database is used for file stream
					 * implementation(?).
					 */
#define PDB_ATTR_OPEN		0x8000	/* Database is open */

/* Record attributes
 * These are the attributes that individual records in a database can have.
 * I've taken the liberty of giving them different names from Palm's, since
 * Palm's names are rather confusing.
 *
 * PDB_REC_PRIVATE is set on a record that has been marked "private" by the
 * user. It is not encrypted, and if the desktop asks for this record, the
 * Palm will not refuse or ask for a password. In short, the Palm needs to
 * trust the desktop.
 *
 * PDB_REC_DIRTY is set on a record whose contents have been modified since
 * the last sync. If the user deletes a record without modifying it,
 * PDB_REC_DIRTY will not be set, but if he modifies it, then deletes it,
 * then both PDB_REC_DIRTY and PDB_REC_DELETED will be set.
 *
 * PDB_REC_DELETED is set on a record that has been deleted by the user
 * since the last sync. Unfortunately, it looks as if not all applications
 * are polite enough to set this flag, so you have to go with
 * PDB_REC_ARCHIVE and PDB_REC_EXPUNGED.
 *
 * If the user chose the "Save archive copy on PC" option when deleting a
 * record, then the PDB_REC_ARCHIVE bit will be set on the record (with any
 * luck, so will PDB_REC_DELETED).
 *
 * If the user did not choose the "Save archive copy on PC" option when
 * deleting a record, the PDB_REC_EXPUNGED bit will be set on the record
 * (as will PDB_REC_DELETED, perhaps). Apparently, what happens is this:
 * when the user deletes a record, a copy is left around so that HotSync
 * will know to delete this record. However, if the user chose not to keep
 * a copy, then, in order to conserve memory, the Palm will delete the
 * record data, although it will keep a copy of the record header for
 * HotSync.
 */
#define PDB_REC_EXPUNGED	0x80	/* The contents of this record have
					 * been deleted, leaving only the
					 * record info. (Palm calls this
					 * 'dlpRecAttrDeleted'.)
					 */
#define PDB_REC_DIRTY		0x40	/* Record has been modified. (Palm
					 * calls this 'dlpRecAttrDirty'.)
					 */
#define PDB_REC_DELETED		0x20	/* This record has been deleted.
					 * (Palm calls this
					 * 'dlpRecAttrBusy'.)
					 */
#define PDB_REC_PRIVATE		0x10	/* Record is private: don't show to
					 * anyone without asking for a
					 * password. (Palm calls this
					 * 'dlpRecAttrSecret'.)
					 */
#define PDB_REC_ARCHIVE		0x08	/* This record should be archived
					 * at the next sync. (Palm calls
					 * this 'dlpRecAttrArchived'.)
					 */

typedef udword localID;			/* Local (card-relative) chunk ID
					 * (basically, a pointer that can
					 * be used as a unique ID).
					 */

#define PDB_HEADER_LEN		72	/* Length of header in a file */
#define PDB_RECORDLIST_LEN	6	/* Length of record index header in
					 * file */

/* pdb_record
 * A plain old record, containing arbitrary data.
 */
struct pdb_record
{
	struct pdb_record *next;	/* Next record on linked list */
	localID offset;			/* Offset of record in file */
	ubyte flags;			/* Record flags (PDB_REC_*) */
	ubyte category;			/* Record's category */
	udword id;			/* Record's unique ID. Actually,
					 * only the bottom 3 bytes are
					 * stored in the file, but for
					 * everything else, it's much
					 * easier to just consider this a
					 * 32-bit integer.
					 */
	uword data_len;			/* Length of this record */
	ubyte *data;			/* This record's data */
};
#define PDB_RECORDIX_LEN	8	/* Size of a pdb_record in a file */

/* pdb_resource
 * Mac hackers should feel at home here: the type of a resource is really a
 * 4-character category identifier, and the ID is an integer within that
 * category.
 */
struct pdb_resource
{
	struct pdb_resource *next;	/* Next resource on linked list */
	udword type;			/* Resource type */
	uword id;			/* Resource ID */
	localID offset;			/* Offset of resource in file */
	uword data_len;			/* Length of this resource */
	ubyte *data;			/* This resource's data */
};
#define PDB_RESOURCEIX_LEN	10	/* Size of a pdb_resource in a file */

/* pdb
 * Structure of a Palm database (file), both resource databases (.prc) and
 * record databases (.pdb).
 */
struct pdb
{
	long file_size;			/* Total length of file */

	char name[PDB_DBNAMELEN];	/* Database name */
	uword attributes;		/* Database attributes */
	uword version;			/* Database version */

	udword ctime;			/* Creation time */
	udword mtime;			/* Time of last modification */
	udword baktime;			/* Time of last backup */
	udword modnum;			/* Modification number */
			/* XXX - What exactly is the modification number?
			 * Does it get incremented each time you make any
			 * kind of change to the database?
			 */
	localID appinfo_offset;		/* Offset of AppInfo block in the
					 * file */
	localID sortinfo_offset;	/* Offset of sort block in the file */

	udword type;			/* Database type */
	udword creator;			/* Database creator */

	udword uniqueIDseed;		/* Used to generate unique IDs for
					 * records and resources. Only the
					 * lower 3 bytes are used. The high
					 * byte is for alignment.
					 */

	localID next_reclistID;		/* ID of next record index in the
					 * file. In practice, this field is
					 * always zero.
					 */
	uword numrecs;			/* Number of records/resources in
					 * the file.
					 */

	long appinfo_len;		/* Length of AppInfo block */
	void *appinfo;			/* Optional AppInfo block */
	long sortinfo_len;		/* Length of sort block */
	void *sortinfo;			/* Optional sort block */

	/* Record/resource list. Each of these is actually a linked list,
	 * to make it easy to insert and delete records.
	 */
	union {
		struct pdb_record *rec;
		struct pdb_resource *rsrc;
	} rec_index;
};

/* Convenience macros */
#define IS_RSRC_DB(db) 		((db)->attributes & PDB_ATTR_RESDB)
					/* Is this a resource database? If
					 * not, it must be a record
					 * database.
					 */

extern int pdb_trace;			/* Debugging level for PDB stuff */

extern struct pdb *new_pdb();
extern void free_pdb(struct pdb *db);
extern void pdb_FreeRecord(struct pdb_record *rec);
extern void pdb_FreeResource(struct pdb_resource *rsrc);
extern struct pdb *pdb_Read(int fd);	/* Load a pdb from a file. */
extern int pdb_Write(const struct pdb *db, int fd);
					/* Write a pdb to a file */
extern struct pdb_record *pdb_FindRecordByID(
	const struct pdb *db,
	const udword id);
extern struct pdb_record *pdb_FindRecordByIndex(
	const struct pdb *db,
	const uword index);
extern int pdb_DeleteRecordByID(
	struct pdb *db,
	const udword id);
extern int pdb_AppendRecord(struct pdb *db, struct pdb_record *newrec);
extern int pdb_AppendResource(struct pdb *db, struct pdb_resource *newrsrc);
extern int pdb_InsertRecord(
	struct pdb *db,
	struct pdb_record *prev,
	struct pdb_record *newrec);
extern int pdb_InsertResource(
	struct pdb *db,
	struct pdb_resource *prev,
	struct pdb_resource *newrsrc);
extern struct pdb_record *new_Record(
	const ubyte attributes,
	const ubyte category,
	const udword id,
	const uword len,
	const ubyte *data);
extern struct pdb_resource *new_Resource(
	const udword type,
	const uword id,
	const uword len,
	const ubyte *data);
extern struct pdb_record *pdb_CopyRecord(
	const struct pdb *db,
	const struct pdb_record *rec);
extern struct pdb_resource *pdb_CopyResource(
	const struct pdb *db,
	const struct pdb_resource *rsrc);
extern int pdb_LoadHeader(int fd, struct pdb *db);

/* XXX - Functions to write:
pdb_setAppInfo		set the appinfo block
pdb_setSortInfo		set the sortinfo block
*/

#endif	/* _pdb_h_ */

/* This is for Emacs's benefit:
 * Local Variables: ***
 * fill-column:	75 ***
 * End: ***
 */
