/*
    XCSV - X Character Separated Values (.???)

    A hopefully not too feeble attempt at parsing whatever separated values
    files into the waypoint structure and back out again.  This is a config-
    file wrapper around csv_util.c.

    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <ctype.h>
#include "defs.h"
#include "csv_util.h"

#define MYNAME	"XCSV"
#define ISSTOKEN(a,b) (strncmp(a,b, strlen(b)) == 0)

static void *mkshort_handle;

/* a table of config file constants mapped to chars */
static 
char_map_t xcsv_char_table[] = {
	{ "COMMA",		"," 	},
	{ "COMMASPACE",		", " 	},
	{ "SINGLEQUOTE",	"'"	},
	{ "DOUBLEQUOTE",	"\""	},
	{ "COLON",		":"	},
	{ "SEMICOLON",		";"	},
	{ "NEWLINE",		"\n"	},
	{ "CR",			"\n"	},
	{ "CRNEWLINE",  	"\r\n"	},
	{ "TAB",  		"\t"	},
	{ "SPACE",  		" "	},
	{ "HASH",  		"#"	},
	{ NULL, 		NULL	}
};	

void
xcsv_destroy_style(void)
{
    queue *elem, *tmp;
    field_map_t *fmp;
    ogue_t *ogp;

    /* 
     * If this xcsv_file struct came from a file we can free it all.
     * If not, we can at least free the queue elements.
     */

    /* destroy the prologue */
    QUEUE_FOR_EACH(&xcsv_file.prologue, elem, tmp) {
        if (xcsv_file.is_internal == 0) {
            ogp = (ogue_t *)elem;
            if (ogp->val)
                free(ogp->val);
        }
        if (elem)
            free(elem);
    }

    /* destroy the epilogue */
    QUEUE_FOR_EACH(&xcsv_file.epilogue, elem, tmp) {
        if (xcsv_file.is_internal == 0) {
            ogp = (ogue_t *)elem;
            if (ogp->val)
                free(ogp->val);
        }
        if (elem)
            free(elem);
    }

    /* destroy the ifields */
    QUEUE_FOR_EACH(&xcsv_file.ifield, elem, tmp) {
        if (xcsv_file.is_internal == 0) {
            fmp = (field_map_t *) elem;
            if (fmp->key)
                free(fmp->key);
            if (fmp->val)
                free(fmp->val);
            if (fmp->printfc)
                free(fmp->printfc);
        }
        if (elem)
            free(elem);
    }

    /* destroy the ofields, if they are not re-mapped to ifields. */
    if (xcsv_file.ofield != &xcsv_file.ifield) {
        QUEUE_FOR_EACH(xcsv_file.ofield, elem, tmp) {
            if (xcsv_file.is_internal == 0) {
                fmp = (field_map_t *) elem;
                if (fmp->key)
                    free(fmp->key);
                if (fmp->val)
                    free(fmp->val);
                if (fmp->printfc)
                    free(fmp->printfc);
            }
            if (elem)
                free(elem);
        }

        if (xcsv_file.ofield)
            free(xcsv_file.ofield);
    }

    if (xcsv_file.is_internal == 0) {
        /* other alloc'd glory */
        if (xcsv_file.field_delimiter)
            free(xcsv_file.field_delimiter);

        if (xcsv_file.record_delimiter)
            free(xcsv_file.record_delimiter);

        if (xcsv_file.badchars)
            free(xcsv_file.badchars);
    }

    /* return everything to zeros */
    memset(&xcsv_file, '\0', sizeof(xcsv_file));
}

static const char *
get_char_from_constant_table(char *key)
{
    char_map_t *cm = xcsv_char_table;

    while ((cm->key) && (strcmp(key, cm->key) != 0)) {
        cm++;
    }

    return (cm->chars);
}

static void
xcsv_read_style(const char *fname)
{
    char sbuff[8192];
    int i, linecount = 0;
    FILE *fp;
    char *s, *p, *sp;
    const char *cp;
    char *key, *val, *pfc;

    xcsv_file_init();

    fp = fopen(fname, "r");

    if (!fp)
        fatal(MYNAME ": Cannot read style file: %s\n", fname);

    do {
        memset(sbuff, '\0', sizeof(sbuff));
        fgets(sbuff, sizeof(sbuff), fp);
        rtrim(sbuff);

        /* 
         * tokens should be parsed longest to shortest, unless something
         * requires a previously set value.  This way something like
         * SHORT and SHORTNAME don't collide.
         */

        /* whack off any comments */
        if ((p = strchr(sbuff, '#')) != NULL)
            *p = '\0';

        if (strlen(sbuff)) {
            if (ISSTOKEN(sbuff, "FIELD_DELIMITER")) {
                sp = csv_stringtrim(&sbuff[16], "\"");
                cp = get_char_from_constant_table(sp);
                if (cp)
                    xcsv_file.field_delimiter = xstrdup(cp);
                else
                    xcsv_file.field_delimiter = sp;
            } else

            if (ISSTOKEN(sbuff, "RECORD_DELIMITER")) {
                sp = csv_stringtrim(&sbuff[17], "\"");
                cp = get_char_from_constant_table(sp);
                if (cp)
                    xcsv_file.record_delimiter = xstrdup(cp);
                else
                    xcsv_file.field_delimiter = sp;
            } else

            if (ISSTOKEN(sbuff, "BADCHARS")) {
                sp = csv_stringtrim(&sbuff[9], "\"");
                cp = get_char_from_constant_table(sp);
                if (cp)
                    xcsv_file.badchars = xstrdup(cp);
                else
                    xcsv_file.badchars = sp;
            } else

            if (ISSTOKEN(sbuff, "PROLOGUE")) {
                xcsv_prologue_add(xstrdup(&sbuff[9]));
            } else

            if (ISSTOKEN(sbuff, "EPILOGUE")) {
                xcsv_epilogue_add(xstrdup(&sbuff[9]));
            } else

            if (ISSTOKEN(sbuff, "IFIELD")) {
                key = val = pfc = NULL;
                
                s = csv_lineparse(&sbuff[6], ",", "", linecount);

                i = 0;
                while (s) {
                    switch(i) {
                    case 0:
                        /* key */
                        key = csv_stringtrim(s, "\"");
                        break;
                    case 1:
                        /* default value */
                        val = csv_stringtrim(s, "\"");
                        break;
                    case 2:
                        /* printf conversion */
                        pfc = csv_stringtrim(s, "\"");
                        break;
                    default:
                        break;
                    }
                    i++;

                    s = csv_lineparse(NULL, ",", "", linecount);
                }

                xcsv_ifield_add(key, val, pfc);

            } else

            /* 
             * as OFIELDs are implemented as an after-thought, I'll
             * leave this as it's own parsing for now.  We could
             * change the world on ifield vs ofield format later..
             */
            if (ISSTOKEN(sbuff, "OFIELD")) {
                key = val = pfc = NULL;

                s = csv_lineparse(&sbuff[6], ",", "", linecount);

                i = 0;
                while (s) {
                    switch(i) {
                    case 0:
                        /* key */
                        key = csv_stringtrim(s, "\"");
                        break;
                    case 1:
                        /* default value */
                        val = csv_stringtrim(s, "\"");
                        break;
                    case 2:
                        /* printf conversion */
                        pfc = csv_stringtrim(s, "\"");
                        break;
                    default:
                        break;
                    }
                    i++;
                    s = csv_lineparse(NULL, ",", "", linecount);
                }

                xcsv_ofield_add(key, val, pfc);
            }
        }
    } while (!feof(fp));

    /* if we have no output fields, use input fields as output fields */
    if (xcsv_file.ofield_ct == 0) {
        if (xcsv_file.ofield) 
            free(xcsv_file.ofield);
        xcsv_file.ofield = &xcsv_file.ifield;
        xcsv_file.ofield_ct = xcsv_file.ifield_ct;
    }

    fclose(fp);
}

static void
xcsv_rd_init(const char *fname, const char *args)
{
    const char *p;

    /* 
     * if we don't have an internal style defined, we need to
     * read it from a user-supplied style file, or die trying.
     */
    if (xcsv_file.is_internal == 0) {
        p = get_option(args, "style");

        if (!p)
            fatal(MYNAME ": XCSV input style not declared.  Use ... -i xcsv,style=path/to/file.style\n");

        xcsv_read_style(p);
    }

    xcsv_file.xcsvfp = fopen(fname, "r");

    if (xcsv_file.xcsvfp == NULL)
        fatal(MYNAME ": Cannot open %s for reading\n", fname);
}

static void
xcsv_rd_deinit(void)
{
    fclose(xcsv_file.xcsvfp);

    xcsv_destroy_style();
}

static void
xcsv_wr_init(const char *fname, const char *args)
{
    const char * p;
    mkshort_handle = mkshort_new_handle();
    
    /* if we don't have an internal style defined, we need to
     * read it from a user-supplied style file, or die trying.
     */
    if (xcsv_file.is_internal == 0) {
        p = get_option(args, "style");

        if (!p)
            fatal(MYNAME ": XCSV output style not declared.  Use ... -o xcsv,style=path/to/file.style\n");

        xcsv_read_style(p);

        /* set mkshort options from the command line */
        if (global_opts.synthesize_shortnames) {
            p = get_option(args, "snlen");
            if (p)
                setshort_length(mkshort_handle, atoi(p));

            p = get_option(args, "snwhite");
            if (p)
                setshort_whitespace_ok(mkshort_handle, atoi(p));

            p = get_option(args, "snupper");
            if (p)
                setshort_mustupper(mkshort_handle, atoi(p));

            setshort_badchars(mkshort_handle, xcsv_file.badchars);
        }
    }

    xcsv_file.xcsvfp = fopen(fname, "w");

    if (xcsv_file.xcsvfp == NULL)
        fatal(MYNAME ": Cannot open %s for writing\n", fname);
}

static void
xcsv_wr_deinit(void)
{
    fclose(xcsv_file.xcsvfp);

    xcsv_destroy_style();
}

ff_vecs_t xcsv_vecs = {
    xcsv_rd_init,
    xcsv_wr_init,
    xcsv_rd_deinit,
    xcsv_wr_deinit,
    xcsv_data_read,
    xcsv_data_write,
};
