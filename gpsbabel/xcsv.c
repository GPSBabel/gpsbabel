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

static char *styleopt;
static char *snlenopt;
static char *snwhiteopt;
static char *snupperopt;

static
arglist_t xcsv_args[] = {
	{"style", &styleopt, "Full path to XCSV style file (required)"},
	{"snlen", &snlenopt, "Max synthesized shortname length"},
	{"snwhite", &snwhiteopt, "(0/1) Allow whitespace synth. shortnames"},
	{"snupper", &snupperopt, "(0/1) UPPERCASE synth. shortnames"},
	{0, 0, 0}
};

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
        ogp = (ogue_t *)elem;
        if (ogp->val)
            xfree(ogp->val);
        if (elem)
            xfree(elem);
    }

    /* destroy the epilogue */
    QUEUE_FOR_EACH(&xcsv_file.epilogue, elem, tmp) {
        ogp = (ogue_t *)elem;
        if (ogp->val)
            xfree(ogp->val);
        if (elem)
            xfree(elem);
    }

    /* destroy the ifields */
    QUEUE_FOR_EACH(&xcsv_file.ifield, elem, tmp) {
        fmp = (field_map_t *) elem;
        if (fmp->key)
            xfree(fmp->key);
        if (fmp->val)
            xfree(fmp->val);
        if (fmp->printfc)
            xfree(fmp->printfc);
        if (elem)
            xfree(elem);
    }

    /* destroy the ofields, if they are not re-mapped to ifields. */
    if (xcsv_file.ofield != &xcsv_file.ifield) {
        QUEUE_FOR_EACH(xcsv_file.ofield, elem, tmp) {
            fmp = (field_map_t *) elem;
            if (fmp->key)
                xfree(fmp->key);
            if (fmp->val)
                xfree(fmp->val);
            if (fmp->printfc)
                xfree(fmp->printfc);
            if (elem)
                xfree(elem);
        }

        if (xcsv_file.ofield)
            xfree(xcsv_file.ofield);
    }

    /* other alloc'd glory */
    if (xcsv_file.field_delimiter)
        xfree(xcsv_file.field_delimiter);

    if (xcsv_file.record_delimiter)
        xfree(xcsv_file.record_delimiter);

    if (xcsv_file.badchars)
        xfree(xcsv_file.badchars);

    if (xcsv_file.description)
        xfree(xcsv_file.description);

    if (xcsv_file.extension)
        xfree(xcsv_file.extension);

    if (xcsv_file.mkshort_handle)
        xfree(xcsv_file.mkshort_handle);

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
xcsv_parse_style_line(const char *sbuff)
{
    int i, linecount = 0;
    char *s, *p, *sp;
    const char *cp;
    char *key, *val, *pfc;

    /* 
     * tokens should be parsed longest to shortest, unless something
     * requires a previously set value.  This way something like
     * SHORT and SHORTNAME don't collide.
     */

    /* whack off any comments */
    if ((p = strchr(sbuff, '#')) != NULL) {
	    if ((p > sbuff) && p[-1] == '\\') {
		memmove(p-1, p, strlen(p));
	    	p[strlen(p)-1] = '\0';
	    } else {
		*p = '\0';
	    }	 
    }

    if (strlen(sbuff)) {
	if (ISSTOKEN(sbuff, "FIELD_DELIMITER")) {
	    sp = csv_stringtrim(&sbuff[16], "\"");
	    cp = get_char_from_constant_table(sp);
	    if (cp) {
		xcsv_file.field_delimiter = xstrdup(cp);
		xfree(sp);
	    }
	    else
		xcsv_file.field_delimiter = sp;
	} else

	if (ISSTOKEN(sbuff, "RECORD_DELIMITER")) {
	    sp = csv_stringtrim(&sbuff[17], "\"");
	    cp = get_char_from_constant_table(sp);
	    if (cp) {
		xcsv_file.record_delimiter = xstrdup(cp);
		xfree(sp);
	    }
	    else
		xcsv_file.field_delimiter = sp;
	} else

	if (ISSTOKEN(sbuff, "DESCRIPTION")) {
		xcsv_file.description = csv_stringtrim(&sbuff[11],"");
	} else

	if (ISSTOKEN(sbuff, "EXTENSION")) {
		xcsv_file.extension = csv_stringtrim(&sbuff[10],"");
	} else

	if (ISSTOKEN(sbuff, "SHORTLEN")) {
            if (xcsv_file.mkshort_handle)
                setshort_length(xcsv_file.mkshort_handle, atoi(&sbuff[9]));
	} else

	if (ISSTOKEN(sbuff, "SHORTWHITE")) {
            if (xcsv_file.mkshort_handle)
                setshort_whitespace_ok(xcsv_file.mkshort_handle, atoi(&sbuff[12]));
	} else

	if (ISSTOKEN(sbuff, "BADCHARS")) {
	    sp = csv_stringtrim(&sbuff[9], "\"");
	    cp = get_char_from_constant_table(sp);
	    if (cp) {
		xcsv_file.badchars = xstrdup(cp);
		xfree(sp);
	    }
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
}


/*
 * A wrapper for xcsv_parse_style_line that reads until it hits
 * a terminating null.   Makes multiple calls to that function so
 * that "ignore to end of line" comments work right.
 */
static void
xcsv_parse_style_buff(const char *sbuff)
{
	char ibuf[256];
	char *ibufp;
	size_t i;

	while (*sbuff) {
		ibuf[0] = 0;
		i = 0;
	 	for (ibufp = ibuf; *sbuff != '\n' && i++ < sizeof(ibuf); ) {
			*ibufp++ = *sbuff++;
		}
		while (*sbuff == '\n' || *sbuff == '\r')
			sbuff++;
		*ibufp = 0;
		xcsv_parse_style_line(ibuf);
	}
}

static void
xcsv_read_style(const char *fname)
{
    char sbuff[8192];
    FILE *fp;

    xcsv_file_init();

    fp = fopen(fname, "r");

    if (!fp)
        fatal(MYNAME ": Cannot read style file: %s\n", fname);

    do {
        memset(sbuff, '\0', sizeof(sbuff));
        fgets(sbuff, sizeof(sbuff), fp);
        rtrim(sbuff);
	xcsv_parse_style_line(sbuff);
    } while (!feof(fp));

    /* if we have no output fields, use input fields as output fields */
    if (xcsv_file.ofield_ct == 0) {
        if (xcsv_file.ofield) 
            xfree(xcsv_file.ofield);
        xcsv_file.ofield = &xcsv_file.ifield;
        xcsv_file.ofield_ct = xcsv_file.ifield_ct;
    }

    fclose(fp);
}

/*
 * Passed a pointer to an internal buffer that would be identical
 * to the series of bytes that would be in a style file, we set up
 * the xcsv parser and make it ready for general use.
 */
void
xcsv_read_internal_style(const char *style_buf)
{
	xcsv_file_init();
	xcsv_file.is_internal = 1;

	xcsv_parse_style_buff(style_buf);

	/* if we have no output fields, use input fields as output fields */
	if (xcsv_file.ofield_ct == 0) {
		if (xcsv_file.ofield) 
			xfree(xcsv_file.ofield);
		xcsv_file.ofield = &xcsv_file.ifield;
		xcsv_file.ofield_ct = xcsv_file.ifield_ct;
	}
}

static void
xcsv_rd_init(const char *fname, const char *args)
{

    /* 
     * if we don't have an internal style defined, we need to
     * read it from a user-supplied style file, or die trying.
     */
    if (xcsv_file.is_internal == 0) {
        if (!styleopt)
            fatal(MYNAME ": XCSV input style not declared.  Use ... -i xcsv,style=path/to/file.style\n");

        xcsv_read_style(styleopt);
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
    /* if we don't have an internal style defined, we need to
     * read it from a user-supplied style file, or die trying.
     */
    if (xcsv_file.is_internal == 0) {

        if (!styleopt)
            fatal(MYNAME ": XCSV output style not declared.  Use ... -o xcsv,style=path/to/file.style\n");

        xcsv_read_style(styleopt);
    }

    xcsv_file.xcsvfp = fopen(fname, "w");

    if (xcsv_file.xcsvfp == NULL)
        fatal(MYNAME ": Cannot open %s for writing\n", fname);

    /* set mkshort options from the command line */
    if (global_opts.synthesize_shortnames) {

        if (snlenopt)
            setshort_length(xcsv_file.mkshort_handle, atoi(snlenopt));

        if (snwhiteopt)
            setshort_whitespace_ok(xcsv_file.mkshort_handle, atoi(snwhiteopt));

        if (snupperopt)
            setshort_mustupper(xcsv_file.mkshort_handle, atoi(snupperopt));

        setshort_badchars(xcsv_file.mkshort_handle, xcsv_file.badchars);

    }

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
    xcsv_args
};
