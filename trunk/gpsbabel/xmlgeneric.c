/*
    Common utilities for XML-based formats.

    Copyright (C) 2004, 2005, 2006, 2007 Robert Lipe, robertlipe@usa.net

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

#include "defs.h"
#include "xmlgeneric.h"
#include "cet_util.h"

#if HAVE_LIBEXPAT
	#include <expat.h>
	static XML_Parser psr;
#endif

static vmem_t current_tag;
static vmem_t cdatastr;
static gbfile *ifd;
static xg_tag_mapping *xg_tag_tbl;
static const char **xg_ignore_taglist;

#define MY_CBUF 4096

#define MYNAME "XML Reader"

void
write_xml_header(gbfile *ofd)
{
	char buff[128];
	cet_cs_vec_t *cs = cet_find_cs_by_name(CET_CHARSET_ASCII);

	if ((global_opts.charset != NULL) && (global_opts.charset != cs))
	    snprintf(buff, sizeof(buff), " encoding=\"%s\"", global_opts.charset_name);
	else
	    buff[0] = 0;
	gbfprintf(ofd, "<?xml version=\"1.0\"%s?>\n", buff);
}

void
write_xml_entity(gbfile *ofd, const char *indent,
                 const char *tag, const char *value)
{
        char *tmp_ent = xml_entitize(value);
        gbfprintf(ofd, "%s<%s>%s</%s>\n", indent, tag, tmp_ent, tag);
        xfree(tmp_ent);
}

void
write_optional_xml_entity(gbfile *ofd, const char *indent,
                          const char *tag, const char *value)
{
        if (value && *value)
                write_xml_entity(ofd, indent, tag, value);
}

void
write_xml_entity_begin0(gbfile *ofd, const char *indent,
							  const char *tag)
{
    gbfprintf(ofd, "%s<%s>\n", indent, tag);
}

void
write_xml_entity_begin1(gbfile *ofd, const char *indent,
							  const char *tag, const char *attr,
							  const char *attrval)
{
    gbfprintf(ofd, "%s<%s %s=\"%s\">\n", indent, tag, attr, attrval);
}

void
write_xml_entity_begin2(gbfile *ofd, const char *indent,
							  const char *tag, const char *attr1,
							  const char *attrval1, const char *attr2,
							  const char *attrval2)
{
    gbfprintf(ofd, "%s<%s %s=\"%s\" %s=\"%s\">\n", indent, tag, attr1, attrval1, attr2, attrval2);
}

void
write_xml_entity_end(gbfile *ofd, const char *indent,
					 const char *tag)
{
    gbfprintf(ofd, "%s</%s>\n", indent, tag);
}

void
xml_fill_in_time(char *time_string, const time_t timep, int microseconds, int long_or_short)
{
	struct tm *tm = gmtime(&timep);
	char *format;
	int n;
	
	if (!tm) {
		*time_string = 0;
		return;
	}
	
	if (long_or_short == XML_LONG_TIME)
		format = "%02d-%02d-%02dT%02d:%02d:%02d";
	else
		format = "%02d%02d%02dT%02d%02d%02d";
	n = sprintf(time_string, format,
		tm->tm_year+1900, 
		tm->tm_mon+1, 
		tm->tm_mday, 
		tm->tm_hour, 
		tm->tm_min, 
		tm->tm_sec);
	if (microseconds) {
		n += sprintf(time_string + n, ".%03d", microseconds / 1000);
	}
	time_string[n++] = 'Z';
	time_string[n++] = '\0';
	
}

void
xml_write_time(gbfile *ofd, const time_t timep, int microseconds, const char *elname)
{
	char time_string[64];
	xml_fill_in_time(time_string, timep, microseconds, XML_LONG_TIME);
	if (time_string[0]) {
		gbfprintf(ofd, "<%s>%s</%s>\n",
			elname,
			time_string,
			elname
		);
	}

}

/***********************************************************************
 * These implement a simple interface for "generic" XML that
 * maps reasonably close to  1:1 between XML tags and internal data
 * structures.   
 * 
 * It doesn't work well for formats (like GPX) that really are "real"
 * XML with extended namespaces and such, but it handles many simpler
 * xml strains and insulates us from a lot of the grubbiness of expat.
 */

xg_callback *
xml_tbl_lookup(const char *tag, xg_cb_type cb_type)
{
	xg_tag_mapping *tm;
        for (tm = xg_tag_tbl; tm->tag_cb != NULL; tm++) {
		if (str_match(tag, tm->tag_name) && (cb_type == tm->cb_type)) {
			return tm->tag_cb;
		}
	}
	return NULL;
}

/*
 * See if tag element 't' is in our list of things to ignore.
 * Returns 0 if it is not on the list.
 */
static int
xml_consider_ignoring(const char *t)
{
	const char **il;

	if (!xg_ignore_taglist) {
		return 0;
	}

	for (il = xg_ignore_taglist; *il; il++) {
		if (0 == strcmp(*il, t)) {
			return 1;
		}
	}
	return 0;
}


static void
xml_start(void *data, const XML_Char *xml_el, const XML_Char **xml_attr)
{
	char *e;
	char *ep;
	xg_callback *cb;
	const char *el;
	const char **attrs;

	el = xml_convert_to_char_string(xml_el);
	attrs = xml_convert_attrs_to_char_string(xml_attr);

	if (xml_consider_ignoring(el))
		return;

	vmem_realloc(&current_tag, strlen(current_tag.mem) + 2 + strlen(el));

	e = current_tag.mem;
        ep = e + strlen(e);
        *ep++ = '/';
        strcpy(ep, el);

	memset(cdatastr.mem, 0, cdatastr.size);

	cb = xml_tbl_lookup(e, cb_start);
	if (cb) {
		(*cb)(NULL, attrs);
	}
	xml_free_converted_string(el);
	xml_free_converted_attrs(attrs);
}

#if HAVE_LIBEXPAT
static void
xml_cdata(void *dta, const XML_Char *xml_s, int len)
{
	char *estr;
	const char *s = xml_convert_to_char_string_n(xml_s, &len);

	vmem_realloc(&cdatastr,  1 + len + strlen(cdatastr.mem));
	estr = (char *) cdatastr.mem + strlen(cdatastr.mem);
	memcpy(estr, s, len);
	estr[len]  = 0;
	xml_free_converted_string(s);
}

static void
xml_end(void *data, const XML_Char *xml_el)
{
	char *s = strrchr(current_tag.mem, '/');
	const char *el = xml_convert_to_char_string(xml_el);
	xg_callback *cb;

	if (xml_consider_ignoring(el))
		return;

	if (strcmp(s + 1, el)) {
		fprintf(stderr, "Mismatched tag %s\n", el);
	}
	cb = xml_tbl_lookup(current_tag.mem, cb_cdata);
	if (cb) {
		(*cb)( (char *) cdatastr.mem, NULL);
	}

	cb = xml_tbl_lookup(current_tag.mem, cb_end);
	if (cb) {
		(*cb)(el, NULL);
	}
	*s = 0;
	xml_free_converted_string(el);
}

void xml_read(void)
{
	int len;
	char buf[MY_CBUF];
	
	while ((len = gbfread(buf, 1, sizeof(buf), ifd))) {
		if (!XML_Parse(psr, buf, len, gbfeof(ifd))) {
			fatal(MYNAME ":Parse error at %d: %s\n",
				(int) XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}
	XML_ParserFree(psr);
	
}

void xml_readstring( char *str ) 
{
	int len = strlen(str);
	if (!XML_Parse(psr, str, len, 1)) {
		fatal( MYNAME ":Parse error at %d: %s\n",
				(int) XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
	}
	XML_ParserFree(psr);
}

void xml_readprefixstring( char *str ) 
{
	int len = strlen(str);
	if (!XML_Parse(psr, str, len, 0)) {
		fatal( MYNAME ":Parse error at %d: %s\n",
				(int) XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
	}
}

void xml_ignore_tags(const char **taglist)
{
	xg_ignore_taglist = taglist;
}

void
xml_init0(const char *fname, xg_tag_mapping *tbl, const char *encoding, 
          gbsize_t offset )
{
	if (fname) {
		ifd = gbfopen(fname, "r", MYNAME);
		if (offset) {
			gbfseek(ifd, offset, SEEK_SET);
		}
	} else {
		ifd = NULL;
	}

	current_tag = vmem_alloc(1,0);
	*((char *)current_tag.mem) = '\0';

	psr = XML_ParserCreate((const XML_Char *)encoding);
	if (!psr) {
		fatal(MYNAME ": Cannot create XML Parser\n");
	}

	cdatastr = vmem_alloc(1, 0);
	*((char *)cdatastr.mem) = '\0';

	xg_tag_tbl = tbl;

	XML_SetUnknownEncodingHandler(psr, cet_lib_expat_UnknownEncodingHandler, NULL);
	XML_SetElementHandler(psr, xml_start, xml_end);
	XML_SetCharacterDataHandler(psr, xml_cdata);
}

/* xml_init0 iwth a default seek argument of zero */
void
xml_init(const char *fname, xg_tag_mapping *tbl, const char *encoding) {
  xml_init0(fname, tbl, encoding, 0);
}

void
xml_init_offset(const char *fname, xg_tag_mapping *tbl, const char *encoding,
                gbsize_t offset) {
  xml_init0(fname, tbl, encoding, offset);
}

void
xml_deinit(void)
{
	vmem_free(&current_tag);
	vmem_free(&cdatastr);
	if (ifd) {
		gbfclose(ifd);
		ifd = NULL;
	}
	xg_ignore_taglist = NULL;
}
#else /* HAVE_LIBEXPAT */
void
xml_init(const char *fname, xg_tag_mapping *tbl, const char *encoding)
{
	fatal("This format does not support reading XML files as libexpat was not present.");
}

void xml_read(void)
{
}

void xml_deinit(void)
{
}

void xml_readstring(char *unused)
{
}

#endif /* HAVE_LIBEXPAT */

/******************************************/
