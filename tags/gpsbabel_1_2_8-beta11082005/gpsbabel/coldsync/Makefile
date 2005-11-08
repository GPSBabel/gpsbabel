# $Id: Makefile,v 1.1 2002-08-16 06:13:10 robertl Exp $

TOP =		..
SUBDIR =	libpdb

LIBNAME =	pdb
SHLIB_MAJOR =	0
SHLIB_MINOR =	0

LIBSRCS =	pdb.c util.c
LIBOBJS =	${LIBSRCS:.c=.o}
SHLIBOBJS =	${LIBSRCS:.c=.So}

CLEAN =		${LIBOBJS} ${SHLIBOBJS} ${LIBRARY} \
		*.ln *.bak *~ core *.core .depend
DISTCLEAN =
SPOTLESS =

DISTFILES =	Makefile ${LIBSRCS}

OTHERTAGFILES =	${LIBSRCS}

include ${TOP}/Make.rules

depend::
	${MKDEP} ${CPPFLAGS} ${LIBSRCS}

all::	${LIBRARY}

# It might be a good idea later on to install the library, so that
# people can write other programs that communicate with the Palm, but
# not just yet.
# install::

# This is for Emacs's benefit:
# Local Variables:	***
# fill-column:	75	***
# End:			***
