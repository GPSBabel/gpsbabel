#include <stdarg.h>
#include <stdio.h>
#include "garmin_tables.h"

tbl_ent(int n, ...)
{
	int i;
	char *t;
	va_list args;
	va_start(args, n);

	for (i = 0; i < n; i++) {
		t = va_arg(args, char *);
printf("%s%s", i > 0 ? "," : "", t);
		
	}
printf("\n");
	va_end(args);
	
	
}

sort_garmin(const void *a, const void *b)
{
	const icon_mapping_t *ap = a;
	const icon_mapping_t *bp = b;
	return (case_ignore_strcmp((ap)->icon, (bp)->icon));
}

garmin()
{
	icon_mapping_t *i;
	int n = 0;
	char pbuf[100];
	char mbuf[100];

	for (i = garmin_icon_table; i->icon; i++) {
		n++;
	}

	qsort(garmin_icon_table, 
		n,
		sizeof(garmin_icon_table[0]),
		sort_garmin);

	for (i = garmin_icon_table; i->icon; i++) {
		snprintf(pbuf, sizeof(pbuf), "%d", i->pcxsymnum);
		snprintf(mbuf, sizeof(mbuf), "%d", i->mpssymnum);
		tbl_ent(3, i->icon, pbuf, mbuf);
	}
}

main()
{
	garmin();
}
