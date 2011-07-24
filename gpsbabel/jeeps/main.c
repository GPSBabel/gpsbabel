#include "gps.h"
// #include "jeeps.h"

main()
{
  int n;
  GPS_PWay *way;
  GPS_PWay *array;

  if (GPS_Init("/dev/ttyS0") < 0) {
    fprintf(stderr, "Can't init\n");
  }

  if ((n=GPS_Command_Get_Waypoint("/dev/ttyS0", &way))<0) {
    fprintf(stderr, "can't get\n");
    return;
  }
//	fprintf(stdout," Done\n");

  GPS_Fmt_Print_Waypoint(way, n, stdout);

  array = (GPS_PWay *) calloc(1, sizeof(GPS_PWay));
  array[0] = GPS_Way_New();
  strcpy(array[0]->ident,"lower @#$%^&* rocks");
  strcpy(array[0]->cmnt,"COMMENTCOMMENTCOMMENTCOMMENTCOMMENT");
  array[0]->wpt_class = 0;
  array[0]->lat = 1.234;
  array[0]->lon = 1.234;
  GPS_Command_Send_Waypoint("/dev/ttyS0", array, 1);

}
