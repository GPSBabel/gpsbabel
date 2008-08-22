/*
    NaviGPS serial protocol.

    Copyright (C) 2007 Tom Hughes, tom@compton.nu

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


/* Based on description at http://wiki.splitbrain.org/navilink */
#include "defs.h"
#include "gbser.h"
#include "jeeps/gpsmath.h"

#define MYNAME "NAVILINK"

static char *nuketrk = NULL;
static char *nukerte = NULL;
static char *nukewpt = NULL;
static char *poweroff = NULL;

static void *serial_handle = NULL;
static gbfile *file_handle = NULL;

static unsigned char *track_data;
static unsigned char *track_data_ptr;
static unsigned char *track_data_end;
static unsigned track_serial;
static waypoint **route_waypts;
static unsigned *route_ids;
static unsigned route_id_ptr;

#define SERIAL_TIMEOUT 8000

#define MAX_WAYPOINTS         1000
#define MAX_SUBROUTES         9
#define MAX_SUBROUTE_LENGTH   14
#define MAX_ROUTE_LENGTH      (MAX_SUBROUTES * MAX_SUBROUTE_LENGTH - 1)
#define MAX_READ_TRACKPOINTS  512
#define MAX_WRITE_TRACKPOINTS 127

#define PID_SYNC              0xd6
#define PID_ACK               0x0c
#define PID_NAK               0x00
#define PID_QRY_INFORMATION   0x20
#define PID_QRY_FW_VERSION    0xfe
#define PID_DATA              0x03
#define PID_ADD_A_WAYPOINT    0x3c
#define PID_QRY_WAYPOINTS     0x28
#define PID_QRY_ROUTE         0x24
#define PID_DEL_WAYPOINT      0x36
#define PID_DEL_ALL_WAYPOINT  0x37
#define PID_DEL_ROUTE         0x34
#define PID_DEL_ALL_ROUTE     0x35
#define PID_ADD_A_ROUTE       0x3d
#define PID_ERASE_TRACK       0x11
#define PID_READ_TRACKPOINTS  0x14
#define PID_WRITE_TRACKPOINTS 0x16
#define PID_CMD_OK            0xf3
#define PID_CMD_FAIL          0xf4
#define PID_QUIT              0xf2

static
const char *const icon_table[] = {
	"Star",
	"Flag",
	"House",
	"Left Sign",
	"Telegraph Pole",
	"People",
	"Fuel",
	"Phone",
	"Pole",
	"Mountain",
	"Water",
	"Tree",
	"Road Narrows",
	"Crossroads",
	"Road Fork",
	"Turn Right",
	"Turn Left",
	"Bird",
	"3D House",
	"Trig Point",
	"Tower",
	"Cable Car",
	"Church",
	"Telegraph Pole",
	"Skier",
	"Anchor",
	"Fish",
	"Fishes",
	"Rain",
	"Fisherman",
	"Tower",
	"Boats",
	"Boat",
	"Bicycle",
	"Railway Track",
	"Dollar Sign",
	"Bus",
	"Camera",
	"Fuel Pump",
	"Cup",
	"Merging Road",
	"Plane",
	"Red Cross",
	"House",
	"Parking"
};

static
arglist_t navilink_args[] = {
	{ "nuketrk", &nuketrk, "Delete all track points", NULL, ARGTYPE_BOOL,
		ARG_NOMINMAX },
	{ "nukerte", &nukerte, "Delete all routes", NULL, ARGTYPE_BOOL,
		ARG_NOMINMAX },
	{ "nukewpt", &nukewpt, "Delete all waypoints", NULL, ARGTYPE_BOOL,
		ARG_NOMINMAX },
	{ "power_off", &poweroff, "Command unit to power itself down",
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX },
	ARG_TERMINATOR
};

static void (*write_waypoint)(const waypoint *) = NULL;
static void (*write_track_start)(const route_head *track) = NULL;
static void (*write_track_point)(const waypoint *waypt) = NULL;
static void (*write_track_end)(const route_head *track) = NULL;
static void (*write_route_start)(const route_head *track) = NULL;
static void (*write_route_point)(const waypoint *waypt) = NULL;
static void (*write_route_end)(const route_head *track) = NULL;

static int
find_icon_from_descr(const char *descr)
{
	int i;

	for (i = 0; i < sizeof(icon_table) / sizeof(const char *); i++) {
		if (strcmp(descr, icon_table[i]) == 0)
			return i;
	}

	return 0;
}

static void
free_waypoints(waypoint **waypts)
{
	waypoint **wayptp;

	for (wayptp = waypts; wayptp < waypts + MAX_WAYPOINTS; wayptp++) {
		if (*wayptp) {
			waypt_free(*wayptp);
		}
	}

	xfree(waypts);
}

static unsigned
compare_waypoints(const waypoint *waypt1, const waypoint *waypt2)
{
	return waypt1->latitude == waypt2->latitude &&
		waypt1->longitude == waypt2->longitude &&
		waypt1->altitude == waypt2->altitude &&
		strcmp(waypt1->shortname, waypt2->shortname) == 0;
}

static unsigned
checksum_packet(const unsigned char *packet, unsigned length)
{
	unsigned checksum = 0;

	while (length-- > 0) {
		checksum += *packet++;
	}

	return checksum & 0x7fff;
}

#ifdef NAVILINK_DEBUG

static void
dump_packet(char *prefix, unsigned char *packet, unsigned length)
{
	unsigned i;

	for (i = 0; i < length; i++ ) {
		if ((i % 16) == 0) fprintf(stderr, "%s %08x :", prefix, i);
		fprintf(stderr, " %02x", packet[i]);
		if ((i % 16) == 15 || i == length - 1) fprintf(stderr, "\n");
	}

	fprintf(stderr, "\n");
}

#endif

static void
write_packet(unsigned type, const void *payload, unsigned length)
{
	unsigned char *packet = xmalloc(length + 9);

	packet[0] = 0xa0;
	packet[1] = 0xa2;
	le_write16(packet + 2, length + 1);
	packet[4] = type;
	memcpy(packet + 5, payload, length);
	le_write16(packet + length + 5, checksum_packet(packet + 4, length + 1));
	packet[length + 7] = 0xb0;
	packet[length + 8] = 0xb3;

#ifdef NAVILINK_DEBUG
	dump_packet(">>>", packet + 4, length + 1);
#endif

	if (gbser_write(serial_handle, packet, length + 9) != gbser_OK) {
		fatal(MYNAME ": Write error\n");
	}

	xfree(packet);
}

static unsigned
read_word(void)
{
	unsigned char buffer[2];

	if (gbser_read_wait(serial_handle, buffer, 2, SERIAL_TIMEOUT) != 2) {
		fatal(MYNAME ": Read error\n");
	}

	return (buffer[1] << 8) | buffer[0];
}

static void
read_packet(unsigned type, void *payload, unsigned minlength, unsigned maxlength)
{
	unsigned      size;
	unsigned char *data;
	unsigned      checksum;

	if (read_word() != 0xa2a0) {
		fatal(MYNAME ": Protocol error: Bad packet header\n");
	}

	if ((size = read_word()) <= minlength) {
		fatal(MYNAME ": Protocol error: Packet too short\n");
	}

	data = xmalloc(size);

	if (gbser_read_wait(serial_handle, data, size, SERIAL_TIMEOUT) != size) {
		fatal(MYNAME ": Read error reading %d byte payload\n", size);
	}

#ifdef NAVILINK_DEBUG
	dump_packet("<<<", data, size);
#endif

	if (data[0] != type) {
		fatal(MYNAME ": Protocol error: Bad packet type (expected 0x%02x but got 0x%02x)\n", type, data[0]);
	}

	if ((checksum = read_word()) != checksum_packet(data, size)) {
		fatal(MYNAME ": Checksum error - expected %x got %x\n", checksum_packet(data, size), checksum);
	}

	if (read_word() != 0xb3b0) {
		fatal(MYNAME ": Protocol error: Bad packet trailer\n");
	}

	if (size - 1 > maxlength) {
		memcpy(payload, data + 1, maxlength);
	} else {
		memcpy(payload, data + 1, size - 1);
	}

	xfree(data);

	return;
}

static time_t
decode_datetime(const unsigned char *buffer)
{
	struct tm tm;

	tm.tm_sec = buffer[5];
	tm.tm_min = buffer[4];
	tm.tm_hour = buffer[3];
	tm.tm_mday = buffer[2];
	tm.tm_mon = buffer[1] - 1;
	tm.tm_year = buffer[0] + 100;

	return mkgmtime(&tm);
}

static void
encode_datetime(time_t datetime, unsigned char *buffer)
{
	struct tm *tm;

	if ((tm = gmtime(&datetime)) != NULL) {
		buffer[0] = tm->tm_year - 100;
		buffer[1] = tm->tm_mon + 1;
		buffer[2] = tm->tm_mday;
		buffer[3] = tm->tm_hour;
		buffer[4] = tm->tm_min;
		buffer[5] = tm->tm_sec;
	} else {
		memset(buffer, 0, 6);
	}
}

static void
decode_position(const unsigned char *buffer, waypoint *waypt)
{
	waypt->latitude = le_read32(buffer + 0) / 10000000.0;
	waypt->longitude = le_read32(buffer + 4) / 10000000.0;
	waypt->altitude = FEET_TO_METERS(le_read16(buffer + 8));
}

static void
encode_position(const waypoint *waypt, unsigned char *buffer)
{
	le_write32(buffer + 0, (int) (waypt->latitude * 10000000));
	le_write32(buffer + 4, (int) (waypt->longitude * 10000000));
	le_write16(buffer + 8, METERS_TO_FEET(waypt->altitude));
}

static unsigned
decode_waypoint_id(const unsigned char *buffer)
{
	unsigned id = le_read16(buffer + 2);

	if (id >= MAX_WAYPOINTS) {
		fatal(MYNAME ": Invalid waypoint ID\n");
	}

	return id;
}

static waypoint *
decode_waypoint(const unsigned char *buffer)
{
	waypoint *waypt = waypt_new();

	decode_position(buffer + 12, waypt);
	waypt->shortname = xstrdup((char *)buffer + 4);
	waypt->icon_descr = icon_table[buffer[28]];
	waypt->creation_time = decode_datetime(buffer + 22);

	return waypt;
}

static void
encode_waypoint(const waypoint *waypt, unsigned char *buffer)
{
	buffer[0] = 0x00;
	buffer[1] = 0x40;
	le_write16(buffer + 2, 0);
	strncpy((char *)buffer + 4, waypt->shortname, 6);
	buffer[10] = 0;
	buffer[11] = 0;
	encode_position(waypt, buffer + 12);
	encode_datetime(waypt->creation_time, buffer + 22);
	buffer[28] = find_icon_from_descr(waypt->icon_descr);
	buffer[29] = 0;
	buffer[30] = 0x00;
	buffer[31] = 0x7e;
}

static waypoint *
decode_trackpoint(const unsigned char *buffer)
{
	waypoint *waypt = waypt_new();

	decode_position(buffer + 12, waypt);
	waypt->creation_time = decode_datetime(buffer + 22);
	WAYPT_SET(waypt, course, le_read16(buffer + 2));
	WAYPT_SET(waypt, speed, KPH_TO_MPS(buffer[29] * 2));

	return waypt;
}

static void
encode_trackpoint(const waypoint *waypt, unsigned serial, unsigned char *buffer)
{
	double x;
	double y;
	int32  z;
	char   zc;

	GPS_Math_WGS84_To_UTM_EN(waypt->latitude, waypt->longitude, &x, &y, &z, &zc);

	le_write16(buffer + 0, serial);
	le_write16(buffer + 2, WAYPT_GET(waypt, course, 0));
	le_write32(buffer + 4, x);
	le_write32(buffer + 8, y);
	encode_position(waypt, buffer + 12);
	encode_datetime(waypt->creation_time, buffer + 22);
	buffer[28] = z;
	buffer[29] = MPS_TO_KPH(WAYPT_GET(waypt, speed, 0) / 2);
	buffer[30] = 0x5a;
	buffer[31] = 0x7e;
}

static waypoint **
serial_read_waypoints(void)
{
	waypoint       **waypts = NULL;
	unsigned char  information[32];
	unsigned short total;
	unsigned short start;

	if (global_opts.masked_objective & RTEDATAMASK) {
		waypts = xcalloc(MAX_WAYPOINTS, sizeof(waypoint *));
	}

	write_packet(PID_QRY_INFORMATION, NULL, 0);
	read_packet(PID_DATA, information, sizeof(information), sizeof(information));

	total = le_read16(information + 0);

	for (start = 0; start < total; start += 32) {
		unsigned short count = total - start;
		unsigned char  payload[7];
		unsigned char  *waypoints;
		unsigned char  *w;

		if (count > 32) count = 32;

		le_write32(payload + 0, start);
		le_write16(payload + 4, count);
		payload[6] = 1;

		write_packet(PID_QRY_WAYPOINTS, payload, sizeof(payload));

		waypoints = xmalloc(count * 32);

		read_packet(PID_DATA, waypoints, count * 32, count * 32);

		for (w = waypoints; w < waypoints + count * 32; w = w + 32) {
			if (global_opts.masked_objective & WPTDATAMASK) {
				waypt_add(decode_waypoint(w));
			}
			if (global_opts.masked_objective & RTEDATAMASK) {
				waypts[decode_waypoint_id(w)] = decode_waypoint(w);
			}
		}

		xfree(waypoints);

		if (global_opts.verbose_status) {
			waypt_status_disp(total, start + count);
		}
	}

	return waypts;
}

static void
serial_write_waypoint(const waypoint *waypt)
{
	unsigned char data[32];
	unsigned char id[2];

	encode_waypoint(waypt, data);
	write_packet(PID_ADD_A_WAYPOINT, data, sizeof(data));
	read_packet(PID_DATA, id, sizeof(id), sizeof(id));
}

static void
serial_read_track(void)
{
	unsigned char  information[32];
	unsigned int   address;
	unsigned short total;
	route_head     *track;

	write_packet(PID_QRY_INFORMATION, NULL, 0);
	read_packet(PID_DATA, information, sizeof(information), sizeof(information));

	address = le_read32(information + 4);
	total = le_read16(information + 12);

	track = route_head_alloc();
	track_add_head(track);

	while (total > 0) {
		unsigned short count = total < MAX_READ_TRACKPOINTS ? total : MAX_READ_TRACKPOINTS;
		unsigned char  payload[7];
		unsigned char  *trackpoints;
		unsigned char  *t;

		le_write32(payload + 0, address);
		le_write16(payload + 4, count * 32);
		payload[6] = 0x00;

		write_packet(PID_READ_TRACKPOINTS, payload, sizeof(payload));

		trackpoints = xmalloc(count * 32);

		read_packet(PID_DATA, trackpoints, count * 32, count * 32);
		write_packet(PID_ACK, NULL, 0);

		for (t = trackpoints; t < trackpoints + count * 32; t = t + 32) {
			track_add_wpt(track, decode_trackpoint(t));
		}

		xfree(trackpoints);

		address = address + count * 32;
		total = total - count;
	}
}

static void
serial_write_track(void)
{
	unsigned char  information[32];
	unsigned int   address;
	unsigned short total;
	unsigned char  data[7];

	write_packet(PID_QRY_INFORMATION, NULL, 0);
	read_packet(PID_DATA, information, sizeof(information), sizeof(information));

	address = le_read32(information + 4);
	total = le_read16(information + 12);

	le_write32(data + 0, address + total * 32);
	le_write16(data + 4, track_data_ptr - track_data);
	data[6] = 0x00;

	write_packet(PID_WRITE_TRACKPOINTS, data, sizeof(data));
	gb_sleep(10000);
	write_packet(PID_DATA, track_data, track_data_ptr - track_data);
	read_packet(PID_CMD_OK, NULL, 0, 0);

	track_data_ptr = track_data;
}

static void
serial_write_track_start(const route_head *track)
{
	track_data = xmalloc(MAX_WRITE_TRACKPOINTS * 32);
	track_data_ptr = track_data;
	track_data_end = track_data + MAX_WRITE_TRACKPOINTS * 32;
}

static void
serial_write_track_point(const waypoint *waypt)
{
	if (track_data_ptr >= track_data_end) {
		serial_write_track();
	}

	encode_trackpoint(waypt, 0, track_data_ptr);

	track_data_ptr += 32;
}

static void
serial_write_track_end(const route_head *track)
{
	if (track_data_ptr > track_data) {
		serial_write_track();
	}

	xfree(track_data);
}

static void
serial_read_routes(waypoint **waypts)
{
	unsigned char information[32];
	unsigned char routec;
	unsigned char r;

	write_packet(PID_QRY_INFORMATION, NULL, 0);
	read_packet(PID_DATA, information, sizeof(information), sizeof(information));

	routec = information[2];

	for (r = 0; r < routec; r++) {
		unsigned char payload[7];
		unsigned char routedata[320];
		route_head    *route;
		int           sr;

		le_write32(payload + 0, r);
		le_write16(payload + 2, 0);
		payload[6] = 0x01;

		write_packet(PID_QRY_ROUTE, payload, sizeof(payload));
		read_packet(PID_DATA, routedata, 64, sizeof(routedata));

		route = route_head_alloc();
		route->rte_num = routedata[2];
		route->rte_name = xstrdup((char *)routedata + 4);
		route_add_head(route);

		for (sr = 0; sr < MAX_SUBROUTES; sr++) {
			int w;

			for (w = 0; w < MAX_SUBROUTE_LENGTH; w++) {
				unsigned short id = le_read16(routedata + 34 + 32 * sr + 2 *w);

				if (id == 0xffffu) {
					w = MAX_SUBROUTE_LENGTH;
					sr = MAX_SUBROUTES;
				} else if (id >= MAX_WAYPOINTS) {
					fatal(MYNAME ": Invalid waypoint ID in route\n");
				} else if (waypts[id] == NULL) {
					fatal(MYNAME ": Non-existent waypoint in route\n");
				} else {
					route_add_wpt(route, waypt_dupe(waypts[id]));
				}
			}
		}
	}
}

static void
serial_write_route_start(const route_head *route)
{
	route_ids = xmalloc(route->rte_waypt_ct * sizeof(unsigned));
	route_id_ptr = 0;
}

static void
serial_write_route_point(const waypoint *waypt)
{
	unsigned w;

	for (w = 0; w < MAX_WAYPOINTS; w++) {
		if (route_waypts[w] && compare_waypoints(waypt, route_waypts[w])) {
			break;
		}
	}

	if (w == MAX_WAYPOINTS) {
		unsigned char data[32];
		unsigned char id[2];

		encode_waypoint(waypt, data);
		write_packet(PID_ADD_A_WAYPOINT, data, sizeof(data));
		read_packet(PID_DATA, id, sizeof(id), sizeof(id));

		w = le_read16(id);

		route_waypts[w] = waypt_dupe(waypt);
	}

	route_ids[route_id_ptr++] = w;
}

static void
serial_write_route_end(const route_head *route)
{
	unsigned char *data;
	unsigned      src;
	unsigned      sr;
	unsigned char id[1];

	if (route_id_ptr > MAX_ROUTE_LENGTH) {
		fatal(MYNAME ": Route %s too long\n", route->rte_name);
	}

	src = (route_id_ptr + MAX_SUBROUTE_LENGTH) / MAX_SUBROUTE_LENGTH;
	data = xmalloc(32 + src * 32);

	le_write16(data + 0, 0x2000);
	data[2] = 0;
	data[3] = 0x20;
	strncpy((char *)data + 4, route->rte_name, 6);
	data[18] = 0;
	data[19] = 0;
	le_write32(data + 20, 0);
	le_write32(data + 24, 0);
	le_write16(data + 28, 0);
	data[30] = 0x7b;
	data[31] = 0x77;

	for (sr = 0; sr < src; sr++) {
		unsigned char *srdata = data + 32 + 32 * sr;
		unsigned      pt_offset = MAX_SUBROUTE_LENGTH * sr;
		unsigned      pt;

		le_write16(srdata + 0, 0x2010);

		for (pt = 0; pt < MAX_SUBROUTE_LENGTH; pt++) {
			if (pt_offset + pt < route_id_ptr) {
				le_write16(srdata + 2 + 2 * pt, route_ids[pt_offset + pt]);
			} else {
				le_write16(srdata + 2 + 2 * pt, 0xffffu);
			}
		}

		srdata[30] = 0x7f;
		srdata[31] = 0x77;
	}

	write_packet(PID_ADD_A_ROUTE, data, 32 + src * 32);
	read_packet(PID_DATA, id, sizeof(id), sizeof(id));

	xfree(data);
	xfree(route_ids);
}

static void
file_read(void)
{
	unsigned char data[32];
	route_head    *track = NULL;

	while (gbfread(data, sizeof(data), 1, file_handle) == 1) {
		switch (le_read16(data)) {
		case 0x2000:
			fatal(MYNAME ": Route objects not supported in file sources\n");
			break;
		case 0x2010:
			fatal(MYNAME ": Subroute objects not supported in file sources\n");
			break;
		case 0x4000:
			if (global_opts.masked_objective & WPTDATAMASK) {
				waypt_add(decode_waypoint(data));
			}
			break;
		default:
			if (global_opts.masked_objective & TRKDATAMASK) {
				if (track == NULL) {
					track = route_head_alloc();
					track_add_head(track);
				}

				track_add_wpt(track, decode_trackpoint(data));
			}
			break;
		}
	}
}

static void
file_write_waypoint(const waypoint *waypt)
{
	unsigned char data[32];

	encode_waypoint(waypt, data);
	gbfwrite(data, sizeof(data), 1, file_handle);
}

static void
file_write_track_start(const route_head *track)
{
	track_serial = 1;
}

static void
file_write_track_point(const waypoint *waypt)
{
	unsigned char data[32];

	encode_trackpoint(waypt, track_serial++, data);
	gbfwrite(data, sizeof(data), 1, file_handle);
}

static void
file_write_track_end(const route_head *track)
{
}

static void
file_write_route_start(const route_head *track)
{
	fatal(MYNAME ": Can't write routes to a file\n");
}

static void
file_write_route_point(const waypoint *waypt)
{
}

static void
file_write_route_end(const route_head *track)
{
}

static void
navilink_common_init(const char *name, const char *mode)
{
	if (gbser_is_serial(name)) {
		if ((serial_handle = gbser_init(name)) == NULL) {
			fatal(MYNAME ": Could not open serial port %s\n", name);
		}

		if (gbser_set_port(serial_handle, 115200, 8, 0, 1) != gbser_OK) {
			fatal(MYNAME ": Can't configure port\n");
		}

		write_packet(PID_SYNC, NULL, 0);
		read_packet(PID_ACK, NULL, 0, 0);

		if (nuketrk) {
			unsigned char information[32];
			unsigned char data[7];

			write_packet(PID_QRY_INFORMATION, NULL, 0);
			read_packet(PID_DATA, information, sizeof(information), sizeof(information));

			le_write32(data + 0, le_read32(information + 4));
			le_write16(data + 4, 0);
			data[6] = 0;

			write_packet(PID_ERASE_TRACK, data, sizeof(data));
			read_packet(PID_CMD_OK, NULL, 0, 0);
		}

		if (nukerte) {
			unsigned char data[4];

			le_write32(data, 0x00f00000);
			write_packet(PID_DEL_ALL_ROUTE, data, sizeof(data));
			read_packet(PID_CMD_OK, NULL, 0, 0);
		}

		if (nukewpt) {
			unsigned char data[4];

			le_write32(data, 0x00f00000);
			write_packet(PID_DEL_ALL_WAYPOINT, data, sizeof(data));
			read_packet(PID_ACK, NULL, 0, 0);
		}

		write_waypoint = serial_write_waypoint;
		write_track_start = serial_write_track_start;
		write_track_point = serial_write_track_point;
		write_track_end = serial_write_track_end;
		write_route_start = serial_write_route_start;
		write_route_point = serial_write_route_point;
		write_route_end = serial_write_route_end;
	} else {
		file_handle = gbfopen(name, mode, MYNAME);

		write_waypoint = file_write_waypoint;
		write_track_start = file_write_track_start;
		write_track_point = file_write_track_point;
		write_track_end = file_write_track_end;
		write_route_start = file_write_route_start;
		write_route_point = file_write_route_point;
		write_route_end = file_write_route_end;
	}

	return;
}

static void
navilink_rd_init(const char *name)
{
	navilink_common_init(name, "r");
}

static void
navilink_wr_init(const char *name)
{
	navilink_common_init(name, "w+");
}

static void
navilink_deinit(void)
{
	if (serial_handle) {
		if (poweroff) {
			write_packet(PID_QUIT, NULL, 0);
		}

		gbser_deinit(serial_handle);
	}

	if (file_handle) {
		gbfclose(file_handle);
	}

	return;
}

static void
navilink_read(void)
{
	if (serial_handle) {
		waypoint **waypts = NULL;

		if (global_opts.masked_objective & (WPTDATAMASK|RTEDATAMASK)) {
			waypts = serial_read_waypoints();
		}

		if (global_opts.masked_objective & TRKDATAMASK) {
			serial_read_track();
		}

		if (global_opts.masked_objective & RTEDATAMASK) {
			serial_read_routes(waypts);
		}

		if (waypts) {
			free_waypoints(waypts);
		}
	} else if (file_handle) {
		file_read();
	}
}

static void
navilink_write(void)
{
	switch (global_opts.objective)
	{
		case trkdata:
			track_disp_all(write_track_start,
				       write_track_end,
				       write_track_point);
			break;
		case wptdata:
			waypt_disp_all(write_waypoint);
			break;
		case rtedata:
			if (serial_handle) {
				route_waypts = serial_read_waypoints();
			}
			route_disp_all(write_route_start,
				       write_route_end,
				       write_route_point);
			if (route_waypts) {
				free_waypoints(route_waypts);
				route_waypts = NULL;
			}
			break;
		default:
			fatal(MYNAME ": Unknown objective.\n");
	}
}

ff_vecs_t navilink_vecs = {
	ff_type_serial,
	FF_CAP_RW_ALL,
	navilink_rd_init,
	navilink_wr_init,
	navilink_deinit,
	navilink_deinit,
	navilink_read,
	navilink_write,
	NULL,
	navilink_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
