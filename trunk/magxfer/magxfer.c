/*
 * Upload maps serially to Magellan GPS units.  
 * Detail maps:
 * 330, SporTrak Map, SporTrak Map/Pro, Meridian Green/Yellow, Gold, Platinum.
 * Base maps:
 * All the above *except* Meridian Green and Yellow.  The units with a 2MB
 *   base map have a different protocol used.
 *
 * Copyright 2003 by Robert Lipe.
 * robertlipe@usa.net
 */


#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>


#define xmalloc malloc
#define FRAME_SIZE 1000

int magfd;
struct termios orig_tio;
struct termios new_tio;

int debug_level = 0;
int synced;

typedef struct {
	unsigned int data_length;
	unsigned char *data;
} vld;

void dump_xframe(vld *frame);
void send_terminate(void);

const char *usagestring = "\
Usage: magxfer [-p portname] [-b bitrate] [-t type ] -f filetosend.img\n\
       portname   port for uplkoad (default /dev/ttyS0)\n\
       bitrate    bitrate (default is 4800)\n\
       type       can be \n\
       			b (secondary base map),\n\
			B (primary base map), or\n\
			d (detailed map)\n\
		   (default d)\n\
       filetosend.img is the image file you wish to upload to the unit\n";
void 
usage()
{
	fprintf(stderr, "\nmagxfer version " VERSION "\n");
	fprintf(stderr,"%s",usagestring);
}

void
debug(const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);

	if (0 == debug_level)
		return;

        vfprintf(stderr, fmt, ap);
}

/*
 * Magellan uses a simple checksum of 16 bit words, but remember that
 * things are optimized for the receiver, so they're big-endian.
 */
unsigned short
xor_checksum(unsigned char *p, int len)
{
	unsigned short data;
	unsigned short checksum = 0;
	int x;

	for (x = 0; x <len; x+= 2) {
		data = (p[x] <<  8 ) | p[x+1];
		checksum ^= data;
	}	

	return checksum;
}

/*
 * Get and return a single byte from the serial line.
 */
int rxc(void)
{
	unsigned char c;
	int maxc;

	for (maxc = 0; maxc < 10; maxc++) {
		size_t x;
		x = read(magfd, &c, 1);
		if (x > 0) {
			if (debug_level >= 9)  {
				fprintf(stderr, ">%02x<", c);
			}
			return c; 
		}
	}
	fprintf(stderr, "Timeout.  Unable to receive from GPS.\n");
	exit(1);
}

/*
 * Certain things in the protocol return a 3 byte sequence
 * starting with '0x8e'.  We know when these will happen, so
 * we read until we get the 0x8e and intuit that the next two
 * bytes are the numeric value to be returned.
 */
int get3(void)
{
	int x1,x2,x3;
	while ((x1 = rxc()) != 0x8e)
		;

	x2 = rxc();
	x3 = rxc();

	return (x2 << 8) | x3;
}

/*
 * Transmit a single framt to the unit.
 */
size_t
xmit_xframe(vld *frame, unsigned int frame_number)
{
	int i;
	int acked_frame;
	
	if (debug_level > 3) {
		dump_xframe(frame); 
	}

	debug("Sending packet %d ", frame_number);

	if (frame->data_length == 6) {
		int flen;
		int unit_sum;
		i = frame->data_length;

		write(magfd, frame->data, i);

		unit_sum = get3();
		if (unit_sum != 0) {
			fprintf(stderr, "Final checksum was 0x%x instead of 0.\n", unit_sum);
			exit(1);
		}
		
		flen = get3() << 16;
		flen |= get3();
		return flen;
	}


	/*
	 * First wait for unit to send us an OK, a hello, or an "I'm waiting"
	 */
retry_tx:
	for (i = 0; (i = rxc()) ;) {
		if (i == 0x55) break;
		if (i == 0x77) break;
		if (i == 0xaa) break;
	}

	i = frame->data_length + 10;
	write(magfd, frame->data, i);
	if (debug_level > 3) {
		int x ;
		fprintf(stderr, "Writing\n");
		for (x=0; x< i;x++) {
		  fprintf(stderr, "%02x ", frame->data[x]);
		}
		fprintf(stderr, "<\n");
	}

	/*
	 * Eat the 'OK' codes the unit spits out after the frame.
	 */	
	for (i = 0; (i = rxc()) ;) {
		if ((i != 0x55) && (i != 0xaa)) break;
	}

	switch (i) {
		case 0x77: 
			send_terminate();
			exit (1);

		case 0x81:
			debug("Retransmitting frame %d\n", frame_number);
			/*
			 * It's not documented, but the unit sends us two
			 * additional bytes in one of these.  I'm guessing it's
			 * the checksum it computed.  Read them and toss them.
			 */
			rxc();
			rxc();
			goto retry_tx;

		case 0x82:
			debug( "Unit saw rec length > 1024.  We sent %d\n",
				frame->data[0] << 8 | frame->data[1]);
			exit(1);
		default:
			abort();
		case 0x8e:
			break;
	}
	acked_frame = rxc() << 24;
	acked_frame |= rxc() << 16;

	for (i = 0; (i = rxc()) ;) {
		if ((i != 0x55) && (i != 0xaa)) break;
	}
	if (i == 0x77) send_terminate();

	if (i != 0x8e) abort();
	acked_frame |= rxc() << 8;
	acked_frame |= rxc();

	/*
	 * The spec is actually wrong on this.   It doesn't ack the frame
	 * we just sent.  The ack contains the next frame it wants.
	 * This has been confirmed by Magellan Engineering.
	 */
	if (acked_frame != frame_number + 1) {
		 fprintf(stderr, "Got ack for %x.  Expected %x\n", 
			acked_frame, frame_number);
		abort();
	}
	
	debug("Acked.\n");

	return 0;
}

/*
 * Display a frame in human-readable format.
 */
void
dump_xframe(vld *frame)
{
	unsigned int i;
	unsigned int edata = frame->data_length + 7;
	unsigned int words;
	unsigned int recnum;
	unsigned int checksum;

	assert(frame->data[0] == '[');
	debug("%x ", frame->data[0]);

	if (frame->data_length == 6) {
		debug("TERMINATION FRAME: ");
		for (i = 0; i < frame->data_length; i++) {
			debug("%02x ", frame->data[i]);
		}
		return;
	}
	words = (frame->data[1] << 8) | frame->data[2];
	debug("Words: %04x ", words);

	recnum = (frame->data[3] << 24) | (frame->data[4] << 16) |
		(frame->data[5] << 8) | (frame->data[6]);
		
	debug("Recnum: %08x\n", recnum);

	for (i = 0; i < frame->data_length; i++) {
		debug("%02x ", frame->data[i+7]);
	}
	checksum = (frame->data[edata] << 8) | frame->data[edata+1];
	debug("Checksum: %04x ", checksum);
/*	assert(0 ==  xor_checksum(&frame->data[3],  frame->data_length + 4)); */
	assert(frame->data[edata+2] == ']');
	debug(" %x\n\n", frame->data[edata+2]);
}

/*
 * Prepare a packet for transmission by adding framing, checksum, etc.
 */
vld *
make_xframe(void *data, int len, int recno)
{
	vld *odata = xmalloc(sizeof *odata);
	unsigned int words;
	unsigned int aligned_len;
	unsigned int checksum;

	/* 
	 * Special case for termination. 
	 */
	if (len == 0) {
		odata->data = xmalloc(6);
		odata->data[0] = '[';
		odata->data[1] = 0;
		odata->data[2] = 0;
		odata->data[3] = 0;
		odata->data[4] = 0;
		odata->data[5] = ']';
		odata->data_length = 6;
		return odata;
	}

	/* Round to even word alignment */

	aligned_len = (len + 1) & ~1;
	words = (4 + aligned_len) / 2;

	odata->data = xmalloc(aligned_len + 9);
	odata->data_length = aligned_len;

	odata->data[0] = '[';

	odata->data[1] = words >> 8;
	odata->data[2] = words;
	assert(words <= 1024);

	odata->data[3] = recno >> 24;
	odata->data[4] = recno >> 16;
	odata->data[5] = recno >> 8;
	odata->data[6] = recno;

	/* If we had to insert padding, this ensures it's zero.   If we
	 * didn't, it'll get clobbered by the memcpy which is fine.
	 */
	odata->data[aligned_len + 6] = 0;
	memcpy(&odata->data[7], data, len);

	checksum = xor_checksum(&odata->data[3],  aligned_len + 4);
	odata->data[aligned_len + 7] = checksum >> 8;
	odata->data[aligned_len + 8] = checksum;
	odata->data[aligned_len + 9] = ']';

	return odata;
}

/*
 * Something Very Bad has happened.  Send a zero-length frame to the unit
 * to tell it we're through playin' now.
 */
void
send_terminate(void)
{
	vld* vld;

	fprintf(stderr, "Hopelessly confused.   Terminating upload.\n");
	
	vld = make_xframe(NULL, 0, 0);
	xmit_xframe(vld, 0);
	exit(1);
}


/*
 * Send 'sz' bytes from buf to the unit, chunking it up, framing it, and
 * retransmitting chunks as needed.
 */
size_t
xmit(char *buf, int sz)
{
	int frame_number = 0;
	int left = sz;
	int n;
	vld* vld;

	for (n = 0; n < sz - FRAME_SIZE; n += FRAME_SIZE,frame_number++) {
		vld = make_xframe(&buf[n], FRAME_SIZE, frame_number);
		printf("\r%3.02f%% done", 100.0 * ((double) n / (double) sz));
		xmit_xframe(vld, frame_number);
		left -= FRAME_SIZE;
	}

	if (left > 0) {
		vld = make_xframe(&buf[n], left, frame_number);
		xmit_xframe(vld, frame_number);
	}

	vld = make_xframe(&buf[0], 0, frame_number++);
		printf("\r100.00%% done\n");
	return xmit_xframe(vld, frame_number);
}


/*
 * Given a numeric bitrate input, return a speed_t suitable for
 * stuffing into a termios.
 */
speed_t 
mkspeed(unsigned br)
{
	switch (br) {
		case 1200: return B1200;
		case 2400: return B2400;
		case 4800: return B4800;
		case 9600: return B9600;
		case 19200: return B19200;
#if defined B57600
		case 57600: return B57600;
#endif
#if defined B115200
		case 115200: return B115200;
#endif
		default: return B4800;
	}
}

void
restore_port()
{
	if (magfd) {
		tcsetattr(magfd, TCSAFLUSH, &orig_tio);
	}
}

/*
 * 
 */
void
setup_port(const char *portname, unsigned bitrate)
{
	magfd = open (portname, O_RDWR);

	if (magfd < 0) {
		fprintf(stderr, "Unable to open '%s'.  Error: %s\n", 
			portname, strerror(errno));
		exit(1);
	}

	tcgetattr(magfd, &orig_tio);
	new_tio = orig_tio;
	new_tio.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|
		IGNCR|ICRNL|IXON);
	new_tio.c_oflag = 0;
	new_tio.c_lflag = 0;
	new_tio.c_cflag &= ~(CSIZE|PARENB);
	new_tio.c_cflag |= CS8;
	new_tio.c_cc[VTIME] = 10;
	new_tio.c_cc[VMIN] = 0;
	cfsetospeed(&new_tio, mkspeed(bitrate));
	cfsetispeed(&new_tio, mkspeed(bitrate));
	tcsetattr(magfd, TCSAFLUSH, &new_tio);
}


/*
 * It might seem underambitious, but we really only need to send 
 * one command to the unit and we won't even get an ack back, so we
 * just sort of spray it out there as a constant and hope to rendezvous
 * soon.   While the spec doesn't say it, we rendezvous at 115.2K, so we
 * slam the local port to that speed immediately after we've written
 * the data.
 */
void
send_upload_cmd(unsigned detailed)
{
	static const char *cmd[] = {
		"$PMGNCMD,MPUPLOAD,1*71\r\n", 
		"$PMGNCMD,MPUPLOAD,2*72\r\n", 
		"$PMGNCMD,MPUPLOAD,3*73\r\n",
	};
	write(magfd, cmd[detailed], strlen(cmd[detailed] + 1));
	cfsetospeed(&new_tio, B115200);
	cfsetispeed(&new_tio, B115200);
	tcsetattr(magfd, TCSADRAIN, &new_tio);
}

void
alarm_handler(int a)
{
	if (synced) {
		signal(SIGALRM, SIG_DFL);
		return;
	}
	restore_port();
	fprintf(stderr, "Fatal error: No communications %d.\n", magfd);
	exit(1);
	
}

/*
 * Listen for "hello" packets from the unit.  If found, the rendezvous must
 * have succeeded so we send back a "hello".
 */
void
sync_receiver(void)
{
	int i;
	char c = 0x55;

	signal(SIGALRM, alarm_handler);
	alarm(5);

	synced = 0;
	for (i = 0;synced == 0;) {
		switch (i = rxc()) {
			case 0:	break;
			case 0xaa:  synced=1; 
			case 0x77:  synced=1;
		}
	}

	write(magfd, &c, 1);

	/*
	 * The spec says we shouldn't have to wait again, but this makes
	 * the communications setup way more reliable.
	 */
	synced = 0;
	for (i = 0;synced == 0;) {
		switch (i = rxc()) {
			case 0:	break;
			case 0xaa:  synced=1; 
			case 0x77:  synced=1;
		}
	}
}

int
main(int argc, char *argv[])
{
	static	char ibuf[64*1024*1024];
	unsigned short cksum;
	size_t file_sz;
	size_t sent_sz;
	FILE *inf;
	int c;
	unsigned bitrate = 4800;
	int detailedmap = 1;
	const char *portname = "/dev/ttyS0";
	const char *ifilename = "";


	while ((c = getopt(argc, argv, "t:f:p:b:D:")) != EOF) {
		switch(c) {
		        case 't':
			    if (strcmp(optarg,"s") == 0)
				detailedmap = 2;
			    else if (strcmp(optarg,"d") == 0)
				detailedmap = 1;
			    else if (strcmp(optarg,"p") == 0)
				detailedmap = 0;
			    else {
				fprintf(stderr,"Map type (-t option) can only be 'd', 'p', or 's'\n");
				exit(1);
			    }
			    break;
			case 'p':
				portname = optarg;
				break;
			case 'f':
				ifilename  = optarg;
				break;
			case 'D':
				debug_level = atoi(optarg);			
				break;
			case 'b':
				bitrate = atoi(optarg);
				break;
			default:
			        usage();
				exit(1);
				break;
		}
	}
	if (ifilename[0] == '\0') {
	        fprintf(stderr, "No input file specified. Exiting.\n");
		usage();
	        exit(1);
	}
	setup_port(portname, bitrate);
	send_upload_cmd(detailedmap);
	sync_receiver();

	inf = fopen(ifilename, "r");
	file_sz = fread(ibuf, 1, sizeof(ibuf), inf);
	if (file_sz == sizeof(ibuf)) {
		fprintf(stderr, "File '%s' bigger than %d bytes.  Exiting.\n",
			ifilename, sizeof(ibuf));
		exit(1);
	}

	cksum = xor_checksum(ibuf, file_sz);
	ibuf[file_sz++] = cksum >> 8;
	ibuf[file_sz++] = cksum ;

	sent_sz = xmit(ibuf, file_sz);
	if (sent_sz != file_sz) {
		fprintf(stderr, "We sent %d bytes but the unit saw %d\n", 
			sent_sz, file_sz);
		exit(1);
	}

	return 0;
}
