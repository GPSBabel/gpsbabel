
















































































































































































































                     sizeof(MSG_QUERY_SOFTWARE_VERSION));
                   sizeof(MSG_QUERY_SOFTWARE_VERSION));
             "sectors at once, falling back to single read.\n");
            ilen, pst->tpn, plen);
           "used but device reported %i.\n",
           ack_msg[0], id);
           i+s, sectors_used);
          -> caller should either resend request or give up.
          /* On failure, try with less sectors */
          /* some (all?) devices first send an ACK with id==0, skip that */
          break;
          break;
          continue;
          continue;
          continue;
          db(1, MYNAME ": Device doesn't seem to support reading multiple "
          db(1, MYNAME ": Warning: Got unexpected ACK (id=0x%02x)\n", ack_msg[1]);
          db(3, "Got ACK (id=0x%02x)\n", id);
          got_sectors = 1;
          got_sectors = read_at_once;
          multi_read_supported = 0;
          read_at_once = MAX(read_at_once/2, 1);
          read_at_once = MIN(read_at_once*2, atoi(opt_read_at_once));
          result=-1;
          result=1;
          return res_OK;
          sizeof(MSG_QUERY_SOFTWARE_VERSION)) != res_OK)
          warning(MYNAME ": cannot set poi %d '%s'\n", poinum, poinames[poinum]);
         "revision (Y/M/D) = %02i/%02i/%02i\n",
         "using max=%i\n", sectors_used_a, sectors_used_b, MAX(sectors_used_a, sectors_used_b));
         * And read at least 1 sector.
         * If tere aren't any so many interesting ones, read the remainder (sectors_used-i).
         */
         buf_end_tag[i], isprint(buf_end_tag[i]) ? buf_end_tag[i] : '.',
         MSG_SOFTWARE_VERSION.kernel_ver[1], MSG_SOFTWARE_VERSION.kernel_ver[2],
         MSG_SOFTWARE_VERSION.kernel_ver[3],
         MSG_SOFTWARE_VERSION.odm_ver[1], MSG_SOFTWARE_VERSION.odm_ver[2],
         MSG_SOFTWARE_VERSION.odm_ver[3],
         MSG_SOFTWARE_VERSION.revision[1], MSG_SOFTWARE_VERSION.revision[2],
         MSG_SOFTWARE_VERSION.revision[3]);
         opt_last_sector_val, sectors_total);
         rcv_len, len);
         SECTOR_READ_END[i], isprint(SECTOR_READ_END[i]) ? SECTOR_READ_END[i] : '.');
        /* Try to read read_at_once sectors at once.
        be_write16(MSG_SET_POI+1, poinum);
        be_write32(MSG_LOG_CONFIGURE_CONTROL+1, tmax);
        be_write32(MSG_LOG_CONFIGURE_CONTROL+13, dmin);
        be_write32(MSG_LOG_CONFIGURE_CONTROL+5, tmin);
        be_write32(MSG_LOG_CONFIGURE_CONTROL+9, dmax);
        be_write_double(MSG_SET_POI+11, ecef_y);
        be_write_double(MSG_SET_POI+19, ecef_z);
        be_write_double(MSG_SET_POI+3, ecef_x);
        break;
        break;
        case res_NACK:
        case res_OK:
        continue;
        continue;
        db(0, "Reconfiguring logging to: tmin=%u, tmax=%u, dmin=%u, dmax=%u\n", tmin, tmax, dmin, dmax);
        db(1, MYNAME ": Empty sector encountered, terminating.\n");
        db(1, MYNAME ": Empty sector encountered: apparently only %i sectors are "
        db(1, MYNAME ": Last sector is nearly full, reading one more sector\n");
        db(1, MYNAME ": Set baud rate to %d failed (%d)\n", baud_rates[i], rc);
        db(1, MYNAME ": set POI[%s]='%f %f %f/%f %f %f'\n", poinames[poinum], lat, lng, alt, ecef_x, ecef_y, ecef_z);
        db(1, MYNAME "Option usage: configlog=tmin:tmax:dmin:dmax");
        db(2, "Didn't receive ACK (%d)\n", rc);
        db(3, "found %d elems '%s':poi=%s@%d, lat=%f, lng=%f, alt=%f over=%s\n", n, opt_poi, poinames[poinum], poinum, lat, lng, alt);
        db(3, "Warning: Got NACK (id=0x%02x)\n", ack_msg[1]);
        db(3, "Warning: Got unexpected message (id=0x%02x), expected ACK (id=0x%02x)\n",
        db(4, "%s\n", dump);
        default:
        dump[16*3] = ' '; // gets overwritten with 0 by snprintf
        dump[3*16+1+(i%16)] = ' ';
        i = sectors_used; /* terminate to avoid reading stale data still in the logger */
        if (ack_msg[1] == id) {
        if (rc == res_OK) {
        if (skytraq_wr_msg_verify((uint8_t*)&MSG_SET_POI, sizeof(MSG_SET_POI)) == res_OK) {
        if (skytraq_wr_msg_verify(MSG_QUERY_SOFTWARE_VERSION,
        j = 0;
        j = 1;
        j++;
        lla2ecef(lat, lng, alt, &ecef_x, &ecef_y, &ecef_z);
        memset(&dump[(i%16)*3], ' ', 3);
        MSG_SET_POI[27]=0;
        rc = skytraq_read_multiple_sectors(i, read_at_once, buffer);
        rc = skytraq_read_single_sector(i, buffer);
        read_at_once = MAX(MIN(read_at_once, sectors_used-i), 1);
        result=-1;
        return -1;
        return res_NACK;
        sectors_used++;
        snprintf(&dump[(i%16)*3], 4, "%02x ", buf[i]);
        snprintf(&dump[3*16+1+(i%16)], 2, "%c", isprint(buf[i]) ? buf[i] : '.');
        switch (rc) {
        warning(MYNAME ": argument to %s needs to be like <lat>:<lng>[:<alt>]\n", poinames[poinum]);
        {
        }
        }
        }
        }
        } else if (ack_msg[1] == 0) {
        } else {
        } else {
        }*/
       * parse format of <lat>:<lng>[:alt]
       * we assume at least two elements in the value string
       */
       buf[i+sizeof(SECTOR_READ_END)], cs);
       buf_end_tag[sizeof(SECTOR_READ_END)], cs);
       c.dt, c.dx, c.dy, c.dz,
       f.gps_week, f.gps_sec,
       f.x, f.y, f.z,
       ITEM_SPEED(pitem));
       ITEM_SPEED(pitem), (pitem->comp.dpos[2] & 0x0F)>>2);
       m.gps_week, m.gps_sec,
       m.lat, m.lon, m.alt,
       opt_first_sector_val, sectors_total);
       poi ? "POI" : "full",
       spe);
      (*errors)--;
      */
      /*
      /* payload too short or didn't receive a message at all
      break;
      break;
      buf[i+c] = buffer[c];
      c.dx = 511-c.dx;  /* make proper signed values */
      c.dy = 511-c.dy;
      c.dz = 511-c.dz;
      continue;   // skip decoding
      db(1, "resending msg (id=0x%02x)...\n", payload[0]);
      db(1, "Warning: device reported inconsistent number of used sectors (a=%i, b=%i), "\
      db(1, "Warning: sector# specified by option last-sector (%i) is beyond reported total sector count (%i)",
      db(1, MYNAME ": desired last-sector #%i reached, terminating.\n", sectors_read-1);
      db(1, MYNAME ": got POI[%s]='%f %f %f/%f %f %f'\n", poinames[poi], lat, lng, alt, ecef_x, ecef_y, ecef_z);
      db(1, MYNAME ": New Track\n");
      db(1, MYNAME ": Not enough bytes in sector for a compact item.\n");
      db(1, MYNAME ": Not enough bytes in sector for a full item.\n");
      db(1, MYNAME ": Not enough bytes in sector for a full item.\n");
      db(1, MYNAME ": rd_char(): Got error: %d\n", c);
      db(1, MYNAME ": rd_word(): Read error\n");
      db(1, MYNAME ": Received message too short (got %i bytes, expected %i)\n",
      db(1, MYNAME ": Set baud rate to %d failed (%d), retrying...\n", baud_rates[i], rc);
      db(1, MYNAME ": Venus device found: Kernel version = %i.%i.%i, ODM version = %i.%i.%i, "\
      db(1, MYNAME ": Wrong end tag: got 0x%02x ('%c'), expected 0x%02x ('%c')\n",
      db(2, "Didn't receive ACK (%d), retrying...\n", rc);
      db(2, "Didn't receive expected reply (%d)\n", rc);
      db(2, MYNAME" : skipped poi %d for X=%f, y=%f, Z=%f\n", ecef_x, ecef_y, ecef_z);
      db(4, "rd_char(): Got char: %02x '%c'\n", c, isprint(c) ? c : '.');
      db(4, MYNAME ": Decoding sector #%i...\n", i+s);
      ECEF_to_LLA(ecef_x, ecef_y, ecef_z, &lat, &lng, &alt);
      fatal(MYNAME ": Error %i while processing data item #%i (starts at %i)\n",
      fatal(MYNAME ": Error reading sector %i\n", i);
      gbfwrite(buffer, SECTOR_SIZE, got_sectors, dumpfile);
      if ((i+1)%16 == 0) {
      if ((rc = gbser_set_speed(serial_handle, baud_rates[i])) != gbser_OK) {
      if ((rc = skytraq_expect_ack(0x02)) != res_OK) {
      if (ack_msg[0] == 0x83) {
      if (atoi(opt_read_at_once) == 0  ||  multi_read_supported == 0) {
      if (buffer[c] == SECTOR_READ_END[j]) {
      if (i < len) {
      if (n >= 2) {
      if (nn>3) {
      if (rc < (4096-FULL_ITEM_LEN)) {
      if (rc == 0) {
      j = 0;
      j = 1;
      j++;
      lat=lng=alt=0.0;
      n = sscanf(opt_poi, "%lf:%lf:%lf", &lat, &lng, &alt);
      nn = sscanf(opt_configure_logging, "%u:%u:%u:%u", &tmin, &tmax, &dmin, &dmax);
      pst->route_head_ = route_head_alloc();
      rc = process_data_sector(&st, buffer+s*SECTOR_SIZE, SECTOR_SIZE);
      return baud_rates[i];
      return c;
      return i;
      return len;
      return rc;
      return rc;
      return res_ERROR;
      return res_ERROR;
      return res_ERROR;
      return res_ERROR;
      return res_ERROR;
      return res_PROTOCOL_ERR;
      sectors_used = opt_first_sector_val + 1;
      skytraq_wr_msg(MSG_QUERY_SOFTWARE_VERSION,  /* get firmware version */
      state = 0;
      state = 1;
      state++;
      track_add_head(pst->route_head_);
      warning(MYNAME ": cannot read poi %d '%s'\n", poi, poinames[poi]);
      waypt_add(new Waypoint(*tpt));
      waypt_add(wpt);
      wpt = new Waypoint;
      wpt->altitude       = alt;
      wpt->description    = QString().sprintf("miniHomer points to this coordinates if the %s symbol is on", poinames[poi]);
      wpt->latitude       = lat;
      wpt->longitude      = lng;
      wpt->shortname      = QString().sprintf("POI_%s", poinames[poi]);
      }
      }
      }
      }
      }
      }
      }
      }
      }
      }
      }
      } else if (ack_msg[0] == 0x84) {
      } else if (buffer[c] == SECTOR_READ_END[0]) {
      } else if (rc >= (4096-FULL_ITEM_LEN) && i+s+1 >= sectors_used && i+s+1 < sectors_total) {
      } else {
      } else {
      } else {
      } else {
      } else {
      } else {
     indicator for how many sectors are currently used. However this isn't correct in every case too.
     sectors_free, sectors_total, 100 - sectors_free*100 / sectors_total, log_wr_ptr);
     The current read logic is aware of that so this shouldn't be necessary anymore.
    "-1", ARGTYPE_INT, "-1", "65535", nullptr
    "-1", ARGTYPE_INT, "-1", "65535", nullptr
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    "0", ARGTYPE_INT, "0", "65535", nullptr
    "0", ARGTYPE_INT, "0", "65535", nullptr
    "0", ARGTYPE_INT, "4800", "230400", nullptr
    "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
    "230400", ARGTYPE_INT, "0", "230400", nullptr
    "255", ARGTYPE_INT, "0", "255", nullptr
    "baud", &opt_dlbaud, "Baud rate used for download",
    "configlog", &opt_configure_logging, "Configure logging parameter as tmin:tmax:dmin:dmax",
    "dump-file", &opt_dump_file, "Dump raw data to this file",
    "erase", &opt_erase, "Erase device data after download",
    "first-sector", &opt_first_sector, "First sector to be read from the device",
    "first-sector", &opt_first_sector, "First sector to be read from the file",
    "gps_utc_offset", &opt_gps_utc_offset, "Seconds that GPS time tracks UTC (0: best guess)",
    "gps_utc_offset", &opt_gps_utc_offset, "Seconds that GPS time tracks UTC (0: best guess)",
    "initbaud", &opt_initbaud, "Baud rate used to init device (0=autodetect)",
    "last-sector", &opt_last_sector, "Last sector to be read from the device (-1: smart read everything)",
    "last-sector", &opt_last_sector, "Last sector to be read from the file (-1: read till empty sector)",
    "no-output", &opt_no_output, "Disable output (useful with erase)",
    "read-at-once", &opt_read_at_once, "Number of sectors to read at once (0=use single sector mode)",
    "targetlocation", &opt_set_location, "Set location finder target location as lat,lng",
    (at your option) any later version.
    /*    note: _verify retries on errors, probe takes too long.
    /* fall through: */
    /* fall through: */
    // todo - how to determine not-set POIs ?
    // we need this call it initialized waypoint list etc...
    0       // attr (u8, 1-> to flash, 0->ro sram)
    0, 0, 0, 0, 0, 0, 0, 0, //alt (double ecef)
    0, 0, 0, 0, 0, 0, 0, 0, //lat (double ecef)
    0, 0, 0, 0, 0, 0, 0, 0, //lng (double ecef)
    0x00      // reserved
    0x00, 0x00, 0x00, 0x00, // min_distance
    0x00, 0x00, 0x00, 0x00, // min_speed
    0x00, 0x00, 0x00, 0x06, // min_time: was 0x00000005
    0x00, 0x00, 0x0e, 0x10, // max_time: was 0x0000ffff (big endian!)
    0x00, 0x00, 0x27, 0x10, // max_distance: was 0x0000ffff
    0x00, 0x00, 0xff, 0xff, // max_speed
    0x01,     // datalog_enable: NOTE: always ON
    0x18,     // message_id
    0x4C, 0,  0,    // cmd + poi (u16)
    2008         J.C Haessig, jean-christophe.haessig (at) dianosis.org
    2009-09-06 | Josef Reisinger | Added "set target location", i.e. -i skytrag,targetlocation=<lat>:<lng>
    2010-10-23 | Josef Reisinger | Added read/write for miniHomer POI
    = { 0x05, 0x00, 0x00, 0x02 };
    along with this program; if not, write to the Free Software
    alt = m.alt * POW_2_M7;
    baud_rates[0] = initbaud;
    baud_rates_count = 1;
    break;
    break;
    break;
    break;
    break;
    break;
    break;
    break;
    break;
    break;
    buf[i] = 0xFF;
    buf[i] = buffer[c];
    buf[i] = c;
    buf[i] = c;
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    c = gbser_readc_wait(serial_handle, TIMEOUT);
    c = rd_char(&errors);
    c = rd_char(&errors);
    c = rd_char(&errors);
    c = rd_char(&errors); /* read checksum byte */
    c.dt = (pitem->comp.dt[0] << 8) | pitem->comp.dt[1];
    c.dx = (pitem->comp.dpos[1] >> 6) | (pitem->comp.dpos[0] << 2);
    c.dy = (pitem->comp.dpos[1] & 0x3F) | ((pitem->comp.dpos[2] & 0xF0) << 2);
    c.dz = pitem->comp.dpos[3] | ((pitem->comp.dpos[2] & 0x03) << 8);
    calc_cs ^= c;
    char buf[32];
    Copyright (C) 2008-2012  Mathias Adam, m.adam (at) adamis.de
    cs ^= buf[i];
    db(1, "Warning: sector# specified by option first-sector (%i) is beyond reported total sector count (%i)",
    db(1, MYNAME ": Checksum error while reading sector: got 0x%02x, expected 0x%02x\n",
    db(1, MYNAME ": Checksum error while reading sector: got 0x%02x, expected 0x%02x\n",
    db(1, MYNAME ": Didn't get message start tag\n");
    db(1, MYNAME ": Didn't get sector end tag\n");
    db(1, MYNAME ": Didn't get sector end tag\n");
    db(1, MYNAME ": Didn't receive ACK\n");
    db(1, MYNAME ": Didn't receive ACK\n");
    db(1, MYNAME ": Didn't receive expected reply (%d)\n", rc);
    db(1, MYNAME ": Error sending LOG STATUS CONTROL message (%d)\n", rc);
    db(1, MYNAME ": Got neither ACK nor NACK, ");
    db(1, MYNAME ": Probing SkyTraq Venus at %ibaud...\n", baud_rates[i]);
    db(1, MYNAME ": rd_buf(): Read error (%d)\n", rc);
    db(1, MYNAME ": rd_buf(): Read timout\n");
    db(1, MYNAME ": rd_drain(): Comm error\n");
    db(1, MYNAME ": Unknown item type encountered: 0x%02x\n", ITEM_TYPE(pitem));
    db(2, "Receiving data of sector #%i...\n", first_sector+i);
    db(2, "Warning: error setting skytraq device baud rate\n");
    db(2, "Warning: error setting uart baud rate\n");
    db(3, "%02x ", MSG_SET_LOCATION[i]);
    db(4, "Got %s item: week=%i  sec=%i  x=%i  y=%i  z=%i  speed=%i\n",
    db(4, "Got compact item: dt=%i  dx=%i  dy=%i  dz=%i  speed=%i uu=%i\n",
    db(4, "Got multi hz item: week=%i sec=%i lat=%i  lon=%i  alt=%i  speed=%f\n",
    db(4, "rd_buf():  dump follows:\n");
    db(4, MYNAME ": Decoding sector #%i...\n", sectors_read++);
    db(4, MYNAME ": Seeking to first-sector index %i\n", opt_first_sector_val*SECTOR_SIZE);
    dump[sizeof(dump)-1] = 0;
    dumpfile = gbfopen(opt_dump_file, "w", MYNAME);
    ECEF_to_LLA(pst->x, pst->y, pst->z, &lat, &lon, &alt);
    ecef_x=be_read_double(buf+1);
    ecef_y=be_read_double(buf+9);
    ecef_z=be_read_double(buf+17);
    f.gps_sec = ts >> 12;
    f.gps_week = ts & 0x000003FF;
    f.x = me_read32(pitem->full.x);
    f.y = me_read32(pitem->full.y);
    f.z = me_read32(pitem->full.z);
    fatal(MYNAME ": Can't find skytraq device on '%s'\n", qPrintable(fname));
    fatal(MYNAME ": Can't get log buffer status\n");
    fatal(MYNAME ": Can't open file '%s'\n", qPrintable(fname));
    fatal(MYNAME ": Can't open port '%s'\n", qPrintable(fname));
    fatal(MYNAME ": cannot set new location\n");
    fatal(MYNAME ": Checksum error: got 0x%02x, expected 0x%02x\n", rcv_cs, calc_cs);
    fatal(MYNAME ": Didn't get message end tag (CR/LF)\n");
    fatal(MYNAME ": Invalid sector count (%i)\n", sector_count);
    fatal(MYNAME ": Invalid sector number (%i)\n", first_sector);
    fatal(MYNAME ": Invalid sector number (%i)\n", sector);
    fatal(MYNAME ": Unsupported baud rate: %ibd\n", baud);
    fatal(MYNAME ": Write error (%d)\n", rc);
    ff_cap_none       /* routes */
    ff_cap_none       /* routes */
    ff_cap_none       /* routes */
    ff_cap_read       /* tracks */,
    ff_cap_read       /* tracks */,
    ff_cap_read       /* tracks */,
    ff_cap_read     /* waypoints */,
    ff_cap_read     /* waypoints */,
    ff_cap_read     /* waypoints */,
    for (c = 0; c < 16 && j < sizeof(SECTOR_READ_END); c++) {
    for (i = 0; i < (len+15)/16*16; i++) {    // count to next 16-byte boundary
    for (s = 0; s < got_sectors; s++) {
    for (t = 0, got_sectors = 0; (t < SECTOR_RETRIES) && (got_sectors <= 0); t++) {
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA
    gb_sleep(50);   /* allow UART to settle. */
    gbfclose(dumpfile);
    gbfseek(file_handle, opt_first_sector_val*SECTOR_SIZE, SEEK_SET);
    GNU General Public License for more details.
    gps_timet -= override;
    gps_timet--;  /*   GPS-UTC = 14s      */
    gps_timet--;  /*   GPS-UTC = 15s      */
    gps_timet--;  /*   GPS-UTC = 16s      */
    gps_timet--;  /*   GPS-UTC = 17s      */
    gps_timet--;  /*   GPS-UTC = 18s      */
    if ((rc = gbser_set_speed(serial_handle, baud_rates[i])) != gbser_OK) {
    if ((rc = skytraq_expect_ack(0x02)) != res_OK) {
    if (*opt_configure_logging) {
    if (*opt_no_output == '1') {
    if (*opt_poi) {
    if (0 == pst->route_head_) {
    if (buf_end_tag[i] != SECTOR_READ_END[i]) {
    if (c < 0) {
    if (c == MSG_START[state]) {
    if (c == SECTOR_READ_END[j]) {
    if (c.dx > 511) {
    if (c.dy > 511) {
    if (c.dz > 511) {
    if (dumpfile) {
    if (ecef_x < 100.0 && ecef_y < 100.0 && ecef_z < 100.0) {
    if (got_sectors <= 0) {
    if (i > 0) {
    if (ilen <= 0) {
    if (len < COMPACT_ITEM_LEN) {
    if (len < FULL_ITEM_LEN) {
    if (len < MULTI_HZ_ITEM_LEN) {
    if (opt_first_sector_val >= sectors_used) {
    if (opt_last_sector_val < 0) {
    if (opt_last_sector_val >= sectors_total) {
    if (payload[0] == id) {
    if (poi) {
    if (rc < (int)sizeof(MSG_SOFTWARE_VERSION)) {
    if (rc < 0) {
    if (rc == res_OK  ||  rc == res_NACK) {
    if (rcv_len >= 0) { /* negative values indicate receive errors */
    if (sectors_used_a != sectors_used_b) {
    if (skytraq_baud > 0)  timeout = TIMEOUT + (long long int)len*1000*10/(long long int)skytraq_baud;
    if (skytraq_rd_msg(ack_msg, sizeof(ack_msg)) == res_OK) {
    if (skytraq_wr_msg_verify((uint8_t*)&MSG_GET_POI, sizeof(MSG_GET_POI)) != res_OK) {
    if (strcmp(poinames[i], name) == 0) {
    ilen = process_data_item(pst, (item_frame*)&buf[plen], len-plen);
    it under the terms of the GNU General Public License as published by
    lat = m.lat * POW_2_M20;
    lon = m.lon * POW_2_M20;
    m.alt = me_read32(pitem->multi_hz.alt);
    m.gps_sec = ((int)(ts & 0x3FFFFFFF)) / 1000;
    m.gps_week = ITEM_WEEK_NUMBER(pitem);
    m.lat = me_read32(pitem->multi_hz.lat);
    m.lon = me_read32(pitem->multi_hz.lon);
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    miniHomer_get_poi();    // add POI as waypoints to the waypoints of the track
    MSG_CONFIGURE_SERIAL_PORT[2] = 0;
    MSG_CONFIGURE_SERIAL_PORT[2] = 1;
    MSG_CONFIGURE_SERIAL_PORT[2] = 2;
    MSG_CONFIGURE_SERIAL_PORT[2] = 3;
    MSG_CONFIGURE_SERIAL_PORT[2] = 4;
    MSG_CONFIGURE_SERIAL_PORT[2] = 5;
    MSG_CONFIGURE_SERIAL_PORT[2] = 6;
    MSG_GET_POI[1]=(poi>>8)&0xff;
    MSG_GET_POI[2]=(poi)&0xff;
    npoi++;
    npoi++;
    npoi++;
    npoi++;
    npoi++;
    NULL, ARGTYPE_OUTFILE, ARG_NOMINMAX, nullptr
    NULL, ARGTYPE_STRING, "", "", nullptr
    NULL, ARGTYPE_STRING, "", "", nullptr
    poi = 1;
    poi = 1;
    pst->gps_sec += c.dt;
    pst->gps_sec = f.gps_sec;
    pst->gps_sec = m.gps_sec;
    pst->gps_week = f.gps_week;
    pst->gps_week = m.gps_week;
    pst->x += c.dx;
    pst->x = f.x;
    pst->y += c.dy;
    pst->y = f.y;
    pst->z += c.dz;
    pst->z = f.z;
    rc = process_data_sector(&st, buffer, got_bytes);
    rc = skytraq_expect_ack(payload[0]);
    rc = skytraq_expect_msg(0x80, (uint8_t*)&MSG_SOFTWARE_VERSION, sizeof(MSG_SOFTWARE_VERSION));
    rc = skytraq_rd_msg(payload, len);
    rd_buf(buf+i*SECTOR_SIZE, SECTOR_SIZE);
    rd_buf(buffer, 16);
    rd_drain();
    res = COMPACT_ITEM_LEN;
    res = FULL_ITEM_LEN;
    res = MULTI_HZ_ITEM_LEN;
    return 0;
    return gps_timet;
    return rc;
    return read_result;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return res_ERROR;
    return;
    return;
    sectors_used = MAX(sectors_used_a, sectors_used_b);
    sectors_used = opt_last_sector_val;
    sectors_used = sectors_total - sectors_free + 1 /*+5*/;
    sectors_used_a = sectors_total - sectors_free;
    sectors_used_b = (log_wr_ptr + SECTOR_SIZE - 1) / SECTOR_SIZE;
    Serial download of track data from GPS loggers with Skytraq chipset.
    skytraq_configure_logging();
    skytraq_erase();
    skytraq_rd_deinit();    // skytraq_read called system_reset, which changes the baud rate.
    skytraq_rd_init(mhport);    // Lets start from scratch and re-init the port
    skytraq_rd_msg(buf, 25);
    skytraq_rd_msg(buf, 32);
    skytraq_read();       // first read tracks (if not supressed by cmd line params)
    skytraq_read_tracks();
    skytraq_set_baud(dlbaud);
    skytraq_set_baud(skytraq_baud);   // note that _system_restart resets baud rate anyway...
    skytraq_set_location();
    skytraq_wr_msg(MSG_QUERY_SOFTWARE_VERSION,  /* get firmware version */
    skytraq_wr_msg(payload, len);
    skytraq_wr_msg_verify(&MSG_GET_LOCATION, 1);
    spe = KPH_TO_MPS(be_read16(pitem->multi_hz.v_kmh));
    struct compact_item_frame comp;
    struct full_item_frame full;
    struct multi_hz_item_frame multi_hz;
    the Free Software Foundation; either version 2 of the License, or
    This program is distributed in the hope that it will be useful,
    This program is free software; you can redistribute it and/or modify
    total_sectors_read += got_sectors;
    tpt = make_trackpoint(pst, lat, lon, alt);
    tpt = make_trackpoint(pst, lat, lon, alt);
    track_add_wpt(pst->route_head_, tpt);
    track_add_wpt(pst->route_head_, tpt);
    ts = me_read32(pitem->full.ts);
    ts = me_read32(pitem->multi_hz.ts);
    uint8_t datalog_enable[1], log_fifo_mode[1];
    uint8_t id;
    uint8_t id[1];
    uint8_t kernel_ver[4];
    uint8_t log_wr_ptr[4];
    uint8_t max_time[4], min_time[4], max_dist[4], min_dist[4], max_speed[4], min_speed[4];
    uint8_t odm_ver[4];
    uint8_t revision[4];
    uint8_t sectors_free[2];
    uint8_t sectors_total[2];
    uint8_t sw_type;
    vprintf(msg, ap);
    WAYPT_SET(tpt, speed, KPH_TO_MPS(ITEM_SPEED(pitem))); /* convert speed to m/s */
    WAYPT_SET(tpt, speed, spe); /* convert speed to m/s */
    wr_char(str[i]);
    You should have received a copy of the GNU General Public License
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    } else if (c == MSG_START[0]) {
    } else if (c == SECTOR_READ_END[0]) {
    } else if (sectors_read-1 >= opt_last_sector_val) {
    } else {
    } else {
    } else {
    } else {
    } else {
    } else {
    }*/
   *     (i.e. between 22 Aug 1999 and 7 April 2019), so this
   *     (i.e. sec >= 7*24*3600 = 604800 is allowed)
   *     <http://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat>
   *     <http://maia.usno.navy.mil/ser7/tai-utc.dat>
   *     Announcement of leap seconds:
   *     as of 2012-10-12. Please update when necessary.
   *     here, beware when using this for really old data
   *     should be taken care of before the next rollover...
   *   * assumes we're between the 1st and 2nd week rollover
   *   * leap seconds of 1999 JAN  1 and before are not reflected
   *   * list of leap seconds taken from
   *   * overflow of sec into next week is allowed
   * baudrate/10 bytes per second (8 data bits, start and stop bit)
   * it seems to write to flash too. The Windows software sends 0x02 so we do here too.
   * read tracks and POI from miniHomer
   * TODO: use dlbaud if selected.
   */
   */
   */
   */
  "Home", "Car", "Boat", "Heart", "Bar"
  */
  *alt = AP/cos(*lat) - CA/sqrt(1 - CE2 * pow(sin(*lat), 2));
  *ecef_x = (double)((n+lalt) * cos(llat) * cos(llng));
  *ecef_y = (double)((n+lalt) * cos(llat) * sin(llng));
  *ecef_z = (double)((n*(1-esqr) + lalt)* sin(llat));
  *lat = *lat /M_PI*180;
  *lat = atan2(z + CE_2 * CB * pow(sin(ATHETA), 3), AP - CE2 * CA * pow(cos(ATHETA), 3));
  *log_wr_ptr = le_readu32(&MSG_LOG_STATUS_OUTPUT.log_wr_ptr);
  *lon = *lon /M_PI*180;
  *lon = atan2(y, x);
  *sectors_free = le_readu16(&MSG_LOG_STATUS_OUTPUT.sectors_free);
  *sectors_total = le_readu16(&MSG_LOG_STATUS_OUTPUT.sectors_total);
  , NULL_POS_OPS,
  , NULL_POS_OPS,
  /*
  /*  if (rd_buf(buffer, 2) != res_OK) {
  /* Allow TIMEOUT plus the time needed to actually receive the data bytes:
  /* auxiliary values: */
  /* constants: */
  /* height above ellipsoid (in meters): */
  /* latitude (in radians): */
  /* leap second compensation: */
  /* longitude (in radians): */
  /* Note: according to AN0003_v3.pdf, attrib == 0x00 means write to SRAM only, however
  /* Notes:
  /* Workaround: sectors_free is sometimes reported wrong. Tried to use log_wr_ptr as an
  /*TODO: timeout gets <0 e.g. when len~=250000 --> 32bit signed int is too small.
  // (dt > tmin & dd >= dmin & v >= vmin) | dt > tmax | dd > dmax | v > vmax
  // (only sensible if init to 4800 can be disabled...)
  // an0008-1.4.14: logs if
  // fifo_mode = *(MSG_LOG_STATUS_OUTPUT.log_fifo_mode);
  // Future: Consult http://maia.usno.navy.mil/ser7/tai-utc.dat
  // log_bool = *(MSG_LOG_STATUS_OUTPUT.datalog_enable);
  // m.ad/090930: removed code that tried reducing read_at_once if necessary since doesn't work with xmalloc
  // print logging parameters -- useful, but does this belong here?
  // read device unless no-output=1 and dump-file=0 (i.e. no data needed at all)
  // TODO: get current serial port baud rate and try that first
  // unsigned char log_bool, fifo_mode;
  // use http://www.stevegs.com/utils/jd_calc/ for Julian to UNIX sec
  ARG_TERMINATOR
  ARG_TERMINATOR
  ARG_TERMINATOR
  be_write16(&MSG_LOG_READ_MULTI_SECTORS[1], first_sector);
  be_write16(&MSG_LOG_READ_MULTI_SECTORS[3], sector_count);
  buf[i] = c;
  buf_end_tag = buf + SECTOR_SIZE*sector_count;
  buffer = (uint8_t*) xmalloc(SECTOR_SIZE);
  buffer = (uint8_t*) xmalloc(SECTOR_SIZE*read_at_once+sizeof(SECTOR_READ_END)+6);
  buffer[0] = rd_char(&errors);
  buffer[1] = rd_char(&errors);
  c = rd_char(&errors); /* read checksum byte */
  calc_cs = skytraq_calc_checksum((const unsigned char*) payload, MIN(rcv_len, len));
  case 0x2: /* Multi HZ item */
  case 0x4: /* full item */
  case 0x6: /* POI item (same structure as full) */
  case 0x8: /* compact item */
  case 0xc: /* POI item (same structure as full) */
  case 115200:
  case 19200:
  case 230400:
  case 38400:
  case 4800:
  case 57600:
  case 9600:
  CET_CHARSET_UTF8, 1         /* master process: don't convert anything */
  CET_CHARSET_UTF8, 1         /* master process: don't convert anything */
  CET_CHARSET_UTF8, 1         /* master process: don't convert anything */
  char dump[16*3+16+2];
  char* mystatus;
  compact_item c;
  const double CA   = 6378137.0;
  const double CB   = 6356752.31424518;
  const double CE2  = (CA*CA - CB*CB) / (CA*CA);    /* =e^2 */
  const double CE_2 = (CA*CA - CB*CB) / (CB*CB);    /* =e'^2 */
  cs = skytraq_calc_checksum(buf, i);
  cs = skytraq_calc_checksum(buf, SECTOR_SIZE*sector_count);
  cs = skytraq_calc_checksum(payload, len);
  db(1, "aborting (msg id was 0x%02x).\n", payload[0]);
  db(1, "Closing file...\n");
  db(1, "Opening file...\n");
  db(1, MYNAME ": Device status: free sectors: %i / total sectors: %i / %i%% used / write ptr: %i\n",
  db(1, MYNAME ": Erasing logger memory...\n");
  db(1, MYNAME ": Got %i trackpoints from %i sectors.\n", st.tpn, sectors_read);
  db(1, MYNAME ": Got %i trackpoints from %i sectors.\n", st.tpn, total_sectors_read);
  db(1, MYNAME ": opt_last_sector_val=%d\n", opt_last_sector_val);
  db(1, MYNAME ": Reading log data from device...\n");
  db(1, MYNAME ": Reading log data from file...\n");
  db(1, MYNAME ": start=%d used=%d\n", opt_first_sector_val, sectors_used);
  db(1, mystatus);
  db(2, "Reading %i sectors beginning from #%i...\n", sector_count, first_sector);
  db(2, "Reading sector #%i...\n", sector);
  db(2, "Receiving message with %i bytes of payload (expected >=%i)\n", rcv_len, len);
  db(2, "restart system\n");
  db(2, "Setting baud rate to %i\n", baud);
  db(3, "\n");
  db(3, "Now setting UART baud rate to %i\n", baud);
  db(3, "Received %i bytes of log data\n", i);
  db(3, MYNAME ": set_location='%s'\n", opt_set_location);
  db(4, "Sending: %02x '%c'\n", (unsigned)c, isprint(c) ? c : '.');
  default:
  default:
  dlbaud = atoi(opt_dlbaud);
  dmax = le_readu32(&MSG_LOG_STATUS_OUTPUT.max_dist);
  dmin = le_readu32(&MSG_LOG_STATUS_OUTPUT.min_dist);
  double AP = sqrt(x*x + y*y);
  double ATHETA = atan2(z*CA, AP*CB);
  double ecef_x, ecef_y, ecef_z;
  double ecef_x, ecef_y, ecef_z;
  double lat, lng, alt;
  double lat, lng, alt;
  double lat, lng;
  double lat, lon, alt, spe;
  fatal(MYNAME ": Too many read errors on serial port\n");
  ff_type_file,
  ff_type_serial,
  ff_type_serial,
  file_deinit,
  file_handle = NULL;
  file_init,
  file_read,
  for (; i < SECTOR_SIZE; i++) {
  for (i = 0, j = 0; i-j < SECTOR_SIZE && j < sizeof(SECTOR_READ_END); i++) {
  for (i = 0, j = 0; i-j < SECTOR_SIZE && j < sizeof(SECTOR_READ_END); i+=c) {
  for (i = 0, state = 0; i < RETRIES && state < sizeof(MSG_START); i++) {
  for (i = 0; i < baud_rates_count; i++) {
  for (i = 0; i < len; i++) {
  for (i = 0; i < len; i++) {
  for (i = 0; i < MSG_RETRIES; i++) {
  for (i = 0; i < MSG_RETRIES; i++) {
  for (i = 0; i < MSG_RETRIES; i++) {
  for (i = 0; i < rcv_len-len; i++) {
  for (i = 0; i < sector_count; i++) {
  for (i = 0; i < sizeof(SECTOR_READ_END); i++) {
  for (i = opt_first_sector_val; i < sectors_used; i += got_sectors) {
  for (i=0; i<NUMPOI; i++) {
  for (i=0; i<sizeof MSG_SET_LOCATION; i++) {
  for (plen = 0; plen < len  &&  buf[plen] != 0xFF; plen += ilen) {
  for (poi=0; poi<NUMPOI; poi++) {
  free(buffer);
  full_item f;
  gb_sleep(50);   /* allow UART to settle. */
  gbfclose(file_handle);
  gbfile* dumpfile = NULL;
  gbser_deinit(serial_handle);
  gbser_set_speed(serial_handle, skytraq_baud);
  gps_timet += (week+1024)*7*SECONDS_PER_DAY + sec;
  gps_timet -= 13;  /* diff GPS-UTC=13s (valid from Jan 01 1999 on) */
  i = i-j;
  if ((file_handle = gbfopen(fname, "rb", MYNAME)) == NULL) {
  if ((rc = skytraq_wr_msg_verify(&MSG_LOG_STATUS_CONTROL, 1)) != res_OK) { /* get memory status */
  if ((rcv_len = rd_word()) < len) {
  if ((serial_handle = gbser_init(qPrintable(fname))) == NULL) {
  if ((skytraq_baud = skytraq_probe()) <= 0) {
  if (*opt_erase == '1') {
  if (*opt_no_output == '0'  ||  opt_dump_file != NULL) {
  if (c < 16) {
  if (cs != buf[i+sizeof(SECTOR_READ_END)]) {
  if (cs != buf_end_tag[sizeof(SECTOR_READ_END)]) {
  if (dlbaud != 0  &&  dlbaud != skytraq_baud) {
  if (dlbaud != 0  &&  dlbaud != skytraq_baud) {
  if (dumpfile) {
  if (first_sector < 0  ||  first_sector > 0xFFFF) {
  if (gbser_flush(serial_handle)) {
  if (gbser_set_speed(serial_handle, baud) != gbser_OK) {
  if (global_opts.debug_level >= 4) {
  if (global_opts.debug_level >= l) {
  if (gps_timet >= 1136073600) {    /* Jan 01 2006 0:00 UTC */
  if (gps_timet >= 1230768000) {    /* Jan 01 2009 0:00 UTC */
  if (gps_timet >= 1341100800) {    /* Jul 01 2012 0:00 UTC */
  if (gps_timet >= 1435708800) {    /* Jul 01 2015 0:00 UTC */
  if (gps_timet >= 1483228800) {    /* Jan 01 2017 0:00 UTC */
  if (initbaud > 0) {
  if (j < sizeof(SECTOR_READ_END)) {
  if (j < sizeof(SECTOR_READ_END)) {
  if (miniHomer_set_poi(0, opt_set_poi_home) > 0) {
  if (miniHomer_set_poi(1, opt_set_poi_car) > 0) {
  if (miniHomer_set_poi(2, opt_set_poi_boat) > 0) {
  if (miniHomer_set_poi(3, opt_set_poi_heart) > 0) {
  if (miniHomer_set_poi(4, opt_set_poi_bar) > 0) {
  if (npoi == 0) {        // do not read if POIs are set (consider set & read distinct operations)
  if (opt_configure_logging) {
  if (opt_configure_logging) {
  if (opt_dump_file) {
  if (opt_first_sector_val > 0) {
  if (opt_first_sector_val >= sectors_total) {
  if (opt_last_sector_val < 0) {
  if (opt_poi) {  // first check opt_poi
  if (opt_set_location) {
  if (override) {
  if (rc != res_OK) {
  if (rc < 0) {
  if (rc < sizeof(MSG_LOG_STATUS_OUTPUT)) {
  if (rc = gbser_writec(serial_handle, c), gbser_OK != rc) {
  if (rcv_cs != calc_cs) {
  if (rd_word() != 0x0D0A) {
  if (read_result != res_OK) {
  if (res == COMPACT_ITEM_LEN  ||  res == FULL_ITEM_LEN) {
  if (sector > 0xFF) {
  if (sector_count > 0xFFFF) {
  if (skytraq_get_log_buffer_status(&log_wr_ptr, &sectors_free, &sectors_total) != res_OK) {
  if (skytraq_wr_msg_verify(&MSG_LOG_ERASE, sizeof(MSG_LOG_ERASE)) != res_OK) {
  if (skytraq_wr_msg_verify((uint8_t*)&MSG_LOG_SECTOR_READ_CONTROL, sizeof(MSG_LOG_SECTOR_READ_CONTROL)) != res_OK) {
  if (skytraq_wr_msg_verify((uint8_t*)&MSG_SET_LOCATION, sizeof(MSG_SET_LOCATION)) != res_OK) {
  if (state < sizeof(MSG_START)) {
  int baud_rates[] = { 9600, 230400, 115200, 57600, 4800, 19200, 38400 };
  int baud_rates_count = sizeof(baud_rates)/sizeof(baud_rates[0]);
  int c;
  int cs;
  int dlbaud;
  int errors = 5;   /* allow this many errors */
  int errors = 5;   /* allow this many errors */
  int errors = 5;   /* allow this many errors */
  int i, cs = 0;
  int i, rc;
  int i, rc;
  int i, rc;
  int i, t, s, rc, got_sectors, total_sectors_read = 0;
  int i/*, rcv_len*/;
  int i;
  int initbaud = atoi(opt_initbaud);
  int multi_read_supported = 1;
  int n, result;
  int npoi=0;
  int opt_first_sector_val = atoi(opt_first_sector);
  int opt_first_sector_val = atoi(opt_first_sector);
  int opt_last_sector_val = atoi(opt_last_sector);
  int opt_last_sector_val = atoi(opt_last_sector);
  int override = atoi(opt_gps_utc_offset);
  int plen, ilen;
  int poi = 0;
  int rc, got_bytes;
  int rc, timeout, i;
  int rc;
  int rc;
  int read_at_once = MAX(atoi(opt_read_at_once), 1);
  int res = 0;
  int sectors_read;
  int16_t dx;
  int16_t dy;
  int16_t dz;
  int32_t  alt;
  int32_t  lat;
  int32_t  lon;
  int32_t  x;
  int32_t  y;
  int32_t  z;
  lalt=alt;
  le_write_double(&MSG_SET_LOCATION[1], lat);
  le_write_double(&MSG_SET_LOCATION[9], lng);
  llat=lat*M_PI/180;
  llng=lng*M_PI/180;
  long double a = 6378137.0;
  long double esqr = 6.69437999014e-3;
  long double llat, llng, lalt;
  long double n;
  long double s;
  long x, y, z;
  mhport.clear();
  mhport=fname;
  miniHomer_args,
  miniHomer_rd_deinit,
  miniHomer_rd_init,
  miniHomer_read,
  multi_hz_item m;
  n = a / sqrt(1 - esqr * s*s);
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  nullptr
  nullptr
  opt_set_location=NULL;  // otherwise it will lead to bus error
  printf("len=%i  skytraq_baud=%i  timeout=%i\n", len, skytraq_baud, timeout);*/
  pst->gps_sec    = 0;
  pst->gps_week   = 0;
  pst->route_head_ = track;
  pst->tpn        = 0;
  pst->wpn        = 0;
  pst->x          = 0;
  pst->y          = 0;
  pst->z          = 0;
  rc = gbser_read_wait(serial_handle, (void*)buf, len, timeout);
  rc = skytraq_expect_msg(0x94, (uint8_t*)&MSG_LOG_STATUS_OUTPUT, sizeof(MSG_LOG_STATUS_OUTPUT));
  rc = skytraq_wr_msg_verify(MSG_CONFIGURE_SERIAL_PORT, sizeof(MSG_CONFIGURE_SERIAL_PORT));
  rcv_cs = rd_char(&errors);
  rd_buf((const unsigned char*) payload, MIN(rcv_len, len));
  rd_buf(buf+SECTOR_SIZE*sector_count, sizeof(SECTOR_READ_END)+6);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_char(&errors);
  rd_drain();
  rd_drain();
  read_result = skytraq_wr_msg_verify((uint8_t*)&MSG_LOG_READ_MULTI_SECTORS, sizeof(MSG_LOG_READ_MULTI_SECTORS));
  result=0;   // result will be 0 if opt_poi isn't set
  return ((unsigned)be_read16(p+2) << 16) | ((unsigned)be_read16(p));
  return (buffer[0] << 8) | buffer[1];
  return -1;
  return -1;
  return cs;
  return gps_timet;     /* returns UTC time */
  return plen;
  return res;
  return res_ERROR;
  return res_NOTFOUND;
  return res_OK;
  return res_OK;
  return res_OK;
  return res_OK;
  return res_OK;
  return res_OK;
  return res_OK;
  return res_PROTOCOL_ERR;
  return res_PROTOCOL_ERR;
  return result;
  return skytraq_wr_msg_verify(MSG_LOG_CONFIGURE_CONTROL, sizeof(MSG_LOG_CONFIGURE_CONTROL));
  return skytraq_wr_msg_verify(MSG_SYSTEM_RESTART, sizeof(MSG_SYSTEM_RESTART));
  return wpt;
  route_head*          route_head_;
  route_head* track;
  s=sin(llat);
  sectors_read = 0;
  serial_handle = NULL;
  signed int rcv_len;
  skytraq_args,
  skytraq_fargs,
  skytraq_rd_deinit();
  skytraq_rd_deinit,
  skytraq_rd_init(fname); // sets global var serial_handle
  skytraq_rd_init,
  skytraq_read,
  skytraq_set_baud(atoi(opt_dlbaud));
  skytraq_system_restart();
  sscanf(opt_set_location, "%lf:%lf", &lat, &lng);
  state_init(&st);
  state_init(&st);
  struct read_state st;
  struct read_state st;
  struct {
  struct {
  switch (baud) {
  switch (ITEM_TYPE(pitem)) {
  time_t gps_timet = 315964800;     /* Jan 06 1980 0:00 UTC */
  timeout = TIMEOUT + len;//*1000/(skytraq_baud/10);
  tmax = le_readu32(&MSG_LOG_STATUS_OUTPUT.max_time);
  tmin = le_readu32(&MSG_LOG_STATUS_OUTPUT.min_time);
  track = route_head_alloc();
  track->rte_desc = "SkyTraq GPS tracklog data";
  track->rte_name = "SkyTraq tracklog";
  track_add_head(track);
  uint16_t dt;
  uint16_t sectors_free, sectors_total, /*sectors_used_a, sectors_used_b,*/ sectors_used;
  uint32_t gps_sec;
  uint32_t gps_sec;
  uint32_t gps_week;
  uint32_t gps_week;
  uint32_t log_wr_ptr;
  uint8_t ack_msg[2];
  uint8_t buf[32];
  uint8_t buffer[16];
  uint8_t buffer[2];
  uint8_t MSG_CONFIGURE_SERIAL_PORT[4]
  uint8_t MSG_GET_LOCATION = 0x35;
  uint8_t MSG_GET_POI[3] = { 0x4D, 0, 0};
  uint8_t MSG_LOG_CONFIGURE_CONTROL[] = {
  uint8_t MSG_LOG_ERASE = 0x19;
  uint8_t MSG_LOG_READ_MULTI_SECTORS[5] = { 0x1D };
  uint8_t MSG_LOG_SECTOR_READ_CONTROL[2] = { 0x1B, (uint8_t)(sector) };
  uint8_t MSG_LOG_STATUS_CONTROL = 0x17;
  uint8_t MSG_QUERY_SOFTWARE_VERSION[2] = { 0x02, 0x01 };
  uint8_t MSG_SET_LOCATION[17] = { 0x36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  uint8_t MSG_SET_POI[MSG_SET_POI_SIZE] = {
  uint8_t MSG_SYSTEM_RESTART[15] =
  uint8_t* buf_end_tag;
  uint8_t* buffer = NULL;
  uint8_t* buffer;
  union {
  unsigned            wpn, tpn;
  unsigned char alt[4];
  unsigned char dpos[4];
  unsigned char dt[2]; /* big endian unsigned short */
  unsigned char lat[4];
  unsigned char lon[4];
  unsigned char ts[4];
  unsigned char ts[4];
  unsigned char type_and_speed[2];
  unsigned char v_kmh[2];
  unsigned char x[4];
  unsigned char y[4];
  unsigned char z[4];
  unsigned gps_sec;
  unsigned gps_week;
  unsigned int c, i, j, cs;
  unsigned int c, i, state;
  unsigned int calc_cs, rcv_cs;
  unsigned int cs, i, read_result;
  unsigned int i;
  unsigned int i;
  unsigned int poi;
  unsigned int rc;
  unsigned int tmax, tmin, dmax, dmin, vmax, vmin;
  unsigned int tmin=6, tmax=3600, dmin=0, dmax=10000, nn=0;
  unsigned int ts;
  va_end(ap);
  va_list ap;
  va_start(ap, msg);
  vmax = le_readu32(&MSG_LOG_STATUS_OUTPUT.max_speed);
  vmin = le_readu32(&MSG_LOG_STATUS_OUTPUT.min_speed);
  Waypoint* tpt = NULL;
  Waypoint* wpt = new Waypoint;
  Waypoint* wpt;
  while ((got_bytes = gbfread(buffer, 1, SECTOR_SIZE, file_handle)) > 0) {
  while (*errors > 0) {
  wpt->altitude       = alt;
  wpt->latitude       = lat;
  wpt->longitude      = lon;
  wpt->SetCreationTime(gpstime_to_timet(st->gps_week, st->gps_sec));
  wpt->shortname = QString().sprintf("TP%04d", ++st->tpn);
  wr_buf(MSG_START, sizeof(MSG_START));
  wr_buf(NL, sizeof(NL));
  wr_buf(payload, len);
  wr_char((len>>8) & 0x0FF);
  wr_char(cs);
  wr_char(len & 0x0FF);
  xasprintf(&mystatus, "#logging: tmin=%u, tmax=%u, dmin=%u, dmax=%u, vmin=%u, vmax=%u\n", tmin, tmax, dmin, dmax, vmin, vmax);
  xfree(buffer);
  xfree(mystatus);
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  {
  { "Bar",          &opt_set_poi_bar,   "POI for Bar Symbol as lat:lng[:alt]", NULL, ARGTYPE_STRING, "", "", nullptr },
  { "baud",         &opt_dlbaud,        "Baud rate used for download", "115200", ARGTYPE_INT, "0", "115200", nullptr },
  { "Boat",         &opt_set_poi_boat,  "POI for Boat Symbol as lat:lng[:alt]", NULL, ARGTYPE_STRING, "", "", nullptr },
  { "Car",          &opt_set_poi_car,   "POI for Car Symbol as lat:lng[:alt]", NULL, ARGTYPE_STRING, "", "", nullptr },
  { "dump-file",    &opt_dump_file,     "Dump raw data to this file", NULL, ARGTYPE_OUTFILE, ARG_NOMINMAX , nullptr},
  { "erase",        &opt_erase,         "Erase device data after download", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
  { "first-sector", &opt_first_sector,  "First sector to be read from the device", "0", ARGTYPE_INT, "0", "65535", nullptr },
  { "Heart",        &opt_set_poi_heart, "POI for Heart Symbol as lat:lng[:alt]", NULL, ARGTYPE_STRING, "", "" , nullptr},
  { "Home",         &opt_set_poi_home,  "POI for Home Symbol as lat:lng[:alt]", NULL, ARGTYPE_STRING, "", "", nullptr },
  { "initbaud",     &opt_initbaud,      "Baud rate used to init device (0=autodetect)", "38400", ARGTYPE_INT, "38400", "38400", nullptr },
  { "last-sector",  &opt_last_sector,   "Last sector to be read from the device (-1: smart read everything)", "-1", ARGTYPE_INT, "-1", "65535", nullptr },
  { "no-output",    &opt_no_output,     "Disable output (useful with erase)", "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
  { "read-at-once", &opt_read_at_once,  "Number of sectors to read at once (0=use single sector mode)", "255", ARGTYPE_INT, "0", "255", nullptr },
  { 0x01, 0x01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  } else if (rc < len) {
  } else {
  } else {
  } MSG_LOG_STATUS_OUTPUT;
  } MSG_SOFTWARE_VERSION;
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  },
  };
  };
  };
 *  the number of the POI will not be checked - if it is not correct, miniHome will send NACK
 * -1 in case of errors
 * 0  if opt_poi was not set
 * 1  if poi was set
 * 2010-10-23 Josef Reisinger
 * http://navin.com.tw/miniHomer.htm
 * Names of the POIs on miniHomer
 * returns
 * set lla (lat/lng/alt) specified as <lat>:<lng>[:<alt] for a given poi [0..4] in miniHomer
 * support POI of skytraq based miniHomer device
 */
 */
 */
 */
#define COMPACT_ITEM_LEN  8
#define FULL_ITEM_LEN   18
#define ITEM_SPEED(item) (item->type_and_speed[1] | ((item->type_and_speed[0] & 0x0F) << 8))
#define ITEM_TYPE(item) (item->type_and_speed[0] >> 4)
#define ITEM_WEEK_NUMBER(item) (item->type_and_speed[1] | ((item->type_and_speed[0] & 0x03) << 8))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MSG_RETRIES   3
#define MSG_SET_POI_SIZE (sizeof(uint8_t)+sizeof(uint16_t)+3*sizeof(double)+sizeof(uint8_t))
#define MULTI_HZ_ITEM_LEN   20
#define MYNAME "miniHomer"
#define MYNAME "skytraq"
#define NUMPOI (sizeof poinames/sizeof poinames[0])
#define POW_2_M20 0.000000953674316
#define POW_2_M7 0.0078125
#define res_ERROR   -1
#define res_NACK    -2
#define res_NOTFOUND    -4
#define res_OK      0
#define res_PROTOCOL_ERR  -3
#define RETRIES     250
#define SECTOR_RETRIES    3
#define SECTOR_SIZE   4096
#define SETPOI(poinum, poiname) if (opt_set_poi_##poiname )  {miniHomer_set_poi(poinum, opt_set_poi_##poiname);}
#define TIMEOUT     5000
#else
#endif
#endif
#endif
#ifdef MYNAME
#ifdef READ_SINGLE_CHARS
#ifdef SINGLE_READ_WORKAROUND
#include "defs.h"
#include "gbser.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#undef MYNAME
* %%%        global callbacks called by gpsbabel main process              %%% *
* %%%        SkyTraq protocol implementation                               %%% *
*******************************************************************************/
*******************************************************************************/
/*
/*
/*
/*
/* Abort when reading a specific sector fails this many times: */
/* Maximum number of chars to skip while waiting for a reply: */
/* Maximum number of messages to read while expecting a specific message or ACK/NACK: */
/* Note: the buffer is being padded with 0xFFs if necessary so there are always SECTOR_SIZE valid bytes */
/* reads 32-bit "middle-endian" fields */
/*******************************************************************************
/*******************************************************************************
/**************************************************************************/
/**************************************************************************/
//    GPS_Math_XYZ_To_WGS84LatLonH(&lat, &lon, &alt, pst->x, pst->y, pst->z);
//    if (rcv_len == sizeof(ack_msg)) {
//    rcv_len = skytraq_rd_msg(ack_msg, sizeof(ack_msg));
//  return MIN(rcv_len, len);
// Algorith taken from these sources:
// capabilities below means: we can only read tracks
// Convert lla (lat, lng, alt) to ECEF
// http://earth-info.nga.mil/GandG/publications/tr8350.2/wgs84fin.pdf
// http://en.wikipedia.org/wiki/Geodetic_system#From_ECEF_to_geodetic
// http://www.mathworks.com/matlabcentral/fileexchange/7942-covert-lat-lon-alt-to-ecef-cartesian
//#define SINGLE_READ_WORKAROUND
arglist_t miniHomer_args[] = {
arglist_t skytraq_args[] = {
arglist_t skytraq_fargs[] = {
db(int l, const char* msg, ...)
ECEF_to_LLA(double x, double y, long z, double* lat, double* lon, double* alt)
ff_vecs_t miniHomer_vecs = {
ff_vecs_t skytraq_fvecs = {
ff_vecs_t skytraq_vecs = {
file_deinit(void)
file_init(const QString& fname)
file_read(void)
gpstime_to_timet(int week, int sec)
int getPoiByName(char* name)
make_trackpoint(struct read_state* st, double lat, double lon, double alt)
miniHomer_rd_deinit(void)
miniHomer_rd_init(const QString& fname)
miniHomer_read(void)
process_data_item(struct read_state* pst, const item_frame* pitem, int len)
process_data_sector(struct read_state* pst, const uint8_t* buf, int len)
rd_buf(const uint8_t* buf, int len)
rd_char(int* errors)
rd_drain(void)
rd_word(void)
skytraq_calc_checksum(const unsigned char* buf, int len)
skytraq_configure_logging(void)
skytraq_erase()
skytraq_expect_ack(uint8_t id)
skytraq_expect_msg(uint8_t id, const uint8_t* payload, int len)
skytraq_get_log_buffer_status(uint32_t* log_wr_ptr, uint16_t* sectors_free, uint16_t* sectors_total)
skytraq_probe(void)
skytraq_rd_deinit(void)
skytraq_rd_init(const QString& fname)
skytraq_rd_msg(const void* payload, unsigned int len)
skytraq_read(void)
skytraq_read_multiple_sectors(int first_sector, unsigned int sector_count, uint8_t* buf)
skytraq_read_single_sector(unsigned int sector, uint8_t* buf)
skytraq_read_tracks(void)
skytraq_set_baud(int baud)
skytraq_set_location(void)
skytraq_system_restart(void)
skytraq_wr_msg(const uint8_t* payload, int len)
skytraq_wr_msg_verify(const uint8_t* payload, int len)
state_init(struct read_state* pst)
static
static
static char* opt_configure_logging = 0;
static char* opt_dlbaud = 0;    /* baud rate used for downloading tracks */
static char* opt_dump_file = 0;   /* dump raw data to this file (optional) */
static char* opt_erase = 0;   /* erase after read? (0/1) */
static char* opt_first_sector = 0;  /* first sector to be read from the device (default: 0) */
static char* opt_gps_utc_offset = 0;
static char* opt_initbaud = 0;    /* baud rate used to init device */
static char* opt_last_sector = 0; /* last sector to be read from the device (default: smart read everything) */
static char* opt_no_output = 0;   /* disable output? (0/1) */
static char* opt_read_at_once = 0;  /* number of sectors to read at once (Venus6 only) */
static char* opt_set_location = 0;  /* set if the "targetlocation" options was used */
static char* opt_set_poi_bar = NULL;  /* set if a "poi" option was used */
static char* opt_set_poi_boat = NULL; /* set if a "poi" option was used */
static char* opt_set_poi_car = NULL;  /* set if a "poi" option was used */
static char* opt_set_poi_heart = NULL;  /* set if a "poi" option was used */
static char* opt_set_poi_home = NULL; /* set if a "poi" option was used */
static const char* poinames[] = {
static gbfile* file_handle = 0;   /* file descriptor (used by skytraq-bin format) */
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int
static int  /* returns number of bytes processed (terminates on 0xFF i.e. empty or padding bytes) */
static int miniHomer_set_poi(uint16_t poinum, const char* opt_poi)
static int skytraq_baud = 0;    /* detected baud rate */
static QString mhport;
static time_t
static unsigned int
static unsigned int me_read32(const unsigned char* p)
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void
static void miniHomer_get_poi()
static void* serial_handle = 0;   /* IO file descriptor */
static Waypoint*
struct compact_item_frame {
struct full_item_frame {
struct multi_hz_item_frame {
struct read_state {
typedef struct {
typedef struct {
typedef struct {
typedef struct {
uint8_t MSG_START[2] = { 0xA0, 0xA1 };
uint8_t NL[2] = { 0x0D, 0x0A };
uint8_t SECTOR_READ_END[13] = { 'E','N','D', 0, 'C','H','E','C','K','S','U','M','=' };
void lla2ecef(double lat, double lng, double alt, double* ecef_x, double* ecef_y, double* ecef_z)
wr_buf(const unsigned char* str, int len)
wr_char(int c)
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
{
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
} compact_item;
} full_item;
} item_frame;
} multi_hz_item;
};
};
};
};
};
};
};
};
};
};
};