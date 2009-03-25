#define delbin_vid 0x1163
#define delbin_pid 0x2020


#define OUT_REQUEST_VERSION 0xa001
#define OUT_NAV_MESSAGE 0xa10
#define OUT_NAV_ACK_MESSAGE 0xaa00
#define IN_NAV_ACK_MESSAGE 0xaa00
#define IN_GET_NUM_WAYPTS_MESSAGE 0xb010
#define OUT_GET_NUM_WAYPTS_MESSAGE 0xb010
#define IN_GET_WAYPOINTS_MESSAGE 0xb012
#define OUT_GET_WAYPOINTS_MESSAGE 0xb013
#define GET_WAYPOINT_MESSAGE 0xb014
#define IN_REQUEST_DEVICE_CAPABILITIES_MESSAGE 0xb001
#define OUT_DEVICE_CAPABILITIES_MESSAGE 0xb001

int delbin_pkt_size(int message_id);

class delbin_packet {
  gbuint16 message_id_;
  unsigned char *payload_;
  int serialized_size_;
 public:
    delbin_packet(int type, void *payload) {
      message_id_ = type;
      payload_ = (unsigned char *) payload;
    }
    delbin_packet(int type) {
      message_id_ = type;
    }

    int serialized_size(void) { return serialized_size_; }

    signed short get_cs(unsigned char *buf, short len) {
      signed short i, cs, tdat;
      cs = 0;
      for (i = 0; i < len; i += 2) {
        if ((len -i) > 1) {
          tdat = *(unsigned char *)(buf + i + 1);
          tdat = tdat << 8;
          tdat |= *(unsigned char *)(buf + i);
          cs += tdat;
        } else {
          tdat = *(unsigned char *)(buf + i);
          cs += tdat;
        }
      }
      cs = (signed short) (0x10000 - (long) cs);
      return (cs);
    }

    void * serialize(void) {
      short psz = delbin_pkt_size(message_id_);; // payload_size
      int sz = psz + 12;
      unsigned char *pbuf = (unsigned char *) xmalloc(sz);
      serialized_size_ = sz;
      gbuint16 *hdr = (gbuint16*) pbuf;
      hdr[0] = 0xfedb;
      hdr[1] = message_id_;
      hdr[2] = psz;
      hdr[3] = get_cs(pbuf, 6);
      memcpy(pbuf + 8, payload_, psz);
      gbuint16 *ftr = (gbuint16*) (pbuf + psz + 8);
      ftr[0] = get_cs(payload_, psz);  // checksum
      ftr[1] = 0xbcad;
      return hdr;
    }
};

class radians {
 private:
  gbint32 radians_;

 public:
  double toDegrees() {
    return (radians_ / 100000000) * 180.0/ M_PI;
  }
  void fromDegrees(double degrees) {
    radians_ = degrees * M_PI / 180.0 * 100000000;
  }
};

class elevation {
 private:
  float elevation_;

 public: 
  void set(double meters) {
     elevation_ = meters == unknown_alt ? -2000000.0 : meters;
   }
  double get() {
    return elevation_ == -2000000 ? unknown_alt : elevation_;
  }
};

class ymdhms {
 private: 
  char year_;
  char month_;
  char day_;
  char hour_;
  char minute_;
  char second_;

 public:
  void set(time_t t) {
    struct tm *tm = gmtime(&t);
    year_ = tm->tm_year;
    month_ = tm->tm_mon;
    day_ = tm->tm_mday;
    hour_ = tm->tm_hour;
    minute_ = tm->tm_min;
    second_ = tm->tm_sec;
  }
  time_t get() {
    struct tm tm;
    memset(&tm, 0, sizeof(tm));
    tm.tm_year = year_;
    tm.tm_mon = month_;
    tm.tm_mday = day_;
    tm.tm_hour = hour_;
    tm.tm_min = minute_;
    tm.tm_sec = second_;
    return mkgmtime(&tm);
  }
};

// 0xa001
typedef struct delbin_version_response {
  gbuint32 firmware_version;
  char company_name[32];
  char product_name[32];
  char firmware_name[32];
  char firmware_longname[48];
  char serial_number[16];
  char extra[16];
} delbin_version_response;

// 0xb012
typedef struct delbin_request_waypoint {
  gbuint8 type;
  gbuint8 reserved;
  radians lat;
  radians lon;
  gbint32 search_region;
  // shortstring waypoint name
} delbin_request_waypoint;

// 0xb013
typedef struct delbin_waypoint_message {
    gbuint32 wpt_total;
    gbuint32 wpt_this_index;
  ymdhms timestamp;
//    gbuint8 year;
//    gbuint8 month;
//    gbuint8 day;
//    gbuint8 hour;
//    gbuint8 minute;
//    gbuint8 second;
    radians lat;
    radians lon;
    elevation alt;
    gbuint8 color;
    gbuint8 symbol;
    // shortstring waypoint name
    // longstring waypoint note
} delbin_waypoint_message;

// 0xb001
typedef struct delbin_device_capabilities {
  gbuint32 max_waypoints;
  gbuint16 max_tracks;
  gbuint32 max_trackpoints;
  gbuint16 max_route;
  gbuint32 max_routepoints;
  gbuint32 max_shapepoints;
  gbuint16 max_maps;
  gbuint16 min_map_version;
  gbuint32 total_file_memory;
  gbuint32 available_internal_memory;
  gbuint32 total_external_memory;
  gbuint32 available_external_memory;;
} delbin_device_capabilities;


const struct delbin_icon {
  int symnum;
  char *symbol;
} delbin_icons[] = {
  { 0x00, "All" },
  { 0x01, "Red Map Pin" },
  { 0x02, "Dark Red Map Pin" },
  { 0x03, "Yellow Map Pin" },
  { 0x04, "Dark Yellow Map Pin" },
  { 0x05, "Green Map Pin" },
  { 0x06, "Dark Green Map Pin" },
  { 0x07, "Turquoise Map Pin" },
  { 0x08, "Dark Turquoise Map Pin" },
  { 0x09, "Blue Map Pin" },
  { 0x0A, "Dark Blue Map Pin" },
  { 0x0B, "Gray Map Pin" },
  { 0x0C, "Dark Gray Map Pin" },
  { 0x0D, "Red Flag" },
  { 0x0E, "Dark Red Flag" },
  { 0x0F, "Yellow Flag" },
  { 0x10, "Dark Yellow Flag" },
  { 0x11, "Green Flag" },
  { 0x12, "Dark Green Flag" },
  { 0x13, "Turquoise Flag" },
  { 0x14, "Dark Turquoise Flag" },
  { 0x15, "Blue Flag" },
  { 0x16, "Dark Blue Flag" },
  { 0x17, "Gray Flag" },
  { 0x18, "Dark Gray Flag" },
  { 0x19, "Red Dot" },
  { 0x1A, "Dark Red Dot" },
  { 0x1B, "Yellow Dot" },
  { 0x1C, "Dark Yellow Dot" },
  { 0x1D, "Green Dot" },
  { 0x1E, "Dark Green Dot" },
  { 0x1F, "Turquoise Dot" },
  { 0x20, "Dark Turquoise Dot" },
  { 0x21, "Blue Dot" },
  { 0x22, "Dark Blue Dot" },
  { 0x23, "Gray Dot" },
  { 0x24, "Dark Gray Dot" },
  { 0x25, "Small Red Dot" },
  { 0x26, "Small Dark Red Dot" },
  { 0x27, "Small Yellow Dot" },
  { 0x28, "Small Dark Yellow Dot" },
  { 0x29, "Small Green Dot" },
  { 0x2A, "Small Dark Green Dot" },
  { 0x2B, "Small Turquoise Dot" },
  { 0x2C, "Small Dark Turquoise Dot" },
  { 0x2D, "Small Blue Dot" },
  { 0x2E, "Small Dark Blue Dot" },
  { 0x2F, "Small Gray Dot" },
  { 0x30, "Small Dark Gray Dot" },
  { 0x31, "Arrow Up" },
  { 0x32, "Arrow Down" },
  { 0x33, "Arrow Left" },
  { 0x34, "Arrow Right" },
  { 0x35, "Arrow Up Left" },
  { 0x36, "Arrow Up Right" },
  { 0x37, "Arrow Down Left" },
  { 0x38, "Arrow Dow Right" },
  { 0x39, "Green Star" },
  { 0x3A, "Yellow Square" },
  { 0x3B, "Red X" },
  { 0x3C, "Turquoise Circle" },
  { 0x3D, "Purple Triangle" },
  { 0x3E, "American Flag" },
  { 0x3F, "Stop" },
  { 0x40, "Parking" },
  { 0x41, "First Aid" },
  { 0x42, "Dining" },
  { 0x43, "Railroad Crossing" },
  { 0x44, "Heliport" },
  { 0x45, "Restroom" },
  { 0x46, "Information" },
  { 0x47, "Diver Down" },
  { 0x48, "Exit" },
  { 0x49, "Health Facility" },
  { 0x4A, "Police" },
  { 0x4B, "Post Office" },
  { 0x4C, "Mining" },
  { 0x4D, "Danger" },
  { 0x4E, "Money" },
  { 0x4F, "Exclamation" },
  { 0x50, "Car" },
  { 0x51, "Jeep" },
  { 0x52, "Truck" },
  { 0x53, "Tow Truck" },
  { 0x54, "Motor Home" },
  { 0x55, "School Bus" },
  { 0x56, "Four-wheeler" },
  { 0x57, "Snowmobile" },
  { 0x58, "Sailboat" },
  { 0x59, "Powerboat" },
  { 0x5A, "Boat Launch" },
  { 0x5B, "Anchor" },
  { 0x5C, "Buoy" },
  { 0x5D, "Shipwreck" },
  { 0x5E, "Glider Area" },
  { 0x5F, "Private Airport" },
  { 0x60, "Public Airport" },
  { 0x61, "Military Airport" },
  { 0x62, "Military Base" },
  { 0x63, "House" },
  { 0x64, "Church" },
  { 0x65, "Building" },
  { 0x66, "School" },
  { 0x67, "Lighthouse" },
  { 0x68, "Bridge" },
  { 0x69, "Radio Tower" },
  { 0x6A, "Dam" },
  { 0x6B, "Tunnel" },
  { 0x6C, "Toll Booth" },
  { 0x6D, "Gas Station" },
  { 0x6E, "Lodging" },
  { 0x6F, "Telephone" },
  { 0x70, "Traffic Light" },
  { 0x71, "Fire Hydrant" },
  { 0x72, "Tombstone" },
  { 0x73, "Picnic Table" },
  { 0x74, "Tent" },
  { 0x75, "Shelter" },
  { 0x76, "Camper" },
  { 0x77, "Fire" },
  { 0x78, "Shower" },
  { 0x79, "Drinking Water" },
  { 0x7A, "Binoculars" },
  { 0x7B, "Camera" },
  { 0x7C, "Geocache" },
  { 0x7D, "Geocache Found" },
  { 0x7E, "Fishing Pole" },
  { 0x7F, "Ice Fishing Trap Set" },
  { 0x80, "Ice Fishing Trap Up" },
  { 0x81, "Moose" },
  { 0x82, "Deer" },
  { 0x83, "Bear" },
  { 0x84, "Bird" },
  { 0x85, "Duck" },
  { 0x86, "Fish" },
  { 0x87, "Deer Tracks" },
  { 0x88, "Animal Tracks" },
  { 0x89, "Bird Tracks" },
  { 0x8A, "Birch Tree" },
  { 0x8B, "Evergreen Tree" },
  { 0x8C, "Deciduous Tree" },
  { 0x8D, "Flower Garden" },
  { 0x8E, "Mountain" },
  { 0x8F, "Cave" },
  { 0x90, "Beach" },
  { 0x91, "Hiking" },
  { 0x92, "Swimming" },
  { 0x93, "Bicycling" },
  { 0x94, "Kayaking" },
  { 0x95, "Canoeing" },
  { 0x96, "Water Skiing" },
  { 0x97, "Cross-country Skiing" },
  { 0x98, "Downhill Skiing" },
  { 0x99, "Ice Skating" },
  { 0x9A, "Dogsledding" },
  { 0x9B, "Shooting" },
  { 0x9C, "Golf Course" },
  { 0x9D, "Ballpark" }
};
