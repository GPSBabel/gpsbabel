// This file is never compiled or linked into GPSBabel itself; it exists
// solely so we can VC the model of things we have to tell Coverity about
// to shush bogus positives.

void fatal(const char *msg, ...) {
  __coverity_panic__();
}

// Forward decls to make the signature match.
class Waypoint; 
class route_head;
class QString;
class route;

// These functions "claim" their arguments 
void
waypt_add(Waypoint* wpt) {
  __coverity_escape__(wpt);
}


void track_add_head(route_head* rte) {
  __coverity_escape__(rte);
}
void route_add_head(route_head* rte) {
  __coverity_escape__(rte);
}
void route_add_wpt(route_head* rte, Waypoint* wpt) {
  __coverity_escape__(wpt);
}
void track_add_wpt(route_head* rte, Waypoint* wpt) {
  __coverity_escape__(wpt);
}
void route_add_wpt_named(route_head* rte, Waypoint* wpt, 
                         const QString& namepart, int number_digits) {
  __coverity_escape__(wpt);
}
void track_add_wpt_named(route_head* rte, Waypoint* wpt,
                         const QString& namepart, int number_digits) {
  __coverity_escape__(wpt);
}
