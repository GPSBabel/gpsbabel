# Security Policy

We are in favor of it. :-)

## Supported Versions

Only the current shipping version and the development trunk are supported.

## Reporting a Vulnerability

File a bugreport at <https://github.com/GPSBabel/gpsbabel/issues> please.
Include all steps necessary to reproduce.

Hypotheticals like "If a bit in RAM is corrupted between this store and
this load" will be closed. We're a real product that deals in real issues
only.

We are inherently reading untrusted input and often from untrusted
sources, so running a file converter as root on your server is bad. Don't
do that. If you insist on doing it, please run ulimit to minimize the
time and CPU load that can be used. An adversary can hand craft (or just
plain have) a corrupt or malformed file and sending it to you might make
GPSBabel loop forever. We try for that to not happen, but it's a necessary
reality of our job. Our goal is a desktop user converting files that they
control.
