
FROM="233 S. Upper Wacker Dr, Chicago, IL"
TO="1060 W. Addison St, Chicago, IL"
wget -O - "http://maps.googleapis.com/maps/api/directions/xml?origin=$FROM&destination=$TO&sensor=false" 2>/dev/null

