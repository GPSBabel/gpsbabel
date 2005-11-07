
FROM="233 S. Upper Wacker Dr, Chicago, IL"
TO="1060 W. Addison St, Chicago, IL"
wget -O - "http://maps.google.com/maps?q=$FROM to $TO&output=js" 2>/dev/null

