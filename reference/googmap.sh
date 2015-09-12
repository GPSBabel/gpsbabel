
FROM="233 S. Wacker, Chicago, IL"
TO="1060 W. Addison, Chicago, IL"
wget -O - "http://maps.google.com/maps?q=$FROM to $TO&output=xml" 2>/dev/null

