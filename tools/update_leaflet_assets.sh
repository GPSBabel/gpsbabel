#!/usr/bin/env bash
# update_leaflet_assets.sh
# Fetch pinned Leaflet + MarkerCluster assets into gui/.
# Usage: tools/update_leaflet_assets.sh

set -euo pipefail

LEAFLET_VER="${LEAFLET_VER:-1.9.4}"
CLUSTER_VER="${CLUSTER_VER:-1.5.3}"

cd "$(dirname "$0")/.."

fetch() {
  local url="$1" out="$2"
  echo "Fetching $url -> $out"
  mkdir -p "$(dirname "$out")"
  curl -fL --retry 5 --retry-delay 2 -C - -o "$out" "$url"
}

# Destinations (keep simple flat layout under gui/)
LEAFLET_JS="gui/leaflet-${LEAFLET_VER}.js"
LEAFLET_CSS="gui/leaflet-${LEAFLET_VER}.css"
MC_JS="gui/leaflet.markercluster-${CLUSTER_VER}.js"
MC_CSS="gui/MarkerCluster-${CLUSTER_VER}.css"
MC_CSS_DEF="gui/MarkerCluster.Default-${CLUSTER_VER}.css"
ICON_DIR="gui/leaflet-icons"

# Sources (cdnjs pinned to versions)
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet/${LEAFLET_VER}/leaflet.js" "$LEAFLET_JS"
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet/${LEAFLET_VER}/leaflet.css" "$LEAFLET_CSS"

fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet.markercluster/${CLUSTER_VER}/leaflet.markercluster.js" "$MC_JS"
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet.markercluster/${CLUSTER_VER}/MarkerCluster.css" "$MC_CSS"
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet.markercluster/${CLUSTER_VER}/MarkerCluster.Default.css" "$MC_CSS_DEF"

# Default Leaflet marker images
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet/${LEAFLET_VER}/images/marker-icon.png"       "${ICON_DIR}/marker-icon.png"
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet/${LEAFLET_VER}/images/marker-icon-2x.png"    "${ICON_DIR}/marker-icon-2x.png"
fetch "https://cdnjs.cloudflare.com/ajax/libs/leaflet/${LEAFLET_VER}/images/marker-shadow.png"     "${ICON_DIR}/marker-shadow.png"

echo "Done. Update your HTML/paths to reference the local files if desired:"
echo "  $LEAFLET_JS"
echo "  $LEAFLET_CSS"
echo "  $MC_JS"
echo "  $MC_CSS"
echo "  $MC_CSS_DEF"
echo "  $ICON_DIR/*"
