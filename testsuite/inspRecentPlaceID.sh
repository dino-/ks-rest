#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)
listOfPlaceIDs=${1:-testsuite/placeIDs.json}

curl \
   --header "Content-Type: application/json" \
   --data @$listOfPlaceIDs \
   "http://localhost:8610/v1.0/inspections/by_placeid?key=$apiKey"
