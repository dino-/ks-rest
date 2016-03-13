#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)
after=1454648400
listOfPlaceIDs='[ "ChIJtQic3uZXrIkRxIFU0wH2WWM", "ChIJeR_O8EdYrIkRWA_OTPH4PCk" ]'

curl \
   --header "Content-Type: application/json" \
   --data "$listOfPlaceIDs" \
   "http://localhost:8610/v1.1/inspections/recent/placeid?key=$apiKey&after=$after" \
   | json_reformat
