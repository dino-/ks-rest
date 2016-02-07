#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

curl \
   "http://localhost:8610/v1.0/inspections/recent/placeid/ChIJeR_O8EdYrIkRWA_OTPH4PCk?key=$apiKey" \
   | json_reformat
