#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

curl "http://localhost:8610/v1.1/inspections/all/name?key=$apiKey&regex=bloom" | json_reformat
