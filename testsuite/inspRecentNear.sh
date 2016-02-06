#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

# hq
lat=35.8080294
lng=-78.5604068

dist=1000

curl "http://localhost:8610/v1.0/inspections/recent/near?key=$apiKey&lat=$lat&lng=$lng&dist=$dist" | json_reformat
