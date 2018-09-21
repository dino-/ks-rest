#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

# Downtown Raleigh
lat=35.7978631
lng=-78.7650704

# More or less the entire county, in meters
dist=34000

curl "http://localhost:8610/v1.1/inspections/recent/sorted?key=$apiKey&lat=$lat&lng=$lng&dist=$dist&sort=%2bscore" | json_reformat
