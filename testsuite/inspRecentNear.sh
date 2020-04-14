#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

hostPort="localhost:8610"      # local development
# hostPort="ks-prod-01"     # AWS development
# hostPort="honuapps.com"   # production

# hq
lat=35.8080294
lng=-78.5604068

dist=1000

curl "http://${hostPort}/v1.1/inspections/recent/near?key=${apiKey}&lat=${lat}&lng=${lng}&dist=${dist}" | json_reformat
