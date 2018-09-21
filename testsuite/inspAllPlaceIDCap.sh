#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

curl "http://localhost:8610/v1.1/inspections/all/placeid/ChIJ9zppEk5frIkR3HfXgmYCHDk?key=$apiKey" | json_reformat
