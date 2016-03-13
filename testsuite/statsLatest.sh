#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

curl "http://localhost:8610/v1.1/stats/latest/by_source?key=$apiKey&sources=nc_wake" | json_reformat
