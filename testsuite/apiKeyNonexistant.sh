#! /bin/bash

apiKey="foobar"

curl -v "http://localhost:8610/v1.0/inspections/all/name?key=$apiKey&regex=bloom"
echo
