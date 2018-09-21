#! /bin/bash

apiKey="foobar"

curl -v "http://localhost:8610/v1.1/inspections/all/name?key=$apiKey&regex=bloom"
echo
