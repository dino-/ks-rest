#! /bin/bash

apiKey="33de540a8b90e0d6f494c60b590b7396da295adc"

curl -v "http://localhost:8610/v1.0/inspections/all/name?key=$apiKey&regex=bloom"
echo
