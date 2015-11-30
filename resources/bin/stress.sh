#! /bin/bash

# Script to stress-test hitting our server for by_placeid results

curlArgs="-s --connect-timeout 5"

placeId=${1:?"Please supply a Google Places ID"}

url="http://localhost:8001/inspections/by_placeid/$placeId"

while true
do
   curl $curlArgs "$url" > /dev/null
   result=$?

   [[ $result == 0 ]] || echo "There was a problem"

   sleep 1
done


# Some place ids to try
#
# ChIJ17b5Ct1YrIkRoum2MD9QN0s
# ChIJUTGXOgBcrIkRyat60K-R3TI
# ChIJn5C3Fx-OrIkRm8_aG9sfV-0
