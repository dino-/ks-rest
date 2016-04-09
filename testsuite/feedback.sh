#! /bin/bash

protocol="http"
#protocol="https"

hostPort="localhost:8610"
#hostPort="ksdev.honuapps.com"

apiKey=$(<$HOME/.config/ksnitch/test-apikey)

#feedback='{ "status": "New", "device_id": "a_test_device", "place_id": "ChIJtQic3uZXrIkRxIFU0wH2WWM", "date": 20160409, "issue_type": "Closed", "comment": "This restaurant is closed!" }'
feedback='{ "status": "New", "device_id": "a_test_device", "place_id": null, "date": 20160409, "issue_type": "Closed", "comment": null }'

curl \
   -v \
   --header "Content-Type: application/json" \
   --data "$feedback" \
   "$protocol://$hostPort/v1.1/feedback?key=$apiKey"

echo
