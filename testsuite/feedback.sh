#! /bin/bash

apiKey=$(<$HOME/.config/ksnitch/test-apikey)
feedback='{ "status": "New", "device_id": "a_test_device", "place_id": "ChIJtQic3uZXrIkRxIFU0wH2WWM", "date": 20160409, "issue_type": "Closed", "comment": "This restaurant is closed!" }'

curl \
   -v \
   --header "Content-Type: application/json" \
   --data "$feedback" \
   "http://localhost:8610/v1.1/feedback?key=$apiKey" \
   | json_reformat
