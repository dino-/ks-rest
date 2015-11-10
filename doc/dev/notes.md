# ks-server

The REST server module for KitchenSnitch

Initially we will use a self-signed SSL cert for HTTPS. I would like to give the folks at [Let's Encrypt](https://letsencrypt.org/) a try when they get rolling, which is supposed to be Q4 2015.

Good to know: Google Maps and nearly everything else in the world has coordinates as lat,lng but the Geospatial shit needs it in the other order!


## API key work

      data APIKey
         = Read { key :: String, desc :: String }
         | ReadWrite { key :: String, desc :: String }
         | Revoked { key :: String, desc :: String }


## Queries we need now

Note that the API version is not necessarily the same as the server version.


### general query shape

We want to get away from having users specify a port number on the outside. I'd like to have the urls look like:

      http://ks.honuapps.com/v1.0/inspections... -> http://localhost:8001/inspections...
      http://ks.honuapps.com/v1.2/inspections... -> http://localhost:8003/inspections...

This can be done with URL rewriting in Nginx, read more about this


### the geospatial query


### by source, top/bot scores

In all of the inspections for a given inspection_source, the N (25?) highest and N lowest. This is two separate queries.

      /inspections/by_source/high?sources=nc_wake?limit=25
      /inspections/by_source/low?sources=nc_durham,nc_orange,nc_chatham

n default to 25


### by place id

All inspections for a given place_id. Sorted reverse by date (most recent first). Default limit to 5 with optional parameter


### by date

      /inspections/by_source/latest?sources=nc_durham,nc_orange,nc_chatham&limit=50

limit optional, default 50


## general junk

In URL encoded strings:

- %20 = space
- %2C = comma


## Recent db adjustment, added recent_inspections collection (2015-11-08)

Did this in the mongo shell

Get the most recent inspection for every place id

      var c = db.inspections.aggregate([{$sort:{"place.place_id":1,"inspection.date":-1}},{$group:{_id:"$place.place_id",last_inspection:{$first:{_id:"$_id",doctype:"$doctype",inspection:"$inspection",place:"$place"}}}}])

Insert that set into the recent_inspections collection

      c.forEach( function(d) { db.recent_inspections.insert( d.last_inspection ); } )


These numbers should match:

      db.recent_inspections.count()
      db.recent_inspections.distinct("place.place_id").length
