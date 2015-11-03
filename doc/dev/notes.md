# ks-server

The REST server module for KitchenSnitch

Initially we will use a self-signed SSL cert for HTTPS. I would like to give the folks at [Let's Encrypt](https://letsencrypt.org/) a try when they get rolling, which is supposed to be Q4 2015.

Good to know: Google Maps and nearly everything else in the world has coordinates as lat,lng but the Geospatial shit needs it in the other order!


## API key work

      data APIKey
         APIKeyActive String String
         APIKeyRevoked String String


## Queries we need now

### the geospatial query


### by source, top/bot scores

In all of the inspections for a given inspection_source, the N (25?) highest and N lowest. This is two separate queries.

      /inspections/by_source/nc_wake/high?n=25
      /inspections/by_source/nc_durham/low

n default to 25


### by place id

All inspections for a given place_id. Sorted reverse by date (most recent first). Default limit to 5 with optional parameter


### by date

      /inspections/latest?limit=50

limit optional, default 50


## general junk

In URL encoded strings:

- %20 = space
- %2C = comma
