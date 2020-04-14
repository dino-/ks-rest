# ks-rest


## Synopsis

REST server for KitchenSnitch (Haskell)


## Description

REST server for the KitchenSnitch project. This server is for
accessing the back-end KS database data which is currently stored
in a MongoDB instance.


## Building and installation

Building the source

    $ stack build
    $ stack clean

Running in development

    $ stack exec ks-rest -- PATH/TO/DIR/CONTAINING/CONF

Tests for this project are a set of shell scripts, not automated, in the
`testsuite` directory. The local running `ks-rest` server will need to be
hooked up to a MongoDB instance for most of these to work.


### Building for deployment

Our production servers have so far been Ubuntu, so what follows are
instructions for building a .deb file which can be installed with dpkg.

You will need the [hsinstall](https://github.com/dino-/hsinstall/releases)
utility version 2.6 or later for this procedure.

Run the script `./util/package.sh` and you should see a
`ks-rest/ks-rest_VER.deb` file with the current version. Check the contents if
you wish with `dpkg-deb -c ...` This file can be added to the ks-rest release
page on github or distributed however you wish.

If you don't care about making a Debian .deb file, build a deployable directory like this:

    $ hsinstall --prefix=ks-rest-VER

Once ks-rest is installed it can be started with

    # systemctl enable --now ks-rest

If you must do it without `hsinstall`, I suggest this:

    $ mkdir -p ks-rest/ks-rest_VER/usr/bin
    $ stack install --local-bin-path ks-rest/ks-rest_VER/usr/bin

Some of these notes exist in a more detailed form on the developer wiki.


## Contact

Dino Morelli <dino@ui3.info>
