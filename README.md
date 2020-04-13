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

Now, starting in the root of the project, do this, where VER is the version of
ks-rest you are building:

    $ hsinstall --prefix=ks-rest/ks-rest_VER/usr
    $ mkdir ks-rest/ks-rest_VER/DEBIAN
    $ cp -t ks-rest/ks-rest_VER/DEBIAN util/resources/DEBIAN/control util/resources/DEBIAN/conffiles

Edit `ks-rest/ks-rest_VER/DEBIAN/control` to make sure the version
matches what you're building.

    $ sudo chown -R root:root ks-rest/ks-rest_VER
    $ dpkg-deb --build ks-rest/ks-rest_VER

And you should see a `ks-rest/ks-rest_VER.deb` file. Check the contents
if you wish with `dpkg-deb -c ...` This file can be added to the ks-rest
release page on github or distributed however you wish.

If you must do it without `hsinstall`, I suggest this:

    $ mkdir -p ks-rest/ks-rest_VER/usr/bin
    $ stack install --local-bin-path ks-rest/ks-rest_VER/usr/bin

And then follow the instructions above starting with the `mkdir ...DEBIAN` part.

Once ks-rest is installed, run like this:

    $ ks-rest PATH/TO/DIR/CONTAINING/CONF

Some of these notes exist in a more detailed form on the developer wiki.


## Contact

Dino Morelli <dino@ui3.info>
