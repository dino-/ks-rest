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

Running in development

    $ stack exec ks-rest -- PATH/TO/DIR/CONTAINING/CONF

Tests for this project are a set of shell scripts, not automated, in the `testsite` directory. The running `ks-rest` server will need to be hooked up to a MongoDB instance for most of these to work.


### Building for deployment

We recommend using the `hsinstall` tool which is available for Linux distributions as a portable AppImage [here]()

Usage would be

    $ hsinstall-x86_64.AppImage -p /opt/ksnitch

If you must do it without `hsinstall`, I suggest this:

    $ stack install --local-bin-path /opt/ksnitch

This will build everything into a deployable directory structure
that you can put somewhere like `/opt/` for instance.

    $ cabal install --prefix=/tmp/ks-rest-VER --datasubdir=.
    $ pushd /tmp
    $ tar czvf ks-download-VER.tgz ks-rest-VER
    $ popd


Once you get it installed, run like this:

    $ ks-rest PATH/TO/DIR/CONTAINING/CONF


## Contact

Dino Morelli <dino@ui3.info>
