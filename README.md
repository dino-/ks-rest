# ks-server


## Synopsis

REST server for KitchenSnitch (Haskell)


## Description


## Installing


## Building from source

Make sure you have `ghc 7.10.x`, `cabal-install` and `darcs` installed.

Update your cabal list

    $ cabal update

And install some native deps that `cabal` can't do for you

On Ubuntu:

    # apt-get install --reinstall g++ 

Get the `ks-server` source code

    $ darcs get http://hub.darcs.net/dino/ks-server

Update your cabal library and tools, we need a modern version

    $ cabal install Cabal cabal-install

Set up a sandbox for building (if you wish to use a sandbox)

    $ mkdir ~/.cabal/sandbox
    $ cabal sandbox init --sandbox=/home/YOU/.cabal/sandbox/kitchensnitch

Then install the dependencies

    $ cabal install --enable-tests --only-dep

This will build for quite some time, when it's done, you can build
ks-download:


### Building for development

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

And you should be good for development from here.


### Building for deployment

This will build everything into a deployable directory structure
that you can put somewhere like `/opt/` for instance.

    $ cabal configure --prefix=/tmp/ks-server-VER
    $ cabal build
    $ cabal copy
    $ pushd /tmp
    $ tar czvf ks-download-VER.tgz ks-server-VER
    $ popd


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
