#! /bin/bash

# This script will package the project as a Debian .deb file for installation

# Extract the project version from the Haskell build file
version=$(perl -ne "print \"\$1\n\" if /version: +'(.*)'/" package.yaml)

# Use it to make the top-level dist path
distRoot="ks-rest/ks-rest_${version}"

# Build and deploy the project to a prefix below the distRoot
hsinstall --prefix="${distRoot}/usr"

# Copy the rest of the files needed for installation and Debian packaging
cp -rv util/resources/* "$distRoot"

# Update the Debian control file with the correct project version
perl -pi -e "s/Version: .*/Version: ${version}/" "${distRoot}/DEBIAN/control"

sudo chown -R root:root "$distRoot"
dpkg-deb --build "$distRoot"
