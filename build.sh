#!/bin/sh

if [ $# -ne 2 ]; then
	echo 1>&2 Usage: ./build.sh branch release|none
	exit 0
fi

# checkout the latest code from trunk
#git clone git@github.com:botskonet/Prism.git
#cd Prism

# checkout the proper branch
git checkout $1

# get the git revision number
gitvers=`git describe`

cp plugin.yml plugin-new.yml
mv plugin.yml plugin-old.yml

name=""
if [ "$1" == "master" ]; then
	name=$gitvers
else
	name="$1-$gitvers"
fi

# add in revision to app.default.config.php
sed -e "s/nightly/$name/g" plugin-new.yml > plugin.yml

# make the jar
jar cf Prism-$name.jar README.md items.yml languages plugin.yml -C bin .

# remove the build yml
rm plugin.yml
rm plugin-new.yml

# replace the old one
mv plugin-old.yml plugin.yml

# send file to amazon bucket
#s3cmd put --acl-public Prism-$name.jar s3://botsko/Prism/Prism-$name.jar

# Create a new version file
echo $name > versions.txt

# send file to amazon bucket
if [ "$2" == "release" ]; then
	s3cmd put --acl-public versions.txt s3://botsko/Prism/versions.txt
fi

# Remove the files
rm Prism-$name.jar
rm versions.txt


echo "BUILD COMPLETE"
