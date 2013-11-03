#!/bin/sh

if [ $# -ne 1 ]; then
	echo 1>&2 Usage: ./deploy.sh master
	exit 0
fi

# checkout the proper branch
git checkout $1

# get the git revision number
gitvers=`git describe`

nameNoV=`echo $gitvers | cut -c 2-`

echo "Setting Version: $nameNoV"

cp pom.xml ../pom-old.xml
mv pom.xml pom-edit.xml

# javadoc -d docs-$name -sourcepath src/main/java -subpackages me.botsko.prism

# add in revision
sed -e "s/nightly/$nameNoV/g" pom-edit.xml > pom.xml
rm pom-edit.xml

# Build maven
mvn deploy

mv ../pom-old.xml pom.xml

echo "BUILD COMPLETE"