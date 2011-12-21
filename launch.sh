#!/bin/bash

die() {
	echo "$1"
	exit 1
}

OLDDIR="$(pwd)"

cd "$(dirname "$0")" || die "Could not change directory"

if [ ! -e "target/afl-assignment-assembly-0.1.jar" ]; then
	echo "*** Build process starting"
	echo "*** Please measure only *subsequent* runs of this program."
	echo
	echo "*** [1/3] Please wait, the build tool is frobnicating the flux capacitor."
	echo "*** Depending on your internet connection, this might take a while."
	echo
	./sbt update || die "Download failed"
	echo
	echo "*** Update successful."
	echo "*** [2/3] Please wait, the build tool is building."
	echo
	./sbt compile || die "Build failed"
	echo
	echo "*** Build successful."
	echo "*** [3/3] Please wait, the build tool is making an awesome package."
	echo
	./sbt assembly || die "Packaging failed"
	echo
	echo "*** Completed."
else
	echo "*** Lucky you, there's already a package."
	echo
fi

cd "$OLDDIR" || die "Could not change directory"

echo "*** Processing files $@"
echo

java -jar "$(dirname "$0")/target/afl-assignment-assembly-0.1.jar" "$@"

echo
echo "*** Done."
