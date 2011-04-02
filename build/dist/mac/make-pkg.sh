#!/bin/sh

thisdir=`pwd`

top_srcdir=$1
package=$2
hr_package=$3
version=$4
dmg_name=$5

builddir=$top_srcdir/temp
pkgdir=$top_srcdir/packages

local_name=$hr_package-$version
workingdir=$builddir/mac/$local_name
componentdir=$workingdir/component
extrasdir=$workingdir/extras
imagedir=$builddir/mac/${local_name}-image

if [ -x "/Developer/Tools/packagemaker" ]; then
  packagemaker="/Developer/Tools/packagemaker"
fi
# from 10.5 / Xcode 3.0 packagemaker has moved
if [ -x "/Developer/usr/bin/packagemaker" ]; then
  packagemaker="/Developer/usr/bin/packagemaker"
fi



# make folder structure
if [ ! -d "$builddir/mac" ]; then
    mkdir $builddir/mac
fi
if [ ! -d "$workingdir" ]; then
    mkdir $workingdir
fi
if [ ! -d "$pkgdir" ]; then
    mkdir $pkgdir
fi

if [ ! -d "$componentdir" ]; then
    mkdir $componentdir
fi

if [ ! -d "$extrasdir" ]; then
    mkdir $extrasdir
fi
if [ ! -d "$imagedir" ]; then
    mkdir $imagedir
fi

cp -r $builddir/dist-release/${hr_package}.app $componentdir/

cp License.rtf $extrasdir/
cp Welcome.rtf $extrasdir/
cp background.jpg $extrasdir/

$packagemaker -build \
	-p $imagedir/${local_name}.pkg \
	-f $componentdir \
	-i $thisdir/Info.plist \
	-d $thisdir/Desc.plist \
  -r $extrasdir/


# prepare the disk image

if [ -f "$dmg_name" ]; then
  rm $dmg_name
fi
hdiutil create -srcfolder $imagedir $dmg_name -volname ${hr_package}-$version
