#!/bin/sh

CUR_DIR=`pwd`
EXT=`date '+%Y%m%d%H%M%S'`
for f in `ls $CUR_DIR`; do
    dot_file=`echo $f | tr -d 'dot'`
    # make sure we don't blindly clobber old file
    if [ -f $dot_file ]; then
        mv $dot_file ${dot_file}.${EXT}
    fi
    ln -s $CUR_DIR/$f ~/$dot_file
done

