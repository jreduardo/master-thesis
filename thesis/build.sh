#!/bin/bash

# =====================================================================
# Make the output pdf of ESALQ-USP Thesis using Rnw
#                                                        Eduardo Junior
#                                                    edujrrib@gmail.com
#                                                            2017-11-21
# =====================================================================

# Create a temporary directory
TMPDIR="/tmp/trash-thesis/"
if [ ! -d $TMPDIR ]
then mkdir $TMPDIR
fi

# Files in initial (initial state)
echo "`ls`" > $TMPDIR.tmp-state0

# Firt compilation. Create auxiliary files.
echo "------------------------------------------------------------"
echo "First xelatex compilation"
echo "------------------------------------------------------------"
xelatex $1

# Bibtex compilation. Create bbl files by chapter
echo "------------------------------------------------------------"
echo "Bibtex compilation"
echo "------------------------------------------------------------"
CHAPTERS=$(sed -n 's/\\include{\(.*\)}/\1/p' $1)
for i in $CHAPTERS; do
    bibtex $i
done

# Compile twice. For update auxiliary files and create correct PDF
echo "------------------------------------------------------------"
echo "Compile xelatex twice. For create final PDF"
echo "------------------------------------------------------------"
for i in 1 2; do
    xelatex $1
done

# Files in final (final state)
echo "`ls`" > $TMPDIR.tmp-state1

# Move files created on process to temporary directory
DIFF=`comm -13 $TMPDIR.tmp-state0 $TMPDIR.tmp-state1`
find $DIFF ! -name '*.pdf' | xargs -I '{}' mv '{}' $TMPDIR

echo "------------------------------------------------------------"
echo "Done. Auxiliary latex files moved to $TMPDIR"
echo "------------------------------------------------------------"

exit 0













#-------------------------------------------
