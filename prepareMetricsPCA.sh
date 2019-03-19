#!/bin/bash
folder=${1:-out}
file=${2:-UsefulMetrics.csv}
path=${folder}/${file}

# Replace Dots by whitespaces
#sed -i -e 's/\./ /g' $path
# Handle type names
#sed -i -e 's/ bool 1 /(bool=1)/g' $path
#sed -i -e 's/ tristate 1 /(tristate=1)/g' $path
#sed -i -e 's/ string 1 /(string=1)/g' $path
#sed -i -e 's/ integer 1  int 1 /(integer=1, int=1)/g' $path
#sed -i -e 's/ hex 1 /(hex=1)/g' $path
# Clean-up of basis vectors
#sed -i -e 's/ 1 0 0 /(1-0-0)/g' $path
#sed -i -e 's/ 0 1 0 /(0-1-0)/g' $path
#sed -i -e 's/ 0 0 1 /(0-0-1)/g' $path
# Remove first column
sed -i -e 's/^[^;]*;//g' $path
