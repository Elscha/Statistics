#!/bin/bash
HEADER_COLOR='\033[1;33m'
NO_COLOR='\033[0m'
dst="out/MergedResults.csv"

#echo "Metrik;Estimate;Std. Error;t value;Pr(>|t|);Significance" > ${dst}
find out -maxdepth 1 -iname "Result*.csv" | sort | while read f; do
	relPath=$(basename "$f")
        relPath=${relPath::-4} #See: https://unix.stackexchange.com/a/144330
        echo -e "Merge File: ${HEADER_COLOR}${relPath}${NO_COLOR}"
	echo "${relPath}" >> ${dst}
	cat "${f}" >> ${dst}
done

sed -i -e 's/:/ X /g' ${dst}
sed -i -e 's/\./ /g' ${dst}
