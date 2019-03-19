#!/bin/bash
function countProcesses() {
	local nProcesses=$(ps aux | fgrep .R | wc -l)
	nProcesses=$((nProcesses - 1))
	echo $nProcesses
}


HEADER_COLOR='\033[1;33m'
NO_COLOR='\033[0m'
isFunctionBased="TRUE"
#type="glm-k"
#type="pca-log"
#type="mergePCs"
#type="mergeOnly"
#type="VisDif"
type="anova"
logDest="out/FullAtomic-"

if [ "$isFunctionBased" = "TRUE" ]; then
	logDest="${logDest}Functions.log"
else
	logDest="${logDest}Files.log"
fi



# runs multiple threads in background, cf. https://stackoverflow.com/a/41920899
fileList=

for f in data/atomic_full/Merged_by_*.csv; do
	relPath=$(basename "$f")
	relPath=${relPath::-4} #See: https://unix.stackexchange.com/a/144330
	fileList="${fileList}${relPath},"
done
fileList=${fileList::-1}

nohup Rscript DS_Analysis.R ${fileList} ${isFunctionBased} ${type} &> ${logDest} & disown

nProcesses=$(countProcesses)
echo -en "${nProcesses} processes running..."
sleep 5s
while [ $nProcesses -gt 0 ]; do
	echo -en \\r"${nProcesses} processes still running..."

	nProcesses=$(countProcesses)
	sleep 5s
done
echo -e "finished"
