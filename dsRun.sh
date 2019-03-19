#!/bin/bash
HEADER_COLOR='\033[1;33m'
NO_COLOR='\033[0m'
type="glm-k"

# runs multiple threads in background, cf. https://stackoverflow.com/a/41920899
for f in data/atomic_full/Merged_by_function_*.csv; do
	relPath=$(basename "$f")
	relPath=${relPath::-4} #See: https://unix.stackexchange.com/a/144330
	echo -e "Process ${HEADER_COLOR}${relPath}${NO_COLOR}"
	dst="out/AnalysisResults_${type}_${relPath}.log"
	nohup Rscript DS_Analysis.R ${relPath} "TRUE" ${type} &> "${dst}" &
done

nProcesses=$(ps aux | fgrep .R | wc -l)
nProcesses=$((nProcesses - 2))
echo -en "${nProcesses} processes running..."
sleep 5s
while [ $nProcesses -gt 0 ]; do
	echo -en \\r"${nProcesses} processes still running..."

	nProcesses=$(ps aux | fgrep .R | wc -l)
	nProcesses=$((nProcesses - 2))
	sleep 5s
done
echo "finished"
