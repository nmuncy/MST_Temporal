#!/bin/bash



workDir=/Volumes/Yorick/Temporal/Experiment3/Behavioral_Data
cd $workDir


> Subj_List.txt

for i in s*; do
	if [ -f ${i}/${i}_timing_log.csv ]; then
		echo $i >> Subj_List.txt
	fi
done
