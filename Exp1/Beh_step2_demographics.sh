#!/bin/bash


parDir=/Volumes/Yorick/Temporal
expDir=${parDir}/Experiment1
outDir=${parDir}/Analyses/Exp1/behAnalysis


print=${outDir}/Master_demographics.txt
echo -e "Subj\tAge\tSex" > $print


cd $expDir
for i in s*; do

	age=`cat ${i}/${i}_timing_log.csv | grep "Subject Age" | sed 's/.*\ //'`
	tmp=`cat ${i}/${i}_timing_log.csv | grep "Subject Sex" | sed 's/.*\ //'`
	sex=`echo ${tmp:0:1} | tr '[:lower:]' '[:upper:]'`

	if [ ! -z $age ]; then
		echo -e "${i}\t${age:0:2}\t${sex} " >> $print
	fi
done