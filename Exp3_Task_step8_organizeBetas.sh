#!/bin/bash


parDir=/Volumes/Yorick/Temporal/Analyses/Exp3/grpAnalysis
workDir=${parDir}/mvm_betas
statsDir=${parDir}/mvm_stats
mkdir $statsDir


compList=(Encoding Response)

arrEncoding=(rHip lErc)
arrResponse=(rErc)


cd $workDir

for i in ${compList[@]}; do

	# Rename cluster to anat
	out=All_Betas_${i}.txt
	> $out
	eval arrHold=(\${arr${i}[@]})

	c=0; for k in Betas_${i}*; do

		cat $k > Betas_${i}_${arrHold[${c}]}.txt
		echo Betas_${i}_${arrHold[${c}]}.txt >> $out
		let c=$[$c+1]
	done
done


> All_list.txt
for i in All_Betas*; do
	echo $i >> All_list.txt
done
