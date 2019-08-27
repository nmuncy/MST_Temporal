#!/bin/bash



workDir=/Volumes/Yorick/Temporal/Experiment3
grpDir=${workDir}/Analyses/grpAnalysis
maskDir=${grpDir}/mvm_masks
betaDir=${grpDir}/mvm_betas


maskArr=("CR-FA" "Hit-Miss" "CR-Hit")
scanArr=(Encoding Encoding Response)
betaArr=("1,3" "5,7" "1,5")


mkdir -p $maskDir $betaDir


# organize
cd $grpDir
mv Clust* $maskDir


# separate masks
cd $maskDir

c=0; while [ $c -lt ${#maskArr[@]} ]; do

# for i in Clust*mask+tlrc.HEAD; do

	mask=Clust_${scanArr[$c]}_${maskArr[$c]}_mask
	3dcopy ${mask}+tlrc ${mask}.nii.gz
	num=`3dinfo ${mask}+tlrc || grep "At sub-brick #0 '#0' datum type is short" | sed 's/[^0-9]*//g' | sed 's/^...//'`

	for ((j=1; j<=$num; j++)); do
		if [ ! -f  ]