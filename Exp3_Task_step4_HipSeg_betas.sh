#!/bin/bash

#SBATCH --time=10:00:00   # walltime
#SBATCH --ntasks=6   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=4gb   # memory per CPU core
#SBATCH -J "TS4"   # job name

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE





# Written by Nathan Muncy on 12/14/18


### --- Notes
#
# 1) This script will pull mean betas from L/R CA1, CA2/3/DG (called Multi), and Sub.
#		- maybe I'll update this in the future for other MTL regions
#		- also, maybe I'll update this to support more than 2 betas p/comparison
#				or...you could
#
# 2) Specifically, each mask for each hemisphere will be resampled,
#		binarized, and voxels defined by mutliple over-lapping masks
#		will be excluded.
#
# 3) A print out of the number of voxels in/excluded is supplied (info_*.txt)
#
# 4) Again, betas will not be extracted from participants who moved too much





# general vars											###??? Update these
parDir=~/compute/Temporal
workDir=${parDir}/Experiment3/derivatives
roiDir=${parDir}/Analyses/Exp3/roiAnalysis
betaDir=${roiDir}/sub_betas
grpDir=${parDir}/Analyses/Exp3/grpAnalysis

tempDir=~/bin/Templates/vold2_mni
priorDir=${tempDir}/priors_HipSeg
specDir=${tempDir}/priors_Special



# decon vars
compList=(Encoding Response)				# matches decon prefix

arrA=(1 1)									# setA beh sub-brik for etacList
arrB=(3 3)									# steB
arrC=(5 5)
arrD=(7 7)

compLen=${#compList[@]}
refFile=${workDir}/sub-3408/${compList[0]}_stats_REML+tlrc



# function - search array for string
MatchString (){
	local e match="$1"
	shift
	for e; do [[ "$e" == "$match" ]] && return 0; done
	return 1
}




### make Sub, CA1, CA2/3/DG masks
mkdir -p $roiDir $betaDir
cd $roiDir


for i in L R; do
	if [ ! -f Mask_Body_${i}_CA1+tlrc.HEAD ]; then

		# resample A/P HC, subregion masks
		for j in CA{1..3} DG Sub hipp_{head,nhead}; do

			if [[ $j == hipp* ]]; then
				tmpPath=$specDir
				tmpString=${i}${j}
			else
				tmpPath=$priorDir
				tmpString=${i}_${j}
			fi

			c3d ${tmpPath}/${tmpString}_prob.nii.gz -thresh 0.3 1 1 0 -o tmp_${tmpString}.nii.gz
			3dfractionize -template $refFile -input tmp_${tmpString}.nii.gz -prefix tmp_${tmpString}_res
			3dcalc -a tmp_${tmpString}_res+tlrc -prefix tmp_${tmpString}_bin -expr "step(a-3000)"
			3dcopy tmp_${tmpString}_bin+tlrc tmp_${tmpString}_bin.nii.gz
		done


		# stitch 2/3/DG (Multi)
		c3d tmp_${i}_CA2_bin.nii.gz tmp_${i}_CA3_bin.nii.gz tmp_${i}_DG_bin.nii.gz -accum -add -endaccum -o tmp_${i}_Multi.nii.gz
		c3d tmp_${i}_Multi.nii.gz -thresh 0.1 inf 1 0 -o tmp_${i}_Multi_bin.nii.gz


		# create removal mask, where overlap occurs
		c3d tmp_${i}_Multi_bin.nii.gz tmp_${i}_CA1_bin.nii.gz tmp_${i}_Sub_bin.nii.gz -accum -add -endaccum -o tmp_${i}_sum.nii.gz
		c3d tmp_${i}_sum.nii.gz -thresh 1.1 10 0 1 -o tmp_${i}_rm.nii.gz
		c3d tmp_${i}_sum.nii.gz -dup -lstat > info_${i}sum.txt

		for k in Multi Sub CA1; do

			# zero out overlapping voxels
			c3d tmp_${i}_${k}_bin.nii.gz tmp_${i}_rm.nii.gz -multiply -o Mask_All_${i}_${k}.nii.gz

			# make head/body masks
			c3d Mask_All_${i}_${k}.nii.gz tmp_${i}hipp_head_bin.nii.gz -multiply -o Mask_Head_${i}_${k}.nii.gz
			c3d Mask_All_${i}_${k}.nii.gz tmp_${i}hipp_nhead_bin.nii.gz -multiply -o Mask_Body_${i}_${k}.nii.gz

			# pull info, convert
			for m in All Head Body; do
				c3d Mask_${m}_${i}_${k}.nii.gz -dup -lstat > info_${m}_${i}_${k}.txt
				3dcopy Mask_${m}_${i}_${k}.nii.gz Mask_${m}_${i}_${k}+tlrc
			done
		done

		rm tmp* Mask*nii.gz
	fi
done




## Pull Betas
for i in ${!compList[@]}; do

	pref=${compList[$i]}
	scan=${pref}_stats_REML+tlrc
	betas=${arrA[$i]},${arrB[$i]},${arrC[$i]},${arrD[$i]}
	arrRem=(`cat ${grpDir}/info_rmSubj_${pref}.txt`)

	for j in All Body Head; do

		print=${betaDir}/Betas_${pref}_${j}_sub.txt
		> $print

		for k in {L,R}_{CA1,Sub,Multi}; do

			mask=Mask_${j}_${k}+tlrc
			echo "Mask ${mask%+*}" >> $print

			for m in ${workDir}/s*; do

				subj=${m##*\/}
				MatchString $subj "${arrRem[@]}"

				if [ $? == 1 ]; then
					stats=`3dROIstats -mask $mask "${m}/${scan}[${betas}]"`
					echo "$subj $stats" >> $print
				fi
			done

			echo >> $print
		done
	done
done


cd $betaDir
> Master_list.txt

for i in Betas*; do
	echo $i >> Master_list.txt
done
