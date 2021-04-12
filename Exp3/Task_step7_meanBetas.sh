#!/bin/bash

#SBATCH --time=01:00:00   # walltime
#SBATCH --ntasks=2   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=4gb   # memory per CPU core
#SBATCH -J "TS7"   # job name

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE





### Set up
workDir=~/compute/Temporal
grpDir=${workDir}/Analyses/Exp3/grpAnalysis
maskDir=${grpDir}/mvm_masks
betaDir=${grpDir}/mvm_betas


maskArr=("CR-FA" "Hit-Miss" "CR-Hit")
scanArr=(Encoding Encoding Response)
betaArr=("1,3" "5,7" "1,5")


mkdir -p $maskDir $betaDir


### Function
MatchString (){
	local e match="$1"
	shift
	for e; do [[ "$e" == "$match" ]] && return 0; done
	return 1
}


### organize
cd $grpDir
mv Clust* $maskDir


### separate clusters
cd $maskDir

c=0; while [ $c -lt ${#maskArr[@]} ]; do

	mask=Clust_${scanArr[$c]}_${maskArr[$c]}_mask
	3dcopy ${mask}+tlrc ${mask}.nii.gz
	num=`3dinfo ${mask}+tlrc | grep "At sub-brick #0 '#0' datum type is short" | sed 's/[^0-9]*//g' | sed 's/^...//'`

	for ((j=1; j<=$num; j++)); do
		if [ ! -f ${mask}_c${j}+tlrc.HEAD ]; then

			c3d ${mask}.nii.gz -thresh $j $j 1 0 -o ${mask}_c${j}.nii.gz
			3dcopy ${mask}_c${j}.nii.gz ${mask}_c${j}+tlrc
		fi
	done


	### extract betas from each cluster
	arrRem=(`cat ${grpDir}/info_rmSubj_${scanArr[$c]}.txt`)
	file=${scanArr[$c]}_stats_REML+tlrc
	print=${betaDir}/Betas_${scanArr[$c]}_${maskArr[$c]}.txt
	> $print

	for i in ${mask}_c*.HEAD; do
		for j in ${workDir}/Experiment3/derivatives/s*; do

			cluster=${i%.*}
			subj=${j##*\/}

			MatchString "$subj" "${arrRem[@]}"
			if [ $? == 1 ]; then
				stats=`3dROIstats -mask $cluster "${j}/${file}[${betaArr[$c]}]"`
				echo "$subj $stats" >> $print
			fi
		done
	done

	let c+=1
done

