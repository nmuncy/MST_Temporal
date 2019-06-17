#!/bin/bash




###??? update these
workDir=~/compute/Temporal/Experiment3
scriptDir=${workDir}/code
slurmDir=${workDir}/derivatives/Slurm_out
time=`date '+%Y_%m_%d-%H_%M_%S'`
outDir=${slurmDir}/TS2_${time}

mkdir -p $outDir

cd ${workDir}/derivatives
for i in sub*; do

	[ $i == sub-4685 ]; test=$?

	if [ -f ${i}/run-1_Test_scale+tlrc.HEAD ] && [ ! -f ${i}/Response_stats_REML+tlrc.HEAD ]; then
		
	    sbatch \
	    -o ${outDir}/output_TS2_${i}.txt \
	    -e ${outDir}/error_TS2_${i}.txt \
	    ${scriptDir}/Task_step2_sbatch_regress.sh $i $test

	    sleep 1
	fi
done
