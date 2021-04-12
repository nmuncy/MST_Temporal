#!/bin/bash


# stderr and stdout are written to ${outDir}/error_* and ${outDir}/output_* for troubleshooting.
# job submission output are time stamped for troubleshooting


workDir=~/compute/Temporal/Experiment3   ###??? update this

scriptDir=${workDir}/code
slurmDir=${workDir}/derivatives/Slurm_out
time=`date '+%Y_%m_%d-%H_%M_%S'`
outDir=${slurmDir}/TS1_${time}

mkdir -p $outDir


cd ${workDir}/derivatives

for i in sub*; do
	if [ ! -f ${i}/run-1_Test_scale+tlrc.HEAD ]; then

	    sbatch \
	    -o ${outDir}/output_TS1_${i}.txt \
	    -e ${outDir}/error_TS1_${i}.txt \
	    ${scriptDir}/Exp3_Task_step1_sbatch_preproc.sh $i

	    sleep 1
	fi
done
