#!/bin/bash



###??? update these
workDir=~/compute/Temporal/Experiment3
scriptDir=${workDir}/code
slurmDir=${workDir}/derivatives/Slurm_out
time=`date '+%Y_%m_%d-%H_%M_%S'`
outDir=${slurmDir}/TS4_${time}

mkdir -p $outDir


sbatch \
-o ${outDir}/output_TS4.txt \
-e ${outDir}/error_TS4.txt \
${scriptDir}/Task_step4_HipSeg_betas.sh

