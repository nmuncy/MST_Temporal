#!/bin/bash





workDir=~/compute/Temporal/Experiment3
slurmDir=${workDir}/derivatives/Slurm_out
time=`date '+%Y_%m_%d-%H_%M_%S'`
outDir=${slurmDir}/TS7_${time}

mkdir -p $outDir


sbatch \
-o ${outDir}/output_TS7.txt \
-e ${outDir}/error_TS7.txt \
Task_step7_meanBetas.sh
