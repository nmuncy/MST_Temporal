#!/bin/bash





workDir=~/compute/Temporal/Experiment3
slurmDir=${workDir}/derivatives/Slurm_out
time=`date '+%Y_%m_%d-%H_%M_%S'`
outDir=${slurmDir}/TS5_${time}

mkdir -p $outDir


sbatch \
-o ${outDir}/output_TS5.txt \
-e ${outDir}/error_TS5.txt \
Task_step5_grpAnalysis.sh
