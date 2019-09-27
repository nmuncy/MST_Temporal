#!/bin/bash





workDir=~/compute/Temporal/Experiment3
slurmDir=${workDir}/derivatives/Slurm_out
time=`date '+%Y_%m_%d-%H_%M_%S'`
outDir=${slurmDir}/TS6_${time}

mkdir -p $outDir


sbatch \
-o ${outDir}/output_TS6.txt \
-e ${outDir}/error_TS6.txt \
Exp3_Task_step6_grpAnalysis_smallVol.sh
