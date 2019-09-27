#!/bin/bash

#SBATCH --time=02:00:00   # walltime
#SBATCH --ntasks=6   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=6gb   # memory per CPU core
#SBATCH -J "TS6"   # job name

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE






### --- Set up --- ###										###??? update variables/arrays
#
# This is where the script will orient itself.
# Notes are supplied, and is the only section
# that really needs to be changed for each
# experiment.


# General variables
parDir=~/compute/Temporal
workDir=${parDir}/Experiment3/derivatives					# par dir of data
outDir=${parDir}/Analyses/Exp3/grpAnalysis					# where output will be written (should match step3)
refFile=${workDir}/Experiment3/sub-3408/Encoding_stats_REML+tlrc		# reference file, for finding dimensions etc


tempDir=~/bin/Templates/vold2_mni							# desired template
svLabels=(0017 0053 1006 2006 1016 2016)
smallMask=Intersection_GM_MTL_mask+tlrc
svPrior=${tempDir}/priors_JLF


runIt=1														# whether ETAC/MVM scripts actually run (and not just written) (1)
blurM=2
compList=(Encoding Response)								# matches decon prefixes, and will be prefix of output files
compLen=${#compList[@]}




# function - search array for string
MatchString () {

	local e match="$1"

	shift
	for e; do
		[[ "$e" == "$match" ]] && return 0
	done
	return 1
}




### make small vol
cd $outDir

if [ ! -f ${smallMask}.HEAD ]; then

	for i in ${svLabels[@]}; do
		c3d ${svPrior}/label_${i}.nii.gz -thresh 0.3 1 1 0 -o tmp_label_${i}.nii.gz
	done

	c3d tmp_label* -accum -add -endaccum -o tmp_mtl_mask.nii.gz
	3dresample -master $refFile -rmode NN -input tmp_mtl_mask.nii.gz -prefix tmp_mtl_mask_res.nii.gz
	c3d tmp_mtl_mask_res.nii.gz Group_epi_mask.nii.gz -multiply -o tmp_mtl_mask_res_ins.nii.gz
	c3d tmp_mtl_mask_res_ins.nii.gz -thresh 0.1 1 1 0 -o tmp_final.nii.gz

	3dcopy tmp_final.nii.gz $smallMask
	rm tmp*
fi


3dcalc -a $smallMask -b Encoding_MVM_REML+tlrc -expr 'a*b' -prefix Encoding_MVM_smallVol
3dcalc -a $smallMask -b Response_MVM_REML+tlrc -expr 'a*b' -prefix Response_MVM_smallVol




### monte simulations
arrCount=0; while [ $arrCount -lt $compLen ]; do

	pref=${compList[$arrCount]}
	outPre=${pref}_MVM_REML_sv
	print=ACF_raw_${pref}_sv.txt
	> $print

	# make subj list
	unset subjList

	for j in ${workDir}/s*; do

		arrRem=(`cat info_rmSubj_${pref}.txt`)
		subj=${j##*\/}
		MatchString "$subj" "${arrRem[@]}"

		if [ $? == 1 ]; then
			subjList+=("$subj ")
		fi
	done


	# blur, determine parameter estimate
	gridSize=`3dinfo -dk $refFile`
	blurH=`echo $gridSize*$blurM | bc`
	blurInt=`printf "%.0f" $blurH`

	for k in ${subjList[@]}; do
		for m in stats errts; do

			hold=${workDir}/${k}/${pref}_${m}_REML
			file=${workDir}/${k}/${pref}_errts_REML_blur${blurInt}+tlrc

			# blur
			if [ ! -f ${hold}_blur${blurInt}+tlrc.HEAD ]; then
				3dmerge -prefix ${hold}_blur${blurInt} -1blur_fwhm $blurInt -doall ${hold}+tlrc
			fi
		done

		# parameter estimate
		3dFWHMx -mask $smallMask -input $file -acf >> $print
	done


	# simulate noise, determine thresholds
	if [ ! -s ACF_MC_${pref}_sv.txt ]; then

		sed '/ 0  0  0    0/d' $print > tmp

		xA=`awk '{ total += $1 } END { print total/NR }' tmp`
		xB=`awk '{ total += $2 } END { print total/NR }' tmp`
		xC=`awk '{ total += $3 } END { print total/NR }' tmp`

		3dClustSim -mask $smallMask -LOTS -iter 10000 -acf $xA $xB $xC > ACF_MC_${pref}_sv.txt
		rm tmp
	fi

	let arrCount=$[$arrCount+1]
done




