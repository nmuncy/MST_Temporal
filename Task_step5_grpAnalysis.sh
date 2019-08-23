#!/bin/bash

#SBATCH --time=40:00:00   # walltime
#SBATCH --ntasks=10   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=10gb   # memory per CPU core
#SBATCH -J "TS5"   # job name

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
parDir=~/compute/Temporal/Experiment3
workDir=${parDir}/derivatives								# par dir of data
outDir=${parDir}/Analyses/grpAnalysis						# where output will be written (should match step3)
refFile=${workDir}/sub-3408/Encoding_stats_REML+tlrc		# reference file, for finding dimensions etc

tempDir=~/bin/Templates/vold2_mni							# desired template
priorDir=${tempDir}/priors_ACT								# location of atropos priors
mask=Intersection_GM_mask+tlrc								# this will be made, just specify name for the interesection gray matter mask


# grpAnalysis
doMVM=1														# MVM (1)
runIt=1														# whether ETAC/MVM scripts actually run (and not just written) (1)

thr=0.3														# thresh value for Group_EPI_mask, ref Group_EPI_mean

compList=(Encoding Response)								# matches decon prefixes, and will be prefix of output files
compLen=${#compList[@]}

arrA=(1 1)													# setA beh sub-brik for compList. Must be same length as compList
arrB=(3 3)													# setB
arrC=(5 5)
arrD=(7 7)
wsArr=ABCD													# list of within-subject arrays used (arrA, arrB, etc), for building permutations (e.g. wsArr=ABC)

namA=(CR CR)								# names of behaviors from arrA. Must be same length as arrA
namB=(FA FA)
namC=(Hit Hit)
namD=(Miss Miss)


# MVM vars/arrs
blurM=2														# blur multiplier, float/int




### --- Functions --- ###

# search array for string
MatchString () {

	local e match="$1"

	shift
	for e; do
		[[ "$e" == "$match" ]] && return 0
	done
	return 1
}


# make perumtations of length 2
MakePerm () {

	local items=$1
	local string i j hArr

	for ((i=0; i<${#items}; i++)); do

		string=${items:$i+1}
		for ((j=0; j<${#string}; j++)); do

			hArr+="${items:$i:1}${string:$j:1} "
		done
	done
	echo $hArr
}




### --- Checks, Permutations --- ###

# check
if [ ${#arrA[@]} != ${#arrB[@]} ] || [ ${#arrA[@]} != $compLen ]; then
	echo >&2
	echo "grpAnalysis variables incorrect. Exit 1" >&2
	echo >&2
	exit 1
fi

if [ $doMVM == 1 ]; then
	if [ ${#bsArr[@]} -gt 1 ] && [ ! -s $bsList ]; then
		echo >&2
		echo "MVM vars/arrs incorrect. Exit 2" >&2
		echo >&2
		exit 2
	fi
fi

if [ ! -f ${refFile%\/*}/${compList[0]}_stats_REML+tlrc.HEAD ]; then
	echo >&2
	echo "REML output not detected. Exit 3" >&2
	echo >&2
	exit 3
fi

afniVer=`afni -ver | sed 's/[^0-9]//g'`
if [ ! ${afniVer:7} -ge 18315 ]; then
	echo >&2
	echo "Update AFNI and try again - AFNI version is not at least 18.3.15. Exit 4" >&2
	echo >&2
	exit 4
fi

# make permutation lists
arr=(`MakePerm $wsArr`)
alpha=(`echo {A..Z}`)
wsList=(${alpha[@]:0:${#wsArr}})


for ((a=0; a<${#bsArr[@]}; a++)); do
	tmpList+=$a
done
arrBS=(`MakePerm $tmpList`)




### --- Create Masks --- ###
#
# This section will create a group mean intersection mask
# then threshold it at $thr to create a binary intersection mask.
# A gray matter mask will be constructed, and then the GM mask
# will be multiplied with the intersection mask to create a
# single GM intersection mask


cd $outDir

if [ $runIt == 1 ]; then

	# intersection mask
	if [ ! -f Group_epi_mask.nii.gz ] && [ ! -f etac_extra/Group_epi_mask.nii.gz ]; then

		for i in ${workDir}/s*; do

			subj=${i##*\/}
			MatchString "$subj" "${arrRem[@]}"

			if [ $? == 1 ]; then
				list+="${i}/mask_epi_anat+tlrc "
			fi
		done

		3dMean -prefix ${outDir}/Group_epi_mean.nii.gz $list
		3dmask_tool -input $list -frac $thr -prefix ${outDir}/Group_epi_mask.nii.gz
	fi


	# make $mask
	if [ ! -f ${mask}.HEAD ]; then

		# GM mask
		c3d ${priorDir}/Prior2.nii.gz ${priorDir}/Prior4.nii.gz -add -o tmp_Prior_GM.nii.gz
		3dresample -master $refFile -rmode NN -input tmp_Prior_GM.nii.gz -prefix tmp_Template_GM_mask.nii.gz

		# combine GM and intersection mask
		c3d tmp_Template_GM_mask.nii.gz Group_epi_mask.nii.gz -multiply -o tmp_Intersection_GM_prob_mask.nii.gz
		c3d tmp_Intersection_GM_prob_mask.nii.gz -thresh 0.1 1 1 0 -o tmp_Intersection_GM_mask.nii.gz
		3dcopy tmp_Intersection_GM_mask.nii.gz $mask
		rm tmp*
	fi

	if [ ! -f ${mask}.HEAD ]; then
		echo >&2
		echo "Could not construct $mask. Exit 5" >&2
		echo >&2
		exit 5
	fi


	# get template
	if [ ! -f vold2_mni_brain+tlrc.HEAD ]; then
		cp ${tempDir}/vold2_mni_brain+tlrc* .
	fi
fi




### --- MVM --- ###
#
# This will blur both stats and errts files according to $blurM, and
# then the blurred errts files will be used to model noise with
# an auto-correlation function. MVM scripts will be written and run,
# and none of this will happen on participants who move too much.
# A variable number of bx/wi subj variables is accepted, but this
# will not run a t-test.
#
# Currently, MVM post-hoc comparisons are permutations of bx/wi-subj
# variables. I.e. Behaviors A B C for groups Aut Con will yield
# comparisons of Aut-Con A-B, Aut-Con A-C, Aut-Con B-C. I could build
# more comparisons in the future.


if [ $doMVM == 1 ]; then

	if [ ${#wsArr} -lt 3 ] && [ ${#bsArr[@]} == 1 ]; then
		echo >&2
		echo "Replace user and try again - don't use ACF for a pairwise comparison. Exit 6" >&2
		echo >&2
		exit 6
	fi


	arrCount=0; while [ $arrCount -lt $compLen ]; do

		pref=${compList[$arrCount]}
		print=ACF_raw_${pref}.txt
		outPre=${pref}_MVM_REML

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

		if [ $runIt == 1 ]; then
			if [ ! -s $print ]; then
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
					3dFWHMx -mask $mask -input $file -acf >> $print
				done
			fi


			# simulate noise, determine thresholds
			if [ ! -s ACF_MC_${pref}.txt ]; then

				sed '/ 0  0  0    0/d' $print > tmp

				xA=`awk '{ total += $1 } END { print total/NR }' tmp`
				xB=`awk '{ total += $2 } END { print total/NR }' tmp`
				xC=`awk '{ total += $3 } END { print total/NR }' tmp`

				3dClustSim -mask $mask -LOTS -iter 10000 -acf $xA $xB $xC > ACF_MC_${pref}.txt
				rm tmp
			fi
		fi


		# set up - determine/construct variables for script
		scan=${pref}_stats_REML_blur${blurInt}+tlrc
		
		unset conVar gltCount dataFrame

		if [ ${#bsArr[@]} -gt 1 ]; then


			# header, bx-subj title
			bsVars=BSVARS
			header="Subj $bsVars WSVARS InputFile"


			# make $conVar (post-hoc comparisons)
			for x in ${!arrBS[@]}; do

				h1=${arrBS[$x]:0:1}
				h2=${arrBS[$x]:1:1}

				bsCon="1*${bsArr[$h1]} -1*${bsArr[$h2]}"
				bsLab=${bsArr[$h1]}-${bsArr[$h2]}

				for y in ${!arr[@]}; do

					gltCount=$[$gltCount+1]
					ws1h=${arr[$y]:0:1}
					ws2h=${arr[$y]:1:1}

					eval declare -a nam1=(nam${ws1h})
					eval declare -a nam2=(nam${ws2h})
					name1=$(eval echo \${${nam1}[$arrCount]})
					name2=$(eval echo \${${nam2}[$arrCount]})

					conVar+="-gltLabel $gltCount ${bsLab}_${name1}-${name2} -gltCode $gltCount '${bsVars}: $bsCon WSVARS: 1*$name1 -1*$name2' "
				done
			done


			# determine group membership, write dataframe
			bsSubj=(`cat $bsList | awk '{print $1}'`)
			bsGroup=(`cat $bsList | awk '{print $2}'`)

			for m in ${subjList[@]}; do
				for n in ${!bsSubj[@]}; do
					if [ $m == ${bsSubj[$n]} ]; then
						for o in ${wsList[@]}; do

							brik=$(eval echo \${arr${o}[$arrCount]})
							name=$(eval echo \${nam${o}[$arrCount]})

							dataFrame+="$m ${bsGroup[$n]} $name ${workDir}/${m}/'${scan}[${brik}]' "
						done
					fi
				done
			done

		else
			bsVars=1
			header="Subj WSVARS InputFile"

			for y in ${!arr[@]}; do

				gltCount=$[$gltCount+1]
				ws1h=${arr[$y]:0:1}
				ws2h=${arr[$y]:1:1}

				eval declare -a nam1=(nam${ws1h})
				eval declare -a nam2=(nam${ws2h})
				name1=$(eval echo \${${nam1}[$arrCount]})
				name2=$(eval echo \${${nam2}[$arrCount]})

				conVar+="-gltLabel $gltCount ${name1}-${name2} -gltCode $gltCount 'WSVARS: 1*$name1 -1*$name2' "
			done

			for m in ${subjList[@]}; do
				for n in ${wsList[@]}; do

					brik=$(eval echo \${arr${n}[$arrCount]})
					name=$(eval echo \${nam${n}[$arrCount]})

					dataFrame+="$m $name ${workDir}/${m}/'${scan}[${brik}]' "
				done
			done
		fi


		# write script
		echo "module load r/3.6

			3dMVM -prefix $outPre \\
			-jobs 10 \\
			-mask $mask \\
			-bsVars $bsVars \\
			-wsVars 'WSVARS' \\
			-num_glt $gltCount \\
			$conVar \\
			-dataTable \\
			$header \\
			$dataFrame" > ${outDir}/${outPre}.sh


		# run MVM
		if [ $runIt == 1 ]; then
			if [ ! -f ${outPre}+tlrc.HEAD ]; then
				source ${outDir}/${outPre}.sh
			fi

			# Check
			if [ ! -f ${outPre}+tlrc.HEAD ]; then
				echo >&2
				echo "MVM failed on $outPre. Exiting. Exit 8" >&2
				echo >&2
				exit 8
			fi
		fi

		let arrCount=$[$arrCount+1]
	done
fi
