#!/bin/bash



rawDir=/Volumes/Yorick/Temporal/bidsFormat
dicomDir=/Volumes/Yorick/MriRawData
session=Temporal


cd $rawDir
echo -e "participant_id\tsex\tage" > participants.tsv

for i in s*; do
	age=`dicom_hdr ${dicomDir}/${i}/ses-${session}/dicom/t1*/IM*001.dcm | grep "0010 1010" | sed 's/.*\///' | sed 's/[^0-9]//'`
	sex=`dicom_hdr ${dicomDir}/${i}/ses-${session}/dicom/t1*/IM*001.dcm | grep "0010 0040" | sed 's/.*\///'`
	echo -e "${i}\t${age:1:2}\t${sex}" >> participants.tsv
done