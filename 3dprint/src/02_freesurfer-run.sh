#!/bin/bash
# the qsub command 
# participant_id='2E_032';SUBJECTS_DIR=/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/3dprint/data/derivatives/freesurfer;mkdir -p ${SUBJECTS_DIR};qsub -cwd -q JM,UI,CCOM -pe smp 14 -N freesurfer_${participant_id} -o logs/freesurfer_${participant_id}.log -j y -ckpt user src/02_freesurfer-run.sh ${participant_id} ${SUBJECTS_DIR}

participant_id=$1
SUBJECTS_DIR=$2

conda activate ENA
module load FSL
module load FreeSurfer

mkdir -p $SUBJECTS_DIR/${participant_id}/mri/orig
# run freesurfer
mri_convert ${SUBJECTS_DIR}/../mri_processed/sub-${participant_id}.nii.gz $SUBJECTS_DIR/${participant_id}/mri/orig/001.mgz
#recon-all -subjid ${participant_id} -all -time -log logfile -sd $SUBJECTS_DIR -parallel

recon-all -subjid ${participant_id} -autorecon1 -time -log logfile -sd $SUBJECTS_DIR -parallel -skullstrip -smooth1 -smooth2 -gcut -norandomness -subcortseg -clean-lta -autorecon2 -autorecon3 -no-isrunning




