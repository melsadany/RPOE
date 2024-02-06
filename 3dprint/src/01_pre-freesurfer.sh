#!/bin/bash
# the qsub command 
# participant_id='2E_032';SUBJECTS_DIR=/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/3dprint/data/derivatives/pre-freesurfer;mkdir -p ${SUBJECTS_DIR}/${participant_id};qsub -cwd -q JM,UI,CCOM -pe smp 14 -N pre-freesurfer_${participant_id} -o logs/pre-freesurfer_${participant_id}.log -j y -ckpt user src/01_pre-freesurfer.sh ${participant_id} ${SUBJECTS_DIR}

participant_id=$1
SUBJECTS_DIR=$2

conda activate ENA
# module load FSL
# module load FreeSurfer

scriptDir=/Shared/pinc/sharedopt/apps

export inputScan=${SUBJECTS_DIR}/../../raw/anat/sub-${participant_id}_MPRAGE_T1.nii.gz
derivatives=${SUBJECTS_DIR}/${participant_id}

# setting static variables
template=${SUBJECTS_DIR}/../../temp/MNI152_T1_0.5mm.nii.gz
templateMask=${SUBJECTS_DIR}/../../temp/MNI152_T1_0.5mm_brain_mask.nii.gz

# order of processing
# 1. reoriting to RPI orientation
# 2. Denoise
# 3. rigid alignment
# 4.
# 3. intensity non-uniformaty correction using N4


outputFolder=${derivatives}/${participant_id}

outputNameBase=${outputFolder}/${participant_id}
if [ ! -d $outputFolder ]; then
  mkdir $outputFolder
elif [ -f ${outputNameBase}_reorient_RPI_denoise_coreg_bfcorr.nii.gz ]; then
  echo "Preprocessing has already been run"
  exit 0
fi

# Step 1: reorient to RPI
${scriptDir}/afni/Linux/x86_64/22.3.07/3dresample -orient rpi -overwrite -prefix ${outputNameBase}_reorient_RPI.nii.gz -input $inputScan

# Step 2: denoising
# not including mask in denoising since it hasn't been generated yet
${scriptDir}/ants/Linux/x86_64/2.4.2/install/bin/DenoiseImage -d 3 -n Rician -s 1 -p 1 -r 2 -v 1 -i ${outputNameBase}_reorient_RPI.nii.gz -o [${outputNameBase}_reorient_RPI_denoise.nii.gz,${outputNameBase}_reorient_RPI_noise.nii.gz]

# Step 3: co-reg
# run antsRegistration on data
${scriptDir}/ants/Linux/x86_64/2.4.2/install/bin/antsRegistration --dimensionality 3 --output ${outputNameBase}_coreg --initial-moving-transform [${template},${outputNameBase}_reorient_RPI_denoise.nii.gz,1] \
  --transform Rigid[0.1] --metric Mattes[${template},${outputNameBase}_reorient_RPI_denoise.nii.gz,1,32,Regular,0.25] --convergence [2000x2000x2000x2000x2000,1e-6,10] --smoothing-sigmas 4x3x2x1x0vox --shrink-factors 8x8x4x2x1 \
  --transform Affine[0.1] --metric Mattes[${template},${outputNameBase}_reorient_RPI_denoise.nii.gz,1,32,Regular,0.25] --convergence [1000x1000x1000x1000x1000,1e-6,10] --smoothing-sigmas 4x3x2x1x0vox --shrink-factors 8x8x4x2x1 \
  --use-histogram-matching 1 --verbose 1 --random-seed 13983981 --winsorize-image-intensities [0.005,0.995] --write-composite-transform 1

# add back in for nonlinear if you want that
#--transform Syn[0.1,3,0] --metric CC[${template},${outputNameBase}_reorient_RPI_denoise.nii.gz,1,4,Regular,0.25] --convergence [1000x1000x1000x1000x1000,1e-6,10] --smoothing-sigmas 4x3x2x1x0vox --shrink-factors 8x8x4x2x1 \


${scriptDir}/ants/Linux/x86_64/2.4.2/install/bin/antsApplyTransforms -d 3 -n BSpline[3] -i ${outputNameBase}_reorient_RPI_denoise.nii.gz -o ${outputNameBase}_reorient_RPI_denoise_coreg.nii.gz -t ${outputNameBase}_coregComposite.h5 -r $template

# Step 4 skullstripping and masking
# 3dSkullStrip -input ${outputNameBase}_reorient_RPI_denoise_coreg.nii.gz -prefix ${outputNameBase}_reorient_RPI_denoise_coreg_mask_AFNI.nii.gz
# fslmaths ${outputNameBase}_reorient_RPI_denoise_coreg_mask_AFNI.nii.gz -bin ${outputNameBase}_reorient_RPI_denoise_coreg_mask_AFNI.nii.gz
# fslmaths ${outputNameBase}_reorient_RPI_denoise_coreg.nii.gz -subsamp2 ${outputNameBase}_reorient_RPI_denoise_coreg_subsamp2.nii.gz
# 3dAutomask -prefix ${outputNameBase}_reorient_RPI_denoise_coreg_mask_AUTO.nii.gz -clfrac 0.1 -q ${outputNameBase}_reorient_RPI_denoise_coreg.nii.gz
# CopyImageHeaderInformation ${IMAGE[0]} ${outputNameBase}_reorient_RPI_denoise_coreg_mask_AUTO.nii.gz $${outputNameBase}_reorient_RPI_denoise_coreg_mask_AUTO.nii.gz 1 1 1


# Step 5: N4 intensity correction
# general N4 command used
${scriptDir}/ants/Linux/x86_64/2.4.2/install/bin/N4BiasFieldCorrection -d 3 -i ${outputNameBase}_reorient_RPI_denoise_coreg.nii.gz -x ${templateMask} -r 1 -s 4 -c [50x50x50x50,0.0] -b [200,3] -t [0.15,0.01,200] -o [${outputNameBase}_reorient_RPI_denoise_coreg_bfcorr.nii.gz,${outputNameBase}_reorient_RPI_denoise_coreg_bf.nii.gz]

cp ${outputNameBase}_reorient_RPI_denoise_coreg_bfcorr.nii.gz ${SUBJECTS_DIR}/../mri_processed/sub-${participant_id}.nii.gz
echo 'ALL Done'
