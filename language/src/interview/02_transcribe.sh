#!/bin/bash
# the qsub command
# qsub -cwd -q JM,UI,CCOM -pe smp 56 -N transcribe_2E_094 -o logs/transcribe_2E_094.log -j y -ckpt user src/interview/02_transcribe.sh
conda activate whisper

mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_094;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_094/phenotype/interview/split/SUBJECT_audio5257_interviewee.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_094
