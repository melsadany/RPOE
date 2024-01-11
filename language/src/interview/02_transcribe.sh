#!/bin/bash
conda activate whisper

mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_060;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_060/phenotype/interview/split/SUBJECT_audio5401_2IntervieweeAudio.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_060


mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_066;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_066/phenotype/interview/split/SUBJECT_audio513411415197527.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_066



