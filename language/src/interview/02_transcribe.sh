#!/bin/bash
# the qsub command
# qsub -cwd -q JM,UI,CCOM -pe smp 56 -N transcribe_2E_075-083 -o logs/transcribe_2E_075-083.log -j y -ckpt user src/interview/02_transcribe.sh
conda activate whisper

mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_080;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_080/phenotype/interview/split/SUBJECT_audio5165_interviewee_audio.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_080


mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_081;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_081/phenotype/interview/split/SUBJECT_audio5244_interviewee_audio.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_081

mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_082;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_082/phenotype/interview/split/SUBJECT_audio4910_interviewee_audio.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_082

mkdir -p /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_083;whisper_timestamped /Dedicated/jmichaelson-sdata/MRI/RPOE/2E_083/phenotype/interview/split/SUBJECT_audio4841_interviewee.m4a --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 30 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/interview_transcription/2E_083
