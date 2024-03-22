#!/bin/bash
# the qsub command
# qsub -cwd -q JM-GPU -pe smp 56 -N ISS_transcribe-94 -o logs/ISS_transcribe-94.log -j y -ckpt user src/ISS/02_transcribe.sh
conda activate whisper

whisper_timestamped data/derivatives/ISS_participants-response/2E_076/full-ISS-cropped_2E_076.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_079/full-ISS-cropped_2E_079.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_080/full-ISS-cropped_2E_080.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_081/full-ISS-cropped_2E_081.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_082/full-ISS-cropped_2E_082.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_083/full-ISS-cropped_2E_083.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_084/full-ISS-cropped_2E_084.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_085/full-ISS-cropped_2E_085.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_086/full-ISS-cropped_2E_086.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_087/full-ISS-cropped_2E_087.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_088/full-ISS-cropped_2E_088.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_089/full-ISS-cropped_2E_089.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_090/full-ISS-cropped_2E_090.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_091/full-ISS-cropped_2E_091.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/

whisper_timestamped data/derivatives/ISS_participants-response/2E_094/full-ISS-cropped_2E_094.wav --model large-v3 --language en --accurate --punctuations_with_words False --verbose True --detect_disfluencies True --output_format tsv --threads 1 --output_dir /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/
