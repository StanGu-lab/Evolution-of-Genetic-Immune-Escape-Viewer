# Download ICGC and TCGA BAM files
# ----------------------------------------------------------
## 1. Downloading ICGC
# ----------------------------------------------------------
```sh
#BSUB -W 240:00
#BSUB -q transfer
#BSUB -o ../data/ICGC-PCAWG/pcawg-icgc/test/icgc_small_1_%J.log
#BSUB -e ../data/ICGC-PCAWG/pcawg-icgc/test/icgc_error_small_1_%J.log
#BSUB –cwd ../data//ICGC-PCAWG/pcawg-icgc/test/
#BSUB –u xxx@mdanderson.org
#BSUB -n 1
#BSUB -M 5
#BSUB -R rusage[mem=5]
#BSUB -J ICGC_download_small_1
#BSUB -P ICGC_download_small_1

wait_for_free_samples() {
  while (( $(jobs -r | grep -c "Downloading sample") >= 50 )); do
    sleep 6000
  done
}

# Ensure we use process substitution for the while loop
eval "$(/risapps/rhel8/miniforge3/24.5.0-0/bin/conda shell.bash hook)"
conda activate pyega3-5.0.2
cd ../data//ICGC-PCAWG/pcawg-icgc/test/

download_list="../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_downloadlist_late1.txt"
path_icgc="../data//ICGC-PCAWG/pcawg-icgc/"

# Ensure we use process substitution for the while loop
tail -n +2 "$download_list" | tr -d '\r' | while IFS=$'\t' read -r project_id project file_accession_id file_id file_name || [[ -n "$file_name" ]]; do
  (
  project=$(echo "$project" | tr -d '\r' | xargs)
  file_accession_id=$(echo "$file_accession_id" | tr -d '\r' | xargs)
  file_name=$(echo "$file_name" | tr -d '\r' | xargs)
  
  # Define the paths
  main_path="${path_icgc}/${project}/${file_accession_id}/${file_name}"
  test_path="${path_icgc}/test/${project}/${file_accession_id}/${file_name}"
  tmp_download_file="${path_icgc}/test/${project}/${file_accession_id}/.tmp_download"
  
  # Skip the sample if the file already exists or if .tmp_download is not empty
  if [[ -e "$main_path" || -e "$test_path" ]]; then
    echo "${file_name} already exists, skipping."
  elif compgen -G "${tmp_download_file}/*.tmp" > /dev/null; then
    echo "Downloading sample ${file_name}"
    rm ${tmp_download_file}/*.tmp
    pyega3 -cf /rsrch6/home/hema_bio-Malignan/wchen20/pyega3/default_credential_file.json -c 15 fetch "${file_accession_id}" --max-retries 100 --retry-wait 60 --output-dir "${path_icgc}/test/${project}/"
  elif [[ ! -e "$main_path" && ! -e "$test_path" ]]; then
    echo "Downloading sample ${file_name}"
    pyega3 -cf /rsrch6/home/hema_bio-Malignan/wchen20/pyega3/default_credential_file.json -c 15 fetch "${file_accession_id}" --max-retries 100 --retry-wait 60 --output-dir "${path_icgc}/test/${project}/"
  fi

  ) &

  wait_for_free_samples
  
done
wait

# Download by different lists
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small1.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small2.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small5.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small6.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small3.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small4.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small7.lsf
bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_download_small8.lsf

## Keep submiting the jobs
#BSUB -W 240:00
#BSUB -q long
#BSUB -o ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_submit_output_%J.log
#BSUB -e ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_submit_error_%J.log
#BSUB -cwd ../data//ICGC-PCAWG/pcawg-icgc/test/
#BSUB -u xxx@mdanderson.org
#BSUB -n 1
#BSUB -M 5
#BSUB -R rusage[mem=5]
#BSUB -J ICGC_submit
#BSUB -P ICGC_submit

#!/bin/bash

# Directory containing the .lsf scripts
script_dir="../data//ICGC-PCAWG/pcawg-icgc/test"

# Get all currently running or pending job suffixes (e.g., small3)
running_jobs=$(bjobs -u $USER | awk '$3=="RUN" || $3=="PEND"' | awk '{print $7}' | grep 'd_small' | sed 's/\*d_small_//')

# Loop through job indices 1 to 8
for i in {1..8}; do
  if echo "$running_jobs" | grep -q "^$i$"; then
    echo "Job small$i is already RUNNING or PENDING. Skipping."
  else
    echo "Submitting ICGC_download_small$i.lsf"
    bsub < "$script_dir/ICGC_download_small$i.lsf"
  fi
done
```

# ----------------------------------------------------------
## 2. Move all the files to the upper folders
# ----------------------------------------------------------
```sh
find ../data//ICGC-PCAWG/pcawg-icgc/test/ -type f \( -name "*.bam" -o -name "*.bam.md5" -o -name "*.bam.bai" \) | while read file; do
    new_path="${file/test\//}"  # This removes "test/" from the path

    mv "$file" "$new_path"
    
    echo "Moved: $file -> $new_path"
done
```

# ----------------------------------------------------------
## 3. Making bai files
# ----------------------------------------------------------
```sh
#BSUB -W 24:00
#BSUB -q medium
#BSUB -o ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_makebai_output.log
#BSUB -e ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_makebai_error.log
#BSUB –cwd ../data//ICGC-PCAWG/pcawg-icgc/test/
#BSUB –u xxx@mdanderson.org
#BSUB -n 20
#BSUB -M 500
#BSUB -R rusage[mem=500]
#BSUB -J ICGC_makebai
#BSUB -P ICGC_makebai

eval "$(/risapps/rhel8/miniforge3/24.5.0-0/bin/conda shell.bash hook)"
conda activate samtools-1.16.1

wait_for_free_slot() {
  while (( $(jobs -p | wc -l) >= 40 )); do
    echo "Waiting for free slot..."
    sleep 1
  done
}

find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam" | while read bam; do
    (
    bai_file="${bam}.bai"  
    bai_alt="${bam%.bam}.bai" 

    if [[ -e "$bai_file" || -e "$bai_alt" ]]; then
        echo "Skipping $bam (index already exists)"
    else
        echo "Indexing $bam..."
        samtools index -@ 4 "$bam"
    fi
    ) &  wait_for_free_slot
done

bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/makebai.lsf
```

# ----------------------------------------------------------
## 4. Making MD5 sum files
# ----------------------------------------------------------
```sh
#BSUB -W 240:00
#BSUB -q long
#BSUB -o ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_make_md5sum_output_%J.log
#BSUB -e ../data//ICGC-PCAWG/pcawg-icgc/test/ICGC_make_md5sum_error_%J.log
#BSUB –cwd ../data//ICGC-PCAWG/pcawg-icgc/test/
#BSUB –u xxx@mdanderson.org
#BSUB -n 20
#BSUB -M 500
#BSUB -R rusage[mem=500]
#BSUB -J ICGC_make_md5sum
#BSUB -P ICGC_make_md5sum

find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam" | \
  parallel -j 50 "md5sum {}" > ../data//ICGC-PCAWG/pcawg-icgc/test/bam_md5sums.txt

bsub < ../data//ICGC-PCAWG/pcawg-icgc/test/make_md5sums.lsf
```

# ----------------------------------------------------------
## 5. Trackining downloading progress
# ----------------------------------------------------------
```sh
## Check new files by time
find ../data//ICGC-PCAWG/pcawg-icgc/ -type f -mmin -30| wc -l
find ../data//ICGC-PCAWG/pcawg-icgc/test/ -type f -mmin -30| wc -l
find ../data//ICGC-PCAWG/pcawg-icgc/ -type f -mmin -60 -exec du -ch {} + | grep total
find ../data//ICGC-PCAWG/pcawg-icgc/test -type f -mmin -60 -exec du -ch {} + | grep total

find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam" -mtime -1|wc -l
find ../data//ICGC-PCAWG/pcawg-icgc/test -type f -name "*.bam" -mtime -1|wc -l

find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam"|wc -l
find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam.md5"|wc -l
find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam.bai"|wc -l

find ../data//ICGC-PCAWG/pcawg-icgc/test -type f -name "*.bam"|wc -l
find ../data//ICGC-PCAWG/pcawg-icgc/test -type f -name "*.bam.md5"|wc -l
find ../data//ICGC-PCAWG/pcawg-icgc/test -type f -name "*.bai"|wc -l

## Check total numbers
find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam" > ../data//ICGC-PCAWG/pcawg-icgc/test/bam_number.txt
find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam.md5" > ../data//ICGC-PCAWG/pcawg-icgc/test/md5_number.txt
find ../data//ICGC-PCAWG/pcawg-icgc -type f -name "*.bam.bai" > ../data//ICGC-PCAWG/pcawg-icgc/test/bai_number.txt
```

# ----------------------------------------------------------
## 6. TCGA download
# ----------------------------------------------------------
```sh
#BSUB -W 240:00
#BSUB -q transfer
#BSUB -o ../data//ICGC-PCAWG/pcawg-tcga/BAM/tcga_missing_%J.log
#BSUB -e ../data//ICGC-PCAWG/pcawg-tcga/BAM/tcga_missing_%J.log
#BSUB –cwd ../data//ICGC-PCAWG/pcawg-tcga/BAM/
#BSUB –u xxx@mdanderson.org
#BSUB -n 1
#BSUB -M 5
#BSUB -R rusage[mem=5]
#BSUB -J TCGA_download
#BSUB -P TCGA_download

eval "$(/risapps/rhel8/miniforge3/24.5.0-0/bin/conda shell.bash hook)"
conda activate gdc-client-2.3

path_gdcclient="/rsrch6/home/hema_bio-Malignan/wchen20/gdc-client/"
path_download="../data//ICGC-PCAWG/pcawg-tcga/BAM"

cd ${path_download}

gdc-client download -t ${path_gdcclient}/gdc-user-token.2025-06-25T14_15_44.953Z.txt \
                    -m ${path_gdcclient}/gdc_manifest.2025-06-25.154648.txt \
                    -d ${path_download}

set -e

# Iterate over types
cd ../data//ICGC-PCAWG/pcawg-tcga/BAM
bam_path="../data//ICGC-PCAWG/pcawg-tcga/BAM"
bam_file="64eaa1a9-4f18-4d9f-8b75-34760d4798d9_wgs_gdc_realn.bam"
find $bam_path -type f -name ${bam_file} -print -quit


module load R/4.1.0

R
```

```r
## Issues were noted on some samples
library(dplyr)
library(data.table)

file_path = "../data//ICGC-PCAWG/pcawg-tcga/BAM/"
file_list = list.files(file_path, pattern = "annotations.txt", full.names = TRUE, recursive = TRUE)

fill_merged <- lapply(file_list, fread) %>% rbindlist()
write.csv(fill_merged, "../data//ICGC-PCAWG/pcawg-tcga/BAM/annotation_all.csv", row.names = F)
```
