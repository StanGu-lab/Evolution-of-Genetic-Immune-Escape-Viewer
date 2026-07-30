# Convert VCF to MAF files
## ----------------------------------------------------------
## 1. Annotate the mutations by VEP
## ----------------------------------------------------------
```sh
#BSUB -W 24:00
#BSUB -q medium
#BSUB –cwd ${path_rsrch}PCAWG/consensus_snv_indel/
#BSUB –u wchen20@mdanderson.org
#BSUB -n 24
#BSUB -M 256
#BSUB -R rusage[mem=256]
#BSUB -P MHC_Evolution
#BSUB -J vcf2maf
#BSUB -o ${path_rsrch}PCAWG/consensus_snv_indel/

#module load singularity/3.7.0
#module load htslib

base_path="${path_rsrch}/PCAWG/consensus_snv_indel/vcf/"
base_inpath="/mnt/scratch/hema_bio-Malignan/wchen20/PCAWG/consensus_snv_indel/vcf/"

projects=("icgc" "tcga")

#for proj in "${projects[@]}"; do
#
#   source_dir="${base_path}/${proj}_filtered/snv_mnv/"
#   target_dir="${base_path}/${proj}_filtered/annoted/"
#
#   # Create the target directory if it doesn't exist
#   mkdir -p "$target_dir"
#   
#   # Loop through all .vcf.gz files in the source directory
#   for file in "$source_dir"/*.vcf.gz; do
#
#    filename=$(basename "$file" .gz)
#    
#    gunzip -c "$file" > "$target_dir/$filename"
#    tabix -p vcf "$target_dir/$filename"
#    done
#done


for proj in "${projects[@]}"; do

  directory="${base_path}/${proj}_filtered/annoted"
  tmp_path="${base_path}/${proj}_filtered/tmp"
  maf_path="${base_inpath}/${proj}_filtered/annoted"
  
  rm ${tmp_path}/*

  for vcf_file in ${vcf_path}/${directory}/*.vcf; do

    vcf_filename=$(basename "$vcf_file")
    maf_filename="${vcf_filename%.vcf}.maf"

    directory_in="${base_inpath}/${proj}_filtered/annoted"
    tmp_inpath="${base_inpath}/${proj}_filtered/tmp"

    vcf_infile="${directory_in}/${vcf_filename}"
    maf_infile="${directory_in}/${maf_filename}"

    singularity exec -C -B /rsrch6/:/mnt -B /home/wchen20/:/data ${path_home}/vcf2maf/vcf2maf.sif \
    perl /data/vcf2maf/vcf2maf-1.6.21/vcf2maf.pl --input-vcf ${vcf_infile} \
                                     --output-maf ${maf_infile} \
                                     --ref-fasta /data/.vep/homo_sapiens/102_GRCh37/Homo_sapiens.GRCh37.dna.toplevel.fa.gz \
                                     --vep-forks 10 \
                                     --vep-data /opt/vep/.vep \
                                     --vep-path /opt/vep/src/ensembl-vep \
                                     --species homo_sapiens \
                                     --ncbi-build GRCh37 \
                                     --retain-info t_R_count,t_A_count,1000genomes_AF,1000genomes_ID,Callers,NumCallers,VAF,cosmic,dbsnp,repeat_masker,dbsnp_somatic,signature_R1,signature_R2,signature_N3,snv_near_indel,Variant_Classification \
                                     --vep-overwrite \
                                     --tmp-dir ${tmp_inpath} &

    # Control the number of background jobs
    while (( $(jobs -r | wc -l) >= 50 )); do
      sleep 1;
    done
  done
done

# Wait for all background jobs to finish before exiting the script
wait
```

## ----------------------------------------------------------
## 2. Functional impact
## ----------------------------------------------------------
```sh
base_path="${path_rsrch}PCAWG/consensus_snv_indel/vcf/"
proj="tcga"
directory="${base_path}/${proj}_filtered/annoted"

module load R/4.1.0

R
```

```R
source("${path_home}/code/source/GRITIC_summary.R")

path_am <- "${path_rsrch}PCAWG/consensus_snv_indel/vcf/AlphaMissense_filtered/"
path_saveam <- "${path_rsrch}PCAWG/consensus_snv_indel/vcf/AlphaMissense_filtered/non_syn/"
dir.create(path_saveam)

## VEP
for (proj in c("icgc", "tcga")) {

  path_vep <- paste0("${path_rsrch}PCAWG/consensus_snv_indel/vcf/", proj, "_filtered/annoted/")
  list_vep <- list.files(path_vep, pattern = "*maf", full.names = TRUE, recursive = TRUE)

  for (file_maf in list_vep) {

  file_name = basename(file_maf)
  save_name = sub(".maf", "_vep.csv", file_name)

  aliquot_id = sub(".consensus.20160830.filtered.somatic.snv_mnv.maf", "", file_name)
  aliquot_id = sub(".consensus.20161006.filtered.somatic.indel.maf", "", aliquot_id)

  maf_nonsyn <- fread(file_maf, skip = "Hugo_Symbol") %>% filter(Variant_Classification %in% non_syn) %>% 
              mutate(Tumor_Sample_Barcode = aliquot_id)

  write.csv(maf_nonsyn, file = paste0(path_saveam, save_name), row.names = F)
  
  }
}
```

## ----------------------------------------------------------
## 3. Oncogenic impact by OncoKB
## ----------------------------------------------------------
```sh
module load python/3.10.5-gdc

#!/usr/bin/env bash
IMAF="data/example_maf.txt"
OMAF="test/example_maf.oncokb.txt"

IMAF="data/example_maf.txt"
OMAF="test/example_maf.oncokb.txt"
TOKEN="" #OncoKB API Token

python3.11 MafAnnotator.py -i "$IMAF" -o "$OMAF" -b "$TOKEN" 

module load python/3.10.5-gdc

cd ${path_home}/oncokb-annotator

TOKEN="" #OncoKB API Token

IMAF="/Users/wchen20/Desktop/PCAWG/consensus_snv_indel/non_syn/mutation_impact/fd163b56-1c2d-4e63-9fad-ba3221ae274b.consensus.20160830.filtered.somatic.snv_mnv_am.maf"
OMAF="test/ffe4bb51-e98a-41a7-a4e1-c3970386889c.oncokb.txt"
python3.11 MafAnnotator.py -i "$IMAF" -o "$OMAF" -b "$TOKEN"

TOKEN="" #OncoKB API Token
INPUT_DIR="/Users/wchen20/Desktop/PCAWG/consensus_snv_indel/non_syn/mutation_impact/oncoKB/input"
OUTPUT_DIR="/Users/wchen20/Desktop/PCAWG/consensus_snv_indel/non_syn/mutation_impact/oncoKB/annot"

cd /Users/wchen20/Desktop/software/oncokb-annotator

for IMAF in "$INPUT_DIR"/*mnv_nonsyn.maf; do
    BASENAME=$(basename "$IMAF" .maf)
    OMAF="$OUTPUT_DIR/${BASENAME}.oncokb.txt"
    LOG="$OUTPUT_DIR/${BASENAME}.log"

    # Skip if output already exists
    if [ -f "$OMAF" ]; then
        echo "Skipping $BASENAME (output exists)"
        continue
    fi

    #echo "Processing $BASENAME"
    python3.11 MafAnnotator.py -i "$IMAF" -o "$OMAF" -b "$TOKEN" > "$LOG" 2>&1
done
```
