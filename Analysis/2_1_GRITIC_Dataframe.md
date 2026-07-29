# GRITIC
## ----------------------------------------------------------
## 1. Convert GRITIC results (Toby's) to txt
## ----------------------------------------------------------
### Convert pkl files to data frame
```sh
#BSUB -W 24:00
#BSUB -q medium
#BSUB –cwd ${path_rsrch}/PCAWG/MHC_evolution/GRITIC
#BSUB –u wchen20@mdanderson.org
#BSUB -n 12
#BSUB -M 64
#BSUB -R rusage[mem=64]
#BSUB -P complex_timing_tcga
#BSUB -J complex_timing_tcga
#BSUB -o ${path_rsrch}/PCAWG/MHC_evolution/GRITIC

module load python/3.11.3
python ${path_home}/code/source/convert_to_csv20240327_tcga.py
python ${path_home}/code/source/convert_to_csv20240327_icgc.py
```

## ----------------------------------------------------------
## 2. RUN GRITIC-SNV
## ----------------------------------------------------------
### Prepare input for GRITIC
```sh
module load R/3.5.0

R
```

```r

rm(list = ls())

library("data.table")
library("dplyr")
library("VariantAnnotation")
library("MutationTimeR")
library("maftools")
library("data.table")
library("rtracklayer")
library("GenomicRanges")
library("SummarizedExperiment")
library("readr")
library("tibble")
library("vcfR")

#tumour_id = "d30d48a0a724507b40b2f5f0d2953c78"
#dataset = "icgc"
#maf_file = paste0(PCAWG_path, "/consensus_snv_indel/final_consensus_snv_indel_passonly_", dataset, "_annoted/annoted/d30d48a0a724507b40b2f5f0d2953c78.consensus.20160830.somatic.snv_mnv.maf")

PCAWG_path <- "${path_rsrch}/PCAWG"
path_griticinput <- paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/240820/")

purity <- fread(paste0(PCAWG_path, "/consensus_cnv/consensus.20170217.purity.ploidy.txt")) %>% 
  mutate(wgd = ifelse(wgd_status == "wgd", "T","F")) %>%
  left_join(., LOH_mut, by = "samplename") %>%
  dplyr::select(samplename, purity, wgd, dcc_project_code)
write.table(purity, file = paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/purity.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

projects <- c("tcga", "icgc")

HLAmut_path <- "${path_rsrch}/PCAWG/MHC_evolution/HLAmutations/"
maf_HLAmut <- fread(paste0(HLAmut_path, "/results/maf_HLAmut.maf")) %>% mutate(Chromosome = sub("chr", "", Chromosome))
hla_sampleid <- unique(maf_HLAmut$Tumor_Sample_Barcode)

for (proj in projects) {
  #proj = "tcga"
  # SNV
  maf_gritic_path <- paste0("${path_rsrch}/PCAWG/consensus_snv_indel/vcf/", proj, "_filtered/annoted/")
  maf_files <- list.files(path = maf_gritic_path, pattern = ".consensus.20160830.filtered.somatic.snv_mnv.maf", recursive = FALSE, full.names = TRUE)
  out_path <- paste0("${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  
  for (maf_file in maf_files) {
    
    #tumour_id = "d4615ca0-b5c7-4a5c-8593-bd50034a78ae"
    #maf_file = paste0(maf_gritic_path, tumour_id, ".consensus.20160830.filtered.somatic.snv_mnv.maf")
    
    tumour_id <- sub(".consensus.20160830.filtered.somatic.snv_mnv.maf", "", basename(maf_file))
    
    maf_snvall <- fread(maf_file) %>%
      filter(!is.na(t_R_count), !is.na(t_A_count))
    
    maf_snv <- maf_snvall %>% 
      dplyr::select(Chromosome, Start_Position, t_R_count, t_A_count) %>%
      dplyr::rename(Position = Start_Position,
                    Tumor_Ref_Count = t_R_count,
                    Tumor_Alt_Count = t_A_count)
    write.table(maf_snv, file = paste0(path_griticinput, "/snv/snv_table_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
    
    ## Combine HLA
    nohla_maf_snv <- maf_snvall %>% 
      filter(!Hugo_Symbol %in% c("HLA-A", "HLA-B", "HLA-C")) %>% 
      dplyr::select(Chromosome, Start_Position, t_R_count, t_A_count) %>%
      dplyr::rename(Position = Start_Position,
                    Tumor_Ref_Count = t_R_count,
                    Tumor_Alt_Count = t_A_count) 
    
    snv_mafmerge <- maf_HLAmut %>% 
      filter(Tumor_Sample_Barcode == !!tumour_id, !Variant_Classification %in% c("In_Frame_Del", "In_Frame_Ins")) %>% 
      dplyr::select(Chromosome, Start_Position, t_ref_count, t_alt_count) %>%
      dplyr::rename(Position = Start_Position,
                    Tumor_Ref_Count = t_ref_count,
                    Tumor_Alt_Count = t_alt_count) %>%
      rbind(nohla_maf_snv, .)
    write.table(snv_mafmerge, file = paste0(path_griticinput, "/snv/hlamut/snv_table_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
    
    ## Combine Indel
    indel_file = paste0(maf_gritic_path, tumour_id, ".consensus.20161006.filtered.somatic.indel.maf")
    
    if (file.exists(indel_file)) {
      # Read the first line to check for a "#"
      first_line <- readLines(indel_file, n = 1)
      
      # Determine how to read the file
      if (startsWith(first_line, "#")) {
        # Skip the first line and read the rest of the file
        maf_indelall <- fread(indel_file, skip = 1) %>%
          filter(!is.na(t_R_count), !is.na(t_A_count))
      } else {
        # Read the file normally
        maf_indelall <- fread(indel_file) %>%
          filter(!is.na(t_R_count), !is.na(t_A_count))
      }
      
      maf_indel <- rbind(maf_snvall, maf_indelall) %>% 
        dplyr::select(Chromosome, Start_Position, t_R_count, t_A_count) %>%
        dplyr::rename(Position = Start_Position,
                      Tumor_Ref_Count = t_R_count,
                      Tumor_Alt_Count = t_A_count)
      write.table(maf_indel, file = paste0(path_griticinput, "/snv_indel/snv_table_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
      
      hla_maf <- maf_HLAmut %>% filter(Tumor_Sample_Barcode == !!tumour_id)  %>% 
        dplyr::select(Chromosome, Start_Position, t_ref_count, t_alt_count) %>%
        dplyr::rename(Position = Start_Position,
                      Tumor_Ref_Count = t_ref_count,
                      Tumor_Alt_Count = t_alt_count)
      
      snv_indel_mafmerge <- rbind(maf_snvall, maf_indelall) %>% 
        filter(!Hugo_Symbol %in% c("HLA-A", "HLA-B", "HLA-C")) %>% 
        dplyr::select(Chromosome, Start_Position, t_R_count, t_A_count) %>%
        dplyr::rename(Position = Start_Position,
                      Tumor_Ref_Count = t_R_count,
                      Tumor_Alt_Count = t_A_count) %>%
        rbind(., hla_maf)
      
      write.table(snv_indel_mafmerge, file = paste0(path_griticinput, "/snv_indel/hlamut/snv_table_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
    }
    # CN
    #cn_path <- paste0(PCAWG_path, "/consensus_cnv/consensus.20170119.somatic.cna.", dataset, ".public")
    #cn_file <- paste0(cn_path, "/", tumour_id, ".consensus.20170119.somatic.cna.txt")
    #cn <- fread(cn_file) %>% na.omit() %>%
    #      dplyr::select(chromosome, start, end, major_cn,	minor_cn) %>%
    #      dplyr::rename(Chromosome = chromosome,
    #             Segment_Start = start,
    #             Segment_End = end,
    #             Major_CN = major_cn,
    #             Minor_CN = minor_cn)
    
    #write.table(cn, file = paste0(path_griticinput, "/cn_table_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
    
    # Subclone
    #subclone_path <- paste0(PCAWG_path, "/subclonal_reconstruction/20170325_consensus_subclonal_reconstruction_beta1.", dataset)
    #subclone_file <- paste0(subclone_path, "/", tumour_id, "_subclonal_structure.txt.gz")
    
    #subclone <- fread(subclone_file) %>%
    #mutate(Subclone_Fraction = n_snvs/sum(n_snvs)) %>%
    #subset(fraction_cancer_cells < 1) %>%
    #      dplyr::select(fraction_cancer_cells, Subclone_Fraction) %>%
    #      dplyr::rename(Subclone_CCF = fraction_cancer_cells)
    
    #write.table(subclone, file =paste0(path_griticinput, "/subclone_table_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
  }
}

## Subclonal table
library("data.table")
library("dplyr")
library("VariantAnnotation")
library("MutationTimeR")
library("mg14")
library("maftools")
library("data.table")
library("rtracklayer")
library("GenomicRanges")
library("SummarizedExperiment")
library("readr")
library("tibble")
library("vcfR")

#tumour_id = "d30d48a0a724507b40b2f5f0d2953c78"
#dataset = "icgc"
#maf_file = paste0(PCAWG_path, "/consensus_snv_indel/final_consensus_snv_indel_passonly_", dataset, "_annoted/annoted/d30d48a0a724507b40b2f5f0d2953c78.consensus.20160830.somatic.snv_mnv.maf")

PCAWG_path <- "${path_rsrch}/PCAWG"
path_griticinput <- paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/240820/")

LOH_mut <- fread(paste0(PCAWG_path, "/donors_and_biospecimens/LOH_MSI.csv")) %>% mutate(samplename = aliquot_id) %>% 
  dplyr::select(samplename, dcc_project_code, histology_abbreviation)

purity <- fread(paste0(PCAWG_path, "/consensus_cnv/consensus.20170217.purity.ploidy.txt")) %>% 
  mutate(wgd = ifelse(wgd_status == "wgd", "T","F")) %>%
  left_join(., LOH_mut, by = "samplename") %>%
  dplyr::select(samplename, purity, wgd, dcc_project_code, histology_abbreviation) %>% inner_join(subclone_all, by = "samplename")
write.table(purity, file = paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/purity.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

projects <- c("tcga", "icgc")

HLAmut_path <- "${path_rsrch}/PCAWG/MHC_evolution/HLAmutations/"
maf_HLAmut <- fread(paste0(HLAmut_path, "/results/maf_HLAmut.maf")) %>% mutate(Chromosome = sub("chr", "", Chromosome))
hla_sampleid <- unique(maf_HLAmut$Tumor_Sample_Barcode)

subclone_all <- data.frame()

for (proj in projects) {
  
  subclone_path <- paste0(PCAWG_path, "/subclonal_reconstruction/20170325_consensus_subclonal_reconstruction_beta1.", proj)
  subclone_files <- list.files(path = subclone_path, pattern = "_subclonal_structure.txt.gz", recursive = FALSE, full.names = TRUE)
  out_path <- paste0("${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  
  for (subclone_file in subclone_files) {
    
    #tumour_id = "d4615ca0-b5c7-4a5c-8593-bd50034a78ae"
    tumour_id = sub("_subclonal_structure.txt.gz", "", basename(subclone_file))
    
    # Subclone
    subclone <- fread(subclone_file) %>%
    mutate(Subclone_Fraction = n_snvs/sum(n_snvs),
           Subclone_CCF = fraction_cancer_cells,
           Cluster = cluster) %>%
    #subset(fraction_cancer_cells < 1) %>%
    dplyr::select(Cluster, Subclone_CCF, Subclone_Fraction) 
    write.table(subclone, file =paste0(path_griticinput, "/subclone_table2_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
    
    subclone3 <- fread(subclone_file) %>%
      mutate(Subclone_Fraction = n_snvs/sum(n_snvs),
             Subclone_CCF = fraction_cancer_cells,
             Cluster = cluster) %>%
      subset(fraction_cancer_cells < 1) %>%
      dplyr::select(Cluster, Subclone_CCF, Subclone_Fraction) 
    write.table(subclone, file =paste0(path_griticinput, "/subclone_table3_", tumour_id, ".tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
    
    if (nrow(subclone3) > 0) {
      
      subclone_df <- data.frame("samplename" = tumour_id,
                                "clonal" = "false")
      
      } else if (nrow(subclone3) == 0) {
      
      subclone_df <- data.frame("samplename" = tumour_id,
                                "clonal" = "true")
      
    }
    
    subclone_all <- rbind(subclone_all, subclone_df)
  }
}

purity <- fread(paste0(PCAWG_path, "/consensus_cnv/consensus.20170217.purity.ploidy.txt")) %>% 
  mutate(wgd = ifelse(wgd_status == "wgd", "T","F")) %>%
  left_join(., LOH_mut, by = "samplename") %>%
  dplyr::select(samplename, purity, wgd, dcc_project_code, histology_abbreviation) %>% inner_join(subclone_all, by = "samplename")
write.table(purity, file = paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/purity.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

types <- unique(purity$histology_abbreviation)

for (type in types) {
  
  purity_sub <-  purity %>% 
    filter(histology_abbreviation == !!type) %>%
    dplyr::select(samplename, purity, wgd, histology_abbreviation, clonal)
  
  write.table(purity_sub, file = paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/purity_", type,".txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  
}
```

### Test for Update version
```sh
module load python/3.11.3
pip install pandas --user
pip install networkx --user
pip install matplotlib --user
pip install gritic --user
pip install numba --user

cd ${path_rsrch}/PCAWG/MHC_evolution/GRITIC
griticnew_path="${path_home}/gritic"

tumour_id="0009b464-b376-4fbc-8a56-da538269a02f"
samplename="0009b464-b376-4fbc-8a56-da538269a02f"
purity="0.6"
wgd="T"
type="BLCA-US"
dcc_project_code="BLCA-US"
complextiming_path="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/"
output="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/"

python ${griticnew_path}/rungritic_cmd.py -ARGS

python ${griticnew_path}/rungritic_cmd.py --mutation_table ${complextiming_path}/input/snv_table_${samplename}.tsv \
--subclone_table ${complextiming_path}/input/subclone_table_${samplename}.tsv \
--copy_number_table ${complextiming_path}/input/cn_table_${samplename}.tsv \
--purity ${purity} \
--wgd_status ${wgd} \
--output  ${complextiming_path}/output \
--sample_id ${samplename}


python ${griticnew_path}/rungritic_cmd.py --mutation_table ${complex_timing_path}/input/${dcc_project_code}/snv_table_${samplename}.tsv \
                      --subclone_table ${complex_timing_path}/input/${dcc_project_code}/subclone_table_${samplename}.tsv \
                      --copy_number_table ${complex_timing_path}/input/${dcc_project_code}/cn_table_${samplename}.tsv \
                      --purity ${purity} \
                      --wgd_status ${wgd} \
                      --output ${output} \
                      --sample_id ${samplename}
```

### Run GRITIC-SNV
### Update the brunch
```sh
## Local
git clone https://github.com/VanLoo-lab/gritic.git
cd gritic
git checkout wgd_timing
git fetch origin
git merge origin/wgd_timing
git pull origin wgd_timing

python3.11 -m pip install git+https://github.com/VanLoo-lab/gritic.git
```

### Test code
```sh
cd /Users/wchen20/Desktop/gritic_snv

gritic="/Users/wchen20/Desktop/GRITIC/gritic/"
gritic_snv="/Users/wchen20/Desktop/GRITIC/gritic_snv/"

python3.11 ${gritic}/rungritic_cmd.py --mutation_table ./test_data/test_mutation_table.tsv \
                        --copy_number_table ./test_data/test_cn_table.tsv \
                        --subclone_table ./test_data/test_subclone_table.tsv \
                        --sample_id test \
                        --purity 0.7 \
                        --output ./test_gritic

python3.11 ./run_snv_timing.py --sample_id test --input_dir ./test_gritic/ --output_dir ./test_snvs

## HPC
bsub -Is -q interactive -W 3:00 -M 16 -R rusage[mem=16] -n 1 /bin/bash

## lognode
module load python/3.10.5-gdc
#pip install emcee --user
#pip install gritic --user
#pip install pandas --user

#git clone https://github.com/VanLoo-lab/gritic.git
git clone https://github.com/VanLoo-lab/gritic.git
cd gritic
git checkout wgd_timing
git fetch origin
git merge origin/wgd_timing
git pull origin wgd_timing

module load python/3.10.5-gdc

cd ${path_home}/GRITIC/gritic_snv
gritic="${path_home}/GRITIC/gritic/"
gritic_snv="${path_home}/GRITIC/gritic_snv/"

python ${gritic}/rungritic_cmd.py --mutation_table ./test_data/test_mutation_table.tsv \
                        --copy_number_table ./test_data/test_cn_table.tsv \
                        --subclone_table ./test_data/test_subclone_table.tsv \
                        --sample_id test \
                        --purity 0.7 \
                        --output ./test_gritic

python ${gritic_snv}/run_snv_timing.py --sample_id test --input_dir ./test_gritic/ --output_dir ./test_snvs
```

### Loop for all samples
```sh
#BSUB -W 240:00
#BSUB -q e40long
#BSUB –cwd ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/
#BSUB –u wchen20@mdanderson.org
#BSUB -n 12
#BSUB -M 400
#BSUB -R rusage[mem=400]
#BSUB -P PCAWG
#BSUB -J GRITIC
#BSUB -o ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/GRITICsnv_output.log
#BSUB -e ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/GRITICsnv_error.log

## If error occur then quit the task
set -e

## Set task numbers
wait_for_free_slot() {
  while (( $(jobs -r | wc -l) >= 10)); do
    sleep 1
    echo "Waiting for free slot"
  done
}

module load python/3.10.5-gdc
gritic="${path_home}/GRITIC/250/gritic"
gritic_snv="${path_home}/GRITIC/250/gritic_snv"

path_gritic="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/"
path_in="${path_gritic}/input/"

datasets=("snv" "snv_indel")

types=("Bone-Osteosarc" "Bladder-TCC" "Bone-Benign" "Bone-Epith" "Breast-LobularCA" "Breast-DCIS" "Biliary-AdenoCA" "Cervix-SCC" "Cervix-AdenoCA" "Lymph-CLL" "Myeloid-MPN" "Myeloid-AML" "Myeloid-MDS" "ColoRect-AdenoCA" "Lymph-BNHL" "Prost-AdenoCA" "Eso-AdenoCA" "Stomach-AdenoCA" "CNS-GBM" "Head-SCC" "Kidney-ChRCC" "Kidney-RCC" "CNS-Oligo" "Liver-HCC" "Lung-AdenoCA" "Lung-SCC" "Ovary-AdenoCA" "Panc-AdenoCA" "Panc-Endocrine" "CNS-Medullo" "CNS-PiloAstro" "SoftTissue-Leiomyo" "SoftTissue-Liposarc" "Thy-AdenoCA" "Uterus-AdenoCA" "Skin-Melanoma" "Breast-AdenoCA")

types="ColoRect-AdenoCA"

for type in "${types[@]}"; do
  
  paired_list="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/input/purity_${type}.txt"

    tail -n +2 "$paired_list" | while IFS=$'\t' read -r samplename purity wgd histology_abbreviation clonal; do
      
      for dataset in "${datasets[@]}"; do
          (
          # Define the log file
          # samplename="05780d48-80e7-4d70-b00c-081f8a9519f2"
          # purity=0.48
          # dataset="snv"
          # type="ColoRect-AdenoCA"

          # Define the log file
          # samplename="3e012b50-06d1-4120-971b-5e54139b00ee"
          # purity=0.602
          # dataset="snv_indel"
          # type="Lymph-BNHL"
          
          samplename=$(echo "$samplename" | tr -d '\r' | xargs)
          purity=$(echo "$purity" | tr -d '\r' | xargs)
          wgd=$(echo "$wgd" | tr -d '\r' | xargs)
          histology_abbreviation=$(echo "$histology_abbreviation" | tr -d '\r' | xargs)
          clonal=$(echo "$clonal" | tr -d '\r' | xargs)
          clonal_lower=$(echo "$clonal" | tr '[:upper:]' '[:lower:]')

          # Define output paths
          output="${path_gritic}/output/GRITIC_snv/${dataset}/${type}/${samplename}"
          mkdir -p "${output}"
          output_snv="${output}/snv_timing"
          mkdir -p "${output_snv}"

          # Check if CSV files already exist in the output directory
          #if ls "${output}/snv_timing"/*.csv 1> /dev/null 2>&1; then
          #  echo "CSV files found for ${type}: ${samplename}. Skipping..."
          #  continue
          #fi

          log_file1="${output}/run_${samplename}_1.log"
          log_file2="${output}/run_${samplename}_2.log"
          log_file3="${output}/run_${samplename}_3.log" 

          echo "Run for ${type}/${samplename}: ${dataset} - ${clonal_lower}"
          
          # Check if clonal is "true" or "false" and run the respective command
          if [[ "$clonal_lower" == "yes" ]]; then
            # Run the first command and redirect output and error to the log file
            echo "clonal"
            python ${gritic}/rungritic_cmd.py \
            --mutation_table "${path_gritic}/input/240820/${dataset}/hlamut/snv_table_${samplename}.tsv" \
            --copy_number_table "${path_gritic}/input/240716/cn_table_${samplename}.tsv" \
            --sample_id "${samplename}" \
            --purity "${purity}" \
            --output "${output}" \
            >> "${log_file1}" 2>&1
    
          else
            echo "subclonal"
            # Run the non-clonal version of the command (includes subclone_table)
             python ${gritic}/rungritic_cmd.py \
             --mutation_table "${path_gritic}/input/240820/${dataset}/hlamut/snv_table_${samplename}.tsv" \
             --copy_number_table "${path_gritic}/input/240716/cn_table_${samplename}.tsv" \
             --subclone_table "${path_gritic}/input/240716/subclone_table_${samplename}_subclonal_structure.txt.gz.tsv" \
             --sample_id "${samplename}" \
             --purity "${purity}" \
             --output "${output}" \
            >> "${log_file1}" 2>&1
          fi
          
          # Run the second command and redirect output and error to the same log file
          #timeout 10m python ${gritic_snv}/run_snv_timing.py \
          #  --sample_id "${samplename}" \
          #  --input_dir "${output}" \
          #  --output_dir "${output_snv}" \
          # >> "${log_file2}" 2>&1

          # Run the third command and redirect output and error to the same log file
          #python ${gritic_snv}/output.py \
          #  "${samplename}" "${output_snv}/snv_timing_dicts" "${output_snv}" \
          #  >> "${log_file3}" 2>&1
        ) &

        wait_for_free_slot
     
       done

    done

done
```

## ----------------------------------------------------------
## 3. Update HLA results by Polysolver
## ----------------------------------------------------------
```sh
paired_list="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/input/250306/purity_icgc.txt"
path_GRITIC="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/"

tail -n +2 "$paired_list" | while IFS=$'\t' read -r samplename purity wgd histology_abbreviation snv_table cn_table clonal; do

    samplename=$(echo "$samplename" | tr -d '\r' | xargs)
    histology_abbreviation=$(echo "$histology_abbreviation" | tr -d '\r' | xargs)
    mv ${path_GRITIC}/input/250306/${samplename}_cn_table.tsv ${path_GRITIC}/input/250306/${histology_abbreviation}/${samplename}_cn_table.tsv
    mv ${path_GRITIC}/input/250306/${samplename}_subclonal.tsv ${path_GRITIC}/input/250306/${histology_abbreviation}/${samplename}_subclonal.tsv

done
```

```sh
#BSUB -W 24:00
#BSUB -q medium
#BSUB –cwd ${path_rsrch}/TCGA/MHC_evolution/
#BSUB –u wchen20@mdanderson.org
#BSUB -n 12
#BSUB -M 500
#BSUB -R rusage[mem=500]
#BSUB -P PCAWG
#BSUB -J PCAWG_GRITIC
#BSUB -o ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/pcawg_output_%J.log
#BSUB -e ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/pcawg_error_%J.log

path_code="${path_home}/code/source/"
path_input="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/input/250306/"
path_output="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/"
project="pcawg"
path_anno="${path_rsrch}/PCAWG/consensus_snv_indel/vcf_HLAupdated/${project}/sample_updatedHLA/"

projects=("pcwag")
job_num=5
timelimit=no
time=30

paired_list="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/input/250306/purity_${project}.txt"
bash ${path_code}/GRITIC_loop.sh ${path_input} ${path_output} ${path_anno} ${job_num} ${projects} ${timelimit} ${time} ${paired_list}

bsub < ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/griticHLA250613.lsf

## Copy to a folder
find ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/ \
  -type f -name "*mafnongritic.csv"|wc -l

find ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/ \
  -type f -name "*_mafgritic.csv" \
  -exec cp {} ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/summary250717 \;

scp -r wchen20@seadragon:${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/summary250717 /Users/wchen20/Desktop/PCAWG/MHC_evolution/GRITIC/output/250717/dict
```

## ----------------------------------------------------------
## 4. Combine all the mafgritic files
## ----------------------------------------------------------
```sh
module load R/4.1.0

R
```

### Combine GRITIC and MAF
```r
rm(list = ls())

library("data.table")
library("dplyr")
library("tidyr")

non_syn = c("Frame_Shift_Del", "Frame_Shift_Ins", "Splice_Site", "Translation_Start_Site", "Nonsense_Mutation", "Nonstop_Mutation", "In_Frame_Del", "In_Frame_Ins", "Missense_Mutation")
APM = c("HLA-A", "HLA-B", "HLA-C","B2M", "NLRC5", "TAP1", "TAP2" ,"TAPBP", "PSMB8", "PSMB9", "PSMB10", "ERAP1", "ERAP2")

PCAWG_path <- "${path_rsrch}/PCAWG"
maf_path <- paste0(PCAWG_path, "/MHC_evolution/snv_indel/annoted_all")
gritic_path <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/SNV_timingnew/"

projects <- c("tcga", "icgc")

for (proj in projects) {
  
  maf_gritic_path <- paste0("${path_rsrch}/PCAWG/consensus_snv_indel/final_consensus_12oct/", proj, "_filtered/annoted/")
  maf_files <- list.files(path = maf_gritic_path, pattern = ".consensus.20160830.filtered.somatic.snv_mnv.maf", recursive = TRUE, full.names = TRUE)
  out_path <- paste0("${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  
  for (maf_file in maf_files) {
    
    tryCatch({
      
      tumour_id <- sub(".consensus.20160830.filtered.somatic.snv_mnv.maf", "", basename(maf_file))
      print(paste0("Processing: ",  tumour_id))
      
      gritic_file <- paste0(gritic_path, tumour_id, "_griticsnv.txt")
      gritic_SNV <- fread(gritic_file) %>% mutate(Chromosome = CHROM,
                                                  Start_Position = POS)
      
      maf_gritic <- fread(maf_file) %>% 
        inner_join(., gritic_SNV, by = c("Chromosome", "Start_Position"))
      write.table(maf_gritic, paste0(out_path, "/", tumour_id, "_mafgritic.csv"), sep = ",", row.names = FALSE)
      
      maf_gritic_non <- maf_gritic %>% 
        filter(Variant_Classification %in% !!non_syn) %>% 
        mutate(event = paste0(Hugo_Symbol, "_", HGVSp_Short))
      write.table(maf_gritic_non, paste0(out_path, "/", tumour_id, "_mafnongritic.csv"), sep = ",", row.names = FALSE)
      
    }, error = function(e) {
      print(paste0("Error processing ", tumour_id, ": ", e$message))
    })
  }
}
```

### Combine all data frame
```R
rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(bayestestR)
library(matrixStats)

PCAWG_path <- "${path_rsrch}/PCAWG"
source("${path_home}/code/source/basic.R")
source("${path_home}/code/source/insert_newlines.R")
source("${path_home}/code/source/plot_segTiming.R")

LOH_MSI <- fread("${path_rsrch}/PCAWG/donors_and_biospecimens/LOH_MSI_final.csv")
column_names <- paste0("X", 0:249)
path_gritic <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/"
breaks_seq <- seq(0, 1.05, by = 0.05)

df_sample_HLA <- fread("${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/sample_HLA.csv") %>% 
pull(sample_id)
path_gritic_HLA <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/summary250717/"

for (type in unique(LOH_MSI$histology_abbreviation)) {
  
  sample_list <- LOH_MSI %>% 
           filter(histology_abbreviation == !!type & MSI_mut == "MSS") %>% 
           pull(aliquot_id)
  
  df_gritic <- list()
  
  for (sample_id in sample_list) {
  
  if (sample_id %in% df_sample_HLA) {
    print(paste0("Sample - ", sample_id, " has HLA mutations"))
    df_gritic[[sample_id]] <- fread(paste0(path_gritic_HLA, "/", sample_id, "_mafgritic.csv"))
    
  } else {
    proj <- LOH_MSI %>% filter(aliquot_id == !!sample_id) %>% pull(proj)
    fpath <- paste0(path_gritic, proj, "/", sample_id, "_mafgritic.csv")
    
    if (file.exists(fpath)) {
      df_gritic[[sample_id]] <- fread(fpath) %>%
        filter(!Hugo_Symbol %in% c("HLA-A", "HLA-B", "HLA-C"))
    } else {
      warning(paste0("File not found for sample: ", sample_id, " — skipping"))
      next 
    }
  }
  
  df_gritic[[sample_id]] <- df_gritic[[sample_id]] %>%
    rename_with(~ paste0("X", .), .cols = matches("^\\d+$")) %>%
    filter(!is.na(X1)) %>%
    mutate(
      Tumor_Sample_Barcode = sample_id,
      pathway               = Hugo_Symbol,
      early_ratio_05        = rowMeans(dplyr::select(., all_of(column_names)) < 0.5, na.rm = TRUE),
      late_ratio_05         = rowMeans(dplyr::select(., all_of(column_names)) > 0.5, na.rm = TRUE),
      subclonal_ratio       = rowMeans(dplyr::select(., all_of(column_names)) == 1.01, na.rm = TRUE),
      median_background     = rowMedians(as.matrix(dplyr::select(., all_of(column_names)))),
      early_bin             = cut(early_ratio_05, breaks = breaks_seq, include.lowest = TRUE, right = FALSE),
      late_bin              = cut(late_ratio_05,  breaks = breaks_seq, include.lowest = TRUE, right = FALSE)
    ) %>% dplyr::select(
      "Hugo_Symbol", "Chromosome", "Start_Position", "End_Position",
      "Variant_Classification", "Variant_Type", "Reference_Allele",
      "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", "Tumor_Sample_Barcode",
      "pathway", "early_ratio_05", "late_ratio_05", "subclonal_ratio",
      "median_background", "early_bin", "late_bin", all_of(column_names)
    )
    }

    df_gritic_all <- rbindlist(df_gritic)
    write.csv(df_gritic_all, file = paste0(path_gritic, "/new/mafgritic_", type, ".csv"), row.names =F)
}
```

## ----------------------------------------------------------
## 5. Calculate the uncertainty for all mutations
## ----------------------------------------------------------
```sh
module load R/4.1.0

R
```

```R
rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(bayestestR)
library(matrixStats)

PCAWG_path <- "${path_rsrch}/PCAWG"
source("${path_home}/code/source/basic.R")
source("${path_home}/code/source/insert_newlines.R")
source("${path_home}/code/source/plot_segTiming.R")

LOH_MSI <- fread("${path_rsrch}/PCAWG/donors_and_biospecimens/LOH_MSI_final.csv")

breaks_seq <- seq(0, 1.05, by = 0.05)

column_names <- paste0("X", 0:249)
bin_levels <- c(
  levels(cut(0, breaks = breaks_seq, include.lowest = TRUE, right = FALSE)),
  "> 0.95"
)

props <- c(0.5, 0.6, 0.7, 0.8, 0.95)

path_gritic <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/"
path_hdi <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/hdi/"

mafgritic_files <- list.files(path = path_gritic, pattern = "mafgritic_*", recursive = FALSE, full.names = TRUE)
mafgritic_files <- mafgritic_files[!grepl("mafgritic_ratio*", mafgritic_files)]

for (file in mafgritic_files) {
  
  type = sub(".csv", "", basename(file))
  type = sub("mafgritic_", "", type)

  all_exist <- all(sapply(seq_along(props), function(i) {
    file_out <- paste0(path_hdi, "/hdi_results_bin_", type, "_", i, ".csv")
    file.exists(file_out)
  }))

  if (!all_exist) {
  nonsyn_filter <- fread(file) %>%
  mutate(row_id = row_number())

  nonsyn_info <- nonsyn_filter %>% 
  dplyr::select("Hugo_Symbol", "Chromosome", "Start_Position", "End_Position","Variant_Classification", 
              "Variant_Type", "Reference_Allele", "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", "Tumor_Sample_Barcode", all_of(column_names))

  nonsyn_filter_sim <- nonsyn_filter %>%
  select(row_id, all_of(column_names)) %>%
  filter(!is.na(X0))
  
  for (i in seq_along(props)) {
     
    file_hdi_results_bin = paste0(path_hdi, "/hdi_results_bin_", type, "_", i ,".csv")

    p <- props[i]
    
    hdi_results <- apply(nonsyn_filter_sim %>% select(-row_id), 1, function(row) {
                    result <- hdi(as.numeric(row), ci = p, verbose = FALSE)
                    c(HDI_low = result$CI_low, HDI_high = result$CI_high)
                    })
                    
    hdi_results <- as.data.frame(t(hdi_results))
    hdi_results$row_id <- nonsyn_filter_sim$row_id

    hdi_results_bin <- hdi_results %>%
    mutate(
      hdi_width = HDI_high - HDI_low,
      uncertainty_bin = case_when(
        hdi_width > 0.95 ~ "> 0.95",
        TRUE ~ as.character(
          cut(hdi_width, breaks = breaks_seq, include.lowest = TRUE, right = FALSE)
        )
      ),
      uncertainty_bin = factor(uncertainty_bin, levels = bin_levels)
  )
  
  nonsyn_merged <- nonsyn_filter %>% 
  left_join(hdi_results_bin, by = "row_id") %>%
  mutate(subclonal_ratio = rowMeans(select(., all_of(column_names)) == 1.01, na.rm = TRUE)) %>%
  dplyr::select("Hugo_Symbol", "Chromosome", "Start_Position", "End_Position","Variant_Classification", 
              "Variant_Type", "Reference_Allele", "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", "Tumor_Sample_Barcode", "row_id", "subclonal_ratio", "HDI_low","HDI_high","hdi_width","uncertainty_bin")

  write.csv(nonsyn_merged, paste0(path_hdi, "/hdi_results_bin_", type, "_", i ,".csv"), row.names = F)
  
  }

  }
  
}
```

## ----------------------------------------------------------
## 6. Calculate the ratio for all mutations
## ----------------------------------------------------------
```sh
module load R/4.1.0

R
```

```R
rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(bayestestR)
library(matrixStats)
library(furrr)
library(future)

PCAWG_path <- "${path_rsrch}/PCAWG"
source("${path_home}/code/source/basic.R")
source("${path_home}/code/source/insert_newlines.R")
source("${path_home}/code/source/plot_segTiming.R")

LOH_MSI <- fread("${path_rsrch}/PCAWG/donors_and_biospecimens/LOH_MSI_final.csv")
column_names <- paste0("X", 0:249)
path_gritic <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/new/"
breaks_seq <- seq(0, 1.05, by = 0.05)

driver_mutations <- fread("${path_rsrch}/PCAWG/driver_mutations/drivermutation_all.csv") %>%
  mutate(Hugo_Symbol = gene,
         Reference_Allele = ref,
         Tumor_Seq_Allele1 = ref,
         Tumor_Seq_Allele2 = alt,
         Tumor_Sample_Barcode = sample_id)

mafgritic_files <- list.files(path = path_gritic, pattern = "mafgritic_*", recursive = FALSE, full.names = TRUE)
mafgritic_files <- mafgritic_files[!grepl("mafgritic_ratio",mafgritic_files)]
mafgritic_files <- mafgritic_files[!grepl("mafgritic_nonsyn_",mafgritic_files)]

for (file in mafgritic_files) {

  type = sub(".csv", "", basename(file))
  type = sub("mafgritic_", "", type)

  cat(paste0(type, "\n"))

  timing_raw <- fread(file) %>% mutate(histology_abbreviation = type) %>% 
  dplyr::select("Hugo_Symbol", "Chromosome", "Start_Position", "End_Position","Variant_Classification", 
              "Variant_Type", "Reference_Allele", "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", "Tumor_Sample_Barcode", "histology_abbreviation", all_of(column_names))
  
  timing_raw_nonsyn <- timing_raw %>% filter(Variant_Classification %in% non_syn)
  write.csv(timing_raw_nonsyn, file = paste0(path_gritic, "/mafgritic_nonsyn_", type, ".csv"), row.names =F)

  data_timing_drivermutations <- timing_raw %>%  
  mutate(driver_gene = "yes",
         Chromosome = as.character(Chromosome)) %>% 
  inner_join(., driver_mutations, by = c("Hugo_Symbol", "Chromosome","Reference_Allele",  "Tumor_Seq_Allele1", "Tumor_Seq_Allele2", 
  "Tumor_Sample_Barcode", "Start_Position", "End_Position"))
  write.csv(data_timing_drivermutations, file = paste0(path_gritic, "/mafgritic_drivermutation_", type, ".csv"), row.names =F)

  #if (!file.exists(paste0(path_gritic, "mafgritic_ratio_", type, ".csv"))) {

  #timing_summary <- timing_raw %>%
  #group_by(Tumor_Sample_Barcode) %>%
  #summarise(
  #  median_all = median(
  #    unlist(across(all_of(column_names))),
  #    na.rm = TRUE
  #  ),
  #  .groups = "drop"
  #)
  #write.csv(timing_summary, file = paste0(path_gritic, "/timing_median_", type, ".csv"), row.names =F)

  #data_timing_ratio <- timing_raw %>% 
  #inner_join(timing_summary, by = "Tumor_Sample_Barcode") %>%
  #mutate(
  #  pathway = Hugo_Symbol,
  #  early_ratio = rowMeans(select(., all_of(column_names)) < median_all, na.rm = TRUE),
  #  late_ratio  = rowMeans(select(., all_of(column_names)) > median_all, na.rm = TRUE),
  #  subclonal_ratio = rowMeans(select(., all_of(column_names)) == 1.01, na.rm = TRUE)
  #) %>%
  #filter(!is.na(early_ratio)) %>%
  #mutate(
  #mean_value   = rowMeans(pick(all_of(column_names)), na.rm = TRUE),
  #median_value = apply(pick(all_of(column_names)), 1, median, na.rm = TRUE),
  #map_value    = apply(pick(all_of(column_names)), 1, function(x) map_estimate(x)$MAP)) %>%
  #ungroup() %>%
  #mutate(
  #  early_bin = cut(early_ratio, breaks = breaks_seq, include.lowest = TRUE, right = FALSE),
  #  late_bin  = cut(late_ratio,  breaks = breaks_seq, include.lowest = TRUE, right = FALSE)
  #) %>%
  #dplyr::select(-all_of(column_names))
  #write.csv(data_timing_ratio, file = paste0(path_gritic, "/mafgritic_ratio_", type, ".csv"), row.names =F)
 
    #}
}
```

## ----------------------------------------------------------
## 7. Calculate the timing difference for all mutations
## ----------------------------------------------------------
```sh
module load R/4.1.0

R
```

```r
rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(bayestestR)
library(matrixStats)

PCAWG_path <- "${path_rsrch}/PCAWG"
source("${path_home}/code/source/basic.R")
source("${path_home}/code/source/insert_newlines.R")
source("${path_home}/code/source/plot_segTiming.R")

mafgritic_files <- list.files(path = path_gritic, pattern = "mafgritic_*", recursive = FALSE, full.names = TRUE)
mafgritic_files <- mafgritic_files[!grepl("mafgritic_ratio_",mafgritic_files)]
mafgritic_files <- mafgritic_files[grepl("*csv",mafgritic_files)]

data_timing_driver <- list()

for (file in mafgritic_files) {

  type <- sub(".csv", "", basename(file))
  type <- sub("mafgritic_", "", type)

  mafgritic <- fread(file) %>%
  mutate(Start_Position = as.character(Start_Position),
       Chromosome = as.character(Chromosome)) %>%
       filter(Hugo_Symbol != "Unknown") %>%
  dplyr::select("Hugo_Symbol", "Chromosome", "Start_Position", "Tumor_Seq_Allele2", "Tumor_Sample_Barcode", "Variant_Classification", "Variant_Type", "Reference_Allele")
  dim(mafgritic)
  
  diff <- fread(paste0(path_gritic, "/diff_allmutations_", type, ".csv")) %>%
    mutate(Tumor_Seq_Allele2 = sub(".*_", "", event1),
           tmp1 = sub("_[^_]+$", "", event1),
           Start_Position = sub(".*_", "", tmp1),
           tmp2 = sub("_[^_]+$", "", tmp1),
           Chromosome = sub(".*_", "", tmp2),
           Hugo_Symbol = sub("_[^_]+_[^_]+_[^_]+$", "", event1),
           Tumor_Sample_Barcode = sample_id,
           Start_Position = as.character(Start_Position)) %>%
           filter(Hugo_Symbol != "Unknown") %>% 
    dplyr::select(-tmp1,-tmp2) %>%
    dplyr::select(-all_of(column_names))
    dim(diff)
    
    data_timing_driver[[type]] <- inner_join(mafgritic, diff, by = c("Hugo_Symbol", "Chromosome", "Start_Position", "Tumor_Seq_Allele2", "Tumor_Sample_Barcode")) %>%
                         filter(Variant_Classification %in% non_syn)
    dim(data_timing_driver[[type]])

    #write.csv(df_merge, file = paste0(path_gritic, "/diff_allmutations_gene_", type, ".csv"), row.names = F)

}

data_timing_driver_all <- rbindlist(data_timing_driver) 
write.csv(data_timing_driver_all, file = paste0(path_gritic, "/nonsyn_filter_diff.csv"), row.names = F)
```

```R
rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(bayestestR)
library(matrixStats)

PCAWG_path <- "${path_rsrch}/PCAWG"
source("${path_home}/code/source/basic.R")
source("${path_home}/code/source/insert_newlines.R")
source("${path_home}/code/source/plot_segTiming.R")

LOH_MSI      <- fread("${path_rsrch}/PCAWG/donors_and_biospecimens/LOH_MSI_final.csv")
column_names <- paste0("X", 0:249)
path_gritic  <- "${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/filtered/"

mafgritic_files <- list.files(path = path_gritic, pattern = "mafgritic_*", recursive = FALSE, full.names = TRUE)
mafgritic_files <- mafgritic_files[!grepl("mafgritic_ratio_", mafgritic_files)]
mafgritic_files <- mafgritic_files[grepl("*csv", mafgritic_files)]

for (file in mafgritic_files) {

  type <- sub(".csv", "", basename(file))
  type <- sub("mafgritic_", "", type)
  cat("Processing:", type, "\n")

  timing_snvall <- fread(file) %>% as.data.frame() %>%
    filter(!is.na(X0))
  dim(timing_snvall)

  # --- Background ---
  df_background <- timing_snvall %>%
    filter(Hugo_Symbol == "Unknown") %>%
    mutate(sample_id = Tumor_Sample_Barcode) %>%
    group_by(sample_id) %>%
    summarise(across(all_of(column_names), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
    mutate(event = "Background_p.A581V",
           Hugo_Symbol = "Background") 

  write.csv(df_background, paste0(path_gritic, "/timing_driverbackground_", type, ".csv"), row.names = FALSE)

  # --- All mutations ---
  timing_allmutations <- timing_snvall %>%
    filter(Hugo_Symbol != "Unknown") %>%
    mutate(
      sample_id   = Tumor_Sample_Barcode,
      Hugo_Symbol = paste0(Hugo_Symbol, "_", Chromosome, "_", Start_Position, "_", Tumor_Seq_Allele2),
      event       = Hugo_Symbol
    ) %>%
    select(sample_id, Hugo_Symbol, event, all_of(column_names))
    dim(timing_allmutations)

  # --- Filter to common samples ---
  common_samples      <- intersect(df_background$sample_id, timing_allmutations$sample_id)
  df_background       <- df_background %>% filter(sample_id %in% common_samples)
  timing_allmutations <- timing_allmutations %>% filter(sample_id %in% common_samples)
  dim(timing_allmutations)

  # --- Join and compute diff (mutation - background) ---
  mut_mat <- timing_allmutations %>% select(all_of(column_names)) %>% as.matrix()
  bg_mat  <- df_background %>%
                left_join(timing_allmutations %>% select(sample_id), by = "sample_id") %>%
                select(all_of(column_names)) %>% as.matrix()

  diff_mat <- mut_mat - bg_mat

  diff_dt <- timing_allmutations %>%
    select(sample_id, event1 = Hugo_Symbol) %>%
    bind_cols(as.data.frame(diff_mat))

  dim(diff_dt)

  # --- Summary stats ---
  mat <- as.matrix(diff_dt %>% select(all_of(column_names)))

  diff_dt <- diff_dt %>%
    mutate(
      aliquot_id         = sample_id,
      pathway            = event1,
      regulator          = "Driver",
      mean_diff          = rowMeans(mat, na.rm = TRUE),
      median_diff        = rowMedians(mat, na.rm = TRUE),
      early_ratio        = rowMeans(mat < 0, na.rm = TRUE),
      late_ratio         = rowMeans(mat > 0, na.rm = TRUE),
      undetermined_ratio = rowMeans(mat == 0, na.rm = TRUE)
    )

  dim(diff_dt)

  write.csv(diff_dt, paste0(path_gritic, "/diff_allmutations_", type, ".csv"), row.names = FALSE)

  rm(timing_snvall, df_background, timing_allmutations, diff_dt, mat)
  gc()
}
```

## ----------------------------------------------------------
## 8. PhylogicNDT -- League model
## ----------------------------------------------------------
```sh
module load python/2.7.18
PhylogicNDT_path=${path_home}/PhylogicNDT/PhylogicNDT

base_dir="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/LeagueModel/PhylogicNDT/comp/min_drivermutation/nocnv/"
base_dir="${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/LeagueModel/PhylogicNDT/comp/min_drivermutation/wgd_hlaloh/"

mkdir -p ${base_dir}
mkdir -p ${plot_dir}

cd "${base_dir}"

for folder in */; do
    (
    cohort=$(basename "${folder}")
    
    echo "Running ${cohort} ..."
    
    cd "${base_dir}/${cohort}" || continue
    
    # skip if no comp.tsv files
    comp_files=$(ls ./*.comp.tsv 2>/dev/null)
    
    if [ -z "${comp_files}" ]; then
        echo "No .comp.tsv files found in ${cohort}, skipping..."
        continue
    fi
    
   python  ${PhylogicNDT_path}/PhylogicNDT.py LeagueModel \
        -cohort "${cohort}" \
        -comps ./*.comp.tsv 

    ) &

done

python ${PhylogicNDT_path}/PhylogicNDT_plot.py \
    --base_dir ${base_dir} \
    --phylogic_path ${PhylogicNDT_path}

## Subsampling
cohort="ColoRect-AdenoCA"
cd ${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/LeagueModel/PhylogicNDT/comp/min_drivermutation/nocnv/ColoRect-AdenoCA_all

for d in */; do
    d=${d%/}      # remove trailing /

    percent=$(echo "$d / 100" | bc -l)

    (
        cd "$d" || exit

        python ${PhylogicNDT_path}/PhylogicNDT.py LeagueModel \
            -cohort "${cohort}_${d}" \
            -comps ./*.comp.tsv \
            --percent_subset "${percent}"
    ) &
done

## Transfer results to loptop
find ${base_dir} -type f -name "*noMedian*" -exec cp {} ${plot_dir} \;
find ${base_dir} -type f -name "*log_odds*" -exec cp {} ${plot_dir} \;

echo ${plot_dir}
mkdir -p /Users/wchen20/Desktop/2026-02-16_Draft/Revision/R1Major5/pathway_min/min_5/all/PhylogicNDT/ColoRect-AdenoCA/
scp -r wchen20@seadragon:${path_rsrch}/PCAWG/MHC_evolution/GRITIC/output/LeagueModel/PhylogicNDT/comp/no_median_League_plot//all/ColoRect-AdenoCA/* /Users/wchen20/Desktop/2026-02-16_Draft/Revision/R1Major5/pathway_min/min_5/all/PhylogicNDT/ColoRect-AdenoCA/

```
