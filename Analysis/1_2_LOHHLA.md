# LOHHLA
## ----------------------------------------------------------
## 1. Install LOHHLA and test data
## ----------------------------------------------------------
```sh
# https://github.com/McGranahanLab/bioinfcollab_cruklungcentre/blob/1023f0988c92dcfc404ff60d4a6af322c74a0ab5/singularity_recipes/lohhla.def#L6
# https://www.dropbox.com/s/o68logeoyh82oph/tracerx.LOHHLA.31_04_21.R
singularity shell -B ../home/wchen20/lohhla/:/data/ -B /rsrch6:/mnt ../home/wchen20/lohhla/lohhla_aj_v2.sif

cd /mnt/home/hema_bio-Malignan/wchen20/lohhla/
base_path=/mnt/home/hema_bio-Malignan/wchen20/lohhla
HLAfastaLoc=/mnt/home/hema_bio-Malignan/wchen20/lohhla/lohhla_mcgranahan/mcgranahanlab-lohhla-e4c7d5e14c39/data/hla_all.fasta
cd ${base_path}/test_out/
out_path=${base_path}/test_out

Rscript /LOHHLA.R --patient_id sample \
                      --out_dir ${out_path} \
                      --tumour_bams ${base_path}/example-file/bam/example_tumor_sorted.bam \
                      --normal_bam /${base_path}/example-file/bam/example_BS_GL_sorted.bam \
                      --patient_hla_alleles ${base_path}/example-file/winners.hla.nofreq.txt \
                      --HLA_fasta ${HLAfastaLoc} \
                      --min_coverage 10 \
                      --fish_reads \
                      --kmer_size 50 \
                      --purity_ploidy ${base_path}/example-file/solutions_test.csv \
                      --hla_region_coordinates ${base_path}/example-file/hla_hg19.csv \
                      --HLA_exons /data/data/hla.dat \
                      --bedtools /bin/bedtools/bedtools \
                      --plotting_step

# Test for TCGA
# Set the parameters by the NG paper: https://github.com/UMCUGenetics/Genetic-Immune-Escape/blob/16c772b0de8cb59f04537a1203110c89ae36dafe/0_process_data/GIE_events/run_lohhla.sh#L6
cd ../Project/PCAWG/HLALOH/
module load singularity/3.7.0

singularity exec -B ../home/wchen20/lohhla/:/data/ -B /rsrch6:/mnt ../home/wchen20/lohhla/lohhla_aj_v2.sif bash /mnt/scratch/hema_bio-Malignan/wchen20/PCAWG/HLALOH/test.sh

cancer_type="COAD"
#tumour_id="00aa769d-622c-433e-8a8a-63fb5c41ea42"
#normal_id="ddfe45b0-0c9b-4b7f-9fb3-17715d85a63d"

out_path=/mnt/scratch/hema_bio-Malignan/wchen20/PCAWG/HLALOH/${cancer_type}/${tumour_id}
bam_path=/mnt/scratch/reflib/TCGA_restricted/TCGA_Bams/${cancer_type}/WGS
poly_path=/mnt/scratch/hema_bio-Malignan/wchen20/PCAWG/HLAmutations/hg19/${cancer_type}/${tumour_id}
solutions_path=/mnt/scratch/hema_bio-Malignan/wchen20/PCAWG/HLALOH/solutions
ref_path=/mnt/home/hema_bio-Malignan/wchen20/lohhla/reference

normal_bam=$(find $bam_path -type f -name "*${normal_id}*_wgs_gdc_realn.bam" -print -quit)
tumour_bam=$(find $bam_path -type f -name "*${tumour_id}*_wgs_gdc_realn.bam" -print -quit)

Rscript /LOHHLA.R --patient_id ${tumour_id} \
                          --out_dir ${out_path} \
                          --tumour_bams ${tumour_bam} \
                          --normal_bam ${normal_bam} \
                          --patient_hla_alleles ${poly_path}/winners.hla.nofreq.txt \
                          --purity_ploidy ${solutions_path}/${tumour_id}_solutions.csv \
                          --min_coverage 5 \
                          --max_mismatch 2 \
                          --fish_reads \
                          --kmer_size 50 \
                          --HLA_fasta ${ref_path}/abc_complete.fasta \
                          --hla_region_coordinates ${ref_path}/hla_hg38.csv \
                          --HLA_exons ${ref_path}/hla.dat \
                          --bedtools /bin/bedtools/bedtools \
                          --plotting_step 

```

## ----------------------------------------------------------
## 2. Prepare HLA LOH input
## ----------------------------------------------------------
```sh
module load R/3.5.0

R
```

```R
## PCAWG-TCGA samples
library("dplyr")
library("data.table")
path <- "../Project/PCAWG"
solution_path <- "../Project/PCAWG/HLALOH/solutions"
purity_pliody <- readRDS("../Project/PCAWG/consensus_cnv/purity_ploidy/finalPurity.Rds") %>% 
                 mutate(tumorPurity = purity,
                        tumorPloidy = ploidy,
                        file_name = paste0(samplename,"_wgs_gdc_realn.bam")) %>%
                 select(file_name, tumorPurity, tumorPloidy)

# Assuming your data frame is named "df" and it contains a column named "samplename"
# Get unique sample names
file_names <- unique(purity_pliody$file_name)

# Split the data frame by sample name and write separate text files
for (file in file_names) {
  # Subset the data frame for the current sample name
  subset_df <- purity_pliody[purity_pliody$file_name == file, ]
  
  sample_name <- gsub("_wgs_gdc_realn.bam", "", file)

  # Define the file name for the text file
  filename <- paste0(solution_path, "/", sample_name, "_solutions.csv")
  
  # Write the subsetted data frame to a text file
  #write.table(subset_df, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)
  fwrite(subset_df, file = filename, sep = ",", col.names = FALSE)
}

## PCAWG-ICGC samples
sample_list <- fread("../Project/PCAWG/donors_and_biospecimens/paired_list/240822/undownloaded_paired.txt")

for (samplename in sample_list$id) {

      bam_name <- subset(sample_list, id == samplename) %>% pull(tumour_bam) %>% basename(.)
      solutions <- fread(paste0("../Project/PCAWG/MHC_evolution/HLALOH/solutions/", samplename, "_solutions.csv")) %>% 
      mutate(V1= bam_name)
      
      filename = paste0("../Project/PCAWG/MHC_evolution/HLALOH/solutions/", samplename, "_solutions_new.csv")
      fwrite(solutions, file = filename, sep = ",", col.names = FALSE)

}
```


## ----------------------------------------------------------
## 3. Run for TCGA data
## ----------------------------------------------------------
```sh
#!/bin/bash
paired_base="../Project/PCAWG/MHC_evolution/HLALOH/paired_tcga_"
base_dir="../Project/PCAWG/MHC_evolution"
HLALOH_dir="${base_dir}/HLALOH/results/tcga/"
HLAmut_dir="${base_dir}/HLAmutation/results/tcga"
solutions_path="${base_dir}/HLALOH/solutions/"
genome="hg38"
job_num="50"

for i in "remain"; do

  paired_list="${paired_base}${i}.txt"

  bsub <<EOF
#BSUB -W 240:00
#BSUB -q long
#BSUB -o ../Project/PCAWG/MHC_evolution/HLALOH/ICGC_HLAHLOH_output_${i}_%J.log
#BSUB -e ../Project/PCAWG/MHC_evolution/HLALOH/ICGC_HLAHLOH_error_${i}_%J.log
#BSUB -cwd ../Project/PCAWG/MHC_evolution/HLALOH
#BSUB -u wchen20@mdanderson.org
#BSUB -n 24
#BSUB -M 200
#BSUB -R "rusage[mem=200]"
#BSUB -P TCGA_HLALOH
#BSUB -J TCGA_HLALOH_${i}

bash ../home/wchen20/code/source/call_hlaloh.sh ${paired_list} ${base_dir} ${HLALOH_dir} ${HLAmut_dir} ${solutions_path} ${genome} ${job_num}

## Delete the temporal files
find ../Project/PCAWG/MHC_evolution/HLALOH/results/ -type f \( -name "*.bam" -o -name "*.sam" -o -name "*.fastq" -o -name "*.fastq.gz" -o -name "*.bai" \) -exec rm -f {} +
EOF

done
```

## ----------------------------------------------------------
## 4. Run for ICGC data
## ----------------------------------------------------------
```sh
#!/bin/bash
paired_base="../Project/PCAWG/donors_and_biospecimens/paired_list/paired_icgc_"
base_dir="../Project/PCAWG/MHC_evolution"
HLALOH_dir="${base_dir}/HLALOH/results/icgc/"
HLAmut_dir="${base_dir}/HLAmutation/results/icgc"
solutions_path="${base_dir}/HLALOH/solutions/"
genome="hg19"
job_num="50"

for i in {1..8}; do

  paired_list="${paired_base}${i}.txt"

  bsub <<EOF
#BSUB -W 240:00
#BSUB -q long
#BSUB -o ../Project/PCAWG/MHC_evolution/HLALOH/ICGC_HLAHLOH_output_${i}_%J.log
#BSUB -e ../Project/PCAWG/MHC_evolution/HLALOH/ICGC_HLAHLOH_error_${i}_%J.log
#BSUB -cwd ../Project/PCAWG/MHC_evolution/HLALOH
#BSUB -u wchen20@mdanderson.org
#BSUB -n 24
#BSUB -M 200
#BSUB -R "rusage[mem=200]"
#BSUB -P ICGC_HLALOH
#BSUB -J ICGC_HLALOH_${i}

bash ../home/wchen20/code/source/call_hlaloh.sh ${paired_list} ${base_dir} ${HLALOH_dir} ${HLAmut_dir} ${solutions_path} ${genome} ${job_num}
## Delete the temporal files

find ../Project/PCAWG/MHC_evolution/HLALOH/results/ -type f \( -name "*.bam" -o -name "*.sam" -o -name "*.fastq" -o -name "*.fastq.gz" -o -name "*.bai" \) -exec rm -f {} +
EOF

done
```

## ----------------------------------------------------------
## 5. Summary the results
## ----------------------------------------------------------
```sh
find ../Project/PCAWG/MHC_evolution/HLALOH/results/tcga \
  -type f -name "*_LOHHLA.csv" \
  -exec grep -l "UnPairedPval_unique" {} + | wc -l

find ../Project/PCAWG/MHC_evolution/HLALOH/results/icgc \
  -type f -name "*_LOHHLA.csv" \
  -exec grep -l "UnPairedPval_unique" {} + > ../Project/PCAWG/MHC_evolution/HLALOH/results/icgc_lohhlarun.txt
```

## ----------------------------------------------------------
## 6. Generate the dataframe
## ----------------------------------------------------------
```sh
module load R/4.1.0

path_code="../home/wchen20/code/source"

type="icgc"
type="tcga"
path_basic="../Project/PCAWG/MHC_evolution/HLALOH/results/${type}/"
path_output="../Project/PCAWG/MHC_evolution/HLALOH/results/${type}/"
Rscript ${path_code}/HLALOHtoDF.R -p ${path_basic} -t ${type} -o ${path_output}

type="icgc"
type="tcga"
scp -r wchen20@seadragon:../Project/PCAWG/MHC_evolution/HLALOH/results/${type}/LOHHLA*.csv /Users/wchen20/Desktop/PCAWG/MHC_evolution/HLALOH/results
```

