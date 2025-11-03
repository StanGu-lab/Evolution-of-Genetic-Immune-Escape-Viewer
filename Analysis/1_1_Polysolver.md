# Polysolver
## ----------------------------------------------------------
## Update Polysolver version by Sachet's lab
## ----------------------------------------------------------
```sh
######-------------------------------------------------------------------------
# In your working directory have a directory named data with normal and tumor .bam/.bai files
# while in the container I copied the test files to the directory data
# inside the working directory
cp /home/polysolver/test/test* data/
 
#To go into the container:
singularity run \
-B $PWD:/home/jjgallegos \
-B $SCRATCH/polysolver-v4:/tmp \
-H $PWD:/home/jjgallegos \
polysolver-v4-mod.sif
 
######-------------------------------------------------------------------------
 
##### 0. Make directory
mkdir data/hla_mut_out
mkdir data/hla_annot_out
 
######-------------------------------------------------------------------------
##### 1. Running the HLA typing functionality of Polysolver
singularity exec -C \
-B $PWD:/home/jjgallegos \
-B $SCRATCH/polysolver-v4:/tmp \
-H $PWD:/home/jjgallegos \
polysolver-v4-mod.sif /home/polysolver/scripts/shell_call_hla_type \
data/test.bam Unknown 1 hg19 STDFQ 0 data/hla_out
 
##### 1a. Copy this file to your working directory
cp data/hla_out/check.status.out.txt .
 
######-------------------------------------------------------------------------
##### 2. POLYSOLVER-based mutation detection
# will create hla_mut folder in files in your working directory
singularity exec -C \
-B $PWD:/home/jjgallegos \
-B $SCRATCH/polysolver-v4:/tmp \
-H $PWD:/home/jjgallegos \
polysolver-v4-mod.sif /home/polysolver/scripts/shell_call_hla_mutations_from_type \
data/test.bam data/test.tumor.bam data/hla_out/winners.hla.txt hg19 STDFQ data/hla_mut_out patient1
 
######-------------------------------------------------------------------------
##### 3. Annotation of mutations
singularity exec -C \
-B $PWD:/home/jjgallegos \
-B $SCRATCH/polysolver-v4:/tmp \
-H $PWD:/home/jjgallegos \
polysolver-v4-mod.sif /home/polysolver/scripts/shell_annotate_hla_mutations patient1 hla_mut.tar.gz data/hla_annot_out
######-------------------------------------------------------------------------
######-------------------------------------------------------------------------
###### Notes: ---------------------------------------------------------------------
# Go in container to view scripts
singularity run \
-B $PWD:/home/jjgallegos \
-B $SCRATCH/polysolver-v4:/tmp \
-H $PWD:/home/jjgallegos \
polysolver-v4-mod.sif
 
# view the file while in container
less /home/polysolver/scripts/shell_call_hla_type
######-------------------------------------------------------------------------
######-------------------------------------------------------------------------
```

## ----------------------------------------------------------
## 1. Intall Polysolver and test data
## ----------------------------------------------------------
``` sh
## Pull Singularity Container
#module avai singularity
module load singularity/3.7.0

cd /rsrch6/home/hema_bio-Malignan/wchen20/polysolver
singularity pull --name polysolver_aj.sif docker://ajchenwenjie/mhc_polysolver:2.0

## Set Environment
singularity shell -C -B /rsrch6/home/hema_bio-Malignan/wchen20/polysolver/:/tmp /rsrch6/home/hema_bio-Malignan/wchen20/polysolver/polysolver_aj.sif

## Run POLYSOLVER
bash /home/polysolver/scripts/shell_call_hla_type /home/polysolver/test/test.bam \
                                      Unknown \
                                      1 \
                                      hg19 \
                                      STDFQ \
                                      0 \
                                      /tmp/test_out
#### If successful, the following command should not yield any differences
diff /tmp/test_out/winners.hla.txt /home/polysolver/test/orig.winners.hla.txt

## POLYSOLVER-based mutation detection
bash /home/polysolver/scripts/shell_call_hla_mutations_from_type /home/polysolver/test/test.bam \
                                       /home/polysolver/test/test.tumor.bam \
                                       /tmp/test_out/winners.hla.txt \
                                       hg19 \
                                       STDFQ \
                                       /tmp/test_out/ \
                                       test
#### If successful, the following command should not yield any differences:
diff /tmp/test_out/call_stats.hla_b_39_01_01_02l.out \
     /home/polysolver/test/orig.call_stats.hla_b_39_01_01_02l.out

## Annotation of mutations
bash /home/polysolver/scripts/shell_annotate_hla_mutations indiv /tmp/test_out/hla_mut.tar.gz /tmp/test_out
#### If successful, the following command should not yield any differences:
diff /tmp/test_out/indiv.mutect.filtered.nonsyn.annotated \
     /home/polysolver/test/orig.indiv.mutect.filtered.nonsyn.annotated
```

## ----------------------------------------------------------
## 2. Prepare sample List
## ----------------------------------------------------------
```sh
module load R/3.5.0

R
```

```r
library("data.table")
library("dplyr")
library("tidyr")

path="../Project/PCAWG"
sample_sheet <- fread(paste0(path,"/donors_and_biospecimens/pcawg_sample_sheet.tsv"))

## Split the data based on dcc_project_code
split_data <- split(sample_sheet, sample_sheet$dcc_project_code)

## Process each split dataset
for (project_code in names(split_data)) {
  data <- split_data[[project_code]]
  
  # Subset and transform the data
    paired <- data %>%
    subset(library_strategy != "RNA-Seq") %>%
    subset(donor_wgs_exclusion_white_gray != "Excluded") %>%
    mutate(sample_type = ifelse(grepl("tumour", dcc_specimen_type, ignore.case = TRUE), "tumour_id", "normal_id")) %>%
    select(icgc_donor_id, aliquot_id, sample_type) %>%
    pivot_wider(names_from = sample_type,
                values_from = aliquot_id,
                names_prefix = "",
                values_fn = toString)
  
  # Write the processed data to a file
  write.table(paired, file = paste0(path, "/donors_and_biospecimens/paired_list/", project_code, "_paired.txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names = TRUE)
}

# Remove duplicated values
filtered_data <- sample_sheet[grepl("-US", sample_sheet$dcc_project_code), ]
project <- gsub("-US", "", unique(filtered_data$dcc_project_code))
write.table(project, file = paste0(path, "/donors_and_biospecimens/paired_list/","project.txt"), , sep="\t", quote=FALSE, row.names=FALSE, col.names = FALSE)
```

## ----------------------------------------------------------
## 3. Iterate over types
## ----------------------------------------------------------
### TCGA
```sh
#!/bin/bash
paired_base="../Project/PCAWG/MHC_evolution/HLAmutation/paired_tcga_"
base_dir="../Project/PCAWG/MHC_evolution"
HLAmut_dir="${base_dir}/HLAmutation/results/tcga"
genome="hg38"
job_num="50"

for i in "remain250703"; do

  paired_list="${paired_base}${i}.txt"

  bsub <<EOF
#BSUB -W 240:00
#BSUB -q long
#BSUB -o ${base_dir}/HLAmutation/ICGC_HLAmutation_output_${i}_%J.log
#BSUB -e ${base_dir}/HLAmutation/ICGC_HLAmutation_error_${i}_%J.log
#BSUB -cwd ${base_dir}/HLAmutation
#BSUB -u wchen20@mdanderson.org
#BSUB -n 24
#BSUB -M 400
#BSUB -R "rusage[mem=400]"
#BSUB -P TCGA_HLAmut
#BSUB -J TCGA_HLAmut_${i}

bash /rsrch6/home/hema_bio-Malignan/wchen20/code/source/call_hlamutations.sh $paired_list $base_dir $HLAmut_dir $genome $job_num
EOF

done
```

### ICGC
```sh
#!/bin/bash
paired_base="../Project/PCAWG/donors_and_biospecimens/paired_list/paired_icgc_"
base_dir="../Project/PCAWG/MHC_evolution"
HLAmut_dir="${base_dir}/HLAmutation/results/icgc"
genome="hg19"
job_num="50"

for i in {1..8}; do

  paired_list="${paired_base}${i}.txt"

  bsub <<EOF
#BSUB -W 24:00
#BSUB -q medium
#BSUB -o ${base_dir}/HLAmutation/ICGC_HLAmutation_output_${i}_%J.log
#BSUB -e ${base_dir}/HLAmutation/ICGC_HLAmutation_error_${i}_%J.log
#BSUB -cwd ${base_dir}/HLAmutation
#BSUB -u wchen20@mdanderson.org
#BSUB -n 24
#BSUB -M 400
#BSUB -R "rusage[mem=400]"
#BSUB -P ICGC_HLAmut
#BSUB -J ICGC_HLAmut_${i}

bash /rsrch6/home/hema_bio-Malignan/wchen20/code/source/call_hlamutations.sh $paired_list $base_dir $HLAmut_dir $genome $job_num
EOF

done
```

## ----------------------------------------------------------
## 4. Summarize the results
## ----------------------------------------------------------
```sh
## Summary results
find ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc/ -type f -name "winners.hla.nofreq.txt" | grep -v "/hla_mut/"|wc -l
find ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc/ -type f -name "*.vcf" | grep -v "/hla_mut/" | xargs -n1 dirname | sort -u|wc -l

## Print results to txt
find ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc/ -type f -name "*.vcf" | grep -v "/hla_mut/" | xargs -n1 dirname | sort -u > ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc/icgc_polysolver_run.txt

find ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc/ -type f -name "winners.hla.nofreq.txt" | grep -v "/hla_mut/" > ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc_winner.txt

find ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc/ -type f -name "*.vcf"| grep -v "/hla_mut/" > ../Project/PCAWG/MHC_evolution/HLAmutation/results/icgc_vcf.txt
```

## ----------------------------------------------------------
## 5. Convert Polysolver results to maf
## ----------------------------------------------------------
```sh
## Summary results
## TCGA
module load R/4.1.0

path_code="/rsrch6/home/hema_bio-Malignan/wchen20/code/source/"
type="tcga"
genome="hg38"
Rscript ${path_code}/PolysolverToDF.R -r ../Project/PCAWG/MHC_evolution/HLAmutation/results/tcga -s ../Project/PCAWG/MHC_evolution/HLAmutation/results -t ${type} -g ${genome}

# Liftover mutation
type="tcga"
maf="../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_tcga_HLAmut.maf"
litoverbed="../Project/PCAWG/MHC_evolution/HLAmutation/results/hglft_genome_tcga.bed"
path_save="../Project/PCAWG/MHC_evolution/HLAmutation/results/"
Rscript ${path_code}/Polysolver_Liftover.R -m ${maf} -b ${litoverbed} -t ${type} -p ${path_save}
mv ${path_save}/maf_${type}_HLAmut_liftover.maf ${path_save}/maf_${type}_HLAmut_final.maf
scp -r wchen20@seadragon:../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_${type}_HLAmut_final.maf /Users/wchen20/Desktop/PCAWG/MHC_evolution/HLAmutations/results

## ICGC
path_code="/rsrch6/home/hema_bio-Malignan/wchen20/code/source/"
type="icgc"
genome="hg19"
path_save="../Project/PCAWG/MHC_evolution/HLAmutation/results/"
Rscript ${path_code}/PolysolverToDF.R -r ../Project/PCAWG/MHC_evolution/HLAmutation/results/${type} -s ../Project/PCAWG/MHC_evolution/HLAmutation/results -t ${type} -g ${genome}
mv ${path_save}/maf_${type}_HLAmut.maf ${path_save}/maf_${type}_HLAmut_final.maf

scp -r wchen20@seadragon:../Project/PCAWG/MHC_evolution/HLAmutation/results/*_final.maf /Users/wchen20/Desktop/PCAWG/MHC_evolution/HLAmutations/results
scp -r wchen20@seadragon:../Project/PCAWG/MHC_evolution/HLAmutation/results/*_runsamples.csv /Users/wchen20/Desktop/PCAWG/MHC_evolution/HLAmutations/results
```

## ----------------------------------------------------------
## 6. Obtain all the genotype
## ----------------------------------------------------------
```sh
module load R/4.1.0

R
```

```r
rm(list=ls())
library(data.table)
library(dplyr)
library(stringr)

path_poly <- "../Project/PCAWG/MHC_evolution/HLAmutation/results/"

projects <- c("tcga", "icgc")
hla_table <- list()

for (proj in projects) {
  
  path_out <- paste0(path_poly, "/", proj, "/")
  winner_files <- list.files(path_out, pattern = "winners.hla.nofreq.txt", full.names = TRUE, recursive = TRUE)
  winner_files <- winner_files[!grepl("hla_mut", winner_files)]

  for (file in winner_files) {

    parts_list <- strsplit(file, "/")[[1]]
    aliquot_id <- parts_list[14] 
    
    hla_table[[aliquot_id]] <- fread(file, header = FALSE) %>%
    mutate(aliquot_id = aliquot_id) %>%
    setNames(c("HLA", "allele_1", "allele_2", "aliquot_id"))

  }

}

hla_genotype <- rbindlist(hla_table) %>% distinct()
length(unique(hla_genotype$aliquot_id))
write.csv(hla_genotype, "../Project/PCAWG/MHC_evolution/HLAmutation/results/hla_genotype.csv", row.names = F)
```

## ----------------------------------------------------------
## 7. Combine Polysolver results with Consensus mutation call
## ----------------------------------------------------------
### Convert polysolver results to consensus mutations and GRITIC SNV table
```sh
module load R/4.1.0

path_code="/rsrch6/home/hema_bio-Malignan/wchen20/code/source/"
maf_consensus="../Project/PCAWG/MHC_evolution/HLAmutation/results/HLA_consensus.maf"
path_gritic="../Project/PCAWG/MHC_evolution/GRITIC/input/250306/"
path_save="../Project/PCAWG/consensus_snv_indel/vcf_HLAupdated/"

## TCGA
maf_poly="../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_tcga_HLAmut_final.maf"
project="tcga"
Rscript ${path_code}/PolysolverToConsensus.R -c ${maf_consensus} -m ${maf_poly} -p ${project} -s ${path_save} -g ${path_gritic}

## ICGC
maf_poly="../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_icgc_HLAmut_final.maf"
project="icgc"
Rscript ${path_code}/PolysolverToConsensus.R -c ${maf_consensus} -m ${maf_poly} -p ${project} -s ${path_save} -g ${path_gritic}
```

### Combine polysolver results with consensus mutations
```sh
module load R/4.1.0

R
```

```r
rm(list=ls())
library(data.table)
library(dplyr)
library(stringr)
library(maftools)

non_syn <- c("Frame_Shift_Del", "Frame_Shift_Ins", "Splice_Site", "Translation_Start_Site", "Nonsense_Mutation", 
             "Nonstop_Mutation", "In_Frame_Del", "In_Frame_Ins", "Missense_Mutation")

## MAF from VCF
pcawgmaf_input <- "../Project/PCAWG/consensus_snv_indel/maf_from_vcf/maf_pcawg_vcf_nonsyn.maf"
maf_out <- "../Project/PCAWG/consensus_snv_indel/maf_from_vcf/"
data <- fread(pcawgmaf_input)
dim(data)

## Maf without all nonsyn HLA mutations but with syn HLA mutations
hla_genes <- c("HLA-A", "HLA-B", "HLA-C")
data_noHLAnonsyn <- data %>%
  filter(!(Hugo_Symbol %in% hla_genes & Variant_Classification %in% non_syn))
dim(data_noHLAnonsyn)
write.table(data_noHLAnonsyn, paste0(maf_out, "/maf_pcawg_vcf_noHLA.maf"), sep = "\t", row.names = FALSE, quote = FALSE)

maf_files <- c(paste0(maf_out, "/maf_pcawg_vcf_noHLA.maf"), "../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_tcga_HLAmut_final.maf", "../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_icgc_HLAmut_final.maf")

mymaf = maftools::merge_mafs(mafs = maf_files, verbose = TRUE)
save(mymaf, file = paste0(maf_out, "/pcawg_HLAupdate.RData"))

## Maf with all **nonsyn nonHLA mutations**
hla_genes <- c("HLA-A", "HLA-B", "HLA-C")
data_noHLAnonsyn <- data %>%
  filter(!Hugo_Symbol %in% hla_genes) %>%
  filter(Variant_Classification %in% non_syn)
dim(data_noHLAnonsyn)
write.table(data_noHLAnonsyn, paste0(maf_out, "/maf_pcawg_vcf_nonsyn_noHLA.maf"), sep = "\t", row.names = FALSE, quote = FALSE)

## Maf with all nonsyn nonHLA mutations**
data_HLA <- data %>% filter(Hugo_Symbol %in% c("HLA-A", "HLA-B", "HLA-C"), Variant_Classification %in% non_syn)
write.table(data_HLA, paste0(maf_out, "/maf_pcawg_vcf_nonsyn_consensusHLA.maf"), sep = "\t", row.names = FALSE, quote = FALSE)

maf_files <- c(paste0(maf_out, "/maf_pcawg_vcf_nonsyn_noHLA.maf"), "../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_tcga_HLAmut_final.maf", "../Project/PCAWG/MHC_evolution/HLAmutation/results/maf_icgc_HLAmut_final.maf")

mymaf = maftools::merge_mafs(mafs = maf_files, verbose = TRUE)
save(mymaf, file = paste0(maf_out, "/pcawg_HLAupdate_nonsyn.RData"))
```

## ----------------------------------------------------------
## 8. Generate DN/DS input
## ----------------------------------------------------------
```R
## DN/DS
non_syn <- c("Frame_Shift_Del", "Frame_Shift_Ins", "Splice_Site", "Translation_Start_Site", "Nonsense_Mutation", 
             "Nonstop_Mutation", "In_Frame_Del", "In_Frame_Ins", "Missense_Mutation")

## MAF from VCF
pcawgmaf_input <- "../Project/PCAWG/consensus_snv_indel/maf_from_vcf/maf_pcawg_vcf.maf"
maf_out <- "../Project/PCAWG/consensus_snv_indel/maf_from_vcf/"
data <- fread(pcawgmaf_input)
dim(data)

## maf without all nonsyn HLA mutations
hla_genes <- c("HLA-A", "HLA-B", "HLA-C")

## dNdScv without all nonsyn HLA mutations
data_noHLAnonsyn <- fread(paste0(maf_out, "/maf_pcawg_vcf_noHLA.maf"))
mutations <- data_noHLAnonsyn %>% select(Tumor_Sample_Barcode, Chromosome, Start_Position, Tumor_Seq_Allele1, Tumor_Seq_Allele2)
names(mutations) <- c("sampleID", "chr", "pos", "ref", "mut")
write.table(mutations, file = paste0(maf_out, "/mutations_dNdScvnoHLAnonsyn.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

## Samples with consensus HLA mutations
data_HLA <- fread(paste0(maf_out, "/maf_pcawg_vcf_nonsyn_consensusHLA.maf")) %>% 
distinct(Hugo_Symbol, Variant_Classification, .keep_all = TRUE) %>% 
mutate(Start_position_dnds = Start_Position, 
       End_position_dnds = End_Position,
       Tumor_Seq_Allele1_dnds = Tumor_Seq_Allele1, 
       Tumor_Seq_Allele2_dnds = Tumor_Seq_Allele2) %>%
dplyr::select(Hugo_Symbol, Variant_Classification, Start_position_dnds, End_position_dnds,Tumor_Seq_Allele1_dnds, Tumor_Seq_Allele2_dnds) 

## dNdScv without polysolver
path_poly = "../Project/PCAWG/MHC_evolution/HLAmutation/results/"
HLAmut_icgc <- fread(paste0(path_poly, "/maf_icgc_HLAmut_final.maf"))
HLAmut_tcga <- fread(paste0(path_poly, "/maf_tcga_HLAmut_final.maf"))

HLAmut_pcawg <- rbind(HLAmut_icgc, HLAmut_tcga, fill= TRUE) %>% 
left_join(.,data_HLA, by = c("Hugo_Symbol", "Variant_Classification")) %>%
mutate(Start_Position = ifelse(!is.na(Start_position_dnds), Start_position_dnds, Start_Position),
       End_position = ifelse(!is.na(End_position_dnds), End_position_dnds, End_Position),
       Tumor_Seq_Allele1 = ifelse(!is.na(Tumor_Seq_Allele1_dnds), Tumor_Seq_Allele1_dnds, Tumor_Seq_Allele1), 
       Tumor_Seq_Allele2 = ifelse(!is.na(Tumor_Seq_Allele2_dnds), Tumor_Seq_Allele2_dnds, Tumor_Seq_Allele2)) %>% 
       select(Tumor_Sample_Barcode, Chromosome, Start_Position, Tumor_Seq_Allele1, Tumor_Seq_Allele2)
names(HLAmut_pcawg) <- c("sampleID", "chr", "pos", "ref", "mut")
write.table(HLAmut_pcawg, file = paste0(maf_out, "/mutations_dNdScvPloyHLA.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

mutations_all <- rbind(mutations, HLAmut_pcawg)
write.table(mutations_all, file = paste0(maf_out, "/mutations_dNdScv_updated_withsynHLA.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
```

