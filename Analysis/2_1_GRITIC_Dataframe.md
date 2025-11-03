# GRITIC
# ----------------------------------------------------------
# 1. Convert GRITIC results (Toby's) to txt
# ----------------------------------------------------------
## Convert pkl files to data frame
```py
## convert_to_csv20240327_tcga/icgc.py
import os
import pysam
import pandas as pd
from concurrent.futures import ThreadPoolExecutor

def process_files(vcf_path, pkl_path, output_path):
    # Read the VCF
    vcf = pysam.VariantFile(vcf_path)

    # Convert VCF to DataFrame
    vcf_data = []
    for record in vcf:
        vcf_data.append([record.chrom, record.pos, record.id, record.ref, record.alts, record.qual, record.filter, record.info])

    vcf_df = pd.DataFrame(vcf_data, columns=["CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO"])
    vcf_df['ALT'] = vcf_df['ALT'].str[0]

    # Read the .pkl file
    pkl_df = pd.read_pickle(pkl_path)

    # If the object read from the .pkl file is a dictionary, convert it to a DataFrame
    if isinstance(pkl_df, dict):
        pkl_df = pd.DataFrame(pkl_df)

    pkl_df_transposed = pkl_df.transpose()
    pkl_df_transposed = pkl_df_transposed.reset_index()
    pkl_df_transposed.columns = ['index'] + list(range(len(pkl_df)))
    pkl_df_transposed['index'] = pkl_df_transposed['index'].astype(int)
    
    # Compute 10th and 90th percentiles for each row
    percentile_10 = pkl_df_transposed.drop('index', axis=1).quantile(0.10, axis=1)
    percentile_90 = pkl_df_transposed.drop('index', axis=1).quantile(0.90, axis=1)

    # Create a new dataframe with percentiles
    percentiles_df = pd.DataFrame({
    'index': pkl_df_transposed['index'],
    'percentile_10': percentile_10,
    'percentile_90': percentile_90
    })

    # Perform the merge
    merged_df = pd.merge(vcf_df, pkl_df_transposed, left_on=vcf_df.index, right_on='index', how='left').drop('index', axis=1)
    merged_df = pd.merge(merged_df, percentiles_df, left_on=vcf_df.index, right_on='index', how='left').drop('index', axis=1)

    # Save to output path
    merged_df.to_csv(output_path, sep='\t', index=True)

def main():
    # Paths
    vcf_dir = "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/final_consensus_12oct/tcga_filtered/graylist/snv_mnv/"
    pkl_dir = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/PCAWG_SNV_Test_mut_clean_autosome_relabelled/"
    output_dir = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timingnew/"

    pkl_files = [f for f in os.listdir(pkl_dir) if f.endswith('.pkl.bz2')]

    # Loop through each pkl file and process
    with ThreadPoolExecutor(max_workers=200) as executor:
        futures = []
        for pkl_file in pkl_files:
            base_name = pkl_file.split('.')[0]
            vcf_file = base_name + ".consensus.20160830.filtered.somatic.snv_mnv.vcf.gz"
            vcf_path = os.path.join(vcf_dir, vcf_file)
            pkl_path = os.path.join(pkl_dir, pkl_file)
            output_path = os.path.join(output_dir, base_name + "_griticsnv.txt")
            if os.path.exists(vcf_path):
                futures.append(executor.submit(process_files, vcf_path, pkl_path, output_path))
            else:
                print(f"VCF file {vcf_file} not found for {pkl_file}")
    # Wait for all futures to complete
        for future in futures:
            future.result()

if __name__ == "__main__":
   main()
```

```sh
#BSUB -W 24:00
#BSUB -q medium
#BSUB –cwd ../Project/PCAWG/MHC_evolution/GRITIC
#BSUB –u xxx@mdanderson.org
#BSUB -n 12
#BSUB -M 64
#BSUB -R rusage[mem=64]
#BSUB -P complex_timing_tcga
#BSUB -J complex_timing_tcga
#BSUB -o ../Project/PCAWG/MHC_evolution/GRITIC

proj="tcga"
proj="icgc"
module load python/3.11.3
python ../Project/PCAWG/MHC_evolution/GRITIC/convert_to_csv20240327_tcga.py
```

## Combine GRITIC and MAF
```r
rm(list = ls())

library("data.table")
library("dplyr")
library("tidyr")

non_syn = c("Frame_Shift_Del", "Frame_Shift_Ins", "Splice_Site", "Translation_Start_Site", "Nonsense_Mutation", "Nonstop_Mutation", "In_Frame_Del", "In_Frame_Ins", "Missense_Mutation")
APM = c("HLA-A", "HLA-B", "HLA-C","B2M", "NLRC5", "TAP1", "TAP2" ,"TAPBP", "PSMB8", "PSMB9", "PSMB10", "ERAP1", "ERAP2")

PCAWG_path <- "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG"
maf_path <- paste0(PCAWG_path, "/MHC_evolution/snv_indel/annoted_all")
gritic_path <- "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timingnew/"

projects <- c("tcga", "icgc")

for (proj in projects) {
  
  maf_gritic_path <- paste0("/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/final_consensus_12oct/", proj, "_filtered/annoted/")
  maf_files <- list.files(path = maf_gritic_path, pattern = ".consensus.20160830.filtered.somatic.snv_mnv.maf", recursive = TRUE, full.names = TRUE)
  out_path <- paste0("../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  
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

## Combine all the output
projects <- c("tcga", "icgc")

non_filter <- list()

for (proj in projects) {
  
  out_path <- paste0("../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  file_list <- list.files(path = out_path, pattern = "_mafnongritic.csv", recursive = FALSE, full.names = TRUE)

  df_list <- list()
  
  for (file in file_list) {
  file_name <- basename(file)
  tumour_id <- gsub("_mafnongritic.csv", "", file_name)
            
  timing <- read.csv(file)
  df_list[[file_name]] <- timing %>% mutate(sample_id = tumour_id)
  
  }
          
  non_filter[[proj]] <- rbindlist(df_list, fill = TRUE)
  write.csv(non_filter[[proj]], file = paste0(gritic_path, "/gritic_non", proj, ".csv"), row.names = FALSE)
          
}

dim(non_filter[["tcga"]])
dim(non_filter[["icgc"]])
all_non <- rbind(non_filter[["tcga"]], non_filter[["icgc"]]) 
icgc_APM <- non_filter[["icgc"]] %>% filter(Hugo_Symbol %in% APM)
tcga_APM <- non_filter[["tcga"]] %>% filter(Hugo_Symbol %in% APM)
all_APM <- rbind(icgc_APM, tcga_APM)
length(unique(all_APM$sample_id))

write.csv(all_non, file = paste0(gritic_path, "/gritic_non.csv"), row.names = FALSE)
```

## Combine GRITIC and MutationTimeR
```r
rm(list = ls())

library("data.table")
library("dplyr")
library("tidyr")
## wgd_median
wgd_median <- fread("../Project/PCAWG/MHC_evolution/GRITIC/output/wgd_median.csv") %>% 
  dplyr::select(wgd_mediantiming, sample_id)

##
projects <- c("tcga", "icgc")

for (proj in projects) {
  
  mutimeR_path <- paste0("/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/subclonal_reconstruction/20170325_consensus_subclonal_reconstruction_beta1.", proj, "/")
  file_list <- list.files(path = mutimeR_path, pattern = "_mutation_timing.txt.gz", full.names = TRUE)
  
  tryCatch({
    for (file in file_list) {
      file_name <- basename(file)
      id <- gsub("_mutation_timing.txt.gz", "", file_name)
      print(id)
      mutimR <- fread(file) %>% 
        mutate(chromosome = as.character(chromosome),
               position = as.numeric(position),
               sample_id = id) %>% 
        filter(mut_type == "SNV")
      
      gritic_path <- paste0("../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
      gritic_maf_file <- paste0(gritic_path, "/", id, "_mafgritic.csv")
      mutimR_gritic_path <- paste0("../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/mutationTimeR_gritic")
      
      if (!file.exists(gritic_maf_file)) {
        message("Skipping ", id, ": gritic_maf file does not exist.")
        next
      }
      
      gritic_maf <- read.csv(gritic_maf_file, header= T) %>%
        mutate(chromosome = as.character(CHROM),
               position = as.numeric(POS),
               sample_id = id)
      
      cols_to_rename <- which(names(gritic_maf) %in% as.character(0:249))
      names(gritic_maf)[cols_to_rename] <- paste0("X", names(gritic_maf)[cols_to_rename])
      
      mutimR_gritic <- left_join(gritic_maf, mutimR, by = c("sample_id", "chromosome", "position")) %>% 
        left_join(., wgd_median, by = "sample_id")
      write_csv(mutimR_gritic, paste0(mutimR_gritic_path, "/", id, "_mutimR_gritic.csv"))
    }
  }, error = function(e) {
    message("Error processing ", id, ": ", e$message)
  })
}

# Combine
file_list <- list.files(path = mutimR_gritic_path, pattern = "_mutimR_gritic.csv", full.names = TRUE)

timing_gritic_list <- list()

for (file in file_list) {
  file_name <- basename(file)
  id <- gsub("_mutimR_gritic.csv", "", file_name)
  timing_gritic_list[[id]] <- fread(file) 
}

data_all <- rbindlist(timing_gritic_list, fill = TRUE)
  
data_all <- data_all %>% mutate(interval = percentile_90 - percentile_10,
  timing_gritic90 = ifelse(percentile_90 < 0.7, "clonal[early]", "NA"))
write_csv(data_all, paste0(mutimR_gritic_path, "/griticnon_filter_mutimR.csv"))

table(data_all$timing_gritic90, data_all$timing)
table(data_all$timing)
table(data_all$timing_gritic)
```

# ----------------------------------------------------------
# 2. Prepare input for GRITIC
# ----------------------------------------------------------
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

PCAWG_path <- "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG"
path_griticinput <- paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/240820/")

purity <- fread(paste0(PCAWG_path, "/consensus_cnv/consensus.20170217.purity.ploidy.txt")) %>% 
  mutate(wgd = ifelse(wgd_status == "wgd", "T","F")) %>%
  left_join(., LOH_mut, by = "samplename") %>%
  dplyr::select(samplename, purity, wgd, dcc_project_code)
write.table(purity, file = paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/purity.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

projects <- c("tcga", "icgc")

HLAmut_path <- "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/HLAmutations/"
maf_HLAmut <- fread(paste0(HLAmut_path, "/results/maf_HLAmut.maf")) %>% mutate(Chromosome = sub("chr", "", Chromosome))
hla_sampleid <- unique(maf_HLAmut$Tumor_Sample_Barcode)

for (proj in projects) {
  #proj = "tcga"
  # SNV
  maf_gritic_path <- paste0("/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/vcf/", proj, "_filtered/annoted/")
  maf_files <- list.files(path = maf_gritic_path, pattern = ".consensus.20160830.filtered.somatic.snv_mnv.maf", recursive = FALSE, full.names = TRUE)
  out_path <- paste0("../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  
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

PCAWG_path <- "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG"
path_griticinput <- paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/240820/")

LOH_mut <- fread(paste0(PCAWG_path, "/donors_and_biospecimens/LOH_MSI.csv")) %>% mutate(samplename = aliquot_id) %>% 
  dplyr::select(samplename, dcc_project_code, histology_abbreviation)

purity <- fread(paste0(PCAWG_path, "/consensus_cnv/consensus.20170217.purity.ploidy.txt")) %>% 
  mutate(wgd = ifelse(wgd_status == "wgd", "T","F")) %>%
  left_join(., LOH_mut, by = "samplename") %>%
  dplyr::select(samplename, purity, wgd, dcc_project_code, histology_abbreviation) %>% inner_join(subclone_all, by = "samplename")
write.table(purity, file = paste0(PCAWG_path, "/MHC_evolution/GRITIC/input/purity.txt"), sep = "\t", row.names = FALSE, quote = FALSE)

projects <- c("tcga", "icgc")

HLAmut_path <- "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/HLAmutations/"
maf_HLAmut <- fread(paste0(HLAmut_path, "/results/maf_HLAmut.maf")) %>% mutate(Chromosome = sub("chr", "", Chromosome))
hla_sampleid <- unique(maf_HLAmut$Tumor_Sample_Barcode)

subclone_all <- data.frame()

for (proj in projects) {
  
  subclone_path <- paste0(PCAWG_path, "/subclonal_reconstruction/20170325_consensus_subclonal_reconstruction_beta1.", proj)
  subclone_files <- list.files(path = subclone_path, pattern = "_subclonal_structure.txt.gz", recursive = FALSE, full.names = TRUE)
  out_path <- paste0("../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/", proj)
  
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

# ----------------------------------------------------------
# 3. Test for Update version
# ----------------------------------------------------------
```sh
module load python/3.11.3
pip install pandas --user
pip install networkx --user
pip install matplotlib --user
pip install gritic --user
pip install numba --user

cd ../Project/PCAWG/MHC_evolution/GRITIC
griticnew_path="/rsrch6/home/hema_bio-Malignan/xxx/gritic"

tumour_id="0009b464-b376-4fbc-8a56-da538269a02f"
samplename="0009b464-b376-4fbc-8a56-da538269a02f"
purity="0.6"
wgd="T"
type="BLCA-US"
dcc_project_code="BLCA-US"
complextiming_path="../Project/PCAWG/MHC_evolution/GRITIC/"
output="../Project/PCAWG/MHC_evolution/GRITIC/output/"

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

### Convert VCF to DataFrame
```python
python

import os
import pysam
import pandas as pd

vcf_path = "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/all/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485.consensus.20160830.somatic.snv_mnv.vcf.gz"

vcf = pysam.VariantFile(vcf_path)
print(vcf)

    vcf_data = []
    for record in vcf:
        vcf_data.append([record.chrom, record.pos, record.id, record.ref, record.alts, record.qual, record.filter, record.info])

    vcf_df = pd.DataFrame(vcf_data, columns=["CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO"])
    vcf_df['ALT'] = vcf_df['ALT'].str[0]
    vcf_df['FILTER'] = vcf_df['FILTER'].astype(str)
    unique_filter_values = vcf_df['FILTER'].unique()
    print("Unique values in the FILTER column:", unique_filter_values)
    vcf_df = vcf_df[vcf_df["FILTER"] == "."]

pkl_path = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/PCAWG_SNV_Test_mut_clean_autosome_relabelled/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485.pkl.bz2"

# Read the .pkl file
pkl_df = pd.read_pickle(pkl_path)
print(pkl_df)

# Read the .pkl file
import bz2
import pickle

pkl_path = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/PCAWG_SNV_Test_mut_clean_autosome_relabelled/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485.pkl.bz2"

output_tsv_path = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485_gritic.tsv"

with bz2.BZ2File(pkl_path, 'rb') as file:
    decompressed_data = file.read()

data = pickle.loads(decompressed_data)

if isinstance(data, pd.DataFrame):
    # Print the number of indices
    print("Number of indices:", len(data.index))
else:
    print("The loaded data is not a pandas DataFrame.")

df = pd.DataFrame(data)
df.to_csv(output_tsv_path, sep='\t', index=False)
```

```sh
## Check one sample
module load python/2.7.18

ls -lh ../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/PCAWG_SNV_Test_mut_clean_autosome_relabelled/
```

```python
python

import os
import pysam
import pandas as pd

vcf_path = "/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/all/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485.consensus.20160830.somatic.snv_mnv.vcf.gz"

vcf = pysam.VariantFile(vcf_path)
print(vcf)

# Convert VCF to DataFrame
    vcf_data = []
    for record in vcf:
        vcf_data.append([record.chrom, record.pos, record.id, record.ref, record.alts, record.qual, record.filter, record.info])

    vcf_df = pd.DataFrame(vcf_data, columns=["CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO"])
    vcf_df['ALT'] = vcf_df['ALT'].str[0]
    vcf_df['FILTER'] = vcf_df['FILTER'].astype(str)
    unique_filter_values = vcf_df['FILTER'].unique()
    print("Unique values in the FILTER column:", unique_filter_values)
    vcf_df = vcf_df[vcf_df["FILTER"] == "."]

pkl_path = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/PCAWG_SNV_Test_mut_clean_autosome_relabelled/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485.pkl.bz2"

# Read the .pkl file
pkl_df = pd.read_pickle(pkl_path)
print(pkl_df)

# Read the .pkl file
import bz2
import pickle

pkl_path = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/PCAWG_SNV_Test_mut_clean_autosome_relabelled/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485.pkl.bz2"

output_tsv_path = "../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/0a6be23a-d5a0-4e95-ada2-a61b2b5d9485_gritic.tsv"

with bz2.BZ2File(pkl_path, 'rb') as file:
    decompressed_data = file.read()

data = pickle.loads(decompressed_data)

if isinstance(data, pd.DataFrame):
    # Print the number of indices
    print("Number of indices:", len(data.index))
else:
    print("The loaded data is not a pandas DataFrame.")

df = pd.DataFrame(data)
df.to_csv(output_tsv_path, sep='\t', index=False)
```

```sh
# Directories to compare
dir1="../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/"
dir2="../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timingnew/"

# Output file
output_file="../Project/PCAWG/MHC_evolution/GRITIC/output/comparison_results.txt"

# Create or clear the output file
touch $output_file
ls -lh $output_file

# Loop through files in dir1
for file in "$dir1"/*_griticsnv.txt; do
    filename=$(basename "$file")
    
    # Check if the file exists in dir2
    if [ -f "$dir2/$filename" ]; then
        # Compare the file from dir1 to the file with the same name in dir2
        if diff -q "$file" "$dir2/$filename" > /dev/null; then
            # Files are the same
            echo "$filename: Identical" >> $output_file
        else
            # Files are different
            echo "$filename: Different" >> $output_file
        fi
    else
        # File does not exist in dir2
        echo "$filename: Missing in dir2" >> $output_file
    fi
done

echo "Comparison complete. Results are in $output_file"

## Compare lines
input="../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/fe083d2d-d088-4ac1-825b-8c5bbfe974ac_griticsnv.txt"
output="../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timing/fe083d2d-d088-4ac1-825b-8c5bbfe974ac_griticsnv.txt"
standard="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_icgc.public//snv_mnv/fe083d2d-d088-4ac1-825b-8c5bbfe974ac.consensus.20160830.somatic.snv_mnv.vcf.gz"
zcat ${input} | awk -F'\t' 'BEGIN {OFS="\t"} /^#/ {print; next} $7 == "." {print}' | bgzip -c > ${output}

ls -lh ${input}
ls -lh ${output}
ls -lh ${standard}

wc -l ${input}
wc -l ${output}
wc -l ${standard}

cp /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/final_consensus_passonly.snv_mnv_indel.icgc.public/single/* /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/final_consensus_passonly.snv_mnv_indel_maf

mkdir -p /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/final_consensus_passonly.snv_mnv_indel_maf
mkdir -p ../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timingnew/maf_gritic

### 
#!/bin/bash

# Define the directory containing the txt files
directory="../Project/PCAWG/MHC_evolution/GRITIC/output/SNV_timingnew"

# Define the output file
output_file="../Project/PCAWG/MHC_evolution/GRITIC/output/line_new.txt"

# Initialize or clear the output file
> "$output_file"

# Find all txt files, count lines, and append the results to the output file
find "$directory" -type f -name "*_griticsnv.txt" -exec wc -l {} + | awk '{print $2 ": " $1}' >> "$output_file"

echo "Line counts have been written to $output_file"
```

### 2. filter/single maf
```sh
#BSUB -W 3:00
#BSUB -q short
#BSUB -o /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/file_compar
#BSUB –cwd /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/file_compar
#BSUB –u xxx@mdanderson.org
#BSUB -n 6
#BSUB -M 32
#BSUB -R rusage[mem=32]
#BSUB -J Print_line
#BSUB -P MHC_PCAWG

module load python
base_path="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/"
compar_path="${base_path}/file_compar"

projects=("icgc" "tcga")
for proj in "${projects[@]}"; do
   filter_maf="${base_path}/final_consensus_12oct/${proj}_filtered/annoted"
   single_maf="${base_path}/final_consensus_passonly.snv_mnv_indel.${proj}.public/single"
   gritic_maf="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/snv_indel/final_consensus_snv_indel_passonly_${proj}_annoted/annoted/"
  
  for file in ${filter_maf}/*.maf; do
        total_lines=$(cat $file | wc -l)
        variant_lines=$(grep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_${proj}_filteredmaf.txt

  for file in ${single_maf}/*.maf; do
        total_lines=$(cat $file | wc -l)
        variant_lines=$(grep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_${proj}_singlemaf.txt

  for file in ${gritic_maf}/*.maf; do
        total_lines=$(cat $file | wc -l)
        variant_lines=$(grep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_${proj}_griticmaf.txt
        
done

### 3. Print the numbers of index of .pkl.bz2
gritic_path="../Project/PCAWG/MHC_evolution/GRITIC/output/Timing"
for file in ${gritic_path}/*pkl_transposed.txt; do
        total_lines=$(cat $file | wc -l)
        variant_lines=$(grep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_pkl_transposed.txt

gritic_path="../Project/PCAWG/MHC_evolution/GRITIC/output/Timing"
for file in ${gritic_path}/*_vcf.txt; do
        total_lines=$(cat $file | wc -l)
        variant_lines=$(grep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_mergeold_vcf.txt

### 4.
scp xxx@ldragon2:../Project/PCAWG/MHC_evolution/GRITIC/output/filtered/mutationTimeR_gritic/mutimR_gritic_nonsyn.csv /Users/xxx/Desktop/PCAWG/MHC_evolution/GRITIC/output/filter
```

# ----------------------------------------------------------
# 4.Run GRITIC-SNV
# ----------------------------------------------------------
## Update the brunch
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

## Test code
```sh
cd /Users/xxx/Desktop/gritic_snv

gritic="/Users/xxx/Desktop/GRITIC/gritic/"
gritic_snv="/Users/xxx/Desktop/GRITIC/gritic_snv/"

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

cd /rsrch6/home/hema_bio-Malignan/xxx/GRITIC/gritic_snv
gritic="/rsrch6/home/hema_bio-Malignan/xxx/GRITIC/gritic/"
gritic_snv="/rsrch6/home/hema_bio-Malignan/xxx/GRITIC/gritic_snv/"

python ${gritic}/rungritic_cmd.py --mutation_table ./test_data/test_mutation_table.tsv \
                        --copy_number_table ./test_data/test_cn_table.tsv \
                        --subclone_table ./test_data/test_subclone_table.tsv \
                        --sample_id test \
                        --purity 0.7 \
                        --output ./test_gritic

python ${gritic_snv}/run_snv_timing.py --sample_id test --input_dir ./test_gritic/ --output_dir ./test_snvs
```

# ----------------------------------------------------------
# 5. Loop for all samples
# ----------------------------------------------------------
```sh
#BSUB -W 240:00
#BSUB -q e40long
#BSUB –cwd ../Project/PCAWG/MHC_evolution/GRITIC/
#BSUB –u xxx@mdanderson.org
#BSUB -n 12
#BSUB -M 400
#BSUB -R rusage[mem=400]
#BSUB -P PCAWG
#BSUB -J GRITIC
#BSUB -o /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/GRITICsnv_output.log
#BSUB -e /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/GRITICsnv_error.log

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
gritic="/rsrch6/home/hema_bio-Malignan/xxx/GRITIC/250/gritic"
gritic_snv="/rsrch6/home/hema_bio-Malignan/xxx/GRITIC/250/gritic_snv"

path_gritic="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/"
path_in="${path_gritic}/input/"

datasets=("snv" "snv_indel")

types=("Bone-Osteosarc" "Bladder-TCC" "Bone-Benign" "Bone-Epith" "Breast-LobularCA" "Breast-DCIS" "Biliary-AdenoCA" "Cervix-SCC" "Cervix-AdenoCA" "Lymph-CLL" "Myeloid-MPN" "Myeloid-AML" "Myeloid-MDS" "ColoRect-AdenoCA" "Lymph-BNHL" "Prost-AdenoCA" "Eso-AdenoCA" "Stomach-AdenoCA" "CNS-GBM" "Head-SCC" "Kidney-ChRCC" "Kidney-RCC" "CNS-Oligo" "Liver-HCC" "Lung-AdenoCA" "Lung-SCC" "Ovary-AdenoCA" "Panc-AdenoCA" "Panc-Endocrine" "CNS-Medullo" "CNS-PiloAstro" "SoftTissue-Leiomyo" "SoftTissue-Liposarc" "Thy-AdenoCA" "Uterus-AdenoCA" "Skin-Melanoma" "Breast-AdenoCA")

types="ColoRect-AdenoCA"

for type in "${types[@]}"; do
  
  paired_list="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/input/purity_${type}.txt"

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

# ----------------------------------------------------------
## 6. Update results by Polysolver
# ----------------------------------------------------------
```sh
paired_list="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/input/250306/purity_icgc.txt"
path_GRITIC="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/"

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
#BSUB –cwd /rsrch6/scratch/hema_bio-Malignan/xxx/TCGA/MHC_evolution/
#BSUB –u xxx@mdanderson.org
#BSUB -n 12
#BSUB -M 500
#BSUB -R rusage[mem=500]
#BSUB -P PCAWG
#BSUB -J PCAWG_GRITIC
#BSUB -o /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/pcawg_output_%J.log
#BSUB -e /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/pcawg_error_%J.log

path_code="/rsrch6/home/hema_bio-Malignan/xxx/code/source/"
path_input="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/input/250306/"
path_output="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/"
project="pcawg"
path_anno="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/consensus_snv_indel/vcf_HLAupdated/${project}/sample_updatedHLA/"

projects=("pcwag")
job_num=5
timelimit=no
time=30

paired_list="/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/input/250306/purity_${project}.txt"
bash ${path_code}/GRITIC_loop.sh ${path_input} ${path_output} ${path_anno} ${job_num} ${projects} ${timelimit} ${time} ${paired_list}

bsub < /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/griticHLA250613.lsf


## Copy to a folder
find /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/ \
  -type f -name "*mafnongritic.csv"|wc -l

find /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/ \
  -type f -name "*mafnongritic.csv" \
  -exec cp {} /rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/summary250717 \;

scp -r xxx@seadragon:/rsrch6/scratch/hema_bio-Malignan/xxx/PCAWG/MHC_evolution/GRITIC/output/HLAupdate/250708/snv/subclonal/summary250717 /Users/xxx/Desktop/PCAWG/MHC_evolution/GRITIC/output/250717/dict
```