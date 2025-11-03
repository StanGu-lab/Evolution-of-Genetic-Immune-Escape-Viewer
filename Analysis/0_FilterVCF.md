# Check data and Filter the vcf
## ----------------------------------------------------------
## 1. Check MAF files
## ----------------------------------------------------------
```sh
## Check maf files
cd ../Project/wchen20/PCAWG/consensus_snv_indel/maf

## ICGC
wc -l ../Project/wchen20/PCAWG/consensus_snv_indel/maf/final_consensus_passonly.snv_mnv_indel.icgc.public/final_consensus_passonly.snv_mnv_indel.icgc.public.maf
23159592 final_consensus_passonly.snv_mnv_indel.icgc.public.maf

## TCGA
wc -l ../Project/wchen20/PCAWG/consensus_snv_indel/maf/final_consensus_passonly.snv_mnv_indel.tcga.public/final_consensus_passonly.snv_mnv_indel.tcga.controlled.maf
29504369 final_consensus_passonly.snv_mnv_indel.tcga.controlled.maf

## PCAWG -- combined from maf files
wc -l ../Project/wchen20/PCAWG/consensus_snv_indel/maf/pcawg.maf
52663960 pcawg.maf

## MAF from VCF
wc -l ../Project/wchen20/PCAWG/consensus_snv_indel/maf_from_vcf/maf_snv_mnv_vcf.maf
48554013 
wc -l ../Project/wchen20/PCAWG/consensus_snv_indel/maf_from_vcf/maf_indel_vcf.maf
3916183
wc -l ../Project/wchen20/PCAWG/consensus_snv_indel/maf_from_vcf/maf_pcawg_vcf.maf
52470195
```

## ----------------------------------------------------------
## 2. Filter the vcf
## ----------------------------------------------------------
```sh
## !!!!! In the TCGA-vcf files, there are unfiltered variants in the files. before perform the analysis, the unpassed variants to be filtered.
module load htslib

## Filter variants in the vcf file
projects=("icgc" "tcga")
lists=("whitelist" "graylist")
datasets="indel"

for proj in "${projects[@]}"; do
   for data in "${datasets[@]}"; do
    for list in "${lists[@]}"; do

   directory="../Project/wchen20/PCAWG/consensus_snv_indel/vcf/${proj}/${list}/${data}/"
   out_path="../Project/wchen20/PCAWG/consensus_snv_indel/vcf/${proj}_filtered/${list}/${data}/"

   mkdir -p "$out_path"
  for input in "$directory"/*.vcf.gz; do
    # Extract the filename without the extension
    filename=$(basename "$input" .somatic.${data}.vcf.gz)
    
    # Define the output file path
    output="$out_path/${filename}.filtered.somatic.${data}.vcf.gz"
    
    # Filter the VCF file and save the output
    zcat ${input} | awk -F'\t' 'BEGIN {OFS="\t"} /^#/ {print; next} $7 == "." {print}' | bgzip -c > ${output} 
    
    done
    done
done

for vcf_file in "$out_path"/*.filtered.somatic.${data}.vcf.gz; do
    echo "Indexing $vcf_file..."
    tabix -p vcf "$vcf_file"
done

echo "Indexing complete."

## Check the vcf and maf
base_path="../Project/wchen20/PCAWG/consensus_snv_indel/"
compar_path="${base_path}/file_compar"
mkdir -p ${compar_path}

projects=("icgc" "tcga")

for proj in "${projects[@]}"; do
   filter="${base_path}/final_consensus_12oct/${proj}_filtered/snv_mnv"
   passonly="${base_path}/final_consensus_snv_indel_passonly_${proj}.public/snv_mnv/"
  
  for file in ${filter}/*.vcf.gz; do
        total_lines=$(zcat $file | wc -l)
        variant_lines=$(zgrep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_${proj}_filtered.txt

  for file in ${passonly}/*.vcf.gz; do
        total_lines=$(zcat $file | wc -l)
        variant_lines=$(zgrep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
        done > ${compar_path}/lines_${proj}_passonly.txt
done
```

## ----------------------------------------------------------
## 3. Check APM mutations
## ----------------------------------------------------------
```r
## There are some differences in the annotation by VCF2MAF and the origninal MAF
APM <- c("HLA-A", "HLA-B", "HLA-C","B2M", "NLRC5", "TAP1", "TAP2" ,"TAPBP", "PSMB8", "PSMB9", "PSMB10", "ERAP1", "ERAP2")
APM_noHLA <- c("B2M", "NLRC5", "TAP1", "TAP2" ,"TAPBP", "PSMB8", "PSMB9", "PSMB10", "ERAP1", "ERAP2")

onco_APM_update <- fread("../Project/PCAWG/consensus_snv_indel/maf/maf_from_vcf/onco_APM_update.txt")%>% 
  column_to_rownames("V1") %>% 
  as.matrix()

APM_mut <- as.data.frame(onco_APM_update) %>%
  rownames_to_column("Gene") %>%
  pivot_longer(-Gene, names_to = "aliquot_id", values_to = "Status") %>%
  filter(Status %in% c("Multi_Hit", non_syn)) %>%
  filter(aliquot_id %in% sample_MSS)
APM_sample <- unique(APM_mut$aliquot_id)

data <- fread("../Project/PCAWG/MHC_evolution/GRITIC/output/250717/summary250717/timing_APM.csv") %>% 
  filter(!aliquot_id %in% APM_sample) 
sample_more <- unique(data$aliquot_id)

table(data$Hugo_Symbol, data$CHROM)

APM <- fread("../Project/PCAWG/consensus_snv_indel/maf/maf_pcawg_APM.maf") %>% 
  filter(Variant_Classification %in% non_syn, Tumor_Sample_Barcode %in% sample_MSS, Hugo_Symbol %in% APM_noHLA)

length(unique(APM$Tumor_Sample_Barcode))

onco_consensus <- fread("../Project/PCAWG/consensus_snv_indel/maf/onco_matrix_APM_consensus.txt") %>% 
  filter(!V1 %in% c("HLA-A", "HLA-B", "HLA-C")) %>% 
  column_to_rownames("V1") %>% 
  as.matrix() %>%
  as.data.frame(.) %>%
  rownames_to_column("Gene") %>%
  pivot_longer(-Gene, names_to = "aliquot_id", values_to = "Status") %>%
  filter(Status != "", Status != "0")  %>% filter(aliquot_id %in% sample_MSS)
length(unique(onco_consensus$aliquot_id))

maf <- fread("../Project/PCAWG/consensus_snv_indel/maf/maf_pcawg_check.maf") %>% filter(Hugo_Symbol %in% APM_noHLA)

selected_sample <- c("5fd77ba9-5015-4d8b-86a0-582e5c76bdd6", "ef673d3d-2031-4036-ba25-4bc7ef04075b", "b86e88e7-0d5f-4b32-a35f-dc97251ab990", 
                     "06ecd127-6c24-422d-a7fb-bf5aee1b8b7b", "692dfa4f-45e5-4183-b5da-6650a1fbcabd", "81cc0f39-6677-4f2e-9a75-d30152b188f3",
                     "108749d2-5c62-4ef1-92df-aec6941ba53b", "8ca665f8-fe78-48bf-8c0f-c606d92885d4", "6f981023-4269-4e8e-a4ab-2c92bb27273c",
                     "5ab6a1d3-76f8-45d4-a430-d9831daa9ec4", "d432e99a-67fb-4609-b90f-99438eee7cae")

selected_sample <- unique(data$icgc_specimen_id)

maf <- fread("../Project/pancan_pcawg_2020/data_mutations.txt") %>% 
  filter( Tumor_Sample_Barcode %in% selected_sample, Hugo_Symbol %in% APM_noHLA)

maf <- fread("../Project/PCAWG/consensus_snv_indel/maf/maf_from_vcf/maf_pcawg_vcf_check.maf") %>% 
  filter( Tumor_Sample_Barcode %in% selected_sample, Hugo_Symbol %in% APM_noHLA, Variant_Classification %in% non_syn)
```

## ----------------------------------------------------------
## 4. Check GRITIC
## ----------------------------------------------------------
```sh
## VCF for GRITIC
../Project/wchen20/PCAWG/consensus_snv_indel/all/

## VCF downloaded from TCGA -- final_consensus_12oct
/rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv

diff ../Project/wchen20/PCAWG/consensus_snv_indel/all/00493087-9d9d-40ca-86d5-936f1b951c93.consensus.20160830.somatic.snv_mnv.vcf.gz /rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv/00493087-9d9d-40ca-86d5-936f1b951c93.consensus.20160830.somatic.snv_mnv.vcf.gz

diff ../Project/wchen20/PCAWG/consensus_snv_indel/all/00493087-9d9d-40ca-86d5-936f1b951c93.consensus.20160830.somatic.snv_mnv.vcf.gz ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_tcga.public/snv_mnv/fc812906-d3b6-4210-a842-96f095280cbf.consensus.20160830.somatic.snv_mnv.vcf.gz 

cd /rsrch6/scratch/reflib/ICGC-PCAWG/
ls -lh /rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv
find /rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv -name "*snv_mnv.vcf.gz"

## MD5 for GRITIC
find ../Project/wchen20/PCAWG/consensus_snv_indel/all/ -name "*snv_mnv.vcf.gz" -exec md5sum {} + > ../Project/wchen20/PCAWG/pcawg_snv_mnv_md5sums.txt

## MD5 -- VCF downloaded from TCGA -- final_consensus_12oct
find /rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv -name "*snv_mnv.vcf.gz" -exec md5sum {} + > ../Project/wchen20/PCAWG/tcga_snv_mnv_md5sums.txt

## MD5 --VCF downloaded from TCGA -- final_consensus_snv_indel_passonly_tcga.public
find ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_tcga.public/snv_mnv/ -name "*snv_mnv.vcf.gz" -exec md5sum {} + > ../Project/wchen20/PCAWG/tcgapassonly_snv_mnv_md5sums.txt

## Check the line for VCF downloaded from TCGA -- final_consensus_12oct
### lines 
zcat /rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv/fc8130e0-ad8a-b832-e040-11ac0d485e14.consensus.20160830.somatic.snv_mnv.vcf.gz | wc -l
### variants
zgrep -vc "^#" /rsrch6/scratch/reflib/ICGC-PCAWG/pcawg-tcga/consensus_snv_indel/final_consensus_12oct/tcga/snv_mnv/fc8130e0-ad8a-b832-e040-11ac0d485e14.consensus.20160830.somatic.snv_mnv.vcf.gz

## Check the VCF -- final_consensus_snv_indel_passonly_tcga.public
### lines 
zcat ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_tcga.public/snv_mnv/00493087-9d9d-40ca-86d5-936f1b951c93.consensus.20160830.somatic.snv_mnv.vcf.gz | wc -l
### variants
zgrep -vc "^#" ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_tcga.public/snv_mnv/00493087-9d9d-40ca-86d5-936f1b951c93.consensus.20160830.somatic.snv_mnv.vcf.gz

## Check md5sum with ICGC portal
md5sum ../Project/wchen20/PCAWG/final_consensus_snv_indel_tcga.controlled.tgz
cc18938fd676caa6bccbd9ba46bfb3e5 final_consensus_snv_indel_tcga.controlled.tgz

ls -lh ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_icgc.public/snv_mnv/
ls -lh ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_snv_indel_passonly_tcga.public/snv_mnv/

## Check lines for filtered output
cd ../Project/wchen20/PCAWG/consensus_snv_indel/final_consensus_12oct/tcga_filtered/snv_mnv

for file in *.vcf.gz; do
        total_lines=$(zcat $file | wc -l)
        variant_lines=$(zgrep -vc "^#" $file)
        echo "$file, $total_lines, $variant_lines"
done > ../Project/wchen20/PCAWG/lines_tcga_filtered.txt
```
