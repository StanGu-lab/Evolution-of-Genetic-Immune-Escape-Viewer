# Convert VCF to MAF files
```sh
#BSUB -W 24:00
#BSUB -q medium
#BSUB –cwd ../Project/PCAWG/consensus_snv_indel/
#BSUB –u wchen20@mdanderson.org
#BSUB -n 24
#BSUB -M 256
#BSUB -R rusage[mem=256]
#BSUB -P MHC_Evolution
#BSUB -J vcf2maf
#BSUB -o ../Project/PCAWG/consensus_snv_indel/

#module load singularity/3.7.0
#module load htslib

base_path="../Project/PCAWG/consensus_snv_indel/vcf/"
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

    singularity exec -C -B /rsrch6/:/mnt -B /home/wchen20/:/data /rsrch6/home/hema_bio-Malignan/wchen20/vcf2maf/vcf2maf.sif \
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
