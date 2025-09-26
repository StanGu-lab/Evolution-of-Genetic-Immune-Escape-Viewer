# helpers.R
# Set CRAN mirror explicitly (required on shinyapps.io)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Activate renv
#renv::activate()

# Load libraries
# Define all the packages you want to load
# Install missing packages and load them
library(patchwork)

libs <- c(
  "broom", "shiny", "ggplot2", "dplyr", "tidyr", "data.table", 
   "stringr", "ggradar", "scales", "gridExtra", "ggsci", "patchwork", 
  "maftools", "ggdist", "ggthemes", "ggrepel", "plotly", "textshape", 
  "survival", "survminer", "purrr", "shinycssloaders", "grid", "fst"
)

# Install missing packages and load them
load_or_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    if (pkg == "maftools") {
      if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
      BiocManager::install(pkg, update = FALSE, ask = FALSE)
    } else {
      install.packages(pkg, dependencies = TRUE)
    }
    suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))
  }
}

invisible(lapply(libs, load_or_install))

# For tidyquant: install if missing, but do NOT load
if (!requireNamespace("tidyquant", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("mdancho84/tidyquant")
}

if (!requireNamespace("ggradar", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("ricardo-bion/ggradar")
}

base_path <- getwd()
data_path <- file.path(base_path, "data")

##################################
# Read Data
##################################
# Load data
load_data_files <- function() {
  list(
    long_data = data.table::fread(file.path(data_path, "CRISPR/GeneMatrix_long.csv")),
    CRISPR_pathways = data.table::fread(file.path(data_path, "CRISPR/C5BP_selected_list.csv")),
    CRISPR_all = data.table::fread(file.path(data_path, "CRISPR/CRISPR_all.csv")),
    ICB_merge = readRDS(file.path(data_path, "ICB/ICB_merge.rds")),
    ICB_surv = readRDS(file.path(data_path, "ICB/ICB_survival.rds")),
    data_crispr = data.table::fread(file.path(data_path, "CRISPR/C5BP_all.csv")),
    data_rra = data.table::fread(file.path(data_path, "CRISPR/RRAcombined_fdr.csv")),
    MouseToHuman_gene = data.table::fread(file.path(data_path, "Refence_data/Referecen_MouseToHuman_gene.csv")),
    driver_list = fread(file.path(data_path, "Refence_data/TableS1_compendium_mutational_drivers_clean.csv"))
  )
}

data_list <- load_data_files()
list2env(data_list, envir = .GlobalEnv)
MouseGene <- MouseToHuman_gene$id

# Function to load cohort-specific data
# ---- fst-only readers -------------------------------------------------------
# Internal: read a set of part files deterministically and bind
.read_fst_parts <- function(cohort_path, prefix, suffix, columns = NULL) {
  # expected names: e.g., diff_all_PCAWG_part01.fst
  files <- list.files(
    cohort_path,
    pattern    = paste0("^", prefix, "_", suffix, "_part\\d+\\.fst$"),
    full.names = TRUE
  )
  if (!length(files)) {
    stop("No .fst parts found for ", prefix, " / ", suffix, " in: ", cohort_path)
  }
  
  # natural order by the numeric partXX
  ord <- order(as.integer(sub(".*_part(\\d+)\\.fst$", "\\1", basename(files))))
  files <- files[ord]
  
  parts <- lapply(files, function(f) {
    dt <- fst::read_fst(f, columns = columns)   # select columns if provided
    as.data.table(dt)
  })
  rbindlist(parts, use.names = TRUE, fill = TRUE)
}

# Public wrappers
read_diff_all_fst     <- function(cohort_path, suffix, columns = NULL)
  .read_fst_parts(cohort_path, "diff_all",     suffix, columns)

read_diff_allgene_fst <- function(cohort_path, suffix, columns = NULL)
  .read_fst_parts(cohort_path, "diff_allgene", suffix, columns)

dynamic_cohort_data <- function(cohort) {
  suffix <- switch(
    cohort,
    "PCAWG"         = "PCAWG",
    "TCGA-OV"       = "TCGA_OV",
    "POG570"        = "POG570",
    "CRC-Prognosis" = "CRC_Prognosis",
    "TNBC"          = "TNBC",
    "Glioma"        = "Glioma",
    stop("Unknown cohort")
  )
  
  cohort_path <- file.path(data_path, "Cohorts", suffix)
  
  list(
    # fst-only reads; pass `columns = c("col1","col2",...)` if you want even faster loads
    diff_all     = read_diff_all_fst(cohort_path, suffix),
    diff_allgene = read_diff_allgene_fst(cohort_path, suffix),
    
    # unchanged bits
    ciber_all     = data.table::fread(file.path(cohort_path, paste0("ciber_", suffix, ".csv"))),
    maf_data      = readRDS(file.path(cohort_path, paste0("maf_object_", suffix, ".rds"))),
    clinical_data = data.table::fread(file.path(cohort_path, paste0("clinical_", suffix, ".csv"))),
    surv_data     = data.table::fread(file.path(cohort_path, paste0("survival_", suffix, ".csv"))),
    Freq_all      = data.table::fread(file.path(cohort_path, paste0("oncoMSS_", suffix, ".csv")))
  )
}

load_clinical_only <- function(cohort) {
  suffix <- switch(
    cohort,
    "PCAWG"         = "PCAWG",
    "TCGA-OV"       = "TCGA_OV",
    "POG570"        = "POG570",
    "CRC-Prognosis" = "CRC_Prognosis",
    "TNBC"          = "TNBC",
    "Glioma"        = "Glioma",
    stop("Unknown cohort")
  )
  
  cohort_path <- file.path(data_path, "Cohorts", suffix)
  
  list(
    clinical_data = data.table::fread(file.path(cohort_path, paste0("clinical_", suffix, ".csv")))
  )
}

load_escape_data <- function(cohort) {
  suffix <- switch(
    cohort,
    "PCAWG"         = "PCAWG",
    "TCGA-OV"       = "TCGA_OV",
    "POG570"        = "POG570",
    "CRC-Prognosis" = "CRC_Prognosis",
    "TNBC"          = "TNBC",
    "Glioma"        = "Glioma",
    stop("Unknown cohort")
  )
  
  cohort_path <- file.path(data_path, "Cohorts", suffix)
  
  list(
    diff_all = read_diff_all_fst(cohort_path, suffix),
    clinical_data = data.table::fread(file.path(cohort_path, paste0("clinical_", suffix, ".csv"))),
    ciber_all = data.table::fread(file.path(cohort_path, paste0("ciber_", suffix, ".csv"))),
    surv_data = data.table::fread(file.path(cohort_path, paste0("survival_", suffix, ".csv"))),
    Freq_all  = data.table::fread(file.path(cohort_path, paste0("oncoMSS_", suffix, ".csv")))
  )
}

load_gene_explore_data <- function(cohort) {
  suffix <- switch(
    cohort,
    "PCAWG"         = "PCAWG",
    "TCGA-OV"       = "TCGA_OV",
    "POG570"        = "POG570",
    "CRC-Prognosis" = "CRC_Prognosis",
    "TNBC"          = "TNBC",
    "Glioma"        = "Glioma",
    stop("Unknown cohort")
  )
  
  cohort_path <- file.path(data_path, "Cohorts", suffix)
  
  list(
    diff_allgene  = read_diff_allgene_fst(cohort_path, suffix),
    ciber_all     = data.table::fread(file.path(cohort_path, paste0("ciber_", suffix, ".csv"))),
    maf_data      = readRDS(file.path(cohort_path, paste0("maf_object_", suffix, ".rds"))),
    clinical_data = data.table::fread(file.path(cohort_path, paste0("clinical_", suffix, ".csv"))),
    surv_data     = data.table::fread(file.path(cohort_path, paste0("survival_", suffix, ".csv")))
  )
}

##################################
# Set cell type list
##################################
cell_type_files <- c(
  "MHC-I Regulators" = "MHCregulators",
  "T-cell Killing" = "Tcells",
  "NK-cell Killing" = "NKcells",
  "Macrophage Killing" = "Macrophages",
  "GDT-cell Killing" = "GammadeltaTcells"
)

##################################
# Set Colors
##################################
type_colors <- c("Biliary-AdenoCA" = "#00CD66", "Bladder-TCC" = "#EEAD0E", "Bone-Osteosarc" = "#FFD700", "SoftTissue-Leiomyo" = "#FFEC8B", 
                   "Bone-Benign" = "#F0EE60", "Bone-Epith" = "#ADAC44", "SoftTissue-Liposarc" = "#CDCB50", "Breast-AdenoCA" = "#CD6090", 
                   "Breast-LobularCA" = "#F095BD", "Cervix-SCC" = "#79CDCD", "CNS-Medullo" = "#D8BFD8", "CNS-PiloAstro" = "#B0B0B0", 
                   "CNS-GBM" = "#3D3D3D", "CNS-Oligo" = "#787878", "ColoRect-AdenoCA" = "#191970", "Eso-AdenoCA" = "#1E90FF", "Head-SCC" = "#8B2323", 
                   "Kidney-RCC" = "#FF4500", "Kidney-ChRCC" = "#B32F0B", "Liver-HCC" = "#006400", "Lung-SCC" = "#f7d99f", "Lung-AdenoCA" = "#E41A1C", 
                   "Lymph-BNHL" = "#698B22", "Lymph-CLL" = "#F4A35D", "Myeloid-MPN" = "#ffc100", "Myeloid-AML" = "#CD6600", "Ovary-AdenoCA" = "#008B8B", 
                   "Panc-AdenoCA" = "#7A378B", "Panc-Endocrine" = "#E066FF", "Prost-AdenoCA" = "#87CEFA", "Skin-Melanoma" = "#000000", 
                   "Stomach-AdenoCA" = "#BFEFFF", "Thy-AdenoCA" = "#9370DB", "Uterus-AdenoCA" = "#FF8C69")

cols = c("Missense_Mutation" = "#377EB8", "Splice_Site" = "#beaed4", "Nonsense_Mutation" = "#4DAF4A", "Frame_Shift_Del" = "#a6d854",
         "Frame_Shift_Ins" = "#e78ac3", "Multi_Hit" = "#8B2323", "Translation_Start_Site" = "#1E90FF", "In_Frame_Del" = "#00CD66",
         "In_Frame_Ins" = "#EEAD0E", "Nonstop_Mutation" = "#fc8d62", "Complex Event" = "#66CCFF", "Amp" = "red", "Del" = "blue", "HLALOH" = "#CD6090")

colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF",
  "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
  "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
  "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"
)

prim_colors <- c("#984EA3", "#377EB8","#00CD66")
MSI_colors <- c("#984EA3", "#377EB8")
Gender_colors <- c("#984EA3", "#377EB8")

## Color platette for complexheatmap
col = c("Missense_Mutation" = "#377EB8", "Splice_Site" = "#beaed4", "Nonsense_Mutation" = "#4DAF4A","Frame_Shift_Del" = "#a6d854", "Frame_Shift_Ins" = "#e78ac3",
        "Multi_Hit" = "#8B2323", "Translation_Start_Site" = "#1E90FF", "In_Frame_Del" = "#00CD66", "In_Frame_Ins" = "#EEAD0E", "Nonstop_Mutation" = "#fc8d62", "Complex Event" = "#66CCFF", 
        "Amp" = "red", "Del" = "blue", "HLALOH" = "#CD6090")

cell_type_files <- c(
  "MHC-I Regulators" = "MHCregulators",
  "T-cell Killing" = "Tcells",
  "NK-cell Killing" = "NKcells",
  "Macrophage Killing" = "Macrophages",
  "GDT-cell Killing" = "GammadeltaTcells"
)

cellline_colors <- c(
  "AML"             = "#000C95",  # placeholder (gray, since not in legend)
  "B cell lymphoma" = "#7876B1FF",  # placeholder
  "Breast cancer"   = "#20854EFF",
  "CML"             = "#CC347C",
  "CRC"             = "#2FACCF",
  "Gastric cancer"  = "#E18727FF",  # placeholder
  "GBM"             = "#6D3000",
  "GSC"             = "#999933",  # placeholder
  "Melanoma"        = "#1E90FF",
  "PDAC"            = "#7A378B",
  "Renal cancer"    = "#BFEFFF"
)

cibercom_cols <- c("T_cells_CD8", "T_cells_CD4", "B_cells", "NK_cells", "Macrophage", "Dendritic_cells", "Mast_cells", "Neutrophils", "Eosinophils")

##################################
# Cleaning Text and Colors
##################################
# 1.Insert line break every 5 words
insert_newlines_every_five_words2 <- function(s) {
  words <- strsplit(s, " ")[[1]]
  paste(sapply(seq_along(words), function(i) {
    if (i %% 5 == 0) return(paste0(words[i], "\n")) else return(words[i])
  }), collapse = " ")
}

rename_list <- data.table::fread(file.path(data_path, "/CRISPR/rename_list.csv")) %>%
  mutate(pathway = str_to_title(pathway),
         pathway_new = sapply(pathway_new, insert_newlines_every_five_words2))

cell_type_files <- c(
  "MHC-I Regulators" = "MHCregulators",
  "T-cell Killing" = "Tcells",
  "NK-cell Killing" = "NKcells",
  "Macrophage Killing" = "Macrophages",
  "GDT-cell Killing" = "GammadeltaTcells"
)

##################################
# Plotting
##################################
## Page 2 Plot Study Frequencies for Pathways
plot_crispr_dotplots <- function(dat, cell_type) {
  # Positive regulators
  CRISPR_pos <- dat %>% filter(Regulators == "pos")
  study_number_pos <- unique(CRISPR_pos$total_study)
  order_pos <- CRISPR_pos %>%
    distinct(pathway_new, .keep_all = TRUE) %>%
    arrange(Freq) %>%
    pull(pathway_new)
  CRISPR_pos$pathway_new <- factor(CRISPR_pos$pathway_new, levels = order_pos)

  # Negative regulators
  CRISPR_neg <- dat %>% filter(Regulators == "neg")
  study_number_neg <- unique(CRISPR_neg$total_study)
  order_neg <- CRISPR_neg %>%
    distinct(pathway_new, .keep_all = TRUE) %>%
    arrange(Freq) %>%
    pull(pathway_new)
  CRISPR_neg$pathway_new <- factor(CRISPR_neg$pathway_new, levels = order_neg)

  # Plot for Pos
  plot1 <- ggplot(CRISPR_pos, aes(x = pathway_new, y = logp, fill = Count)) +
    geom_point(shape = 21, stroke = 0.5, size = 3) +
    coord_flip() +
    scale_fill_gradient(low = "#66AAD4", high = "#08306B") +
    geom_text(aes(label = Freq, y = max(logp) + 1), size = 4) +
    ggtitle(paste0(cell_type, " - Sensitivity\n(n = ", study_number_pos, ")")) +
    theme_minimal(base_size = 12) +
    labs(x = "", y = "-Log(p.adj)") +
      theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 12, color = "black"),
            axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 12, color = "black"),
            legend.position = "right",
            legend.key.size = unit(0.5, "cm"), 
            legend.text = element_text(size = 6), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

  # Plot for Neg
  plot2 <- ggplot(CRISPR_neg, aes(x = pathway_new, y = logp, fill = Count)) +
    geom_point(shape = 21, stroke = 0.5, size = 3) +
    coord_flip() +
    scale_fill_gradient(low = "#66AAD4", high = "#08306B") +
    geom_text(aes(label = Freq, y = max(logp) + 1), size = 4) +
    ggtitle(paste0(cell_type, " - Resistance\n(n = ", study_number_neg, ")")) +
    theme_minimal(base_size = 12) +
    labs(x = "", y = "-Log(p.adj)") +
    theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 12, color = "black"),
            axis.text.y = element_text(hjust = 1, vjust = 0.5, size = 12, color = "black"),
            legend.position = "right",
            legend.key.size = unit(0.5, "cm"), 
            legend.text = element_text(size = 6), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

  # Return list of plots
  list(pos = plot1, neg = plot2)
}

## 2. Plot Mutation Frequencies for Pathways
# Radar plot function
plot_radar_all <- function(Freq_all, cell_type) {
  # Compute common cancer types with Freq_mut > 5 in both pos & neg
  Freq_pos <- Freq_all %>%
    filter(cell_type == !!cell_type, regulator == "pos", Freq_mut > 5) %>%
    mutate(histology_abbreviation = as.character(histology_abbreviation)) %>%
    pull(histology_abbreviation) %>%
    unique()
  
  Freq_neg <- Freq_all %>%
    filter(cell_type == !!cell_type, regulator == "neg", Freq_mut > 5) %>%
    mutate(histology_abbreviation = as.character(histology_abbreviation)) %>%
    pull(histology_abbreviation) %>%
    unique()
  
  Type_com <- intersect(Freq_pos, Freq_neg)
  
  plot_radar <- function(data, cell_type, reg, Type_com) {
    Freq_sub <- data %>%
      filter(cell_type == !!cell_type, regulator == !!reg) %>%
      filter(histology_abbreviation %in% !!Type_com)
    
    data <- Freq_sub %>%
      mutate(pathway = pathway_new) %>%
      dplyr::select(-cell_type, -regulator, -Sum, -Freq_mut, -pathway_new, -percent_whole)
    
    wide_data <- data %>%
      tidyr::pivot_wider(names_from = histology_abbreviation, values_from = percent_mut) %>%
      mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
      mutate(pathway = sapply(pathway, insert_newlines_every_five_words2)) %>%
      dplyr::select(pathway, where(is.numeric))
    
    scale_settings <- list(
      MHCregulators_pos  = c("0%", "20%", "40%", 0, 0.2, 0.4),
      MHCregulators_neg  = c("0%", "30%", "60%", 0, 0.3, 0.6),
      Tcells_pos         = c("0%", "15%", "40%", 0, 0.15, 0.4),
      Tcells_neg         = c("0%", "25%", "50%", 0, 0.25, 0.5),
      NKcells            = c("0%", "15%", "30%", 0, 0.15, 0.3),
      GammadeltaTcells   = c("0%", "7.5%", "15%", 0, 0.075, 0.15),
      Macrophages        = c("0%", "15%", "45%", 0, 0.15, 0.45)
    )
    
    key <- if (cell_type == "MHCregulators" || cell_type == "Tcells") {
      paste0(cell_type, "_", reg)
    } else {
      cell_type
    }
    
    settings <- scale_settings[[key]]
    
    suppressMessages(ggradar(wide_data,
            values.radar = settings[1:3],
            grid.min = as.numeric(settings[4]),
            grid.mid = as.numeric(settings[5]),
            grid.max = as.numeric(settings[6]),
            base.size = 10,
            grid.line.width = 0.3,
            grid.label.size = 4,
            group.line.width = 1,
            gridline.label.offset = 0.05,
            group.point.size = 1.5,
            axis.label.size = 3, # text size
            axis.label.offset = 0.9,
            legend.position = "bottom",
            background.circle.colour = "white",
            gridline.min.colour = "black",
            gridline.mid.colour = "black",
            gridline.max.colour = "black",
            plot.extent.x.sf = 1,
            plot.extent.y.sf = 1
    ) + scale_color_npg() +
      guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
      theme(
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.2, "cm"),
        legend.key.height = unit(0.15, "cm"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.margin = margin(t = 1, b = 1),
        legend.box.spacing = unit(0.1, "cm")
      ))
  }
  
  list(pos = plot_radar(Freq_all, cell_type, "pos", Type_com) +
         ggtitle("Positive Regulation") +
         theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)),
       neg = plot_radar(Freq_all, cell_type, "neg", Type_com) +
         ggtitle("Negative Regulation") +
         theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )
  
}


## Plotting KM plots
grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

## Plotting Timing
data_legend <- data.frame(value = c(0, 0.5, 1),
                          group = c("A", "B", "C"))

range_breaks <- seq(0, 1, length.out = 251)
bin_edges <- seq(0, 1, length.out = 251)

timing_bin_levels <- paste0("(", head(bin_edges, -1), ",", tail(bin_edges, -1), "]")
timing_bin_levels[1] <- paste0("[", bin_edges[1], ",", bin_edges[2], "]")

color_summary <- c("Early" = "#60c4a5", "Undetermined" = "#bdcddb", "Late" = "#ad88c2")
color_bin <- c("Early" = "#2e7d64", "Undetermined" = "grey95", "Late" = "#7b5094")

color_palette <- colorRampPalette(color_bin)(250)
color_vector <- setNames(color_palette, timing_bin_levels)

## Plotting Timing Distribution
plot_legend <- ggplot(data_legend, aes(x = group, y = value, fill = value)) + 
  geom_point(alpha = 0) +  # Invisible points to trigger the legend
  scale_fill_gradient2(
    low = "#2e7d64", 
    high = "#7b5094", 
    mid = "grey95", 
    midpoint = 0.50,
    breaks = c(0, 1),  # Breaks at 0 and 1
    labels = c("Early", "Late"),  # Custom labels for the legend
    name = "Probability"  # Set the legend title
  ) +
  theme_void() +  # Remove all axes and grid lines
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# Extract the legend from plot_legend
legend <- suppressMessages(cowplot::get_legend(plot_legend))

# Timing colnames
column_names <- paste0("X", 0:249)

# Extract the legend from plot_legend
legend <- suppressMessages(cowplot::get_legend(plot_legend))

data_legend <- data.frame(value = c(0, 0.5, 1),
                          group = c("A", "B", "C"))

range_breaks <- seq(0, 1, length.out = 251)
bin_edges <- seq(0, 1, length.out = 251)

timing_bin_levels <- paste0("(", head(bin_edges, -1), ",", tail(bin_edges, -1), "]")
timing_bin_levels[1] <- paste0("[", bin_edges[1], ",", bin_edges[2], "]")

color_summary <- c("Early" = "#60c4a5", "Undetermined" = "#bdcddb", "Late" = "#ad88c2")
color_bin <- c("Early" = "#2e7d64", "Undetermined" = "grey95", "Late" = "#7b5094")

color_palette <- colorRampPalette(color_bin)(250)
color_vector <- setNames(color_palette, timing_bin_levels)

data_legend <- data.frame(value = c(0, 0.5, 1),
                          group = c("A", "B", "C"))

plot_timing_summary <- function(diff_all, driver_list, clinical_data, type, cell_type,
                                cutoff_mut = 0.6, cutoff_pathway = 0.5,
                                cutoff_ratio = 2, mode = 2, num_filter = 4) {
  
  drivergene_cancer <- driver_list %>% filter(grepl(type, Tissue)) %>% pull(Gene)
  data_driver <- diff_all %>% filter(histology_abbreviation == !!type, pathway %in% drivergene_cancer) %>% filter(!is.na(mean_diff)) 
  
  if (length(cell_type > 1)) {
    data_reg <- diff_all %>% filter(histology_abbreviation == !!type, celltype %in% !!cell_type) %>% filter(!is.na(mean_diff))
    data_APM <- diff_all %>% filter(histology_abbreviation == !!type, pathway == "APM") %>% filter(!is.na(mean_diff))
    if(nrow(data_APM) > 0) {
      data_nondriver <- rbind(data_reg, data_APM)
    } else {
      data_nondriver <- data_reg
    }
    
  } else {
    data_nondriver <- diff_all %>% filter(histology_abbreviation == !!type, celltype %in% !!cell_type) %>% filter(!is.na(mean_diff)) 
  }
  
  data_all <- rbind(data_driver, data_nondriver)
  pathway_list <- data_all %>% distinct(pathway, regulator, .keep_all = F)
  
  data_filter1 <- data_all %>% group_by(pathway, histology_abbreviation) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup() %>%
    inner_join(., data_all, by = c("histology_abbreviation", "pathway")) %>%
    mutate(n_base = 1,
           percent_base = n_base/count,
           n_late = 250 * late_ratio,
           n_undetermined = 250 * undetemined_ratio,
           ratio_plot = (n_late + n_undetermined/2) / 250,
           timing_bin = cut(ratio_plot, breaks = range_breaks, include.lowest = TRUE)) %>% 
    arrange(desc(count))
  
  data_filter <- data_filter1 %>% filter(timing_cat != "Undetermined") %>%
    group_by(pathway) %>% 
    summarise(Sum_determined = n(), .groups = 'drop') %>% 
    ungroup %>% 
    inner_join(data_filter1, ., by = "pathway") %>%
    filter(Sum_determined > 4)
  
  height = 7
  
  if (type == "Liver-HCC") {
    
    data_filter <- data_filter %>% filter(!pathway %in% c("AXIN1", "CTNNB1", "SETDB1", "APOB", "NFE2L2", "ARID2", "MACF1", "ASH1L"))
    
  } else {
    
    data_filter <- data_filter
    
  }
  
  Freq <- as.data.frame(table(data_filter$pathway, data_filter$timing_cat))
  
  if (nrow(Freq) > 2 ) {
    
    Freq <- Freq %>% setNames(c("pathway", "time_period", "Freq")) 
    type_sum <- as.data.frame(table(clinical_data$histology_abbreviation)) %>% filter(Var1 == !!type) %>% pull(Freq)
    
    Sum <- as.data.frame(table(data_filter$pathway)) %>% 
      mutate(Type_sum = type_sum) %>% 
      setNames(c("pathway", "Sum", "Sum_type")) %>% 
      mutate(Prevelance = Sum/Sum_type) 
    
    data_prop <- inner_join(Freq, Sum, by = "pathway") %>% mutate(Proportion = Freq/Sum) %>% inner_join(pathway_list, by = "pathway")
    
    if (nrow(data_filter) > 1) {
      
      ratio <- data_prop %>% filter(time_period != "Undetermined") %>%
        group_by(pathway, time_period) %>% 
        summarise(sum_freq = sum(Freq), .groups = 'drop') %>% 
        pivot_wider(names_from = time_period, values_from = sum_freq) %>% 
        mutate(ratio = (Late + 1)/(Early + 1)) %>% 
        inner_join(., data_prop, by = "pathway") %>%
        distinct(pathway, .keep_all = TRUE) %>% 
        mutate(pro_early = Early/Sum,
               pro_late = Late/Sum) %>% 
        arrange(desc(ratio)) 
      
      order_all <- ratio %>% arrange(desc(ratio)) %>% pull(pathway)
      
      prop_early <- data_prop %>% filter(time_period == "Early", Proportion >= cutoff_pathway) %>% arrange(desc(Proportion), pathway) %>% pull(pathway) %>% as.character() %>% unique()
      prop_late <- data_prop %>% filter(time_period == "Late", Proportion >= cutoff_pathway) %>% arrange(desc(Proportion), pathway) %>% pull(pathway) %>% as.character() %>% unique()
      prop_undetermined <- data_prop %>% filter(!pathway %in% c(prop_early, prop_late), time_period == "Early") %>% arrange(desc(Proportion), pathway) %>% pull(pathway) %>% as.character() %>% unique()
      
      ratio_early <- ratio %>% filter(ratio < 1/cutoff_ratio) %>% arrange(ratio) %>% pull(pathway) %>% as.character() %>% unique()
      ratio_late <- ratio %>% filter(ratio > cutoff_ratio) %>% arrange(ratio) %>% pull(pathway) %>% as.character() %>% unique()
      ratio_undetermined <- ratio %>% filter(!pathway %in% c(ratio_early, ratio_late)) %>% arrange(ratio, desc(pathway)) %>% pull(pathway) %>% as.character() %>% unique()
      
      ratio_earlydriver <- ratio %>% filter(ratio < 1/cutoff_ratio, regulator == "Driver") %>% arrange(ratio) %>% pull(pathway) %>% as.character() %>% unique()
      ratio_latedriver <- ratio %>% filter(ratio > cutoff_ratio, regulator == "Driver") %>% arrange(ratio) %>% pull(pathway) %>% as.character() %>% unique()
      ratio_undetermineddriver <- ratio %>% filter(!pathway %in% c(ratio_early, ratio_late), regulator == "Driver") %>% arrange(ratio, desc(pathway)) %>% pull(pathway) %>% as.character() %>% unique()
      
      ratio_earlypathway <- ratio %>% filter(ratio < 1/cutoff_ratio, regulator != "Driver") %>% arrange(ratio) %>% pull(pathway) %>% as.character() %>% unique()
      ratio_latepathway <- ratio %>% filter(ratio > cutoff_ratio, regulator != "Driver") %>% arrange(ratio) %>% pull(pathway) %>% as.character() %>% unique()
      ratio_undeterminedpathway <- ratio %>% filter(!pathway %in% c(ratio_early, ratio_late), regulator != "Driver") %>% arrange(ratio, desc(pathway)) %>% pull(pathway) %>% as.character() %>% unique()
      
      if (mode == 1) {
        
        order_early <- prop_early
        order_late <- prop_late
        order_undetermined <- prop_undetermined
        
      }
      
      if (mode == 2) {
        
        order_early <- ratio_early
        order_late <- ratio_late
        order_undetermined <- ratio_undetermined
        
        order_earlydriver <- ratio_earlydriver
        order_latedriver <- ratio_latedriver
        order_undetermineddriver <- ratio_undetermineddriver
        
        order_earlypathway <- ratio_earlypathway
        order_latepathway <- ratio_latepathway
        order_undeterminedpathway <- ratio_undeterminedpathway
        
        order_all <- ratio %>% arrange(desc(ratio)) %>% pull(pathway)
      }
      
      if (mode == 3) {
        
        order_early <- intersect(prop_early, ratio_early)
        order_late <- intersect(prop_late, ratio_late)
        order_undetermined <- setdiff(order_all, union(order_early, order_late))
        
      }
      
      ## Plot for proportion
      data_bar <- data_filter %>% filter(!is.na(late_ratio))  %>% 
        mutate(pathway = factor(pathway, levels = order_all),
               timing_bin = factor(timing_bin, levels = rev(timing_bin_levels)))
      
      plot_bar <- ggplot(data_bar, aes(x = pathway, fill = timing_bin)) +
        geom_bar(position = "fill") +
        theme_minimal() +
        labs(x = "",  y = "", fill = "Timing Probability", title = "") +
        scale_fill_manual(values = color_vector, drop = FALSE) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 16, colour = "black", hjust = 1),
              plot.title = element_text(size = 20, vjust = 0.5),
              axis.title.y = element_text(size = 18),
              axis.title.x = element_text(size = 18),
              legend.text = element_blank(),  
              legend.title = element_text(size = 16),
              legend.position = "none",
              plot.margin = margin(0, 30, 5, 5), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        coord_flip()
      
      plot_bar_legend <- suppressMessages(cowplot::plot_grid(plot_bar, legend, ncol = 2, rel_widths = c(1, 0.1)))
      
      ratio_row <- ratio %>%
        mutate(timing_cat = case_when(
          ratio > 2 ~ "Late",
          ratio < 0.5 ~ "Early",
          TRUE ~ "Undetermined"),
          `Ratio(Log2)` = log2(ratio),
          pathway = factor(pathway, levels = order_all))
      
      max <- max(abs(ratio_row$`Ratio(Log2)`))
      
      plot_ratio <- ggplot(ratio_row, aes(x = `Ratio(Log2)`, y = pathway, fill = timing_cat)) +
        geom_point(size = 3, shape = 21, color = "black") +  # shape 21 allows for fill and color
        theme_bw() +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_text(size = 18),
              legend.title = element_blank(), 
              legend.text = element_text(size = 12),
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 18, colour = "black"),
              plot.margin = margin(18, 30, 5, 5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_fill_manual(values = color_bin) +  
        xlim(-(max + 0.5), max + 0.5) + 
        geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +  
        geom_vline(xintercept = -1, linetype = "dashed", color = "grey60")
      
      plot_barcom <- suppressMessages(cowplot::plot_grid(plot_bar, 
                                        legend, 
                                        plot_ratio,
                                        ncol = 3,  
                                        rel_widths = c(1, 0.1, 0.5)))
      
      order_earlydriver <- ratio_earlydriver
      order_latedriver <- ratio_latedriver
      order_undetermineddriver <- ratio_undetermineddriver
      
      early_textdriver <- paste("Driver gene\n", paste(order_earlydriver, collapse = "\n"))
      late_textdriver <- paste("Driver gene\n", paste(order_latedriver, collapse = "\n"))
      variable_textdriver <- paste("Driver gene\n", paste(order_undetermineddriver, collapse = "\n"))
      
      early_textpathway <- paste("Pathway\n", paste(order_earlypathway, collapse = "\n"))
      late_textpathway <- paste("Pathway\n", paste(order_latepathway, collapse = "\n"))
      variable_textpathway <- paste("Pathway\n", paste(order_undeterminedpathway, collapse = "\n"))
      
      if (length(order_earlydriver) > 0  & length(order_earlypathway) > 0) {
        early_text <- paste(early_textdriver, "\n", early_textpathway)
      } else if (length(order_earlydriver) == 0 & length(order_earlypathway) > 0) { 
        early_text <- early_textpathway
      } else if (length(order_earlydriver) > 0 & length(order_earlypathway) == 0) { 
        early_text <- early_textdriver
      } else {
        early_text <- "" 
      }
      
      if (length(order_latedriver) > 0  & length(order_latepathway) > 0) {
        late_text <- paste(late_textdriver, "\n", late_textpathway)
      } else if (length(order_latedriver) == 0 & length(order_latepathway) > 0) { 
        late_text <- late_textpathway
      } else if (length(order_latedriver) > 0 & length(order_latepathway) == 0) { 
        late_text <- late_textdriver
      } else {
        late_text <- "" 
      }
      
      if (length(order_undetermineddriver) > 0  & length(order_undeterminedpathway) > 0) {
        variable_text <- paste(variable_textdriver, "\n", variable_textpathway)
      } else if (length(order_undetermineddriver) == 0 & length(order_undeterminedpathway) > 0) { 
        variable_text <- variable_textpathway
      } else if (length(order_undetermineddriver) > 0 & length(order_undeterminedpathway) == 0) { 
        variable_text <- variable_textdriver
      } else {
        variable_text <- "" 
      }
      
      text_data <- data.frame(category = c("Early", "Undetermined", "Late"),
                              text = c(early_text, variable_text, late_text),
                              x = c(0.165, 0.495, 0.83),
                              y = c(0.5, 0.5, 0.5),
                              hjust = c(0.5, 0.5, 0.5),
                              vjust = c(0.5, 0.5, 0.5))
      
      title_data <- data.frame(x = c(0.165, 0.495, 0.83),
                               y = rep(0.95, 3),
                               label = c("Early", "Undetermined", "Late"))
      
      bg_data <- data.frame(
        xmin = c(0, 0.33, 0.66),
        xmax = c(0.33, 0.66, 1),
        ymin = c(0, 0, 0),
        ymax = c(1, 1, 1),
        fill = color_summary)
      
      # Create the base plot with colored background sections
      base_plot <- ggplot() +
        geom_rect(data = bg_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.9) +
        scale_fill_identity() +
        theme_void()
      
      for (i in 1:nrow(text_data)) {
        base_plot <- base_plot +
          annotate("text", x = text_data$x[i], y = text_data$y[i], label = text_data$text[i],
                   hjust = text_data$hjust[i], vjust = text_data$vjust[i], size = 5)
        
      }
      
      for (i in 1:nrow(title_data)) {
        base_plot <- base_plot +
          annotate("text", x = title_data$x[i], y = title_data$y[i], label = title_data$label[i],
                   hjust = 0.5, vjust = 1, size = 8, fontface = "bold")
      }
      
      plot_com2 <- plot_barcom / base_plot + patchwork::plot_layout(heights = c(4, 4)) +
        plot_annotation(
          title = paste0("Timeline of ", type),
          theme = theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5))) + 
          theme(plot.margin = margin(5, 5, 5, 5))

      return(plot_com2)
      rm(Freq)
      rm(data_bar)
      rm(plot_com1)
      rm(plot_group)
      rm(plot_bar)
    }
  }
}

## Timing plot for genes
plot_timing_bar_by_genes <- function(selected_genes, diff_data, type_list, column_names, timing_bin_levels, color_vector, legend) {
  df_Reg <- diff_data %>%
    filter(Hugo_Symbol %in% selected_genes) %>%
    group_by(sample_id) %>%
    summarise(across(all_of(column_names), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      pathway = "Reg_nonsyn",
      Hugo_Symbol = "Reg",
      early_ratio = rowMeans(dplyr::select(., all_of(column_names)) < 0, na.rm = TRUE),
      late_ratio = rowMeans(dplyr::select(., all_of(column_names)) > 0, na.rm = TRUE),
      undetemined_ratio = rowMeans(dplyr::select(., all_of(column_names)) == 0, na.rm = TRUE),
      timing_cat = case_when(
        early_ratio > 0.6 ~ "Early",
        late_ratio > 0.6 ~ "Late",
        TRUE ~ "Undetermined"
      )
    ) %>%
    dplyr::select(sample_id, Hugo_Symbol, pathway, everything()) %>%
    inner_join(type_list, by = "sample_id")
  
  data_filter1 <- df_Reg %>%
    group_by(pathway, histology_abbreviation) %>%
    summarise(count = n(), .groups = "drop") %>%
    inner_join(df_Reg, by = c("histology_abbreviation", "pathway")) %>%
    mutate(
      n_base = 1,
      percent_base = n_base / count,
      timing_bin = cut(late_ratio, breaks = range_breaks, include.lowest = TRUE)
    ) %>%
    arrange(desc(count)) %>%
    filter(count > 4)
  
  data_filter <- data_filter1 %>%
    filter(timing_cat != "Undetermined") %>%
    group_by(histology_abbreviation) %>%
    summarise(Sum_determined = n(), .groups = "drop") %>%
    inner_join(data_filter1, by = "histology_abbreviation")
  
  ratio <- as.data.frame(table(data_filter$histology_abbreviation, data_filter$timing_cat)) %>%
    setNames(c("histology_abbreviation", "time_period", "Freq")) %>%
    filter(time_period != "Undetermined") %>%
    group_by(histology_abbreviation, time_period) %>%
    summarise(sum_freq = sum(Freq), .groups = "drop") %>%
    pivot_wider(names_from = time_period, values_from = sum_freq) %>%
    mutate(
      ratio = (Late + 1) / (Early + 1),
      histology_abbreviation = as.character(histology_abbreviation)
    ) %>%
    arrange(desc(ratio))
  
  order_all <- ratio$histology_abbreviation
  
  data_bar <- data_filter %>%
    mutate(
      histology_abbreviation = factor(histology_abbreviation, levels = order_all),
      timing_bin = factor(timing_bin, levels = rev(timing_bin_levels))
    )
  
  if (nrow(data_bar) == 0) {
    grid::grid.newpage()
    grid::grid.text("No mutation timing was available in selected genes.", gp = grid::gpar(fontsize = 16))
    return(NULL)
  }
  
  timing_prop <- ggplot(data_bar, aes(x = histology_abbreviation, fill = timing_bin)) +
    geom_bar(position = "fill") +
    theme_minimal() +
    labs(x = "", y = "", fill = "Timing Probability", title = "") +
    scale_fill_manual(values = color_vector, drop = FALSE) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 16, colour = "black", hjust = 1),
      plot.title = element_text(size = 20, vjust = 0.5),
      axis.title.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),
      legend.text = element_blank(),
      legend.title = element_text(size = 16),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0, 30, 5, 5)
    ) +
    coord_flip()
  
  suppressMessages(cowplot::plot_grid(timing_prop, legend, ncol = 2, rel_widths = c(1.5, 0.3)))
}

## By regulators
 plot_timing_bar <- function(diff_data, selected_genes, type_list) {
    df_Reg <- diff_data %>%
      filter(Hugo_Symbol %in% selected_genes) %>%
      group_by(sample_id) %>%
      summarise(across(all_of(column_names), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
      mutate(pathway = "Reg_nonsyn",
             Hugo_Symbol = "Reg",
             early_ratio = rowMeans(dplyr::select(., all_of(column_names)) < 0, na.rm = TRUE),
             late_ratio = rowMeans(dplyr::select(., all_of(column_names)) > 0, na.rm = TRUE),
             undetemined_ratio = rowMeans(dplyr::select(., all_of(column_names)) == 0, na.rm = TRUE),
             timing_cat = case_when(early_ratio > 0.6 ~ "Early",
                                    late_ratio > 0.6 ~ "Late",
                                    TRUE ~ "Undetermined")) %>%
      dplyr::select(c("sample_id", "Hugo_Symbol", "pathway", everything())) %>%
      inner_join(type_list, by = "sample_id")
    
    data_filter1 <- df_Reg %>% group_by(pathway, histology_abbreviation) %>%
      summarise(count = n(), .groups = 'drop') %>%
      ungroup() %>%
      inner_join(., df_Reg, by = c("histology_abbreviation", "pathway")) %>%
      mutate(n_base = 1,
             percent_base = n_base/count,
             n_late = 250 * late_ratio,
             n_undetermined = 250 * undetemined_ratio,
             ratio_plot = (n_late + n_undetermined/2) / 250,
             timing_bin = cut(ratio_plot, breaks = range_breaks, include.lowest = TRUE)) %>% 
      arrange(desc(count)) %>%
      filter(count > 4)
    
    data_filter <- data_filter1 %>% filter(timing_cat != "Undetermined") %>%
      group_by(histology_abbreviation) %>%
      summarise(Sum_determined = n(), .groups = 'drop') %>%
      ungroup() %>%
      inner_join(data_filter1, ., by = "histology_abbreviation")
    
    ratio <- as.data.frame(table(data_filter$histology_abbreviation, data_filter$timing_cat)) %>%
      setNames(c("histology_abbreviation", "time_period", "Freq")) %>%
      filter(time_period != "Undetermined") %>%
      group_by(histology_abbreviation, time_period) %>%
      summarise(sum_freq = sum(Freq), .groups = 'drop') %>%
      pivot_wider(names_from = time_period, values_from = sum_freq) %>%
      mutate(ratio = (Late + 1)/(Early + 1),
             histology_abbreviation = as.character(histology_abbreviation)) %>%
      arrange(desc(ratio))
    
    order_all <- ratio %>% arrange(desc(ratio)) %>% pull(histology_abbreviation)
    
    data_bar <- data_filter %>%
      mutate(histology_abbreviation = factor(histology_abbreviation, levels = order_all),
             timing_bin = factor(timing_bin, levels = rev(timing_bin_levels)))
    
    if (nrow(data_bar) == 0) {
      grid::grid.newpage()
      grid::grid.text("No mutation timing was available in selected genes.", gp = grid::gpar(fontsize = 16))
      return()
    }
    
    timing_prop <- ggplot(data_bar, aes(x = histology_abbreviation, fill = timing_bin)) +
      geom_bar(position = "fill") +
      theme_minimal() +
      labs(x = "", y = "", fill = "Timing Probability", title = "") +
      scale_fill_manual(values = color_vector, drop = FALSE) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 16, colour = "black", hjust = 1),
            plot.title = element_text(size = 20, vjust = 0.5),
            axis.title.y = element_text(size = 18),
            axis.title.x = element_text(size = 18),
            legend.text = element_blank(),
            legend.title = element_text(size = 16),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(0, 30, 5, 5)) +
      coord_flip()
    
    suppressMessages(cowplot::plot_grid(timing_prop, legend, ncol = 2, rel_widths = c(1.5, 0.3)))
  }

## Plot Top Ranking
plot_selection <- function(gstable, rank_col, score_col_index, title_text, top_num = 10, color_offset = 0) {
  top_genes <- gstable %>% arrange(.data[[rank_col]]) %>% dplyr::slice(1:top_num) %>% pull(HumanGene)
  pvec <- gstable[[score_col_index]]
  names(pvec) <- gstable$HumanGene
  pvec <- sort(pvec)
  
  df <- data.frame(Gene = names(pvec), Score = pvec) %>%
    mutate(
      Rank = row_number(),
      isTarget = Gene %in% top_genes,
      Size = ifelse(Gene %in% top_genes, 3, 2),
      NegLogScore = -log10(Score)
    )
  
  color_map <- setNames(colors[(1:top_num) + color_offset], top_genes)
  
  ggplot(df, aes(x = Rank, y = NegLogScore)) +
    geom_line() +
    geom_point(data = df %>% filter(isTarget), aes(color = Gene, size = Size)) +
    scale_color_manual(values = color_map) +
    geom_text_repel(
      data = df %>% filter(isTarget),
      aes(label = Gene),
      size = 4,
      max.overlaps = 20,
      box.padding = 0.5,
      point.padding = 0.2,
      segment.color = "black",
      segment.size = 0.5
    ) +
    labs(
      title = title_text,
      x = "Genes",
      y = "-log10(RRA score)"
    ) +
    theme_minimal() +
    scale_size_identity() +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 12, color = "black"),
      axis.text.y = element_text(hjust = 1, size = 12, color = "black"),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(hjust = 0.5, size = 18, color = "black", face = "bold")
    )
}

## Plot crispr_heatmap
plot_crispr_heatmap <- function(long_data, selected_genes, selected_celltype) {
  long_data_filtered <- long_data %>%
    filter(celltype == selected_celltype)
  
  all_combos <- expand.grid(
    HumanGene = selected_genes,
    Study = unique(long_data_filtered$Study),
    stringsAsFactors = FALSE
  )
  
  filtered_data <- long_data_filtered %>%
    filter(HumanGene %in% selected_genes) %>%
    right_join(all_combos, by = c("HumanGene", "Study")) %>%
    mutate(Presence = ifelse(is.na(Presence), 0, Presence)) %>%
    group_by(HumanGene) %>%
    mutate(Freq = sum(Presence)) %>%
    ungroup() %>%
    mutate(HumanGene = factor(HumanGene, levels = unique(HumanGene[order(Freq)])))
  
  ggplot(filtered_data, aes(x = Study, y = HumanGene, fill = factor(Presence))) +
    geom_tile(color = "black", size = 0.25, linewidth = 0.2) +
    scale_fill_manual(
      name = "Top-Rank",
      values = c("-1" = "#4DBBD5FF", "0" = "white", "1" = "#E64B35FF"),
      labels = c("-1" = "Negative", "0" = "No", "1" = "Positive")
    ) +
    labs(x = "", y = "") +
    theme_minimal(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black", hjust = 1),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold"),
      axis.ticks = element_line(size = 0.4),
      plot.background = element_blank(),
      panel.border = element_blank()
    )
}

## Plot ICB response
plot_forest_by_genes <- function(df, candidate_genes, min_clip = 0.1, max_clip = 10) {
  
  par(mar = c(1, 1, 1, 1))  # Remove outer margins
  
  df <- df %>%
    mutate(Response_bin = ifelse(BinaryResponse == "R", 1, 0))
  
  mutated_samples <- df %>%
    filter(Hugo_Symbol %in% candidate_genes) %>%
    distinct(SampleName, Study) %>%
    mutate(Mutated = 1)
  
  sample_data <- df %>%
    distinct(SampleName, Study, Response_bin, TMB) %>%
    left_join(mutated_samples, by = c("SampleName", "Study")) %>%
    mutate(Mutated = ifelse(is.na(Mutated), 0, 1))
  
  logit_results <- sample_data %>%
    group_by(Study) %>%
    do({
      model <- tryCatch(
        suppressMessages(glm(Response_bin ~ Mutated + TMB, data = ., family = binomial)),
        error = function(e) return(NA)
      )
      if (inherits(model, "glm")) {
        tidy(model, conf.int = TRUE, exponentiate = TRUE)
      } else {
        tibble(term = "Mutated", estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
      }
    }) %>%
    filter(term == "Mutated") %>%
    ungroup() %>%
    mutate(p_adj = p.adjust(p.value, method = "fdr"))
  
  plot_data <- logit_results %>%
    mutate(
      estimate = ifelse(estimate < 0.01 | estimate > 100, NA, estimate),
      conf.low = ifelse(conf.low < 0.01 | conf.low > 100, NA, conf.low),
      conf.high = ifelse(conf.high < 0.01 | conf.high > 100, NA, conf.high)
    ) %>%
    transmute(
      Study,
      label = ifelse(is.na(estimate), "NA", sprintf("%.2f [%.2f–%.2f]", estimate, conf.low, conf.high)),
      OR = pmin(pmax(estimate, min_clip), max_clip),
      lower = pmin(pmax(conf.low, min_clip), max_clip),
      upper = pmin(pmax(conf.high, min_clip), max_clip)
    )
  
  # No header row
  tabletext <- cbind(plot_data$Study, plot_data$label)
  
  forestplot(
    labeltext = tabletext,
    mean = plot_data$OR,
    lower = plot_data$lower,
    upper = plot_data$upper,
    new_page = FALSE,
    graph.pos = 3,
    clip = c(min_clip, max_clip),
    xlog = TRUE,
    xlab = "OR (95% CI)",
    xticks = c(0.1, 0.5, 1, 2, 5, 10),
    xticks.digits = 1,
    lineheight = unit(4, "mm"),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.0),
      ticks = gpar(cex = 1.0),
      xlab = gpar(cex = 1.0)
    ),
    col = fpColors(box = "steelblue", line = "darkblue"),
    is.summary = rep(FALSE, nrow(plot_data)),
    boxsize = 0.3,
    vertices = TRUE
  )
}

plot_forest_gg <- function(df, candidate_genes, min_clip = 0.1, max_clip = 10) {
  df <- df %>%
    mutate(Response_bin = ifelse(BinaryResponse == "R", 1, 0))
  
  mutated_samples <- df %>%
    filter(Hugo_Symbol %in% candidate_genes) %>%
    distinct(SampleName, CohortName) %>%
    mutate(Mutated = 1)
  
  sample_data <- df %>%
    distinct(SampleName, CohortName, Response_bin, TMB) %>%
    left_join(mutated_samples, by = c("SampleName", "CohortName")) %>%
    mutate(Mutated = ifelse(is.na(Mutated), 0, 1)) %>% filter(!is.na(Response_bin)) %>%
    group_by(CohortName) %>%
    filter(length(unique(Response_bin)) > 1, length(unique(Mutated)) > 1) %>%
    ungroup()
  
  logit_results <- sample_data %>%
    group_by(CohortName) %>%
    do({
      model <- tryCatch(suppressMessages(glm(Response_bin ~ Mutated + TMB, data = ., family = binomial)),
                        error = function(e) return(NA))
      if (inherits(model, "glm")) {
        tidy(model, conf.int = TRUE, exponentiate = TRUE)
      } else {
        tibble(term = "Mutated", estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
      }
    }) %>%
    filter(term == "Mutated") %>%
    ungroup() %>%
    mutate(p_adj = p.adjust(p.value, method = "fdr")) %>%
    mutate(
      estimate = ifelse(estimate < 0.01 | estimate > 100, NA, estimate),
      conf.low = ifelse(conf.low < 0.01 | conf.low > 100, NA, conf.low),
      conf.high = ifelse(conf.high < 0.01 | conf.high > 100, NA, conf.high)
    ) %>%
    filter(!is.na(estimate) & !is.na(conf.low) & !is.na(conf.high))
  
  plot_data <- logit_results %>%
    mutate(
      OR_clipped = pmin(pmax(estimate, min_clip), max_clip),
      lower_clipped = pmin(pmax(conf.low, min_clip), max_clip),
      upper_clipped = pmin(pmax(conf.high, min_clip), max_clip),
      CohortName = factor(CohortName, levels = rev(unique(CohortName)))
    )
  
  ggplot(plot_data, aes(x = OR_clipped, y = CohortName)) +
    geom_point(shape = 15, size = 3, color = "steelblue") +
    geom_errorbarh(aes(xmin = lower_clipped, xmax = upper_clipped), height = 0.25, color = "darkblue") +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10(limits = c(min_clip, max_clip * 1.5), breaks = c(0.1, 0.5, 1, 2, 10)) +
    labs(x = "OR (95% CI)", y = NULL, title = NULL) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(hjust = 1, size = 10,color = "black"),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.title.x = element_text(size = 12, face = "bold", color = "black"),
      axis.line = element_line(color = "black"),
      plot.margin = margin(5, 5, 5, 5)
    )
}


## Plot Timing Density
plot_raincloud <- function(data, x = "histology_abbreviation", y = "mean_diff", mode = "mean", title, width = 4, height = 5) {
  
  x_sym <- ensym(x)
  y_sym <- ensym(y)
  
  if (mode == "mean") {
    
    ## Sort genes by mean timing
    mean_order <- data %>% 
      group_by(!!x_sym) %>%
      summarise(mean_diff_mean = mean(!!y_sym, na.rm = TRUE)) %>%
      arrange(desc(mean_diff_mean)) %>%
      pull(!!x_sym)
    
  } else if (mode == "median") {
    ## Sort genes by median timing
    mean_order <- data %>% 
      group_by(!!x_sym) %>%
      summarise(mean_diff_mean = median(!!y_sym, na.rm = TRUE)) %>%
      arrange(desc(mean_diff_mean)) %>%
      pull(!!x_sym)
    
  }
  
  max <- max(c(abs(max(data$mean_diff)), abs(min(data$mean_diff)))) + 0.02
  
  data <- data %>% mutate(!!x_sym := factor(!!x_sym, levels = mean_order))
  
  data_stats <- data %>%
    group_by(!!x_sym) %>%
    summarise(
      ymin = min(!!y_sym),
      lower = quantile(!!y_sym, 0.10),
      middle = mean(!!y_sym),
      upper = quantile(!!y_sym, 0.90),
      ymax = max(!!y_sym)) %>%
    mutate(mean_diff = middle)
  
  plot_cloudlrain <- data %>% 
    ggplot(aes(x = !!x_sym, y = !!y_sym, fill = !!x_sym)) +
    stat_halfeye(
      adjust = 2,
      justification = -0.2,
      .width = 0,
      point_colour = NA,
      scale = 0.9) +
    geom_boxplot(
      data = data_stats,
      aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
      stat = "identity",
      width = 0.2,
      outlier.color = NA,
      alpha = 0.5) +
    tidyquant::scale_fill_tq() +
    tidyquant::theme_tq() +
    labs(
      title = title,
      x = "",
      y = "Timing Difference",
      fill = "Cylinders"
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.8) +
    theme(
      axis.text.x = element_text(size = 16, color = "black"),
      axis.text.y = element_text(size = 16, color = "black"),
      plot.title = element_text(size = 18, color = "black", hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 16, colour = "black"),
      legend.position = "none",
      plot.margin = margin(b = 5, t = 5, l = 0, r = 10),
      base_family = "Helvetica"
    ) +
    ylim(-max, max) +
    coord_flip()
  
  return(plot_cloudlrain)
}


KM_plot <- function(df, surv_type = "OS", group_var = "Mutated") {

  surv_map <- list(
    OS = list(time = "OS_time", status = "OS_status"),
    PFS = list(time = "PFS_time", status = "PFS_status"),
    DFS = list(time = "DFS_time", status = "DFS_status")
  )
  
  # Extract relevant column names
  time_col <- surv_map[[surv_type]]$time
  status_col <- surv_map[[surv_type]]$status
  
  # Filter and rename
  df <- df %>%
    filter(!is.na(.data[[time_col]]), !is.na(.data[[status_col]]), !is.na(.data[[group_var]])) %>%
    rename(time = all_of(time_col), status = all_of(status_col))
  
  # Convert Mutated to WT/Mut if numeric
  df[[group_var]] <- factor(df[[group_var]], levels = c("WT", "Mut"))
  legend_labels <- levels(df[[group_var]])
  
  # Skip if not enough data
  if (length(unique(df[[group_var]])) < 2 || sum(df$status) < 1||nrow(df) < 20) {
    plot.new()
    text(0.5, 0.5, "Not enough data to plot", cex = 1.5)
    return()
  }
  
  # Static formula now works
  fit <- survfit(Surv(time, status) ~ df[[group_var]], data = df)
  
  # X-axis scale
  max_time <- ceiling(max(df$time, na.rm = TRUE) / 12) * 12
  interval <- ceiling(max_time / 60) * 12
  
  # Plot
  surv_plot <- ggsurvplot(
    fit,
    data = df,
    risk.table = TRUE,
    pval = TRUE,
    conf.int = FALSE,
    legend.title = group_var,
    legend.labs = legend_labels,
    palette = c("WT" = "#4DBBD5FF", "Mut" = "#E64B35FF"),
    xlab = "",
    break.x.by = interval,
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE
  )
  
  cowplot::plot_grid(
    surv_plot$plot +
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
      labs(title = if ("CohortName" %in% colnames(df)) unique(df$CohortName) else ""),
    surv_plot$table,
    align = "v",
    ncol = 1,
    rel_heights = c(4, 1.5)
  )
}

## 
plot_survival_by_timing <- function(filtered_data, wt_included = "no", title_prefix = "") {
  # WT logic
  if (wt_included == "no") {
    filtered_data <- filtered_data %>%
      filter(Timing_group != "WT") %>%
      mutate(Timing_group = factor(Timing_group, levels = c("Mut_Early", "Mut_Late")))
    
    cols_group <- c("Mut_Early" = "#2e7d64", "Mut_Late" = "#7b5094")
    legend_levels <- c("Mut_Early", "Mut_Late")
  } else {
    cols_group <- c("WT" = "#1f77b4", "Mut_Early" = "#2e7d64", "Mut_Late" = "#7b5094")
    legend_levels <- c("WT", "Mut_Early", "Mut_Late")
  }
  
  # Fit survival model
  fit <- survfit(Surv(OS_month, OS_status) ~ Timing_group, data = filtered_data)
  max_time <- ceiling(max(filtered_data$OS_month, na.rm = TRUE) / 12) * 12
  interval <- ceiling(max_time / 60) * 12
  x_text <- max_time / 2
  
  # P-values
  res <- pairwise_survdiff(Surv(OS_month, OS_status) ~ Timing_group, data = filtered_data)
  pvals <- round(res$p.value, 4)
  
  get_pval <- function(a, b, mat) {
    if (a %in% rownames(mat) && b %in% colnames(mat)) {
      val <- mat[a, b]
    } else if (b %in% rownames(mat) && a %in% colnames(mat)) {
      val <- mat[b, a]
    } else {
      val <- NA
    }
    if (is.na(val)) "NA" else format(val, digits = 2)
  }
  
  if (wt_included == "no") {
    pval_text <- paste("Mut_Early vs Mut_Late: ", get_pval("Mut_Early", "Mut_Late", pvals))
  } else {
    pval_text <- paste(
      "WT vs Mut_Early: ", get_pval("WT", "Mut_Early", pvals), "\n",
      "WT vs Mut_Late: ", get_pval("WT", "Mut_Late", pvals), "\n",
      "Mut_Early vs Mut_Late: ", get_pval("Mut_Early", "Mut_Late", pvals)
    )
  }
  
  # Plot
  surv_plot <- ggsurvplot(
    fit,
    data = filtered_data,
    risk.table = TRUE,
    pval = TRUE,
    conf.int = FALSE,
    legend.title = "",
    legend.labs = legend_levels,
    palette = cols_group,
    xlab = "Time (months)",
    break.x.by = interval,
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE
  )
  
  surv_plot$plot <- surv_plot$plot +
    #annotate("text", x = x_text, y = 0.95, label = pval_text, hjust = 0, vjust = 1, size = 4) +
    labs(title = "") +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  
  cowplot::plot_grid(
    surv_plot$plot,
    surv_plot$table,
    align = "v",
    ncol = 1,
    rel_heights = c(4, 1.5)
  )
}

## Plot of Ciber
plot_ciber <- function(ciber_long, histology_type, selected_pathways) {
  y_max <- max(ciber_long$Value_1, na.rm = TRUE)
  padding <- 0.1 * y_max
  
  ggplot(ciber_long, aes(x = cell_type, y = Value_1, fill = Timing_group)) +
    geom_boxplot(outlier.shape = NA, alpha = 1) +
    geom_jitter(
      color = "black",
      position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
      size = 1.5, alpha = 0.8, show.legend = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"),
      axis.title.y = element_text(size = 16),
      title = element_text(size = 19, face = "bold"),
      text = element_text(size = 16),
      axis.line = element_line(color = "black")
    ) +
    labs(x = "", y = "Score", fill = "Timing") +
    ggtitle(paste0(histology_type, " - ", selected_pathways)) +
    scale_fill_manual(values = c("Mut_Early" = "#2e7d64", "Mut_Late" = "#7b5094")) +
    stat_compare_means(
      aes(group = Timing_group),
      method = "wilcox.test",
      label = "p.signif",
      size = 6
    ) +
    coord_cartesian(ylim = c(0, y_max + padding))
}


run_robust_logistic <- function(data) {
  # Check if we have enough data and variation
  if (nrow(data) < 10) {
    return(tibble(
      term = "Mutated", 
      estimate = NA_real_, 
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_, 
      conf.high = NA_real_,
      issue = "insufficient_sample_size"
    ))
  }
  
  # Check for variation in response and predictor
  if (length(unique(data$Response_bin)) < 2 || 
      length(unique(data$Mutated)) < 2) {
    return(tibble(
      term = "Mutated", 
      estimate = NA_real_, 
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_, 
      conf.high = NA_real_,
      issue = "no_variation"
    ))
  }
  
  # Try to fit the model
  model <- tryCatch({
    glm(Response_bin ~ Mutated + TMB, data = data, family = binomial)
  }, error = function(e) {
    return(list(error = as.character(e)))
  }, warning = function(w) {
    # Capture warnings but still return the model
    model_result <- glm(Response_bin ~ Mutated + TMB, data = data, family = binomial)
    attr(model_result, "warning") <- as.character(w)
    return(model_result)
  })
  
  # Check if model fitting failed
  if (is.list(model) && !is.null(model$error)) {
    return(tibble(
      term = "Mutated", 
      estimate = NA_real_, 
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_, 
      conf.high = NA_real_,
      issue = paste("model_error:", model$error)
    ))
  }
  
  # Try to get confidence intervals with more robust method
  result <- tryCatch({
    # First try the standard approach
    tidy_result <- tidy(model, exponentiate = TRUE)
    
    # Try to get confidence intervals
    ci_result <- tryCatch({
      confint(model, level = 0.95)
    }, error = function(e) {
      # If profile CI fails, use Wald CI
      confint.default(model, level = 0.95)
    })
    
    # Exponentiate CI for odds ratios
    ci_result <- exp(ci_result)
    
    # Combine results
    tidy_result$conf.low <- ci_result[rownames(ci_result) == "Mutated", 1]
    tidy_result$conf.high <- ci_result[rownames(ci_result) == "Mutated", 2]
    
    return(tidy_result)
    
  }, error = function(e) {
    # If everything fails, just return basic results without CI
    tidy_basic <- tidy(model, exponentiate = TRUE)
    tidy_basic$conf.low <- NA_real_
    tidy_basic$conf.high <- NA_real_
    tidy_basic$issue <- paste("ci_error:", as.character(e))
    return(tidy_basic)
  })
  
  return(result)
}


plot_selection_rank <- function(gstable, rank_col, score_col_index, title_text,
                                top_num = 10, color_offset = 0, select_gene = NULL) {
  # top genes
  top_genes <- gstable %>%
    arrange(.data[[rank_col]]) %>%
    dplyr::slice(1:top_num) %>%
    pull(HumanGene)
  
  # add selected gene(s) to highlight list
  highlight_genes <- unique(c(top_genes, select_gene))
  
  # prepare scores
  pvec <- gstable[[score_col_index]]
  names(pvec) <- gstable$HumanGene
  pvec <- sort(pvec)
  
  df <- data.frame(Gene = names(pvec), Score = pvec) %>%
    mutate(
      Rank = row_number(),
      isTarget = Gene %in% highlight_genes,
      isSelected = Gene %in% select_gene,
      Size = ifelse(Gene %in% highlight_genes, 3, 2),
      NegLogScore = -log10(Score)
    )
  
  # assign colors for highlight genes
  color_map <- setNames(colors[(1:length(highlight_genes)) + color_offset],
                        highlight_genes)
  
  p <- ggplot(df, aes(x = Rank, y = NegLogScore)) +
    geom_line() +
    geom_point(
      data = df %>% filter(isTarget),
      aes(color = Gene, size = Size)
    ) +
    scale_color_manual(values = color_map) +
    geom_text_repel(
      data = df %>% filter(isTarget & !isSelected),
      aes(label = Gene),
      size = 4,
      max.overlaps = 20,
      box.padding = 0.5,
      point.padding = 0.2,
      segment.color = "black",
      segment.size = 0.5
    ) +
    # 🔴 Special: add labels with box for select_gene
    geom_label_repel(
      data = df %>% filter(isSelected),
      aes(label = Gene),
      size = 5,
      fill = "white",       # background of the box
      color = "black",      # text color
      fontface = "bold",
      box.padding = 0.6,
      point.padding = 0.3,
      segment.color = "red",
      segment.size = 0.7
    ) +
    labs(
      title = title_text,
      x = "Genes",
      y = "-log10(RRA score)"
    ) +
    theme_minimal() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
    scale_size_identity() +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 15, color = "black"),
      axis.text.y = element_text(hjust = 1, size = 15, color = "black"),
      axis.title.y = element_text(size = 15, color = "black"),
      axis.title.x = element_text(size = 15, color = "black"),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(hjust = 0.5, size = 18, color = "black", face = "bold")
    )
  
  return(p)
}
