# Source helper functions
rm(list = ls())

options(warn = -1)

# Define file paths
base_path <- getwd()
data_path <- file.path(base_path, "data")

# Source helper script
source(file.path(base_path, "helpers.R"), local = TRUE)

cell_type_image_map <- c(
  "MHC-I Regulators" = "celltypes/mhc1.png",
  "T-cell killing"   = "celltypes/tcell.png",
  "NK-cell killing"  = "celltypes/nk.png",
  "Macrophage killing" = "celltypes/macrophage.png",
  "gdT-cell killing" = "celltypes/gdt.png"
)

# UI code
ui <- tagList(
  navbarPage("Evolution of Genetic Immune Escape Viewer",
             tabPanel("Introduction",
                      fluidRow(
                        column(
                          10, offset = 1,
                          tabsetPanel(
                            id = "intro_tabs",
                            type = "hidden",  # hide tab headers
                            tabPanel("home",
                                     div(
                                       style = "text-align: center; margin-top: 10px; margin-bottom: 30px; font-style: italic; font-size: 20px; color: #555;",
                                       HTML("&ldquo;<b>Nothing in biology makes sense except in the light of evolution</b>&rdquo;<br>- Theodosius Dobzhansky")
                                     ),
                                     h2("Welcome to the Evolution of Genetic Immune Escape Viewer"),
                                     # --- Intro paragraph ---
                                     p("Immune escape is a critical hallmark of cancer progression, yet its evolutionary path largely remains unclear. When do genetic alterations in immunomodulatory pathways arise during cancer development? What precedes them, and what happens afterwards? We seek to build a pan-cancer atlas that maps the genetic evolution of immune escape. How can we get there? Click below to learn more.",
                                       style = "font-size: 17px; line-height: 1.4; color: #333;"
                                     ),
                                     # --- Two cards row ---
                                     fluidRow(
                                       column(
                                         6,
                                         wellPanel(
                                           style = "min-height: 500px; display: flex; flex-direction: column; justify-content: space-between;",
                                           h3("Methodology", style = "text-align: center; margin-top: 0px;"),
                                           div(
                                             img(src = "Page1.png", width = "100%", height = "auto",
                                                 style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 20px;"),
                                             style = "text-align: center;"
                                           ),
                                           p("Learn how immunomodulatory genes and mutation timing are identified from large-scale CRISPR screens and WGS data.",
                                             style = "text-align: center; font-size: 16px; color: #333; line-height: 1.5;"),
                                           div(
                                             style = "text-align: center;",
                                             actionButton("go_method", "Explore Methodology", class = "btn-primary")
                                           )
                                         )
                                       ),
                                       
                                       column(
                                         6,
                                         wellPanel(
                                           style = "min-height: 500px; display: flex; flex-direction: column; justify-content: space-between;",
                                           h3("Tutorials", style = "text-align: center; margin-top: 0px;"),
                                           div(
                                             img(src = "Page2.png", width = "100%", height = "auto",
                                                 style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 30px;"),
                                             style = "text-align: center;"
                                           ),
                                           p("Step-by-step guidance on how to use EvoGIE to explore immune escape evolution across different cancer WGS cohorts.",
                                             style = "text-align: center; font-size: 16px; color: #333; line-height: 1.5;"),
                                           div(
                                             style = "text-align: center;",
                                             actionButton("go_tutorial", "Open Tutorials", class = "btn-success")
                                           )
                                         )
                                       )
                                     ),
                                     
                                     # --- Footer row ---
                                     fluidRow(
                                       column(
                                         12,
                                         div(
                                           style = "margin-top: 40px; text-align: left; font-size: 16px; color: #444; line-height: 1.6; clear: both;",
                                           p("The Shiny App is developed by Wenjie Chen. The results are generated by Dr. Shengqing Gu lab and Dr. Peter Van Loo lab."),
                                           p(
                                             HTML("For more details, please refer to our <a href='https://www.biorxiv.org/content/10.1101/2025.01.17.632799v1' target='_blank'>BioRxiv preprint</a>.")
                                           )
                                         )
                                       )
                                     )
                            ),
                            # --- Subpage 1: Methodology ---
                            tabPanel("method",
                                     # Previous button at top-left
                                     div(
                                       style = "text-align: left; margin-bottom: 18px;",
                                       actionButton("back_home1", "Previous (Home)", class = "btn-secondary")
                                     ),
                                     
                                     h3("Identifying Immunomodulatory Genes", style = "font-size: 22px;"),
                                     p("The public CRISPR screen studies are comprehensively collected to identify the potential mechanisms for immune escape in cancers.",
                                       style = "font-size: 16px; line-height: 1.6; color: #333;"),
                                     tags$ul(
                                       tags$li("1. Published CRISPR screens were collected for identifying the regulators of MHC-I expression, response to CD8 T-cell-mediated killing, NK-cell-mediated killing, macrophage-mediated phagocytosis and γδ T-cell-mediated killing.",
                                               style = "font-size: 16px; line-height: 1.6;"),
                                       tags$li("2. The top 100 positive/negative regulators in each study were respectively unitized for Gene Set Enrichment Analysis.",
                                               style = "font-size: 16px; line-height: 1.6;"),
                                       tags$li("3. Next, we selected the featured enrichments for further analysis based on the frequencies of studies reporting these enrichments.",
                                               style = "font-size: 16px; line-height: 1.6;"),
                                       tags$li("4. We then combined the immunomodulatory genes in each frequently enriched pathway as a gene set for that pathway.",
                                               style = "font-size: 16px; line-height: 1.6;")
                                     ),
                                     tags$div(
                                       img(src = "workflow.png", width = "750px", height = "auto",
                                           style = "display: block; margin-left: auto; margin-right: auto;"),
                                       style = "margin-top: 20px; margin-bottom: 20px;"
                                     ),
                                     
                                     h3("Inferring Mutation Timing", style = "font-size: 22px;"),
                                     p(HTML("GRITIC is used to estimate when clonal copy number gains happened in a tumor's evolution. <a href='https://pubmed.ncbi.nlm.nih.gov/38943574/' target='_blank'>(PMID: 38943574)</a>"),
                                       style = "font-size: 16px; line-height: 1.6; color: #333;"),
                                     tags$ul(
                                       tags$li("1. Posterior gain timing distributions for clonal copy number segments are calculated based on the copy number, tumor purity and the read counts for SNVs in the region of the gain.",
                                               style = "font-size: 16px; line-height: 1.6;"),
                                       tags$li("2. For each SNV, GRITIC samples when the gain happened, how many copies the SNV has, and then estimates when the SNV occurred.",
                                               style = "font-size: 16px; line-height: 1.6;"),
                                       tags$li("3. The exact timing of each SNV is sampled within a time window defined by nearby gains.",
                                               style = "font-size: 16px; line-height: 1.6;"),
                                       tags$li("4. The timing of SNVs is measured on a 'mutation time' scale that goes from 0 (representing conception) to 1 (the end of clonal evolution). Each SNV is sampled 250 times to create a full timing distribution.",
                                               style = "font-size: 16px; line-height: 1.6;")
                                     ),
                                     tags$div(
                                       img(src = "Timing.jpg", width = "750px", height = "auto",
                                           style = "display: block; margin-left: auto; margin-right: auto;"),
                                       style = "margin-top: 20px; margin-bottom: 20px;"
                                     )
                            ),
                            
                            # --- Subpage 2: Tutorials ---
                            tabPanel("tutorial",
                                     # Previous button at top-left
                                     div(
                                       style = "text-align: left; margin-bottom: 18px;",
                                       actionButton("back_home2", "Previous (Home)", class = "btn-secondary")
                                     ),
                                     tags$div(
                                       img(src = "Tutorials.png", width = "900px", height = "auto",
                                           style = "display: block; margin-left: auto; margin-right: auto;"),
                                       style = "margin-top: 20px; margin-bottom: 20px;"
                                     )
                            )
                          )
                        )
                      )
             ),
             tabPanel("Immunomodulatory Pathways",
                      tabsetPanel(
                        # Tab 1: RRA Analysis
                        tabPanel("Overall Ranking",
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 2,
                                     div(style = "display: none;",
                                         selectInput("cell_type_rra",
                                                     "Immunity Scenarios", 
                                                     choices = names(cell_type_files),
                                                     selected = "MHC-I Regulators")),
                                     selectInput("immuno_regulators",
                                                 "Select Regulator Type", 
                                                 choices = c("Positive" = "pos", "Negative" = "neg"),
                                                 selected = "pos"),
                                     tagList(
                                       textAreaInput(
                                         "geneList_ranking", 
                                         "Enter Gene Symbols (Hugo_Symbol, e.g., B2M; newline separated)", 
                                         value = "",
                                         rows = 8,        
                                         resize = "vertical"
                                       ),
                                       actionButton("submit_genes", "Submit Genes")
                                     ),
                                   ),
                                   mainPanel(
                                     width = 9,
                                     uiOutput("cell_type_gallery_rra"),
                                     tags$hr(),
                                     div(
                                       style = "display: flex; justify-content: center; align-items: center; width: 100%;",
                                       withSpinner(plotOutput("immuno_rra_plot", height = "450px", width = "450px"))
                                     ),
                                     br(),
                                     uiOutput("rra_interpretation")
                                   )
                                 )
                        ),
                        
                        # Tab 2: Frequent Pathways
                        tabPanel("Frequent Pathways",
                                 sidebarLayout(
                                   sidebarPanel(
                                     width = 2,
                                     div(style = "display: none;",
                                         selectInput("cell_type_pathway",
                                                     "Immunity Scenarios", 
                                                     choices = names(cell_type_files),
                                                     selected = "MHC-I Regulators")),
                                     selectInput("pathway_regulators",
                                                 "Select Regulator Type", 
                                                 choices = c("Positive" = "pos", "Negative" = "neg"),
                                                 selected = "pos"),
                                     numericInput("immuno_n", "Number of Top Pathways", value = 10, min = 5, max = 50),
                                   ),
                                   mainPanel(
                                     width = 9,
                                     uiOutput("cell_type_gallery_pathway"),
                                     tags$hr(),
                                     div(
                                       style = "text-align: center;",
                                       uiOutput("immuno_pathway_plot_ui")
                                     ),
                                     br(),
                                     uiOutput("pathway_interpretation")
                                   )
                                 )
                        )
                      ),
                      
                      # CSS styling
                      tags$head(
                        tags$style(HTML("
                        .gallery-grid {
                          display: grid; grid-template-columns: repeat(5, 1fr); gap: 15px; margin-bottom: 20px;
                          }
                        .gallery-card {
                          border: 2px solid #ddd; border-radius: 10px; padding: 15px; text-align: center;
                          background: #f9f9f9; cursor: pointer; transition: all 0.2s;
                          }
                        .gallery-card:hover { transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
                        .gallery-card.selected { border-color: #007bff; background: #e7f3ff; }
                        .gallery-card img { width: 160px; height: 100px; object-fit: contain; }
                        .gallery-title { margin-top: 10px; font-weight: bold; }
                      "))
                      )
             ),
             tabPanel(
               "Select the WGS Datasets",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   selectInput("cohort_select", "Select Cohort",
                               choices = c("PCAWG", "TCGA-OV")),
                   actionButton("submit_btn", "Select"),
                   div(style = "margin-top: 15px;"),   # <-- 15px space
                   selectInput("cancer_select", "Select Cancer Type",
                               choices = c("All"), selected = "All")
                 ),
                 mainPanel(
                   conditionalPanel(
                     condition = "output.loading == true",
                     h4("Loading data.....", style = "color: #FF5733; text-align:center;")
                   ),
                   tabPanel(
                     "Select the WGS Datasetss",
                     fluidRow(
                       column(width = 9, uiOutput("cohort_summary_left")),
                       column(width = 3, uiOutput("cohort_summary_right")),
                       br(),
                       uiOutput("cohortsummary_interpretation")
                     )
                   )
                 )
               )
             ),
             tabPanel(
               "Immune Escape Evolution",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   
                   # Cohort selector for this tab
                   selectInput("escape_cohort_select", "Select Cohort",
                               choices = c("PCAWG", "TCGA-OV")),
                   actionButton("escape_submit_btn", "Select"),
                   tags$hr(),
                   # Cell type selector for specific tabs
                   conditionalPanel(
                     condition = "input.crispr_tab_selected == 'Single Pathway' || input.crispr_tab_selected == 'Multiple Pathways' || input.crispr_tab_selected == 'Survival' || input.crispr_tab_selected =='Immune Cell Infiltration'",
                     uiOutput("cell_type_ui")
                   ),
                   
                   # Timeline specific controls
                   conditionalPanel(
                     condition = "input.crispr_tab_selected == 'Timeline'",
                     uiOutput("timing_type_ui"),
                     uiOutput("cell_type_timeline_ui")
                   ),
                   
                   # Single Pathway specific controls
                   conditionalPanel(
                     condition = "input.crispr_tab_selected == 'Single Pathway'",
                     uiOutput("regulator_selector"),
                     uiOutput("pathway_selector")
                   ),
                   
                   # Survival specific controls
                   conditionalPanel(
                     condition = "input.crispr_tab_selected == 'Survival'",
                     uiOutput("pathway_selector_surv"),
                     uiOutput("select_histology_ui")
                   ),
                   
                   # Immune Cell Infiltration specific controls
                   conditionalPanel(
                     condition = "input.crispr_tab_selected == 'Immune Cell Infiltration'",
                     uiOutput("pathway_selector_ciber"),
                     uiOutput("select_histology_ciber")
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     id = "crispr_tab_selected",
                     tabPanel("Single Pathway",
                              uiOutput("singlepathway_panel")
                     ),
                     tabPanel("Multiple Pathways",
                              fluidPage(
                                withSpinner(plotOutput("radarplot", width = "800px", height = "500px")),
                                br(),
                                uiOutput("multipathways_interpretation")
                              )
                     ),
                     tabPanel("Timeline",
                              fluidPage(
                                  withSpinner(plotOutput("timing_plot", width = "1000px", height = "900px"))
                              ),
                              br(),
                              uiOutput("timeline_interpretation")
                     ),
                     tabPanel("Survival",
                              withSpinner(plotOutput("timing_survival_plot", height = "500px", width = "450px")),
                              br(),
                              uiOutput("timingsurvival_interpretation")
                     ),
                     tabPanel("Immune Cell Infiltration",
                              withSpinner(plotOutput("ciber_plot", height = "400px", width = "800px")),
                              br(),
                              uiOutput("cibersort_interpretation")
                     )
                   )
                 )
               )
             ),
             tabPanel(
               "Explore Your Genes",
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   
                   # Gene list input ONLY for Immunomodulatory Effect tab
                   conditionalPanel(
                     condition = "input.gene_tab_selected == 'Immunomodulatory Effect'",
                     actionButton("useExampleGenes", "Select Example Gene List"),
                     tags$hr(),
                     textAreaInput(
                       "geneList_explore",
                       "Enter Gene Symbols (Hugo_Symbol, e.g., TP53; newline separated)",
                       value = "",
                       rows = 8,
                       resize = "vertical"
                     ),
                     actionButton("submit_genes", "Submit Genes")
                   ),
                   
                   # Cohort selector ONLY for Mutation Frequency tab
                   conditionalPanel(
                     condition = "input.gene_tab_selected == 'Mutation Frequency'",
                     selectInput("gene_cohort_select", "Select Cohort",
                                 choices = c("PCAWG", "TCGA-OV")),
                     actionButton("gene_submit_btn", "Select"),
                     tags$hr()
                   ),
                   
                   # Show histology dropdown in Gene Timing, Pathway Timing, Survival
                   conditionalPanel(
                     condition = "['Gene Timing','Pathway Timing','Survival'].includes(input.gene_tab_selected)",
                     uiOutput("select_histology_list")
                   ),
                   
                   conditionalPanel(
                     condition = "input.gene_tab_selected == 'ICB Response'",
                     selectInput("therapy_select_genes", "Select Therapy:", choices = NULL)
                   ),
                   
                   conditionalPanel(
                     condition = "input.gene_tab_selected == 'ICB Survival'",
                     tagList(selectInput("therapy_surv_genes", "Select Therapy:", choices = NULL))
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     id = "gene_tab_selected",
                     tabPanel("Immunomodulatory Effect",
                              div(class = "hcenter", withSpinner(uiOutput("radar_plot_ui"))),
                              br(),
                              uiOutput("radar_interpretation")
                     ),
                     tabPanel("Mutation Frequency", 
                              div(class = "hcenter", withSpinner(uiOutput("oncoplot_ui"))), 
                              br(),
                              uiOutput("mutation_freq_interpretation")),
                     tabPanel("Gene Timing", withSpinner(uiOutput("timing_plot_singlegene_ui")), br(), uiOutput("gene_timing_interpretation")),
                     tabPanel("Pathway Timing", withSpinner(plotOutput("timing_plot_gene", width = "520px", height = "500px")), br(), uiOutput("pathway_timing_gene_interpretation")),
                     tabPanel("Survival", withSpinner(plotOutput("timing_survival_plot_genelist", height = "500px", width = "450px")), br(), uiOutput("timingsurvival_gene_interpretation")),
                     tabPanel("ICB Response",
                              div(
                                style = "margin-top: 0px; padding-top: 0px; display: flex; justify-content: left; align-items: flex-start;",
                                withSpinner(plotOutput("forestplot", width = "450px", height = "600px"))
                              ),
                              br(),
                              uiOutput("ICBresponse_interpretation")
                     ),
                     tabPanel("ICB Survival",
                              withSpinner(uiOutput("ICB_survival_plot")),
                              br(),
                              uiOutput("ICBsurvival_interpretation")
                     )
                   ),
                   br(),
                   uiOutput("tab_description")
                 )
               )
             ),
             tabPanel("Contact",
                      fluidPage(
                        h4("Manuscript under preparation:"),
                        style = "margin-top: 10px; text-align: left; font-size: 16px; color: #444; line-height: 1.5;",
                        p(
                          HTML("Wenjie Chen, Toby Baker, Zhihui Zhang, Huw A Ogilvie, Peter Van Loo, Shengqing Stan Gu. Evolutionary trajectories of immune escape across cancers. <a href='https://www.biorxiv.org/content/10.1101/2025.01.17.632799v1' target='_blank'>BioRxiv preprint</a>")
                          ),
                        br(),
                        p(
                          HTML("For issues with the app, please contact: Dr. Wenjie Chen, wchen20@mdanderson.org.")
                        )
                      )
             )
  )
)

# Server logic
server <- function(input, output, session) {
  
  cohort_data <- reactiveVal(NULL)
  loading_flag <- reactiveVal(FALSE)
  
  output$loading <- reactive({
    loading_flag()
  })
  outputOptions(output, "loading", suspendWhenHidden = FALSE)
  
  ###################################################
  ## Page 1
  ###################################################
  # Default: show Home page
  updateTabsetPanel(session, "intro_tabs", selected = "home")
  
  # Buttons to go forward
  observeEvent(input$go_method, {
    updateTabsetPanel(session, "intro_tabs", selected = "method")
  })
  
  observeEvent(input$go_tutorial, {
    updateTabsetPanel(session, "intro_tabs", selected = "tutorial")
  })
  
  # Buttons to go back
  observeEvent(input$back_home1, {
    updateTabsetPanel(session, "intro_tabs", selected = "home")
  })
  
  observeEvent(input$back_home2, {
    updateTabsetPanel(session, "intro_tabs", selected = "home")
  })
  
  ###################################################
  ## Page 2
  ###################################################
  ## Page2 -- Pathway Summary
  # RRA Tab Gallery
  output$cell_type_gallery_rra <- renderUI({
    current_selection <- input$cell_type_rra
    
    cards <- lapply(names(cell_type_files), function(label) {
      cell_code <- cell_type_files[[label]]
      is_selected <- identical(current_selection, label)
      
      div(
        class = paste("gallery-card", if(is_selected) "selected"),
        onclick = sprintf("Shiny.setInputValue('gallery_click_rra', '%s')", label),
        img(src = paste0(cell_code, ".png"), alt = label),
        div(class = "gallery-title", label)
      )
    })
    
    div(class = "gallery-grid", cards)
  })
  
  # Pathway Tab Gallery
  output$cell_type_gallery_pathway <- renderUI({
    current_selection <- input$cell_type_pathway
    
    cards <- lapply(names(cell_type_files), function(label) {
      cell_code <- cell_type_files[[label]]
      is_selected <- identical(current_selection, label)
      
      div(
        class = paste("gallery-card", if(is_selected) "selected"),
        onclick = sprintf("Shiny.setInputValue('gallery_click_pathway', '%s')", label),
        img(src = paste0(cell_code, ".png"), alt = label),
        div(class = "gallery-title", label)
      )
    })
    
    div(class = "gallery-grid", cards)
  })
  
  # RRA Tab click handler
  observeEvent(input$gallery_click_rra, {
    updateSelectInput(session, "cell_type_rra", selected = input$gallery_click_rra)
  })
  
  # Pathway Tab click handler
  observeEvent(input$gallery_click_pathway, {
    updateSelectInput(session, "cell_type_pathway", selected = input$gallery_click_pathway)
  })
  
  # Keep galleries synchronized (optional)
  observeEvent(input$cell_type_rra, {
    updateSelectInput(session, "cell_type_pathway", selected = input$cell_type_rra)
  })
  
  observeEvent(input$cell_type_pathway, {
    updateSelectInput(session, "cell_type_rra", selected = input$cell_type_pathway)
  })
  
  # Study count for RRA tab
  study_count_rra <- reactive({
    req(input$cell_type_rra, input$immuno_regulators)
    cell_type_code <- cell_type_files[[input$cell_type_rra]]
    reg <- input$immuno_regulators
    
    df <- data_crispr %>%
      dplyr::filter(celltype == cell_type_code, regulators == reg)
    
    length(unique(df$Study))
  })
  
  # Study count for pathway tab
  study_count_pathway <- reactive({
    req(input$cell_type_pathway, input$pathway_regulators)
    cell_type_code <- cell_type_files[[input$cell_type_pathway]]
    reg <- input$pathway_regulators
    
    df <- data_crispr %>%
      dplyr::filter(celltype == cell_type_code, regulators == reg)
    
    length(unique(df$Study))
  })
  
  # Pathway plot UI
  output$immuno_pathway_plot_ui <- renderUI({
    req(input$immuno_n, input$cell_type_pathway)
    
    study_n <- study_count_pathway()
    if (study_n == 0) {
      return(h4("No data available for this selection",
                style = "text-align: center; color: #666; margin-top: 50px;"))
    }
    
    total_width <- min(600 + 30 * study_n, 2000)
    total_height <- min(500 + 35 * input$immuno_n, 4000)
    
    plotlyOutput("immuno_pathway_plot",
                 height = paste0(total_height, "px"),
                 width = paste0(total_width, "px"))
  })
  
  # Pathway plot
  output$immuno_pathway_plot <- renderPlotly({
    req(input$cell_type_pathway, input$pathway_regulators, input$immuno_n)
    
    cell_type_code <- cell_type_files[[input$cell_type_pathway]]
    reg <- input$pathway_regulators
    num <- input$immuno_n
    
    # Filter data
    df <- data_crispr %>%
      dplyr::filter(celltype == cell_type_code, regulators == reg)
    
    req(nrow(df) > 0)
    
    all_studies <- unique(df$Study_2)
    
    # Select top N unique Descriptions
    top_desc <- df %>%
      arrange(desc(Freq), desc(`-log10(Padj)`)) %>%
      distinct(Description, .keep_all = TRUE) %>%
      slice_head(n = num) %>%
      pull(Description)
    
    # Filter data for plotting
    plot_data <- df %>%
      filter(Description %in% top_desc) %>%
      mutate(
        tooltip_text = paste0(
          "Genes: ", geneName, "<br>",
          "FDR: ", format(p.adjust, scientific = TRUE, digits = 3), "<br>",
          "NES: ", round(NES, 1)
        )
      )
    
    # Set y-axis order by Freq
    plot_order <- df %>%
      filter(Description %in% top_desc) %>%
      arrange(desc(Freq)) %>%
      pull(Description) %>%
      unique()
    
    # Ensure factors for axis ordering and show all Study_2 levels
    plot_data <- plot_data %>%
      mutate(
        Description = factor(Description, levels = plot_order),
        Description = str_wrap(Description, width = 40),
        Study_2 = factor(Study_2, levels = all_studies)  # ensures all x-axis labels appear
      )
    
    # Plot
    p <- ggplot(plot_data, aes(
      x = Study_2,
      y = Description,
      size = `-log10(Padj)`,
      text = tooltip_text
    )) +
      geom_point(
        aes(fill = Cell_line),
        shape = 21,
        colour = "black",
        stroke = 0.3,
        alpha = 0.8
      ) +
      labs(size = "", title = "", x = "", y = "", fill = "Cancer Type") +
      theme_minimal() +
      scale_fill_manual(values = cellline_colors) +
      scale_x_discrete(drop = FALSE) +  # 👈 ensures all Study_2 labels show even if no data
      scale_y_discrete(labels = function(x) str_wrap(x, width = 40)) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.line = element_line(color = "black", size = 0.05),
        axis.ticks = element_line(color = "black"),
        plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Create reactive for processed gene list
  processed_genes <- reactive({
    input$submit_genes  # Take dependency on button
    
    isolate({
      # Check if geneList input exists and is character
      if(is.null(input$geneList_ranking) || !is.character(input$geneList_ranking) || input$geneList_ranking == "") {
        return(character(0))
      }
      
      # Split by comma, newline, or semicolon and clean up
      genes <- strsplit(as.character(input$geneList_ranking), "[,;\n\r]+")[[1]] %>%
        trimws() %>%
        unique() %>%
        .[. != ""]  # Remove empty strings
      
      return(genes)
    })
  })
  
  # RRA plot
  output$immuno_rra_plot <- renderPlot({
    req(input$cell_type_rra, input$immuno_regulators)
    
    # Use the processed gene list
    select_gene <- processed_genes()
    
    cell_type_code <- cell_type_files[[input$cell_type_rra]]
    
    df_sub <- data_rra %>%
      filter(cell_type == cell_type_code)
    
    validate(need(nrow(df_sub) > 0, "No data available for this selection"))
    
    # Create title mapping
    title_mapping <- c(
      "MHCregulators" = "MHC-I Regulators",
      "Tcells" = "T-cell Killing",
      "NKcells" = "NK-cell Killing",
      "Macrophages" = "Macrophage Killing",
      "GammadeltaTcells" = "GDT-cell Killing"
    )
    
    # Create regulation type mapping
    regulation_mapping <- c(
      "pos" = "Positive Regulation",
      "neg" = "Negative Regulation"
    )
    
    # Get the mapped title or use the original if not found
    mapped_title <- title_mapping[input$cell_type_rra]
    if(is.na(mapped_title)) {
      mapped_title <- input$cell_type_rra
    }
    
    # Get the mapped regulation type
    mapped_regulation <- regulation_mapping[input$immuno_regulators]
    if(is.na(mapped_regulation)) {
      mapped_regulation <- input$immuno_regulators
    }
    
    plot_selection_rank(
      gstable = df_sub,
      rank_col = ifelse(input$immuno_regulators == "pos", "Rank_pos", "Rank_neg"),
      score_col_index = ifelse(input$immuno_regulators == "pos", "Score_pos", "Score_neg"),
      title_text = paste(mapped_title, "-", mapped_regulation),
      top_num = 10,
      color_offset = 0,
      select_gene = select_gene
    )
  })
  
  ###################################################
  ## Page 3 -- Select the WGS Datasets
  ###################################################
  # Load results from different cohorts
  observeEvent(input$submit_btn, {
    loading_flag(TRUE)
    
    withProgress(message = "Loading data...", value = 0.1, {
      data <- load_clinical_only(input$cohort_select)
      incProgress(0.6, detail = "Processing clinical data...")
      
      cohort_data(data)
      
      # extract cancer types
      type_list <- data$clinical_data %>%
        distinct(aliquot_id, histology_abbreviation)
      
      # conditional update
      if (input$cohort_select %in% c("PCAWG", "TCGA-OV")) {
        updateSelectInput(
          session, "cancer_select",
          choices = c("All", base::unique(type_list$histology_abbreviation)),
          selected = "All"
        )
      } else {
        updateSelectInput(
          session, "cancer_select",
          choices = base::unique(type_list$histology_abbreviation),
          selected = base::unique(type_list$histology_abbreviation)[1]
        )
      }
      
      incProgress(0.3, detail = "Finalizing...")
      Sys.sleep(0.5)
    })
    
    loading_flag(FALSE)
  })
  
  # Top-left -- make barplot for sample number for each cancer type
  output$cohort_summary_left <- renderUI({
    if (is.null(input$cancer_select) || input$cancer_select == "All") {
      tagList(
        plotOutput("histology_bar", height = "300px"),
        plotOutput("total_perMB_log_box", height = "300px")
      )
    } else {
      tagList(
        plotOutput("barplot_age", height = "300px"),
        plotOutput("barplot_NRPCC", height = "300px")
      )
    }
  })
  
  clinical_data_filtered <- reactive({
    req(cohort_data())
    clinical_data <- cohort_data()$clinical_data
    
    # only filter when not "All"
    if (!is.null(input$cancer_select) && input$cancer_select != "All") {
      clinical_data <- clinical_data %>%
        filter(histology_abbreviation == input$cancer_select)
    } else {
      hist_count <- clinical_data %>%
        dplyr::count(histology_abbreviation) %>%
        filter(n >= 5)
      
      clinical_data <- clinical_data %>%
        filter(histology_abbreviation %in% hist_count$histology_abbreviation)
    }
    
    clinical_data
  })
  
  output$histology_bar <- renderPlot({
    req(cohort_data())
    clinical_data <- cohort_data()$clinical_data
    
    clinical_data_filtered <- clinical_data_filtered()
    
    hist_order <- clinical_data_filtered %>%
      dplyr::count(histology_abbreviation) %>%
      arrange(desc(n)) %>%
      pull(histology_abbreviation)
    
    ggplot(clinical_data_filtered, aes(x = factor(histology_abbreviation, levels = hist_order), fill = histology_abbreviation)) +
      geom_bar() +
      scale_fill_manual(values = type_colors) +
      theme_minimal(base_size = 14) +
      labs(title = "Sample Frequency", x = "", y = "Sample Count") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.position = "none",
            plot.title = element_text(size = 18, face = "bold"))
  })
  
  # Bottom-left -- make boxplot for the distribution of TMB
  output$total_perMB_log_box <- renderPlot({
    req(cohort_data())
    clinical_data_filtered <- clinical_data_filtered()
    
    hist_order <- clinical_data_filtered %>%
      group_by(histology_abbreviation) %>%
      summarise(median_tmb = median(TMB_log, na.rm = TRUE)) %>%
      arrange(desc(median_tmb)) %>%
      pull(histology_abbreviation)
    
    ggplot(clinical_data_filtered, aes(x = factor(histology_abbreviation, levels = hist_order), y = TMB_log, fill = histology_abbreviation)) +
      geom_boxplot() +
      scale_fill_manual(values = type_colors) +
      theme_minimal(base_size = 14) +
      scale_y_continuous(breaks = log10(c(0.1, 1, 10, 100, 1000, 10000)), 
                         labels = c(0.1, 1, 10, 100, 1000, 10000)) +
      labs(title = "Tumor Mutation Burden", x = "", y = "TMB") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            legend.position = "none",
            plot.title = element_text(size = 18, face = "bold"))
  })
  
  output$barplot_age <- renderPlot({
    req(clinical_data_filtered())
    clinical_data <- clinical_data_filtered()
    
    ggplot(clinical_data, aes(x = Age)) +
      geom_histogram(binwidth = 5, boundary = 0, fill = "#69b3a2", color = "black") +
      labs(x = "", y = "Sample Number", title = "Age") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12, colour = "black"), 
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 12, colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.margin = margin(30, 30, 30, 30)
      )
  })
  
  output$barplot_NRPCC <- renderPlot({
    req(clinical_data_filtered())
    clinical_data <- clinical_data_filtered()
    
    ggplot(clinical_data, aes(x = NRPCC)) +
      geom_histogram(binwidth = 5, boundary = 0, fill = "skyblue", color = "black") +
      labs(x = "", y = "Sample Number", title = "NRPCC") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12, colour = "black"), 
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 12, colour = "black"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.margin = margin(30, 30, 30, 30)
      )
  })
  # Right -- make pieplot for sample type and age
  output$cohort_summary_right <- renderUI({
    tagList(
      plotOutput("gender_pie", height = "300px"),
      plotOutput("primary_pie", height = "300px")
    )
  })
  
  output$primary_pie <- renderPlot({
    req(cohort_data())
    clinical_data_filtered <- clinical_data_filtered()
    
    data <- clinical_data_filtered %>% dplyr::count(Sample_Type)
    ggplot(data, aes(x = "", y = n, fill = Sample_Type)) +
      geom_col() +
      coord_polar("y") +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(size = 18, face = "bold")) +
      labs(title = "Sample Type", fill = NULL) +
      scale_fill_npg() 
  })
  
  output$gender_pie <- renderPlot({
    req(cohort_data())
    clinical_data_filtered = clinical_data_filtered()
    
    data <- clinical_data_filtered %>% dplyr::count(Gender)
    ggplot(data, aes(x = "", y = n, fill = Gender)) +
      geom_col() +
      scale_fill_manual(values = Gender_colors) +
      coord_polar("y") +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(size = 18, face = "bold")) +
      labs(title = "Sex", fill = NULL)
  })
  
  ###################################################
  ## Page 4 Immune Escape Evolution
  ###################################################
  # Single Pathway -- make bar plot for top 10 cancer types by Freq_mut for selected pathways
  observeEvent(input$escape_submit_btn, {
    loading_flag(TRUE)
    
    withProgress(message = "Loading Immune Escape data...", value = 0.1, {
      data <- load_escape_data(input$escape_cohort_select)
      incProgress(0.6, detail = "Processing Immune Escape data...")
      
      cohort_data(data)   # replace reactiveVal with only heavy datasets
      
      incProgress(0.3, detail = "Finalizing...")
      Sys.sleep(0.5)
    })
    
    loading_flag(FALSE)
  })
  
  output$cell_type_ui <- renderUI({
    selectInput("cell_type", "Immunity Scenarios", choices = names(cell_type_files))
  })
  
  output$cell_type_timeline_ui <- renderUI({
    selectInput("cell_type_timeline", "Immunity Scenarios", choices = c("All", names(cell_type_files)))
  })
  
  output$regulator_selector <- renderUI({
    selectInput("selected_regulator", "Select Regulator Type",
                choices = c("Positive" = "pos", "Negative" = "neg"),
                selected = "pos")
  })
  
  output$pathway_selector <- renderUI({
    req(input$cell_type, input$selected_regulator)
    
    choices <- CRISPR_all %>%
      filter(Celltype == cell_type_files[[input$cell_type]],
             Regulators == input$selected_regulator) %>%
      pull(pathway_new) %>%
      base::unique() %>%
      sort()
    
    selectInput("selected_pathways", "Select Pathways",
                choices = choices,
                selected = "Antigen Presentation via MHC-I (MHC-I)",
                multiple = FALSE)
  })
  
  output$top_cancer_plot <- renderPlot({
    tryCatch({
    req(input$cell_type, input$selected_pathways, cohort_data())
    
    top_data <- cohort_data()$Freq_all %>% filter(Freq_mut > 4) %>%
      filter(cell_type == cell_type_files[[input$cell_type]], pathway_new %in% input$selected_pathways) %>%
      group_by(histology_abbreviation) %>%
      summarise(
        total_mut = sum(percent_mut, na.rm = TRUE),
        number_mut = sum(Freq_mut, na.rm = TRUE)  # Add raw mutation count
      ) %>%
      arrange(desc(total_mut)) %>%
      dplyr::slice_head(n = 10)
    
    if (nrow(top_data) > 0 ) {
      ggplot(top_data, aes(x = reorder(histology_abbreviation, total_mut), y = total_mut)) +
        geom_col(fill = "steelblue") +
        geom_text(aes(label = number_mut),
                  hjust = -0.1, size = 5) +
        coord_flip() +
        scale_y_continuous(labels = percent_format(accuracy = 1),
                           expand = expansion(mult = c(0, 0.15))) +
        labs(x = "", y = "Mutation Frequency", title = "Top 10 Cancer Types") +
        theme_minimal(base_size = 14) +
        theme(
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text.x = element_text(hjust = 0.5, size = 14, colour = "black"),
          axis.text.y = element_text(hjust = 1, size = 14, colour = "black"),
          title = element_text(size = 18, colour = "black", face = "bold")
        )
      
    } else {
      
      ggplot() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
      
    }
  
    }, error = function(e) {
      ggplot() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
    })
  })
  
  # Multiple Pathways -- make Radar Plot for the frequencies of mutations in different pathway across cancers
  output$radarplot <- renderPlot({
    req(cohort_data(), input$cell_type)
    
    validate(need(input$cell_type %in% names(cell_type_files), "Loading..."))
    
    cell_type_key <- cell_type_files[[input$cell_type]]
    
    plots <- plot_radar_all(
      Freq_all = cohort_data()$Freq_all,
      cell_type = cell_type_key
    )
    
    gridExtra::grid.arrange(plots$pos, plots$neg, ncol = 2)
  }, res = 100)
  
  # Timeline -- make plots for the temporal orders of immunomodulatory pathways and driver genes in cancers
  output$timing_type_ui <- renderUI({
    selectInput("timing_type", "Select Cancer Type", choices = base::unique(cohort_data()$diff_all$histology_abbreviation), selected = "Breast-AdenoCA")
  })
  
  output$timing_plot <- renderPlot({
    req(input$timing_type, input$cell_type_timeline, cohort_data())
    
    cell_types <- if (input$cell_type_timeline == "All") {
      unname(cell_type_files)
    } else {
      cell_type_files[[input$cell_type_timeline]]  
    }
    
    p <- plot_timing_summary(
      diff_all = cohort_data()$diff_all,
      driver_list = driver_list,
      clinical_data = cohort_data()$clinical_data,
      type = input$timing_type,
      cell_type = cell_types
    )
    
    if (!is.null(p)) {
      return(p)
    } else {
      return(
        ggplot() + theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
      )
    }
  })
  
  # Survival -- make survival plot by mutation timing
  output$select_histology_ui <- renderUI({
    req(cohort_data())
    histologies <- sort(base::unique(cohort_data()$diff_all$histology_abbreviation))
    selectInput("histology_type", "Select Cancer Type:", choices = c("All", histologies), selected = "All")
  })
  
  output$pathway_selector_surv <- renderUI({
    req(input$cell_type, input$selected_regulator, cohort_data())
    
    choices <- cohort_data()$diff_all %>%
      filter(celltype == !!cell_type_files[[input$cell_type]]) %>%
      pull(pathway) %>% base::unique()
    
    selectInput("selected_pathways", "Select Pathways",
                choices = choices,
                selected = "Antigen Presentation via MHC-I (MHC-I)",
                multiple = FALSE)
  })
  
  output$timing_survival_plot <- renderPlot({
    tryCatch({
      req(input$histology_type,  input$selected_regulator)
      
      wt_included <- "no"
      histology_type <- input$histology_type
      selected_pathways <- input$selected_pathways
      
      data_survival <- cohort_data()$surv_data
      diff_all <- cohort_data()$diff_all %>%
        mutate(Timing_group = case_when(early_ratio > 0.5 ~ "Mut_Early",
                                        late_ratio > 0.5 ~ "Mut_Late",
                                        TRUE ~ "Undetermined")) %>%
        filter(Timing_group != "Undetermined")
      
      sample_survival <- base::unique(data_survival$aliquot_id)
      diff_all <- diff_all %>% filter(aliquot_id %in% sample_survival)
      
      data_survival_sub <- if (histology_type == "All") data_survival else filter(data_survival, histology_abbreviation == histology_type)
      diff_all_sub <- if (histology_type == "All") diff_all else filter(diff_all, histology_abbreviation == histology_type)
      
      filtered_data <- diff_all_sub %>%
        filter(pathway == selected_pathways) %>%
        left_join(data_survival_sub, .,by = "aliquot_id") %>%
        mutate(
          Timing_group = ifelse(is.na(Timing_group), "WT", Timing_group),
          Timing_group = factor(Timing_group, levels = c("WT", "Mut_Early", "Mut_Late"))
        )
      
      plot_survival_by_timing(
        filtered_data = filtered_data,
        wt_included = "no",
        title_prefix = ""
      )
      
    }, error = function(e) {
      ggplot() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
    })
  }, res = 90)
  
  # Immune cell infiltration -- make boxplot for the composistion of immune cells by mutation timing (Cibersort)
  output$select_histology_ciber <- renderUI({
    req(cohort_data())
    histologies <- sort(base::unique(cohort_data()$diff_all$histology_abbreviation))
    selectInput("histology_ciber", "Select Cancer Type:", choices = c("All", histologies), selected = "All")
  })
  
  output$pathway_selector_ciber <- renderUI({
    req(input$cell_type, input$selected_regulator, cohort_data())
    
    choices <- cohort_data()$diff_all %>%
      filter(celltype == !!cell_type_files[[input$cell_type]]) %>%
      pull(pathway) %>% base::unique()
    
    selectInput("pathway_ciber", "Select Pathways",
                choices = choices,
                selected = "Antigen Presentation via MHC-I (MHC-I)",
                multiple = FALSE)
  })
  
  output$ciber_plot <- renderPlot({
    tryCatch({
      req(input$histology_ciber, input$pathway_ciber, cohort_data())
      
      histology_type <- input$histology_ciber
      selected_pathways <- input$pathway_ciber
      
      ciber_all <- cohort_data()$ciber_all
      diff_all <- cohort_data()$diff_all %>%
        mutate(Timing_group = case_when(early_ratio > 0.5 ~ "Mut_Early", 
                                        late_ratio > 0.5 ~ "Mut_Late",
                                        TRUE ~ "Undetermined")) %>% 
        filter(Timing_group != "Undetermined")
      
      diff_all_sub <- if (histology_type == "All") diff_all else filter(diff_all, histology_abbreviation == histology_type)

      diff_ciber <- inner_join(diff_all_sub, ciber_all, by = "aliquot_id") %>%
        filter(pathway == selected_pathways)
      
      ciber_long <- diff_ciber %>% 
        gather(cell_type, Value_1, -pathway, -histology_abbreviation, -celltype, -regulator, -Timing_group) %>% 
        mutate(Value_1 = as.numeric(Value_1),
               cell_type = as.factor(cell_type)) %>% 
        filter(cell_type %in% cibercom_cols)
      
      validate(need(nrow(ciber_long) > 0, "No sufficient data"))
      
      plot_ciber(ciber_long, histology_type, selected_pathways)
      
    }, error = function(e) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
    })
  })
  
  ###################################################
  ## Page 5
  ###################################################
  observeEvent(input$gene_submit_btn, {
    loading_flag(TRUE)
    
    withProgress(message = "Loading gene exploration data...", value = 0.1, {
      data <- load_gene_explore_data(input$gene_cohort_select)
      incProgress(0.7, detail = "Processing gene exploration data...")
      
      cohort_data(data)   # now contains diff_allgene, ciber_all, maf_data, clinical_data, surv_data
      
      incProgress(0.2, detail = "Finalizing...")
      Sys.sleep(0.5)
    })
    
    loading_flag(FALSE)
  })
  
  # Mutation Frequency -- make the onocoplot for the selected gene list
  # Reactive for selected genes
  # Store selected genes
  selectedGenes <- reactiveVal(character(0))
  
  # Example genes list
  exampleGenes <- c(
    "TP53", "PTPN2", "ADAR", "ADGRG1", "HEY1", "HEY2", "L1CAM",
    "LDB1", "AOAH"
  )
  
  # When "Select Example Gene List" is clicked → use example genes directly
  observeEvent(input$useExampleGenes, {
    selectedGenes(exampleGenes)
  })
  
  # When "Submit Genes" is clicked → only use what's typed in the textarea
  observeEvent(input$submit_genes, {
    if (nzchar(input$geneList_explore)) {
      genes <- unlist(strsplit(input$geneList_explore, "[,\n]")) %>%
        trimws() %>%              
        .[nzchar(.)] %>%
        unique()
      selectedGenes(genes)
    } else {
      selectedGenes(character(0))
    }
  })
  
  # Final reactive accessor
  get_selected_genes <- reactive({
    selectedGenes()
  })
  
  output$radar_plot_ui <- renderUI({
   
    gene_n <- length(get_selected_genes())
    base_height <- 300
    extra_height <- 250
    height_cap <- 2000
    
    ncol <- 3
    n_rows <- ceiling(gene_n / ncol)
    total_height <- min(base_height + extra_height * (n_rows - 1), height_cap)
    
    plotOutput("radar_plot", height = paste0(total_height, "px"), width = "1000px")
  })
  
  output$radar_plot <- renderPlot({
    selected_genes <- get_selected_genes()
    req(length(selected_genes) > 0)
    
    RRA_all <- data_rra
    
    radar_list <- lapply(selected_genes, function(gene) {
      df_wide <- RRA_all %>%
        dplyr::filter(HumanGene == gene) %>%
        mutate(cell_type = factor(cell_type, levels = c("MHCregulators", "Tcells", "NKcells", "Macrophages", "GammadeltaTcells"))) %>%
        arrange(cell_type) %>% 
        dplyr::select(HumanGene, Regulation, Score) %>%
        tidyr::pivot_wider(names_from = Regulation, values_from = Score)
      
      if (nrow(df_wide) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = paste("No data for", gene),
                          size = 5, hjust = 0.5))
      }
      
      axis_labels_left <- paste0(colnames(df_wide)[-1], "   ")  # Add spaces for left alignment
      
      ggradar(
        df_wide,
        values.radar = c("-1", "0", "1"),
        grid.min = -1,
        grid.mid = 0,
        grid.max = 1,
        group.colours = "blue",
        fill = TRUE,
        fill.alpha = 0.3,
        base.size = 10,
        grid.line.width = 0.8,
        grid.label.size = 8,
        group.line.width = 1,
        gridline.label.offset = -0.15,
        group.point.size = 1.5,
        axis.label.size = 6,
        axis.label.offset = 1.2,
        axis.labels = axis_labels_left,  # Use custom labels
        legend.position = "bottom",
        background.circle.colour = "white",
        gridline.min.colour = "black",
        gridline.mid.colour = "red",
        gridline.max.colour = "black",
        plot.extent.x.sf = 1.5,
        plot.extent.y.sf = 1.2
      ) +
        ggtitle(gene) +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 10, t = 10)),
          legend.text = element_text(size = 4),
          legend.title = element_text(size = 7),
          legend.key.size = unit(0.2, "cm"),
          legend.key.height = unit(0.15, "cm"),
          legend.spacing.x = unit(0.5, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          legend.margin = margin(t = 1, b = 1),
          legend.box.spacing = unit(0.1, "cm"),
          plot.margin = margin(t = 20, r = 15, b = 10, l = 15)
        )
    })
    
    cowplot::plot_grid(plotlist = radar_list, ncol = 3)
  }, res = 60)
  
  maf_obj <- reactive({
    req(cohort_data())
    cohort_data()$maf_data
  })
  
  output$oncoplot_ui <- renderUI({
    # If no cohort loaded, show message instead of empty plot
    if (is.null(maf_obj())) {
      return(
        div(
          style = "text-align: center; margin-top: 20px;",
          h4("Please select the cohort on the left button.",
             style = "color: #555; font-weight: normal;")
        )
      )
    }
    
    req(get_selected_genes())
    gene_n <- length(get_selected_genes())
    
    if (gene_n == 1) {
      # Single gene → lollipop plot
      plotOutput("oncoplot", height = "400px", width = "600px")
    } else {
      # Multiple genes → oncoplot, adjust height dynamically
      base_height <- 300
      extra_height <- 20
      height_cap <- 1200
      total_height <- min(base_height + extra_height * gene_n, height_cap)
      
      plotOutput("oncoplot", height = paste0(total_height, "px"), width = "600px")
    }
  })
  
  
  output$oncoplot <- renderPlot({
    tryCatch({
      req(maf_obj())
      genes_to_plot <- get_selected_genes()
      req(length(genes_to_plot) > 0)
      
      if (length(genes_to_plot) == 1) {
        # Use lollipopPlot if only one gene is selected
        validate(need(genes_to_plot %in% maf_obj()@gene.summary$Hugo_Symbol,
                      "No mutation data available for this gene"))
        
        lollipopPlot(
          maf = maf_obj(),
          gene = genes_to_plot,
          AACol = "HGVSp",    # or "Protein_Change"
          showMutationRate = TRUE
        )
      } else {
        validate(need(any(genes_to_plot %in% maf_obj()@gene.summary$Hugo_Symbol),
                      "No mutation data available for selected genes"))
        
        oncoplot(
          maf = maf_obj(),
          genes = genes_to_plot,
          bgCol = "#efefef",
          colors = cols,
          removeNonMutated = TRUE
        )
      }
    }, error = function(e) {
      # graceful error fallback
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No sufficient data",
                 size = 8, hjust = 0.5)
    })
  }, res = 100)
  
  # Mutation Timing -- make the density plot for distribution of mutation timing for the selected gene list
  output$timing_plot_singlegene_ui <- renderUI({
    req(get_selected_genes())
    
    gene_n <- length(get_selected_genes())
    base_height <- 150
    extra_height <- 20
    height_cap <- 1500
    
    total_height <- min(base_height + extra_height * gene_n, height_cap)
    
    plotOutput("timing_plot_singlegene", height = paste0(total_height, "px"), width = "400px")
  })
  
  output$timing_plot_singlegene <- renderPlot({
    tryCatch({
      req(cohort_data(), get_selected_genes(), input$histology_type_genelist)
      
      selected_genes <- get_selected_genes()
      diff_density <- cohort_data()$diff_allgene %>%
        filter(Hugo_Symbol %in% selected_genes)
      
      if (input$histology_type_genelist != "All") {
        diff_density <- diff_density %>%
          filter(histology_abbreviation == input$histology_type_genelist)
      }
      
      # If no rows remain
      validate(need(nrow(diff_density) > 0, "No sufficient data"))
      
      diff_density <- diff_density %>%
        mutate(mean_diff = rowMeans(as.matrix(dplyr::select(., all_of(column_names)))))
      
      p <- plot_raincloud(
        diff_density,
        x = "Hugo_Symbol",
        y = "mean_diff",
        mode = "mean",
        "",
        width = 4,
        height = 5
      )
      
      p
    }, error = function(e) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No sufficient data",
                 size = 8, hjust = 0.5)
    })
  }, res = 70)
  
  # Pathway Timing -- make the density plot for distribution of mean mutation timing for the selected gene list
  output$timing_plot_gene <- renderPlot({
    tryCatch({
      req(cohort_data(), get_selected_genes(), input$histology_type_genelist)
      
      selected_genes <- get_selected_genes()
      diff_data <- cohort_data()$diff_allgene
      histology_type <- input$histology_type_genelist
      
      if (histology_type == "All") {
        # === Case 1: barplot across all histologies ===
        p <- plot_timing_bar_by_genes(
          selected_genes = selected_genes,
          diff_data = diff_data,
          type_list = diff_data %>% distinct(sample_id, histology_abbreviation),
          column_names = column_names,
          timing_bin_levels = timing_bin_levels,
          color_vector = color_vector,
          legend = legend
        )
      } else {
        # === Case 2: pie chart for a single histology ===
        data_filter1 <- diff_data %>%
          filter(Hugo_Symbol %in% selected_genes,
                 histology_abbreviation == histology_type) %>%
          group_by(sample_id) %>%
          summarise(across(all_of(column_names), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
          mutate(
            aliquot_id = sample_id,
            early_ratio = rowMeans(across(all_of(column_names)) < 0, na.rm = TRUE),
            late_ratio = rowMeans(across(all_of(column_names)) > 0, na.rm = TRUE),
            undetemined_ratio = rowMeans(dplyr::select(., all_of(column_names)) == 0, na.rm = TRUE),
            n_late = 250 * late_ratio,
            n_undetermined = 250 * undetemined_ratio,
            ratio_plot = (n_late + n_undetermined/2) / 250,
            timing_bin = cut(ratio_plot, breaks = range_breaks, include.lowest = TRUE),
            timing_bin = factor(timing_bin, levels = names(color_vector))
          ) %>%
          count(timing_bin)
        
        if (nrow(data_filter1) < 5) {
          # No data case
          p <- ggplot() + theme_void() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "No sufficient data",
                     size = 8, hjust = 0.5)
        } else {
          # Normal pie chart case
          pie <- ggplot(data_filter1, aes(x = "", y = n, fill = timing_bin)) +
            geom_col(width = 1) +
            coord_polar(theta = "y") +
            scale_fill_manual(values = color_vector) +
            theme_void(base_size = 10) +
            labs(title = paste("Pathway Timing in", histology_type),
                 fill = "Timing Bin") +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
            )
          
          p <- suppressMessages(cowplot::plot_grid(pie, legend, ncol = 2, rel_widths = c(1, 0.15)))
        }
      }
      
      req(p)
      p
    }, error = function(e) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No sufficient data",
                 size = 14, hjust = 0.5)
    })
  })
  
  # Survival -- make survival plot by mutation timing for the selected gene list
  output$select_histology_list <- renderUI({
    req(cohort_data())
    histologies <- sort(base::unique(cohort_data()$diff_all$histology_abbreviation))
    selectInput("histology_type_genelist", "Select Cancer Type:", choices = c("All", histologies), selected = "All")
  })
  
  output$timing_survival_plot_genelist <- renderPlot({
    tryCatch({
      
      req(get_selected_genes(), input$histology_type_genelist, cohort_data())
      
      LOH_type = cohort_data()$clinical_data %>% dplyr::select(aliquot_id, histology_abbreviation)
      
      selected_genes <- get_selected_genes()
      histology_type <- input$histology_type_genelist
      wt_included <- "no"
      
      df_Reg <- cohort_data()$diff_allgene %>%
        filter(Hugo_Symbol %in% selected_genes) %>%
        group_by(sample_id) %>%
        summarise(across(all_of(column_names), ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
        mutate(
          aliquot_id = sample_id,
          early_ratio = rowMeans(across(all_of(column_names)) < 0, na.rm = TRUE),
          late_ratio = rowMeans(across(all_of(column_names)) > 0, na.rm = TRUE),
          Timing_group = case_when(early_ratio > 0.5 ~ "Mut_Early", 
                                   late_ratio > 0.5 ~ "Mut_Late",
                                   TRUE ~ "Undetermined")) %>% 
        filter(Timing_group != "Undetermined") %>% inner_join(., LOH_type, by = "aliquot_id")
      
      data_survival <- cohort_data()$surv_data
      diff_all <- df_Reg %>%
        filter(aliquot_id %in% base::unique(data_survival$aliquot_id))
      
      data_survival_sub <- if (histology_type == "All") data_survival else filter(data_survival, histology_abbreviation == histology_type)
      diff_all_sub <- if (histology_type == "All") diff_all else filter(diff_all, histology_abbreviation == histology_type)
      
      filtered_data <- diff_all_sub %>%
        left_join(data_survival_sub, by = "aliquot_id") %>%
        mutate(
          Timing_group = ifelse(is.na(Timing_group), "WT", Timing_group),
          Timing_group = factor(Timing_group, levels = c("WT", "Mut_Early", "Mut_Late"))
        )
      
      plot_survival_by_timing(
        filtered_data = filtered_data,
        wt_included = "no",
        title_prefix = ""
      )
      
    }, error = function(e) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
    })
  }, res = 90)
  
  # ICB Response - make the forest plots for ICB response for the patients with or withour mutations in the selected gene list
  observe({
    req(ICB_merge)
    therapy_choices <- c("All", base::unique(ICB_merge$Therapy))
    updateSelectInput(session, "therapy_select_genes",
                      choices = therapy_choices,
                      selected = "All")
  })
  
  output$forestplot <- renderPlot({
    tryCatch({
    req(cohort_data(), input$therapy_select_genes)
    selected_genes <- get_selected_genes()
    
    ICB_subset <- if (input$therapy_select_genes == "All") {
      ICB_merge
    } else {
      ICB_merge %>% filter(Therapy == input$therapy_select_genes)
    }
    
    plot_forest_gg(ICB_subset, selected_genes)
    }, error = function(e) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
    })}, res = 100)
  
  # ICB Survival - make the survival for ICB response for the patients with or without mutations in the selected gene list
  observe({
    req(ICB_surv)
    therapy_choices <- c("All", base::unique(ICB_surv$Therapy))
    updateSelectInput(session, "therapy_surv_genes",
                      choices = therapy_choices,
                      selected = "All")
  })
  
  output$ICB_survival_plot <- renderUI({
    tryCatch({
    req(cohort_data(), get_selected_genes(), input$therapy_surv_genes)
    selected_genes <- get_selected_genes()
    
    ICB_survival <- ICB_surv %>%
      filter(!is.na(OS_status)) %>%
      { if (input$therapy_surv_genes == "All") . else filter(., Therapy == input$therapy_surv_genes) }
    
    mutated_samples <- ICB_survival %>%
      filter(Hugo_Symbol %in% selected_genes) %>%
      distinct(SampleName, CohortName) %>%
      mutate(Mutated = 1)
    
    sample_data <- ICB_survival %>%
      distinct(SampleName, CohortName, OS_status, OS_time) %>%
      left_join(mutated_samples, by = c("SampleName", "CohortName")) %>%
      mutate(Mutated = ifelse(is.na(Mutated), 0, 1)) %>%
      filter(!is.na(OS_status), !is.na(OS_time)) %>%
      group_by(CohortName) %>%
      filter(length(base::unique(OS_status)) > 1, length(base::unique(Mutated)) > 1) %>%
      ungroup()
    
    n_plots <- length(base::unique(sample_data$CohortName))
    
    base_height <- 350  
    plots_per_row <- 4 
    n_rows <- ceiling(n_plots / plots_per_row)
    plot_height <- base_height * n_rows
    
    cols_group <- c("WT" = "#4DBBD5FF", "Mutated" = "#E64B35FF")
    legend_labels <- c("WT", "Mutated")
    
    km_plots <- sample_data %>%
      split(.$CohortName) %>%
      purrr::map(~{
        data <- .
        fit <- survfit(Surv(OS_time, OS_status) ~ Mutated, data = data)
        
        max_time <- ceiling(max(data$OS_time, na.rm = TRUE) / 12) * 12
        interval <- ceiling(max_time / 60) * 12
        
        surv_plot <- ggsurvplot(
          fit,
          data = data,
          risk.table = TRUE,
          pval = TRUE,
          conf.int = FALSE,
          legend.title = "Mutation",
          legend.labs = legend_labels,
          palette = cols_group,
          xlab = "",
          break.x.by = interval,
          risk.table.y.text.col = TRUE,
          risk.table.y.text = FALSE
        )
        
        surv_plot$plot <- surv_plot$plot +
          theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
          labs(title = base::unique(data$CohortName))
        
        cowplot::plot_grid(
          surv_plot$plot,
          surv_plot$table,
          align = "v",
          ncol = 1,
          rel_heights = c(4, 1.5)
        )
      })
    
    final_plot <- cowplot::plot_grid(plotlist = km_plots, ncol = 4, align = "v")
    
    renderPlot({
      final_plot
    }, height = plot_height, res = 70)
    
    }, error = function(e) {
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No sufficient data", size = 8, hjust = 0.5)
    })
  })
  
  ## Page 2
  output$rra_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The Dot plot shows overall ranking of the regulators acorss CRIPSR screen. Top 10 regulators and genes of interst are shown with labels.",
        "<br> The pos|rank or neg|rank are obtained from each studies and combined by using robust rank aggregation (RRA). <a href='https://pubmed.ncbi.nlm.nih.gov/22247279/' target='_blank'>(PMID: 22247279)</a>"
      )
    )
  })
  
  output$pathway_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The enrichments are generated based on top 100 positive/negative regulators in each study by MAGeCKFlute. <a href='https://pubmed.ncbi.nlm.nih.gov/30710114/' target='_blank'>(PMID: 30710114)</a>",
        "<br>The Dot plot shows the most frequently enriched pathways reported by CRISPR screen studies.",
        "<br>X-axis presents each study (Year_Journal_Author_Condition).",
        "<br>Y-axis shows the most frequently reported pathways.",
        "<br>Dot size shows the -log10(adjusted p-value) for the enrichement.",
        "<br>Each dot containes informtion for the enriched genes, adjusted p-value and NES score.",
        "<br>C5BP+KEGG+REACTOME databases are untilized as the reference database. The genesets with 5~500 are included for the analysis</p>"
      )
    )
  })
  
  ## Page 3
  output$cohortsummary_interpretation <- renderUI({
    req(cohort_data())
    HTML(
      paste0(
        "<br><strong>PCAWG:</strong> 2658 cancers from Pan-cancer analysis of whole genomes. <a href='https://pubmed.ncbi.nlm.nih.gov/32025007/' target='_blank'>(PMID: 32025007)</a>",
        "<br><strong>TCGA-OV:</strong> 314 ovarian cancers from latest TCGC-OV WGS cohort. <a href='https://pubmed.ncbi.nlm.nih.gov/21720365/' target='_blank'>(PMID: 21720365)</a>"
      )
    )
  })
  
  ## Page 4
  output$singlepathway_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The barplot shows the top cancer types with the most frequent mutations in the selected pathway.",
        "<br>The number on the right of each bar represents the number of samples with mutations in that pathway.</p>"
      )
    )
  })
  
  output$singlepathway_panel <- renderUI({
    tagList(
      #h4("Top 10 Cancer Types by Mutation Frequency"),
      withSpinner(plotOutput("top_cancer_plot", width = "400px", height = "400px")),
      br(),
      uiOutput("singlepathway_interpretation")
    )
  })
  
  output$multipathways_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The radar plot shows the mutation frequency of different immunomodulatory pathway categories across cancer types.</p>"
      )
    )
  })
  
  output$timeline_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The plot shows the mutation timing of immunomodulatory pathways and cancer drivers in selected cancer types.</p>"
      )
    )
  })
  
  output$timingsurvival_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The Kaplan-Meier (KM) plot shows survival outcomes stratified by mutation timing groups.",
        "<br><strong>Timing Difference</strong> = MeanTiming(Pathway) - BackgroundTiming",
        "<br><strong>Mut_Early:</strong> Samples where more than 50% of the 250 timing difference samplings are less than 0.",
        "<br><strong>Mut_Late:</strong> Samples where more than 50% of the 250 timing difference samplings are greater than 0."
      )
    )
  })
  
  output$cibersort_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The boxplot shows the composition of immune cell infiltration stratified by mutation timing in different immunomodulatory pathways.",
        "<br><strong>Timing Difference:</strong> = MeanTiming(Pathway) - BackgroundTiming",
        "<br><strong>Mut_Early:</strong> Samples where more than 50% of the 250 timing difference samplings are less than 0.",
        "<br><strong>Mut_Late:</strong> Samples where more than 50% of the 250 timing difference samplings are greater than 0.",
        "<br>Differences between groups are assessed using the Wilcoxon test.",
        "<br>The composition of immune cell infiltration are calculated by CIBERSORT Absolute mode. <a href='https://pubmed.ncbi.nlm.nih.gov/29344893/' target='_blank'>(PMID: 29344893)</a>",
        "<br>Specifically in PCAWG cohort, only PCAWG-TCGA samples with available RNA-seq data are included.</p>"
      )
    )
  })
  
  output$radar_interpretation <- renderUI({
    req(get_selected_genes())
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The Radar plot shows the statistal sigficance of the gene for the immunomodulatory effect. ",
        "<br><strong>The overall rank score</strong> is shown for each gene by using robust rank aggregation (RRA). <a href='https://pubmed.ncbi.nlm.nih.gov/22247279/' target='_blank'>(PMID: 22247279)</a> The direction of each gene is determined based on the overall ranking for positive selection or negative selection.",
        "<br><strong>Score > 0:</strong> If the positive ranking is higher than negative ranking, then the score is set as positive. The gene is considered as the positive regulator for the immunomodulatory effect.",
        "<br><strong>Score < 0:</strong> If the negative ranking is higher than postive ranking, then the score is set as negative. The gene is considered as the negative regulator for the immunomodulatory effect.",
        "<br><strong>P_value:</strong> The statistical sigficance of each gene is calculated by Hommel’s method. The pos|fdr and neg|fdr are used for calculating overall p-values from multiple tests. * : P < 0.1; ** : P < 0.01; *** : P < 0.001. The statistical method is adapted from the previous work. <a href='https://pubmed.ncbi.nlm.nih.gov/40023158/' target='_blank'>(PMID: 40023158)</a>"
      )
    )
  })
  
  output$mutation_freq_interpretation <- renderUI({
    req(cohort_data()$maf_data)
    HTML("
    <p><strong>Interpretation:</strong> 
    The Oncoplot shows the mutation status of the selected genes across samples. 
    The results are visuliazed by maftools <a href='https://pubmed.ncbi.nlm.nih.gov/30341162/' target='_blank'>(PMID: 30341162)</a>.
    The mutation information is derived from 
    <a href='https://www.cbioportal.org/' target='_blank'>cBioPortal</a>.
    </p>
  ")
  })
  
  output$gene_timing_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The raincloud plot shows the mutation timing distribution of the selected genes across samples.",
        "<br><strong>Timing Difference:</strong> = MeanTiming(Pathway) - BackgroundTiming",
        "<br>The genes are sorted by the mean timing difference, where negative values indicate earlier mutations and positive values indicate later mutations.</p>"
      )
    )
  })
  
  output$pathway_timing_gene_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The barplot shows the average mutation timing of all selected genes across cancer types.",
        "<br><span style='color:green;'><strong>Green</strong></span> indicates samples with mutations likely occurring early (based on the proportion of timing values less than 0).",
        "<br><span style='color:purple;'><strong>Purple</strong></span> indicates samples with mutations likely occurring late (based on the proportion of timing values greater than 0).</p>"
      )
    )
  })
  
  output$timingsurvival_gene_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The Kaplan-Meier (KM) plot shows survival outcomes stratified by mutation timing groups.",
        "<br><strong>Timing Difference:</strong> = MeanTiming(Pathway) - BackgroundTiming",
        "<br><strong>Mut_Early:</strong> Samples where more than 50% of the 250 timing difference samplings are less than 0.",
        "<br><strong>Mut_Late:</strong> Samples where more than 50% of the 250 timing difference samplings are greater than 0."
      )
    )
  })
  
  output$ICBresponse_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The forest plot shows odds ratios (OR) for ICB response in samples with versus without mutations in the selected genes.",
        "<br>OR values are calculated using a generalized linear model (GLM), adjusting for tumor mutation burden (TMB).",
        "<br>For more details, please refer to the summary table. </p>"
      )
    )
  })
  
  output$ICBsurvival_interpretation <- renderUI({
    HTML(
      paste0(
        "<p><strong>Interpretation:</strong> The Kaplan-Meier (KM) plot shows overall survival outcomes for samples with or without mutations in the selected genes.",
        "<br>For more details, please refer to the summary table. </p>"
      )
    )
  })
  
}

# Run app
shinyApp(ui, server)
