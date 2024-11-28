
## Install packages if needed
required_packages <- c(
  "shiny", "ggplot2", "Seurat", "patchwork", "dplyr",
  "billboarder", "highcharter", "viridis", "RColorBrewer",
  "shinycssloaders", "DT", "scales"
)

install_if_missing <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

install_if_missing(required_packages)

lapply(required_packages, library, character.only = TRUE)

## Load Packages
library(shiny)
library(ggplot2)
library(Seurat)
library(patchwork)
library(dplyr)
library(billboarder)
library(highcharter)
library(viridis)
library(RColorBrewer)
library(shinycssloaders)
library(DT)
library(scales)

options(shiny.maxRequestSize = 3000*1024^2) 

# Options for Spinner
options(spinner.color="#59D5B4", spinner.color.background="#ffffff", spinner.size=2)

ui <- navbarPage(
  
  title = "scRNAseq Viewer v0.1.0",
  
  tabPanel(
      "README",
      tags$style(HTML("
      #mainPanel {
        width: calc(100vw - 100px) !important;
        height: calc(100vh - 100px) !important;
      }
      #imageContainer {
        position: absolute;
        top: 0;
        right: 0;
        padding: 0px;
      }
    ")),
      tags$head(
        tags$style(HTML("
    #inline label { 
      display: table-cell; 
      text-align: left; 
      vertical-align: middle; 
    }
    
    #inline .form-group { 
      display: table-row; 
      margin-bottom: 5px; 
    }

    h5.indent { 
      margin-left: 40px; 
      margin-right: 15px; 
    }
    
    /* Fixed width and height for selectize input */
    .fixed-width-select .selectize-input { 
      width: 150px !important; 
      height: 30px !important; 
      line-height: 30px !important; 
      text-align: center !important; 
      padding: 0 !important; 
      display: flex; 
      align-items: center; 
    }

    /* Ensure no height change on focus */
    .fixed-width-select .selectize-input:focus {
      height: 30px !important; 
      outline: none; 
    }

    /* Style the dropdown item to prevent text misalignment */
    .selectize-dropdown-content .option { 
      line-height: 30px !important; 
      height: 30px !important; 
    }

    /* Ensure all input boxes are aligned */
    #inline .form-group { 
      display: flex;
      align-items: center;
      justify-content: flex-start;
    }
  "))
      ),
      
      
      
      
      
      mainPanel(
        id = "mainPanel",
        div(id = "imageContainer",
            img(src = "https://avatars.githubusercontent.com/u/157121172?s=400&u=b127d13c6c44a0f6e5368f79be98176105d1c30b&v=4", 
                height = "75%", 
                width = "75%")
        ),
        
        br(),
        p("This App can be used to quickly visualize scRNAseq data and create plots."),
        br(),
        
        h3("REMARKS"),
        p("To use this tool, an RDS file with gene expression data, metadata and UMAP coordinates must be provided."),
        
        # Start two-row, two-column grid layout
        fluidRow(
          column(5,
                 wellPanel(
                   h3("UPLOAD RDS File"),
                   p("User to upload an RDS file containing the scRNAseq data"),
                   p("After being uploaded, some exploratory plots will be generated."),
                   p("Only considering variables with less than 50 factors.")
                 )
          ),
          column(5,
                 wellPanel(
                   h3("UMAP plots: Clusters"),
                   p("This tab shows a UMAP dividing samples into meta data categories."),
                   p("User can select which meta data category will be used to separate data."),
                   p("Only considering variables with less than 50 factors.")
                 )
          )
        ),
        br(),
        fluidRow(
          column(5,
                 wellPanel(
                   h3("UMAP plots: Genes"),
                   p("This tab shows a UMAP with gene expression per sample."),
                   p("1. If just one gene is selected, its expression is displayed."),
                   p("2. If there are between 2 and 9 genes, the expression of each gene is displayed in a separate UMAP."),
                   p("3. If there are more than 9 genes, the expression of the signature of all the genes is displayed.")
                 ),
                 br(),
                 strong("All UMAP plots can be downloaded in pdf")
          ),
          column(5,
                 wellPanel(
                   h3("UMAP plots: Selected clusters"),
                   p("Plots highlighting the cells that are annotated as one (or more than one) cell type / cluster.")
                 ),
                 wellPanel(
                   h3("Gene Expression"),
                   p("Plots showing the expression of selected genes per cluster.")
                 )
          )
        )
      )
      
    ),
    tabPanel("UPLOAD RDS File", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_rds", "Upload RDS file:", 
                           accept = ".rds"),
                 
                 uiOutput("Column_Info"),
                 uiOutput("Sample_Info"),
                 
                 uiOutput("vln_column"),
                 
                 uiOutput("Palette_Info"),
                 splitLayout(DTOutput("metadata_table"))
                 
               ),
               mainPanel(align="center",
                 # Espacio para mostrar el archivo cargado o los resultados de selección
                 splitLayout(cellWidths = c("40%", "60%"),
                             withSpinner(highchartOutput("pie_preview", height="38vh")),
                             withSpinner(highchartOutput("barplot", height="50vh"))),
                 withSpinner(plotOutput("violins", height="40vh", width = "30vw"))
               )
             )
    ),
    tabPanel("UMAP plots: Clusters", 
             sidebarLayout(
               sidebarPanel(
                 # Input 3: Lista dinámica de nombres de columnas
                 uiOutput("column_selector"),
                 textInput("plot_name", value = "plot", label = "Name of the plot:"),
                 uiOutput("Palette_Clusters")
                 
               ),
               mainPanel(align="center",
                 withSpinner(plotOutput("UMAP_Clusters", height="85vh", width = "110vh")),
                 downloadButton(outputId = "Clust_Download", label = "Download Plot")
               )
             )
    ),
  tabPanel("UMAP plots: Genes", 
           sidebarLayout(
             sidebarPanel(
               uiOutput("Ident"),
               uiOutput("gene"),
               checkboxInput("GroupCells", "Show only a group of cells", FALSE),
               uiOutput("column_custom_gene"),
               uiOutput("column_custom_var_gene"),
               
               br(),
               textInput("plot_name_genes", value = "plot", label = "Name of the plot:")
               
             ),
             mainPanel(align="center",
                       withSpinner(plotOutput("UMAP_Genes", height="85vh", width = "110vh")),
                       downloadButton(outputId = "Genes_Download", label = "Download Plot")
             )
           )
  ),
  
  tabPanel("UMAP plots: Selected clusters", 
           sidebarLayout(
             sidebarPanel(
               uiOutput("column_custom"),
               uiOutput("column_custom_var"),
               uiOutput("custom_name"),
               uiOutput("custom_color"),
               br(),
               textInput("plot_name_custom", value = "plot", label = "Name of the plot:")
               
             ),
             mainPanel(align="center",
                       withSpinner(plotOutput("UMAP_Custom", height="85vh", width = "110vh")),
                       downloadButton(outputId = "Custom_Download", label = "Download Plot")
             )
           )
  ),
  
  tabPanel("Boxplots: Genes Expression", 
           sidebarLayout(
             sidebarPanel(
               uiOutput("Ident_dot"),
               uiOutput("gene_dot"),
               uiOutput("column_dot"),
               uiOutput("column_custom_dot"),
               
               checkboxInput("Vln2Variables", "Divide plot by another variable", FALSE),
               uiOutput("column_dot_2"),
               uiOutput("column_custom_dot_2")
             ),
             mainPanel(
               withSpinner(plotOutput("violin", height="85vh")
               )
             )
           )
  )
 )


server <- function(input, output, session) {
  
    ## README parameters
  
  column_info <- reactive(input$column_info)
  sample_info <- reactive(input$sample_info)
  vln_column <- reactive(input$vln_column)
  palette_info <- reactive(input$palette_info)
  
  output$Column_Info <- renderUI({
    req(data())
    seurat <- data()
    selectInput("column_info", "Select Variable with *Cluster* information:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50)], selected = "cell.ident")
  })
  
  output$Sample_Info <- renderUI({
    req(data())
    seurat <- data()
    selectInput("sample_info", "Select Variable with *Sample* information:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50)], selected = "Sample_ID")
  })
  
  output$vln_column <- renderUI({
    req(data())
    seurat <- data()
    selectInput("vln_column", "Select a continuous variable from metadata to plot a violin plot",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) > 50)])
  })
  
  output$Palette_Info <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    
    selectInput("palette_info", "Choose Palette:",
                lapply(rownames(brewer.pal.info), function(x) x), selected = "Set2")
  })
  
  pal_cols_info <- reactive({
    req(data_rds())
    seurat <- data_rds()
    pal <- palette_info()
    sample_info <- sample_info()
    
    cols <- brewer.pal(n = brewer.pal.info[pal,]$maxcolors, name = pal)
    cols <- colorRampPalette(cols)(length(unique(seurat[["Metadata"]][,sample_info])))
  })
  
  ### DATA

  data <- reactive({
    req(input$file_rds)
    seu_list <- readRDS(input$file_rds$datapath)
    
  })
  
  data_rds <- reactive({
    req(data())
    req(sample_info())
    sample_info <- sample_info()
    df <- data()
    
    return(df)
  })
  
  
  ### Input Seurat File : Plots

  output$pie_preview <- renderBillboarder({
    req(data_rds())
    req(sample_info())
    seurat <- data_rds()
    sample_info <- sample_info()
    pal_cols_info <- pal_cols_info()

    y = seurat[["Metadata"]] %>%
      group_by(across(all_of(sample_info))) %>%
      summarise(value = n())

    hchart(y, type = 'pie', hcaes(x = !!(sample_info), y = value)) %>%
      hc_colors(pal_cols_info)
  })

  output$barplot <- renderHighchart({
    req(data_rds())
    req(sample_info())
    req(column_info())

    seurat <- data_rds()
    column_info <- column_info()
    sample_info <- sample_info()
    pal_cols_info <- pal_cols_info()
    both <- c(column_info, sample_info)

    y = seurat[["Metadata"]] %>%
      group_by(across(all_of(both))) %>%
      summarise(value = n())

    hchart(y, type = 'column', hcaes(x = !!(column_info), y = value, group = !!(sample_info)), showInLegend = F) %>%
      hc_colors(pal_cols_info)
  })
  
  output$violins <- renderPlot({
    req(data_rds())
    req(vln_column())
    seurat <- data_rds()
    
    pal_cols_info <- pal_cols_info()
    sample_info <- sample_info()
    vln_column <- vln_column()
    
    
    ggplot(seurat[["Metadata"]], aes(x=.data[[sample_info]], y=.data[[vln_column]], fill=.data[[sample_info]])) + 
            geom_violin() + 
            scale_fill_manual(values=pal_cols_info) +
            theme_classic() +
      theme(legend.position = "none")
    
  
    })
  
  output$metadata_table <- DT::renderDT({
    req(data_rds())
    seurat <- data_rds()
    head(seurat[["Metadata"]], 5)
  }, options = list(pageLength = 5), server = FALSE)
  
  
  ### UMAP plots: Clusters
  
  # Reactive Variables
  
  column <- reactive(input$column)
  plot_name <- reactive(input$plot_name)
  Ident <- reactive(input$Ident)
  
  # Outputs
  
  output$column_selector <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    selectInput("column", "Select variable to divide cells into groups:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50)], selected = "Sample_ID")
  })
  
  
  output$Ident <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    selectInput("Ident", "Select expression matrix:",
                choices = names(seurat[["Expression_Matrix"]]), selected = "SCT")
  })
  
  colors <- colors()
  colors2 <- c("turquoise", "salmon1", "dodgerblue2", "moccasin", "lightpink", 
               "lightgreen", "lightgoldenrod1", "lightseagreen", "darkseagreen2", 
               "goldenrod2", "khaki1", "indianred2", "grey35", "grey58", "grey79",
               "hotpink3", "lemonchiffon3", "lightcoral", "maroon4", "midnightblue",
               "olivedrab", "mediumpurple", "mistyrose3", "lightsteelblue3", "mediumslateblue")

  
  
  output$Palette_Clusters <- renderUI({
    req(data_rds())
    req(column())
    seurat <- data_rds()
    column <- column()
    
    colors.chosen <- colors2[c(1:length(unique(seurat[["Metadata"]][,column])))]
    
    groups <- unique(seurat[["Metadata"]][,column])
    output = list()
    
    for(i in 1:length(groups)) {
      output[[i]] = tagList()
      ID <- paste0(groups[i], "_pal")
      output[[i]][[1]] = div(id = "inline", 
                             class = "fixed-width-select",
                             selectInput(inputId = ID,
                                         label = h5(paste(groups[i]), class = 'indent'), 
                                         choices = colors, 
                                         selected = colors.chosen[i])
                             
      )
    }
    
    output
})
  
  output$UMAP_Clusters <- renderPlot({
    req(data_rds())
    seurat <- data_rds()
    column <- column()
    
    groups <- unique(seurat[["Metadata"]][,column])
    
    pal <- NULL
    
    for(i in 1:length(groups)){
      pal <- c(pal, input[[paste0(groups[i], "_pal")]])
    }
    
    pal2 <- setNames(as.character(pal),  
                       as.character(groups)) 
    
    
    plot_data <- data.frame(UMAP_1 = seurat[["Reductions"]][["umap"]][,1], 
                            UMAP_2 = seurat[["Reductions"]][["umap"]][,2], 
                            column = seurat$Metadata[,column])
    
    p <- ggplot(plot_data, aes(x = UMAP_1, y = UMAP_2, color = column)) +
        geom_point(size = 0.8) +
        scale_color_manual(values = pal2) +
        theme(legend.position = "right",
              legend.text = element_text(size=12),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              axis.line = element_line()) + 
        labs(color = "") + guides(colour = guide_legend(override.aes = list(size=6))) 
      
    
    plot(p)
    
  })

  output$Clust_Download<- downloadHandler(
    filename <- function(file){
      plot_name <- plot_name()
      paste0(plot_name, ".pdf")},
    content = function(file) {
      req(data_rds())
      seurat <- data_rds()
      column <- column()
      
      groups <- unique(seurat[["Metadata"]][,column])
      
      pal <- NULL
      
      for(i in 1:length(groups)){
        pal <- c(pal, input[[paste0(groups[i], "_pal")]])
      }
      
      pal2 <- setNames(as.character(pal),  
                       as.character(groups)) 
      
      
      plot_data <- data.frame(UMAP_1 = seurat[["Reductions"]][["umap"]][,1], 
                              UMAP_2 = seurat[["Reductions"]][["umap"]][,2], 
                              column = seurat$Metadata[,column])
      
      plot <- ggplot(plot_data, aes(x = UMAP_1, y = UMAP_2, color = column)) +
        geom_point(size = 0.8) +
        scale_color_manual(values = pal2) +
        theme(legend.position = "right",
              legend.text = element_text(size=12),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.background = element_rect(fill='transparent'),
              axis.line = element_line()) + 
        labs(color = "") + guides(colour = guide_legend(override.aes = list(size=6))) 
    
      
      pdf(file, width = 9, height = 7)
      print(plot)
      dev.off()
    }
  )

  
  ### UMAP plots: Genes
  
  # Reactive Variables
  
  gene <- reactive(input$gene)
  plot_name_genes <- reactive(input$plot_name_genes)  
  GroupCells <- reactive(input$GroupCells)
  column_custom_gene <- reactive(input$column_custom_gene)
  column_custom_var_gene <- reactive(input$column_custom_var_gene)
  
  # Outputs
  
  output$gene <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    textInput("gene", value = "", label = "Select gene(s):")
    
  })
  

  
  output$column_custom_gene <- renderUI({
    req(data_rds())
    req(GroupCells())
    seurat <- data_rds()
    selectInput("column_custom_gene", "Choose which variable has the desired clusters:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50)], selected = "cell.ident")
  })
  
  output$column_custom_var_gene <- renderUI({
    req(data_rds())
    req(GroupCells())
    req(column_custom_gene())
    seurat <- data_rds()
    column_custom_gene <- column_custom_gene()
    selectInput("column_custom_var_gene", "Select 1 or more clusters to be highlighted:",
                choices = unique(seurat[["Metadata"]][column_custom_gene])[[1]], multiple = TRUE)
  })
  
  
  
  output$UMAP_Genes <- renderPlot({
    req(data_rds())
    req(gene())
    seurat <- data_rds()
    gene <- gene()
    Ident <- Ident()
    
    GroupCells <- GroupCells()
    column_custom_gene <- column_custom_gene()
    column_custom_var_gene <- column_custom_var_gene()
    
    gene <- unlist(strsplit(gene, split=" "))
    
    gene <- gene[gene %in% rownames(seurat$Expression_Matrix[[Ident]])]
    

    if (length(gene) == 1) {
      gene_exprs <- as.matrix(seurat$Expression_Matrix[[Ident]][gene,])
      colnames(gene_exprs) <- gene
    } else {
      gene_exprs <- t(as.matrix(seurat$Expression_Matrix[[Ident]][gene,]))
    }
    
    plot_data <- data.frame(UMAP_1 = seurat[["Reductions"]][["umap"]][,1], 
                            UMAP_2 = seurat[["Reductions"]][["umap"]][,2])
    
    if (GroupCells) {
      cells <- rownames(seurat[["Metadata"]][which(seurat[["Metadata"]][[column_custom_gene]] %in% column_custom_var_gene),])
      plot_data_red <- plot_data[cells,]
      
      if (length(gene) == 1) {
        gene_exprs_red <- as.matrix(gene_exprs[cells,])
        colnames(gene_exprs_red) <- gene
      } else {
        gene_exprs_red <- gene_exprs[cells,]
      }
      
    } else {
      plot_data_red <- plot_data
      gene_exprs_red <- gene_exprs
    }
    
    if (length(gene) == 1) {
      
      plot_data_final <- cbind(plot_data_red, gene_exprs_red)
      
      plt <- ggplot(plot_data_final, aes(x = UMAP_1, y = UMAP_2, color = .data[[gene]])) +
        geom_point(size = 0.8) +
        theme(legend.position = "right",
              legend.text = element_text(size=12),
              axis.text.x=element_blank(), 
              axis.ticks.x=element_blank(), 
              axis.text.y=element_blank(), 
              axis.ticks.y=element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              plot.title = element_text(hjust = 0.5)) + 
        labs(color = "") + 
        ggtitle(gene) & scale_color_gradientn(colors = c("navy", "white", "darkred"))
      
      
    } else if (length(gene) < 9) {
      
      plot_data_final <- cbind(plot_data_red, as.data.frame(gene_exprs_red))
      
      plots <- vector("list", length = length(gene))
      
      for (a in 1:length(gene)){
        plots[[a]] <- ggplot(plot_data_final, aes(x = UMAP_1, y = UMAP_2, color = .data[[gene[a]]])) +
          geom_point(size = 0.8) +
          theme(legend.position = "right",
                legend.text = element_text(size=12),
                axis.text.x=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(), 
                axis.ticks.y=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.background = element_rect(fill='transparent'),
                plot.background = element_rect(fill='transparent', color=NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill='transparent'),
                plot.title = element_text(hjust = 0.5)) + 
          labs(color = "")  +
          ggtitle(gene[a]) & scale_color_gradientn(colors = c("navy", "white", "darkred"))
        
        
      }
      names(plots) = gene
      
      if (length(gene) == 1 | length(gene) > 9) {
        col = 1
      } else if (length(gene) == 2 | length(gene) == 3 | length(gene) == 4) {
        col = 2
      } else if (length(gene) == 5 | length(gene) == 6 | length(gene) == 7 | length(gene) == 8 | length(gene) == 9) {
        col = 3
      }
      
      plt <- patchwork::wrap_plots(plots, ncol=col)
      
      
    } else {
      
      plot_data_red <- cbind(plot_data_red, "Mean_Expression" = rowMeans(t(as.data.frame(gene_exprs_red))))
      
      plt <- ggplot(plot_data_red, aes(x = UMAP_1, y = UMAP_2, color = Mean_Expression)) +
        geom_point(size = 0.8) +
        theme(legend.position = "right",
              legend.text = element_text(size=12),
              axis.text.x=element_blank(), 
              axis.ticks.x=element_blank(), 
              axis.text.y=element_blank(), 
              axis.ticks.y=element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              plot.title = element_text(hjust = 0.5)) + 
        labs(color = "") + 
        ggtitle("Mean Expression") & scale_color_gradientn(colors = c("navy", "white", "darkred"))
      
    }
    
    plt
    
  })
   
  output$Genes_Download<- downloadHandler(
    filename <- function(file){
      plot_name_genes <- plot_name_genes()
      paste0(plot_name_genes, ".pdf")},
    content = function(file) {
      req(data_rds())
      req(gene())
      seurat <- data_rds()
      gene <- gene()
      Ident <- Ident()
      
      GroupCells <- GroupCells()
      column_custom_gene <- column_custom_gene()
      column_custom_var_gene <- column_custom_var_gene()
      
      gene <- unlist(strsplit(gene, split=" "))
      
      gene <- gene[gene %in% rownames(seurat$Expression_Matrix[[Ident]])]
      
      
      if (length(gene) == 1) {
        gene_exprs <- as.matrix(seurat$Expression_Matrix[[Ident]][gene,])
        colnames(gene_exprs) <- gene
      } else {
        gene_exprs <- t(as.matrix(seurat$Expression_Matrix[[Ident]][gene,]))
      }
      
      plot_data <- data.frame(UMAP_1 = seurat[["Reductions"]][["umap"]][,1], 
                              UMAP_2 = seurat[["Reductions"]][["umap"]][,2])
      
      if (GroupCells) {
        cells <- rownames(seurat[["Metadata"]][which(seurat[["Metadata"]][[column_custom_gene]] %in% column_custom_var_gene),])
        plot_data_red <- plot_data[cells,]
        
        if (length(gene) == 1) {
          gene_exprs_red <- as.matrix(gene_exprs[cells,])
          colnames(gene_exprs_red) <- gene
        } else {
          gene_exprs_red <- gene_exprs[cells,]
        }
        
      } else {
        plot_data_red <- plot_data
        gene_exprs_red <- gene_exprs
      }
      
      if (length(gene) == 1) {
        
        plot_data_final <- cbind(plot_data_red, gene_exprs_red)
        
        plt <- ggplot(plot_data_final, aes(x = UMAP_1, y = UMAP_2, color = .data[[gene]])) +
          geom_point(size = 0.8) +
          theme(legend.position = "right",
                legend.text = element_text(size=12),
                axis.text.x=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(), 
                axis.ticks.y=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.background = element_rect(fill='transparent'),
                plot.background = element_rect(fill='transparent', color=NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill='transparent'),
                plot.title = element_text(hjust = 0.5)) + 
          labs(color = "") + 
          ggtitle(gene) & scale_color_gradientn(colors = c("navy", "white", "darkred"))
        
        
      } else if (length(gene) < 9) {
        
        plot_data_final <- cbind(plot_data_red, as.data.frame(gene_exprs_red))
        
        plots <- vector("list", length = length(gene))
        
        for (a in 1:length(gene)){
          plots[[a]] <- ggplot(plot_data_final, aes(x = UMAP_1, y = UMAP_2, color = .data[[gene[a]]])) +
            geom_point(size = 0.8) +
            theme(legend.position = "right",
                  legend.text = element_text(size=12),
                  axis.text.x=element_blank(), 
                  axis.ticks.x=element_blank(), 
                  axis.text.y=element_blank(), 
                  axis.ticks.y=element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_rect(fill='transparent'),
                  plot.background = element_rect(fill='transparent', color=NA),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.background = element_rect(fill='transparent'),
                  plot.title = element_text(hjust = 0.5)) + 
            labs(color = "")  +
            ggtitle(gene[a]) & scale_color_gradientn(colors = c("navy", "white", "darkred"))
          
          
        }
        names(plots) = gene
        
        if (length(gene) == 1 | length(gene) > 9) {
          col = 1
        } else if (length(gene) == 2 | length(gene) == 3 | length(gene) == 4) {
          col = 2
        } else if (length(gene) == 5 | length(gene) == 6 | length(gene) == 7 | length(gene) == 8 | length(gene) == 9) {
          col = 3
        }
        
        plt <- patchwork::wrap_plots(plots, ncol=col)
        
        
      } else {
        
        plot_data_red <- cbind(plot_data_red, "Mean_Expression" = rowMeans(t(as.data.frame(gene_exprs_red))))
        
        plt <- ggplot(plot_data_red, aes(x = UMAP_1, y = UMAP_2, color = Mean_Expression)) +
          geom_point(size = 0.8) +
          theme(legend.position = "right",
                legend.text = element_text(size=12),
                axis.text.x=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(), 
                axis.ticks.y=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.background = element_rect(fill='transparent'),
                plot.background = element_rect(fill='transparent', color=NA),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill='transparent'),
                plot.title = element_text(hjust = 0.5)) + 
          labs(color = "") + 
          ggtitle("Mean Expression") & scale_color_gradientn(colors = c("navy", "white", "darkred"))
        
      }
      
      if (length(gene) == 1 | length(gene) == 2 | length(gene) > 9) {
        height = 8
      } else if (length(gene) == 3 | length(gene) == 4 | length(gene) == 5 | length(gene) == 6) {
        height = 16
      } else if (length(gene) == 7 | length(gene) == 8 | length(gene) == 9) {
        height = 24
      }
      
      if (length(gene) == 1 | length(gene) > 9) {
        width = 9
      } else if (length(gene) == 2 | length(gene) == 3 | length(gene) == 4) {
        width = 18
      } else if (length(gene) == 5 | length(gene) == 6 | length(gene) == 7 | length(gene) == 8 | length(gene) == 9) {
        width = 27
      }
      
      pdf(file, width = width, height = height)
      print(plt)
      dev.off()
    }
  )
  
  ### UMAP plots: Selected clusters
  
  # Reactive Variables
  
  column_custom <- reactive(input$column_custom) 
  column_custom_var <- reactive(input$column_custom_var) 
  custom_name <- reactive(input$custom_name)
  custom_color <- reactive(input$custom_color)
  
  # Outputs
  
  output$column_custom <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    selectInput("column_custom", "Choose which variable has the desired clusters:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50)], selected = "cell.ident")
  })

  output$column_custom_var <- renderUI({
    req(data_rds())
    req(column_custom())
    seurat <- data_rds()
    column_custom <- column_custom()
    selectInput("column_custom_var", "Select 1 or more clusters to be highlighted:",
                choices = unique(seurat[["Metadata"]][column_custom])[[1]], multiple = TRUE)
  })
  
  output$custom_name <- renderUI({
    req(data_rds())
    req(column_custom_var())
    column_custom_var <- column_custom_var()
    
    if (length(column_custom_var) == 1) {
      textInput("custom_name", value = column_custom_var, label = "Name the Cluster Combination:")
    } else if (length(column_custom_var) > 1) {
      textInput("custom_name", value = "Custom", label = "Name the Cluster Combination:")
    }
  })

  output$custom_color <- renderUI({
    req(data_rds())
    req(column_custom_var())
    column_custom_var <- column_custom_var()
    
    textInput("custom_color", value = "#FF3FFF", label = "Pick a colour:")
    
  })
  
  output$UMAP_Custom <- renderPlot({
    req(data_rds())
    req(column_custom_var())
    seurat <- data_rds()
    
    column_custom_var <- column_custom_var()
    column_custom <- column_custom()
    
    custom_color <- custom_color()
    name <- custom_name()
    
    plot_data <- data.frame(UMAP_1 = seurat[["Reductions"]][["umap"]][,1], 
                            UMAP_2 = seurat[["Reductions"]][["umap"]][,2])
    
    plot_data$custom <- ifelse(seurat[["Metadata"]][column_custom][[1]] %in% column_custom_var, name, "Other")
    
    plt <- ggplot(plot_data, aes(x = UMAP_1, y = UMAP_2, color = custom)) +
      geom_point(size = 0.8) + 
      scale_color_manual(values = setNames(c(custom_color, "#CFCFCF"), c(name, "Other"))) +
      theme(legend.position = "right",
            legend.text = element_text(size=12),
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank(), 
            axis.text.y=element_blank(), 
            axis.ticks.y=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent'),
            plot.title = element_text(hjust = 0.5)) + 
      labs(color = "") + 
      ggtitle(name)
    
    plt
  })
  
  plot_name_custom <- reactive(input$plot_name_custom)
  
  output$Custom_Download<- downloadHandler(
    filename <- function(file){
      plot_name_custom <- plot_name_custom()
      paste0(plot_name_custom, ".pdf")},
    content = function(file) {
      req(data_rds())
      req(column_custom_var())
      seurat <- data_rds()
      
      column_custom_var <- column_custom_var()
      column_custom <- column_custom()
      
      custom_color <- custom_color()
      name <- custom_name()
      
      plot_data <- data.frame(UMAP_1 = seurat[["Reductions"]][["umap"]][,1], 
                              UMAP_2 = seurat[["Reductions"]][["umap"]][,2])
      
      plot_data$custom <- ifelse(seurat[["Metadata"]][column_custom][[1]] %in% column_custom_var, name, "Other")
      
      plt <- ggplot(plot_data, aes(x = UMAP_1, y = UMAP_2, color = custom)) +
        geom_point(size = 0.8) + 
        scale_color_manual(values = setNames(c(custom_color, "#EFEFEF"), c(name, "Other"))) +
        theme(legend.position = "right",
              legend.text = element_text(size=12),
              axis.text.x=element_blank(), 
              axis.ticks.x=element_blank(), 
              axis.text.y=element_blank(), 
              axis.ticks.y=element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill='transparent'),
              plot.title = element_text(hjust = 0.5)) + 
        labs(color = "") + 
        ggtitle(name)
      
      
      pdf(file, width = 9, height = 7)
      print(plt)
      dev.off()
    })
  
  ### Genes Expression (Dotplot + Violin)
  
  # Reactive Variables  
  
  column_dot <- reactive(input$column_dot)
  column_custom_dot <- reactive(input$column_custom_dot)
  gene_dot <- reactive(input$gene_dot)
  Ident_dot <- reactive(input$Ident_dot)
  column_dot_2 <- reactive(input$column_dot_2)
  column_custom_dot_2 <- reactive(input$column_custom_dot_2)
  Vln2Variables <- reactive(input$Vln2Variables)
  
  # Outputs
  
  output$Ident_dot <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    selectInput("Ident_dot", "Select expression matrix:",
                choices = names(seurat[["Expression_Matrix"]]), selected = "SCT")
  })
  
  output$gene_dot <- renderUI({
    req(data_rds())
    textInput("gene_dot", value = "", label = "Select gene(s):")
  })
  
  output$column_dot <- renderUI({
    req(data_rds())
    seurat <- data_rds()
    selectInput("column_dot", "Select Variable:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50)], selected = "cell.ident")
  })
  
  output$column_custom_dot <- renderUI({
    req(data_rds())
    req(column_dot())
    seurat <- data_rds()
    column_dot <- column_dot()
    selectInput("column_custom_dot", "Select 1 or more clusters:",
                choices = names(table(seurat[["Metadata"]][column_dot]))[which(table(seurat[["Metadata"]][column_dot]) > 3)], multiple = TRUE)
  })
  
  
  output$column_dot_2 <- renderUI({
    req(data_rds())
    req(Vln2Variables())
    seurat <- data_rds()
    column_dot <- column_dot()
    selectInput("column_dot_2", "Select Second Variable:",
                choices = names(seurat[["Metadata"]])[which(apply(seurat[["Metadata"]], 2, function(x) length(unique(x))) < 50 & !names(seurat[["Metadata"]]) %in% column_dot)])
  })
  
  output$column_custom_dot_2 <- renderUI({
    req(data_rds())
    req(Vln2Variables())
    req(column_dot_2())
    seurat <- data_rds()
    column_dot_2 <- column_dot_2()
    selectInput("column_custom_dot_2", "Select 1 or more clusters:",
                choices = names(table(seurat[["Metadata"]][column_dot_2]))[which(table(seurat[["Metadata"]][column_dot_2]) > 3)], multiple = TRUE)
  })
  

  output$violin <- renderPlot({
    req(data_rds())
    req(gene_dot())
    req(column_custom_dot())
    
    seurat <- data_rds()
    column_dot <- column_dot()
    column_custom_dot <- column_custom_dot()
    gene_dot <- gene_dot()
    Ident_dot <- Ident_dot()
    
    Vln2Variables <- Vln2Variables()
    
    column_dot_2 <- column_dot_2()
    column_custom_dot_2 <- column_custom_dot_2()
    
    gene_dot <- unlist(strsplit(gene_dot, split=" "))
    
    gene_dot <- gene_dot[gene_dot %in% rownames(seurat$Expression_Matrix[[Ident_dot]])]
    

    plots <- vector("list", length = length(gene_dot))
    
    for (a in 1:length(gene_dot)){
      
      if (Vln2Variables) {
        plot_data <- seurat[["Metadata"]][c(column_dot, column_dot_2)] 
        
        plot_data <- cbind(plot_data, seurat$Expression_Matrix[[Ident_dot]][gene_dot[a],])
        
        colnames(plot_data) <- c(colnames(plot_data[c(1:2)]), gene_dot[a])
        
        
        mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique(plot_data[[column_dot_2]])))
        
        mycolors2 <- setNames(mycolors, unique(plot_data[[column_dot_2]]))
        
        
        plot_data <- plot_data[plot_data[[column_dot]] %in% column_custom_dot,]
        
        plot_data <- plot_data[plot_data[[column_dot_2]] %in% column_custom_dot_2,]
        
      } else {
        plot_data <- seurat[["Metadata"]][column_dot] 
        
        plot_data <- cbind(plot_data, seurat$Expression_Matrix[[Ident_dot]][gene_dot[a],])
        
        
        mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(length(unique(plot_data[[column_dot]])))
        
        mycolors2 <- setNames(mycolors, unique(plot_data[[column_dot]]))
        
        
        colnames(plot_data) <- c(colnames(plot_data[1]), gene_dot[a])
        
        plot_data <- plot_data[plot_data[[column_dot]] %in% column_custom_dot,]
        
      }


      
      if (Vln2Variables) {
        plots[[a]] <- ggplot(plot_data, aes(x=reorder(.data[[column_dot]],-(.data[[gene_dot[a]]]), mean) , y=.data[[gene_dot[a]]], fill=.data[[column_dot_2]], )) + 
          geom_boxplot(position = position_dodge(width = 0.9), size = 1.2, color = "black") +
          geom_point(position = position_jitterdodge(seed = 1, dodge.width = 0.9, jitter.width = 0.08), size = 0.3) +
          scale_fill_manual(values=mycolors2) +
          scale_y_continuous(limits = c(0, NA), oob = scales::squish) +
          scale_x_discrete(guide = guide_axis(angle = 20)) +
          theme_classic() +
          theme(legend.position = "right",
                legend.text = element_text(face="bold", size=18),
                panel.background = element_rect(fill='transparent'),
                plot.background = element_rect(fill='transparent', color=NA),
                legend.background = element_rect(fill='transparent')) + 
          theme(axis.text = element_text(size = 14, color = "black"),
                plot.title = element_text(face="bold", hjust = 0.5, size = 16),
                axis.title = element_text(size = 14),
                axis.line = element_line(size=1.2),
                legend.title=element_blank(),
                legend.key.size = unit(8,"mm")) +
          ylab("Gene expression") +
          xlab(element_blank()) +
          ggtitle(gene_dot[a])
      } else {
        plots[[a]] <- ggplot(plot_data, aes(x=reorder(.data[[column_dot]],-(.data[[gene_dot[a]]]), mean), y=.data[[gene_dot[a]]], fill=.data[[column_dot]])) + 
          geom_boxplot(size = 1.1, color = "black") +
          geom_point(position = position_jitter(seed = 1, width = 0.08), size = 0.5) +
          scale_fill_manual(values=mycolors2) +
          scale_y_continuous(limits = c(0, NA), oob = scales::squish) +
          scale_x_discrete(guide = guide_axis(angle = 20)) +
          theme_classic() +
          theme(legend.position = "none",
                axis.text = element_text(size = 14, color = "black"),
                plot.title = element_text(face="bold", hjust = 0.5, size = 16),
                axis.title = element_text(size = 14),
                axis.line = element_line(size=1.2)) +
          ylab("Gene expression") +
          xlab(column_dot) +
          ggtitle(gene_dot[a])
      }

      
    }
    
    names(plots) = gene_dot
    
    if (length(gene_dot) == 1 | length(gene_dot) > 9) {
      col = 1
    } else if (length(gene_dot) == 2 | length(gene_dot) == 3 | length(gene_dot) == 4) {
      col = 2
    } else if (length(gene_dot) == 5 | length(gene_dot) == 6 | length(gene_dot) == 7 | length(gene_dot) == 8 | length(gene_dot) == 9) {
      col = 3
    }
    
    plt <- patchwork::wrap_plots(plots, ncol=col)
    
    plt
  })
  

}

shinyApp(ui = ui, server = server)


