# LAB_Shiny_App_scRNAseq_Viewer <img src="https://avatars.githubusercontent.com/u/157121172?s=400&u=b127d13c6c44a0f6e5368f79be98176105d1c30b&v=4" align="right" height="130"></a>

<br/>

## Overview
**scRNAseq Viewer** is a Shiny App created to quickly visualize single cell RNA sequencing data. 

- 🕐 Plots generated at the moment
- 🎨 Customize UMAP colors
- 📝 Option to download the generated plots
- ✈️ Download the current version to use it with no need for internet connection!

## Instructions
There are two ways of using the app (internet dependent/independent).
1. If you download the "RunApp.R" file and run it. It will always run the current version of the app (no need to download the newest version each time an update is released).
2. If you download the "app.R" file inside the "App/" subfolder, you will be able to run this version from anywhere.

If method 1 is performed, the "runGitHub" function is used to access the "app.R" file inside the "App/" subfolder of this repository.

### 1. README
The first thing you will see when you open the app will be a brief summary of its distribution.

### 2. UPLOAD RDS File
In this tab, the only option that will be available at the start will be to choose a file with a ".rds" extension from your computer (or from a shared folder).

This file should be structured in this way:

- Main rds file (list)
  - Expression_Matrix (list)
    - SCT (dgCMatrix object)
    - ... (MAGIC, lognormalized...)
  - Reductions (list)
    - umap (matrix with UMAP coordinates)
  - Metadata (data.frame)

After some seconds some exploratory plots will appear to see the distribution of the data.

### 3. UMAP plots: Clusters
In this tab, a UMAP will be plotted and categorical data from the metadata will be used to group the cells.

### 4. UMAP plots: Genes
In this tab, a UMAP will be plotted and cells will be colored by gene expression.
If needed, only a subset of cells can be ploted.

### 5. UMAP plots: Selected clusters
In this tab, a UMAP will be plotted and one or more groups can be highlighted.

### 6. Boxplots: Genes Expression
In this tab, boxplots will be plotted to express one or more genes.
The data needs to be grouped by one variable, but it can also be grouped by two.

<br/>

---

Feedback is welcome, feel free to ask if you want any modification or addition to the app! 
