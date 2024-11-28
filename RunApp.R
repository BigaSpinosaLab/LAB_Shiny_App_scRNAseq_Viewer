# ---------------------------------------------------------------- #
#  Description : This script is for running the "scRNAseq_Viewer"  # 
#                Shiny App.                                        #
# ---------------------------------------------------------------- #


# Load Shiny Package
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

# Run Shiny App
runGitHub("LAB_Shiny_App_scRNAseq_Viewer", "BigaSpinosaLab", subdir = "App/")
