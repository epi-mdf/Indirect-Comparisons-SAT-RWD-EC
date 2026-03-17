#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 001main.R
#' Matylde Diouf
#' 26/02/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Main script for the analyses of the systematic review
#' SAT designs with RWD EC.
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




# Libraries and utils loading ---------------------------------------------

library(tidyverse)
library(gtsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(pheatmap)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(ggpattern)
library(kableExtra)
library(patchwork)
library(janitor)
library(compareDF)
library(plotly)
library(ggsankey)
library(forcats)
library(Polychrome)
library(scales)
library(stringr)
library(flextable)
library(officer)
library(ggradar)


# Parameters to set up ----------------------------------------------------

main.folder <- getwd() # /!\ project
data.folder <- "000data"
codes.folder <- "000codes"
figures.res.folder <- "000results/002_figures"
tables.res.folder <- "000results/001_tables"


# Data loading ------------------------------------------------------------

# Global env handmade functions
source(paste(codes.folder, "000utils.R", sep = "/"))

# Main dataset
df <- read.table(paste(data.folder, "data_100326.csv", sep = "/"),
                 sep = ",",
                 h = T)

# Recoded pathology and intervention
pathology_recoded <- read.csv(paste(data.folder, "pathology_recoded.csv", sep = "/"))

# ATC codes
atc_raw <- read.csv(paste(data.folder, "atc_output", "WHO ATC-DDD 2025-02-25.csv", sep = "/"))

# ICD codes
icd_raw <- read.csv(paste(data.folder, "icd11", "icd11_250225.csv", sep = "/"))
icd_chap <- read.table(paste(data.folder, "icd11", "icd11_chapters.txt", sep = "/"),
                       h = T, sep = ";")

# JCR ranking
jcr <- read.table(paste(data.folder, "MatyldeDIouf_JCR_JournalResults_05_2025.csv", sep = "/"),
                  sep = ",",
                  h = T)



# Data management ---------------------------------------------------------

source(paste(codes.folder, "002data_management.R", sep = "/"))



# Analyses ----------------------------------------------------------------

### Figures ####################################################################

source(paste(codes.folder, "003figures.R", sep = "/"))




### Descriptive tables #########################################################

source(paste(codes.folder, "005table1.R", sep = "/"))
source(paste(codes.folder, "006table2.R", sep = "/"))
source(paste(codes.folder, "007table3.R", sep = "/"))
source(paste(codes.folder, "008table4.R", sep = "/"))
source(paste(codes.folder, "009table5.R", sep = "/"))
source(paste(codes.folder, "010table6.R", sep = "/"))




### Cross validation ###########################################################

source(paste(codes.folder, "010discrepancies.R", sep = "/"))



# Clean env ---------------------------------------------------------------

sessionInfo() %>% 
  capture.output() %>% 
  write(file = "05_data_analysis/000codes/000sessionInfo.txt")


rm(list = setdiff(ls(),
                  c("df", "df_diff", "atc_raw", "icd_raw", "icd_chap", "nct_countries",
                    "jcr", "main.folder", "pathology_recoded",
                    "%!in%", "round_any",
                    grep("^get_", ls(), value = T),
                    grep(".folder$", ls(), value = T))))
