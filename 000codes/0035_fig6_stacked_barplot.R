#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 0035_fig6_stacked_barplot.R
#' Matylde Diouf
#' 09/03/26
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Figure 2: Stacked barplot of study outcome over time.
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



stack_barplot <- get_stacked_barplot(df = df_inclus_all)


# Save figure -------------------------------------------------------------

ggsave(
  filename = paste(figures.res.folder, "stacked_barplot_study_outcome.tiff", sep = "/"),
  plot = stack_barplot,          
  width = 178 / 25.4,            # width in inches: 86 mm ÷ 25.4 mm/inch (JCE)
  height = 178 / 25.4,           # choose an appropriate height in inches
  dpi = 400,                     # set dpi according to guidelines
  units = "in",                  # units of width and height
  device = "tiff",              
  compression = "lzw" # optional, for TIFF compression
)
