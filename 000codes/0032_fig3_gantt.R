#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 0032_fig3_gantt.R
#' Matylde Diouf
#' 23/06/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Figure n: Gantt chart for experimental and control
#' arm enrollement timeline
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



gantt_caus_inf <- get_enrollment_period(strata = "Balancing-based methods",
                                        nn = "(A)",
                                        title = "Experimental and Control Arms Enrollment Timeline")
gantt_regression_naive <- get_enrollment_period(strata = "Non-balancing methods",
                                                nn = "(B)",
                                                title = "")

# Combine plots
gantt_both <- gantt_caus_inf / gantt_regression_naive  +
  patchwork::plot_layout(
    guides = "collect",
    nrow = 2) &
  theme(legend.position = "right")



# Save figure -------------------------------------------------------------

ggsave(
  filename = paste(figures.res.folder, "gantt.tiff", sep = "/"),
  plot = gantt_both,          
  width = 178 / 25.4,            # width in inches: 86 mm ÷ 25.4 mm/inch (JCE)
  height = 200 / 25.4,           # choose an appropriate height in inches
  dpi = 400,                     # set dpi according to guidelines
  units = "in",                  # units of width and height
  device = "tiff",              
  compression = "lzw" # optional, for TIFF compression
)


# # Poster
# ggsave(
#   filename = paste(figures.res.folder, "gantt_poster.svg", sep = "/"),
#   plot = p,          
#   width = 178 / 25.4,             # width in inches (mm/25.4 == inches)
#   height = 200 / 25.4,           # height in inches
#   dpi = 400,                     # set dpi according to guidelines
#   units = "in"                  # units of width and height
# )
