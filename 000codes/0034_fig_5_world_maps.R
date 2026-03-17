#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 0034_fig_5_world_maps.R
#' Matylde Diouf
#' 03/07/25
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



# /!\ when updating database, update countries list by hand, see function

tmp_ci <- df_inclus_all %>% 
  filter(adj_method_bin == "Balancing-based methods")
tmp_reg <- df_inclus_all %>%  
  filter(adj_method_bin == "Non-balancing methods")

map_first_CI <- get_map_density(df = tmp_ci,
                                variable = "country_first_author",
                                type = "(A)")
 
map_first_naive <- get_map_density(df = tmp_reg,
                                   variable = "country_first_author",
                                   col_low = "lightblue",
                                   col_high = "darkcyan",
                                   type = "(B)")
 


# Combine plots ------------------------------------------------------------

maps_both <- (map_first_CI / map_first_naive) +
  plot_layout(guides = "keep", nrow = 2, heights = c(1, 1)) +
  plot_annotation(title = NULL) &
  theme(plot.margin = unit(rep(0, 4), "pt"))




# Save figure -------------------------------------------------------------

ggsave(
  filename = paste(figures.res.folder, "world_maps_first.tiff", sep = "/"),
  plot = maps_both,        
  width = 178 / 25.4,            # width in inches: 86 mm ÷ 25.4 mm/inch (JCE)
  height = 250 / 25.4,           # choose an appropriate height in inches
  dpi = 400,                     # set dpi according to guidelines
  units = "in",                  # units of width and height
  device = "tiff",              
  compression = "lzw" # optional, for TIFF compression
)

