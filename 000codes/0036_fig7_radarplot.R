#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 0036_fig7_radarplot.R
#' Matylde Diouf
#' 09/03/26
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Figure 4: Radar plots (overall and by adjustment method) of 
#' TTE items:
#' - Alignment of selection criteria
#' - Overlap in arm enrollment period
#' - Time 0 alignment
#' - Description of RWD strategies
#' - Definition of primary outcome
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





# Variables used in radar plots
radar_vars <- c(
  "rp_estimand",
  "rp_select_crit",
  "rp_overlap",
  "rp_t0_eq",
  "rwd_descr"
)


# Nice labels for variables on the radar plot
var_labels <- c(
  rp_estimand    = "Estimand\ndefinition",
  rp_select_crit = "Eligibility\ncriteria\nalignment",
  rp_overlap     = "Enrollment\nperiod\noverlap",
  rp_t0_eq       = "Index date\nalignment",
  rwd_descr      = "RWD\nstrategies\ndescription"
)

# Recode vars
df_radar <- df_inclus_all %>%
  transmute(
    rp_estimand    = if_else(rp_estimand == "Yes", 1, 0),
    rp_select_crit = if_else(rp_select_crit == "Yes or Partially", 1, 0),
    rp_overlap     = if_else(rp_overlap == "Yes", 1, 0),
    rp_t0_eq       = if_else(rp_t0_eq == "Yes", 1, 0),
    rwd_descr      = if_else(rwd_descr == "Yes", 1, 0),
    adj_method_bin
  )


# Overall summary for ggradar: first column must be the group (we rename rownames)
radar_overall <- df_radar %>%
  summarise(across(all_of(radar_vars), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(group = "Overall", .before = 1)

# By-group summary for ggradar (idem)
radar_group <- df_radar %>%
  filter(adj_method_bin %in% c("Balancing-based methods",
                               "Non-balancing methods")) %>%
  group_by(adj_method_bin) %>%
  summarise(across(all_of(radar_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  rename(group = adj_method_bin)



# Plot objects
p_overall <- ggradar(
	# See help for arguments
  radar_overall,
  axis.labels = var_labels,
  values.radar = c("0%", "50%", "100%"),
  grid.min = 0,
  grid.mid = 0.5,
  grid.max = 1,
  axis.label.size = 3.5,
  legend.text.size = 9,
  fill = F,
  group.colours = "#00AFBB",
  plot.title = "(A)",
  grid.label.size = 3.5,
  gridline.label.offset = -0.3,
  group.point.size = 3,
  legend.title = ""
)

p_group <- ggradar(
  radar_group,
  axis.labels = var_labels,
  values.radar = c("0%", "50%", "100%"),
  grid.min = 0,
  grid.mid = 0.5,
  grid.max = 1,
  axis.label.size = 3.5,
  legend.text.size = 9,
  fill = F,
  group.colours = c("#2E8B57", "#C0392B"),
  plot.title = "(B)",
  grid.label.size = 3.5,
  gridline.label.offset = -0.3,
  group.point.size = 3,
  legend.title = "Adjustment method"
)


# Combine plots
radar_both <- p_overall / p_group  +
  patchwork::plot_layout(
    guides = "collect",
    nrow = 2) &
  theme(legend.position = "right")



# Save figure -------------------------------------------------------------

ggsave(
  filename = paste(figures.res.folder, "radarplots_adjmeth.tiff", sep = "/"),
  plot = radar_both,          
  width = 178 / 25.4,            # width in inches: 86 mm ÷ 25.4 mm/inch (JCE)
  height = 200 / 25.4,           # choose an appropriate height in inches
  dpi = 400,                     # set dpi according to guidelines
  units = "in",                  # units of width and height
  device = "tiff",               
  compression = "lzw" # optional, for TIFF compression
)
