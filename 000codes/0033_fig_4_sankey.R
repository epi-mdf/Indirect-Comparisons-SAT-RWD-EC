#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 0033_fig_4_sankey.R
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
#' 17/03/26:
#' "ci" and "reg" refer to the previous adjustement group classification
#' "causal inference methods" --> "balancing-based methods"
#' "regression and naive" --> "non-balancing methods"
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



# # Create the dataset for sankey plots

# tmp <- df_inclus_all %>% 
#   select(record_id, adj_method_bin, 
#          atc, icd10,
#          intervention, pathology
#          # ,
#          # atc_code, atc_name, 
#          # chap, chap_title
#   )
# 
# 
# # Replace empty strings/NA with N/A
# tmp$atc[tmp$atc == ""] <- "N/A"
# tmp$atc[is.na(tmp$atc)] <- "N/A"
# tmp$icd10[tmp$icd10 == ""] <- "N/A"
# tmp$icd10[is.na(tmp$icd10)] <- "N/A"
# 
# 
# 
# # Expand dataframe to only have 1 code by cell
# library(data.table)
# tmp_dt <- data.table(tmp)
# tmp_dt <- tmp_dt[, list(atc = unlist(strsplit(atc, ";"))), 
#                  by = c("record_id", "adj_method_bin", "icd10",
#                         "intervention", "pathology")]
# tmp_dt <- tmp_dt[, list(icd10 = unlist(strsplit(icd10, ";"))), 
#                  by = c("record_id", "adj_method_bin", "atc",
#                         "intervention", "pathology")]
# 
# # clean cols
# tmp_dt[, c("atc", "icd10")] <- lapply(tmp_dt[, c("atc", "icd10")],
#                                       function(x) gsub(" ", "", x))
# 
# 
# # Join with icd and atc dictionnaries
# tmp_dt <- tmp_dt %>% 
#   mutate(icd11 = str_sub(string = icd10,
#                          start = 1, end = 4)) %>%
#   left_join(icd11_inclus_df, by = "icd11") %>% 
#   mutate(atc_code = str_sub(string = atc,
#                             start = 1, end = 3)) %>% 
#   left_join(atc_inclus_df, by = "atc_code") %>% 
#   select(record_id, adj_method_bin, 
#          intervention, atc_code, atc_name,
#          pathology, icd11, chap, chap_title) %>% 
#   mutate(chap_title = ifelse(is.na(chap_title),
#                              "N/A",
#                              chap_title)) %>% 
#   mutate(atc_name = str_to_sentence(atc_name)) %>% 
#   mutate(atc_name = ifelse(is.na(atc_name),
#                            "N/A",
#                            atc_name))
# 
# 
# tmp_sankey <- tmp_dt %>% 
#   select(adj_method_bin, chap_title, atc_name) 
# 
# tmp_sankey_wide <- tmp_sankey %>% 
#   make_long(adj_method_bin, chap_title, atc_name)
# 
# 
# # Plot
# sankey_icd_atc <- 
#   ggplot(tmp_sankey_wide, aes(x = x, 
#                               next_x = next_x, 
#                               node = node, 
#                               next_node = next_node,
#                               fill = factor(node),
#                               label = node)) +
#   geom_sankey(flow.alpha = 0.5, node.color = 1) +
#   geom_sankey_label(size = 2, color = 1, fill = "white") +
#   scale_fill_viridis_d() +
#   theme_sankey(base_size = 16) +
#   theme(legend.position = "none")
# 
# rm(tmp_sankey_wide, tmp, tmp_sankey)



# Recode icd and atc for visibility/interpretability ##########################

# Recode pathology:
# Under represented categories are coded as "other".
# Display choice in sankey, ascending by default??

# # Recode pathologies
# write.csv(tmp_dt, file = "000data/pathology_intervention_recoded.csv",
          # row.names = F)


# Import recoded dataset
pathology_intervention_recoded <- read.csv(
  paste(data.folder, "pathology_intervention_recoded.csv" , sep = "/"))

tmp_sankey2 <- pathology_intervention_recoded %>% 
  select(adj_method_bin, pathology_recoded, pathology_onco_bin, atc_name) %>% 
  mutate(pathology_onco_bin = case_when(
    pathology_onco_bin == 1 ~ "Oncology",
    pathology_onco_bin == 0 ~ "Other",
    is.na(pathology_onco_bin) ~ "N/A"
  ),
  pathology_onco_bin = factor(pathology_onco_bin,
                              levels = c("Oncology", "Other", "N/A")))

tmp_sankey2 <- tmp_sankey2 %>%
  mutate(
    pathology_onco_bin = as.character(pathology_onco_bin),
    pathology_recoded = as.character(pathology_recoded),
    atc_name = as.character(ifelse(atc_name == "N/A", "No ATC code", atc_name))
  )




# Balancing-based methods -------------------------------------------------

# Reorder factor levels AFTER converting to wide format because make_long()
# converts everything to character
tmp_sankey_wide_ci <- tmp_sankey2 %>%
  mutate(atc_name = str_wrap(atc_name, width = 40)) %>% 
  filter(adj_method_bin == "Balancing-based methods") %>% 
  make_long(pathology_onco_bin, pathology_recoded, atc_name)

# Set x (column) order manually
tmp_sankey_wide_ci <- tmp_sankey_wide_ci %>%
  mutate(x = factor(x, levels = c("pathology_onco_bin", "pathology_recoded", "atc_name")))

# Reorder node levels by x column explicitly
tmp_sankey_wide_ci <- tmp_sankey_wide_ci %>%
  group_by(x) %>%
  mutate(node = {
    if (unique(x) == "pathology_onco_bin") {
      fct_relevel(node, "Oncology", "Other")
    } else if (unique(x) == "atc_name") {
      fct_infreq(node)
    } else if (unique(x) == "pathology_recoded") {
      factor(node) # alphabetical order, non overlapping
    } else {
      factor(node)  # Convert to factor just in case
    }
  }) %>%
  ungroup()



# Regression and naive analysis -------------------------------------------

# Reorder factor levels AFTER converting to wide format because make_long()
# converts everything to character
tmp_sankey_wide_reg <- tmp_sankey2 %>%
  # mutate(atc_name = str_wrap(atc_name, width = 40)) %>% 
  filter(adj_method_bin == "Non-balancing methods") %>% 
  make_long(pathology_onco_bin, pathology_recoded, atc_name)

# Set x (column) order manually
tmp_sankey_wide_reg <- tmp_sankey_wide_reg %>%
  mutate(x = factor(x, levels = c("pathology_onco_bin", "pathology_recoded", "atc_name")))

# Reorder node levels by x column explicitly
tmp_sankey_wide_reg <- tmp_sankey_wide_reg %>%
  group_by(x) %>%
  mutate(node = {
    if (unique(x) == "pathology_onco_bin") {
      fct_relevel(node, "Oncology", "Other")
    } else if (unique(x) == "atc_name") {
      fct_infreq(node)
    } else if (unique(x) == "pathology_recoded") {
      fct_infreq(node)
    } else {
      factor(node)  # Convert to factor just in case
    }
  }) %>%
  ungroup()



# Get unique labels for all nodes of both datasets
all_labels <- unique(c(
  tmp_sankey_wide_ci$node,
  tmp_sankey_wide_reg$node
))


# Create a named palette
n_labels <- length(all_labels)

# Generate 45 distinct colors
seed_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
palette_45 <- createPalette(n_labels, seedcolors = seed_colors)

# Assign names to match your labels
common_palette <- setNames(palette_45, sort(all_labels))



# Remove end of label because of formatting issue ☠️ after making the palette
# otherwise colors are hideous
tmp_sankey_wide_reg$node <- gsub(" and analogues", "", tmp_sankey_wide_reg$node)
tmp_sankey_wide_reg$next_node <- gsub(" and analogues", "", tmp_sankey_wide_reg$next_node)



# Plot sankey diagrams ----------------------------------------------------

# Plot CI
sankey_ci <- 
  ggplot(tmp_sankey_wide_ci, aes(x = x, 
                                 next_x = next_x, 
                                 node = node, 
                                 next_node = next_node,
                                 fill = factor(node),
                                 type = "sankey",
                                 label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3, color = 1, fill = "white") +
  scale_fill_manual(values = common_palette) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none") +
  # scale_x_discrete(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL,
       subtitle = "(A)") +
  theme(plot.margin = margin(0, 0, 0, 0),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian(clip = "off")



# Plot reg
sankey_reg <- 
  ggplot(tmp_sankey_wide_reg, aes(x = x, 
                                  next_x = next_x, 
                                  node = node, 
                                  next_node = next_node,
                                  fill = factor(node),
                                  type = "sankey",
                                  label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(aes(label = node), size = 3, color = 1, fill = "white") +
  scale_fill_manual(values = common_palette) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none") +
  scale_x_discrete(
    # expand = expansion(mult = c(0, 0.1)),
    labels = c(
      "pathology_recoded" = "Pathology",
      "pathology_onco_bin" = "Pathology type",
      "atc_name" = "Drug Class (ATC, 2d level)"
    )) +
  # scale_x_discrete(expand = c(0, 0.31)) + # removes extra x padding) +
  labs(x = NULL,
       subtitle = "(B)") +
  theme(plot.margin = margin(0, 0, 0, 0),
        axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian(clip = "off")


# Combine plots ------------------------------------------------------------

sankey_both <- (sankey_ci / sankey_reg) +
  plot_layout(guides = "collect", nrow = 2, heights = c(1, 1)) +
  plot_annotation(title = NULL) &
  theme(plot.margin = unit(rep(0, 4), "pt"))



# Save figure -------------------------------------------------------------

ggsave(
  filename = paste(figures.res.folder, "sankey.tiff", sep = "/"),
  plot = sankey_both,        
  width = 178 / 25.4,             # width in inches: 86 mm ÷ 25.4 mm/inch (JCE)
  height = 250 / 25.4,           # choose an appropriate height in inches
  dpi = 400,                     # set dpi according to guidelines
  units = "in",                  # units of width and height
  device = "tiff",            
  compression = "lzw" # optional, for TIFF compression
)
