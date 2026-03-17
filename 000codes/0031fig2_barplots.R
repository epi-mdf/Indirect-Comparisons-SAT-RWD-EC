#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 0031fig2_barplots.R
#' Matylde Diouf
#' 23/06/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Figure 1: general information and publication variables
#' - Publication trends + JIF
#' - Funding type
#' - Data source
#' - Data denomination 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




# Grouped categories ------------------------------------------------------

### A ####

# Count dataset
df_rwd_count <- df_inclus_all %>%
  count(publication_year, name = "n_articles")

# Compute scaling factor between count and JIF
scale_factor <- max(df_rwd_count$n_articles, na.rm = T) / 
  max(df_inclus_all$X2023.JIF, na.rm = T)

tmp <- df_inclus_all %>%
  group_by(publication_year) %>%
  summarize(mean_JIF = mean(X2023.JIF, na.rm = TRUE)) %>%
  ungroup()

df_rwd_count <- df_rwd_count %>% 
  left_join(tmp, by = "publication_year")

# Plot
p_a <- ggplot() +
  # Bar plot for article count
  geom_col(data = df_rwd_count, aes(x = publication_year, y = n_articles), 
           fill = "steelblue", alpha = 0.7) +
  
  # Scatter plot for JIF scaled to match left y-axis
  geom_point(data = df_rwd_count, aes(x = publication_year, 
                                      y = mean_JIF * scale_factor), 
             color = "firebrick", alpha = 0.7, size = 2) +
  
  # Primary y-axis (count)
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mean 2023 JIF per year")
  ) +
  
  labs(
    x = NULL,
    title = "(A)"
  ) +
  scale_x_continuous(
    breaks = seq(1985, 2025, by = 5),   # 5 years ticks
    labels = seq(1985, 2025, by = 5)    # labels to match ticks
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "steelblue"),
    axis.text.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "firebrick"),
    axis.text.y.right = element_text(color = "firebrick")
  )



### B ####

# Datset for plotting
df_funding <- df_inclus_all %>%
  count(publication_year, funding_grouped) %>%
  group_by(publication_year) %>%
  mutate(total = sum(n)) %>%
  ungroup()

# Plot with full funding types
p_b <- ggplot(df_funding, aes(x = publication_year, y = n, fill = funding_grouped)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_brewer(palette = "Set2", name = "(B) Study Funding") +
  scale_x_continuous(
    breaks = seq(1985, 2025, by = 5),   # idem
    labels = seq(1985, 2025, by = 5)    # idem
  ) +
  labs(
    title = "(B)",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal()


### C ####

# Dataset for plotting
df_source <- df_inclus_all %>%
  count(publication_year, rwd_source_grouped) %>%
  group_by(publication_year) %>%
  mutate(total = sum(n)) %>%
  ungroup()

# Plot with source types
p_c <- ggplot(df_source, aes(x = publication_year, y = n, fill = rwd_source_grouped)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_brewer(palette = "Paired", name = "(C) Control Arm Source") +
  scale_x_continuous(
    breaks = seq(1985, 2025, by = 5),   # idem
    labels = seq(1985, 2025, by = 5)    # idem
  ) +
  labs(
    title = "(C)",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal()


### D ####

# Terminology per year
df_term <- df_inclus_all %>%
  select(record_id, publication_year, starts_with("qualif_ctrl___"), qualif_ctrl_other) %>%
  pivot_longer(
    cols = starts_with("qualif_ctrl___"),
    names_to = "qualif_var",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%
  mutate(qualif_var = case_when(
    qualif_var == "qualif_ctrl___1" ~ "Real-World",
    qualif_var == "qualif_ctrl___2" ~ "Historical",
    TRUE ~ "Other"
  ) %>% 
    fct_relevel(c("Real-World", "Historical", "Other"))) %>% 
  count(publication_year, qualif_var)


# Plot
p_d <- ggplot(df_term, aes(x = publication_year, y = n, fill = qualif_var)) +
  geom_col() +
  scale_fill_brewer(palette = "Accent", name = str_wrap("(D) Control Arm Data Denomination", width = 20)) +
  labs(title = "(D)", y = "Count", x = "Year", fill = "Data Denomination") +
  scale_x_continuous(
    breaks = seq(1985, 2025, by = 5),   # idem
    labels = seq(1985, 2025, by = 5)    # idem
  ) +
  theme_minimal()


### Combine ####

final_plot_grouped <- (p_a / p_b / p_c / p_d) +
  plot_annotation(title = "") +
  plot_layout(guides = "collect") 


### Clean env #####

rm(df_rwd_count, p_a, p_b, p_c, p_d, df_funding, df_source)


# Save figure -------------------------------------------------------------

ggsave(
  filename = paste(figures.res.folder, "stacked_barplots.tiff", sep = "/"),
  plot = final_plot_grouped,          
  width = 178 / 25.4,             # width in inches: 86 mm ÷ 25.4 mm/inch (JCE)
  height = 200 / 25.4,           # choose an appropriate height in inches
  dpi = 400,                     # set dpi according to guidelines
  units = "in",                  # units of width and height
  device = "tiff",              
  compression = "lzw" # optional, for TIFF compression
)
