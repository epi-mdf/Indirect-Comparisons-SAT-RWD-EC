#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 006table2.R
#' Matylde Diouf
#' 08/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Table 2: Real-world control arm data features, 
#' stratified by adjustment method
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




df_tab_2 <- df_inclus_all %>% 
  select(
    # RWD settings
    justif_rwd_def,
    qualif_ctrl___1,
    qualif_ctrl___2,
    qualif_ctrl___4,
    
    # Control arm settings
    rwd_source_grouped,
    rwd_strategies,
    ctrl_first_enroll,
    n_post_ctrl,
    ratio_n_ctrl,
    ctrl_number_centers,
    
    # stratification var
    adj_method_bin)



# Tables ------------------------------------------------------------------

### 1. Adjustment method #######################################################

tab_2_stat <- tbl_summary(df_tab_2,
                          by = adj_method_bin,
                          type = list(c(justif_rwd_def,
                                        rwd_source_grouped,
                                        rwd_strategies) ~ "categorical",
                                      c(qualif_ctrl___1,
                                        qualif_ctrl___2,
                                        qualif_ctrl___4) ~ "dichotomous",
                                      c(n_post_ctrl,
                                        ratio_n_ctrl,
                                        ctrl_first_enroll,
                                        ctrl_number_centers) ~ "continuous"),
                          label = list(justif_rwd_def ~ "Rationale for using RWD",
                                       qualif_ctrl___1 ~ "    Real-World",
                                       qualif_ctrl___2 ~ "    Historical",
                                       qualif_ctrl___4 ~ "    Other",
                                       ctrl_first_enroll ~ "Year of first enrollment",
                                       ctrl_number_centers ~ "Number of participating centers",
                                       rwd_source_grouped ~ "Control arm data source",
                                       rwd_strategies ~ "Multiple therapeutical strategies in the control arm",
                                       n_post_ctrl ~ "No. of analyzed patients",
                                       ratio_n_ctrl ~ "Ratio of analyzed to included patients"),
                          digits = list(c(all_categorical(),
                                          all_dichotomous(),
                                          all_continuous()) ~ 0),
) %>%
  
  add_overall() %>% 
  
  modify_header(label = "**Characteristics**\nn(%); median (IQR)") %>% 
  
  # Change "Unknown" to "NR" or "N/A"
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "Unknown",
                   "Not reported",
                   label)) 


# Export to word ----------------------------------------------------------


### 1. Adjustment method #######################################################

# Convert to flextable
ft <- as_flex_table(tab_2_stat)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = paste(tables.res.folder, "Word", "02tab2_stat.docx", sep = "/"))

rm(ft, doc)

