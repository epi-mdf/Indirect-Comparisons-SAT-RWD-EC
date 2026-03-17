#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 009table5.R
#' Matylde Diouf
#' 14/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Table 5: RWD settings and arm comparability, stratified by adjustment 
#' type
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


df_tab_5 <- df_inclus_all %>% 
  filter(adj_method_bin == "Balancing-based methods") %>% 
  select(
    # Study details
    publication_year,
    pathology_onco_fact,
    
    # RWD settings
    justif_rwd_def,
    qualif_ctrl___1,
    qualif_ctrl___2,
    qualif_ctrl___4,
    rwd_source_grouped,
    
    ctrl_same_select_crit,
    same_center,
    decalage,
    diff_start_enroll,
    ctrl_exp_fu_start_eq,
    
    # Sample sizes
    ratio_n_included,
    ratio_n_analyzed,
    
    # Arm comparability
    type_adj_var,
    justif_adj_vars,
    n_adj_vars,
    baseline_comp_meth_fact,
    
    # Stratification variable
    adj_method_all
  )

# Format numeric cols
df_tab_5$n_adj_vars <- as.numeric(df_tab_5$n_adj_vars)
df_tab_5$publication_year <- as.numeric(df_tab_5$publication_year)
df_tab_5$adj_method_all <- droplevels(df_tab_5$adj_method_all)




tab_5_stat <- tbl_summary(df_tab_5,
                          by = adj_method_all,
                          type = list(
                            c(pathology_onco_fact,
                              justif_rwd_def,
                              rwd_source_grouped,
                              ctrl_same_select_crit,
                              same_center,
                              ctrl_exp_fu_start_eq,
                              baseline_comp_meth_fact) ~ "categorical",
                            c(decalage,
                              publication_year,
                              ratio_n_included,
                              ratio_n_analyzed,
                              diff_start_enroll,
                              n_adj_vars) ~ "continuous",
                            c(qualif_ctrl___1,
                              qualif_ctrl___2,
                              qualif_ctrl___4,
                              type_adj_var,
                              justif_adj_vars) ~ "dichotomous"),
                          
                          digits = list(c(all_categorical(), all_dichotomous(),
                                          all_continuous()) ~ 0),
                          label = list(
                            publication_year ~ "Publication year",
                            pathology_onco_fact ~ "Pathology type",
                            justif_rwd_def ~ "Rationale for using RWD",
                            qualif_ctrl___1 ~ "    Real-World",
                            qualif_ctrl___2 ~ "    Historical",
                            qualif_ctrl___4 ~ "    Other",
                            type_adj_var ~ "Description of adjustment variables used",
                            justif_adj_vars ~ "Justification for adjustment variables used",
                            rwd_source_grouped ~ "Control arm data source",
                            ctrl_same_select_crit ~ "Same eligibility criteria",
                            same_center ~ "Same centers among both arms",
                            decalage ~ "Enrollment time gap",
                            diff_start_enroll ~ "Enrollment start lag",
                            ctrl_exp_fu_start_eq ~ "Same index date definition",
                            n_adj_vars ~ "Number of planned adjustment variables",
                            ratio_n_included ~ "Ratio of included patients in the control/experimental arm",
                            ratio_n_analyzed ~ "Ratio of analyzed patients in the control/experimental arm",
                            baseline_comp_meth_fact ~ "Baseline comparison method")) %>% 
  
  modify_footnote(everything() ~ NA) %>% # to remove footnote n(%), median (IQR)
  
  add_overall() %>% 
  
  modify_header(label="**Characteristics**\nn(%); median (IQR)") %>% 
  # Change "Unknown" from "NR" to "N/A"
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "Unknown",
                   "Not reported",
                   label)) 



# Export to word ----------------------------------------------------------

# Convert to flextable
ft <- as_flex_table(tab_5_stat)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = paste(tables.res.folder, "Word", "05tab5_stat.docx", sep = "/"))

rm(ft, doc)
