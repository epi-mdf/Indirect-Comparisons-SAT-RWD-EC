#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 007table3.R
#' Matylde Diouf
#' 01/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Table 3: Arms comparability, stratified 
#' by arm adjustment method.
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


df_tab_3 <- df_inclus_all %>% 
  select(
    # Arms comparability
    ctrl_same_select_crit,
    ratio_n_analyzed,
    ratio_centers,
    same_center,
    
    # Temporal alignment
    decalage,
    diff_start_enroll,
    
    # Treatment onset
    ctrl_exp_fu_start_eq,
    
    # Stratification var
    adj_method_bin
  )



# Tables ------------------------------------------------------------------

### 1. Adjustment method #######################################################

tab_3_stat <- tbl_summary(df_tab_3,
                          by = adj_method_bin,
                          type = list(c(
                            ctrl_same_select_crit,
                            same_center,
                            ctrl_exp_fu_start_eq) ~ "categorical",
                            c(ratio_centers,
                              ratio_n_analyzed,
                              diff_start_enroll,
                              decalage) ~ "continuous"),
                          label = list(ctrl_same_select_crit ~ "Same eligibility criteria",
                                       ratio_n_analyzed ~ "Ratio of included patients (control/experimental)",
                                       ratio_centers ~ "Ratio of participating centers (control/experimental)",
                                       same_center ~ "Same centers among both arms",
                                       ctrl_exp_fu_start_eq ~ "Same index date definition",
                                       decalage ~ "Enrollment time gap",
                                       diff_start_enroll ~ "Enrollment start lag"
                          ),
                          digits = list(c(all_categorical(), all_continuous()) ~ 0),
) %>% 
  
  add_overall() %>% 
  
  modify_header(label="**Characteristics**\nn(%); median (IQR)") %>%
  
  # Change "Unknown" to "NR" or "N/A"
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "Unknown",
                   "Not reported",
                   label)) 


# Export to word ----------------------------------------------------------


### 1. Adjustment method #######################################################

# Convert to flextable
ft <- as_flex_table(tab_3_stat)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = paste(tables.res.folder, "Word", "03tab3_stat.docx", sep = "/"))

rm(ft, doc)
