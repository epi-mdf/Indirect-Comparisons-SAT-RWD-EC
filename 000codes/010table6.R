#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 010table6.R
#' Matylde Diouf
#' 03/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Table 6/S2: Study and methodological main characteristics, stratified 
#' by control arm data source.
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


df_tab_6 <- df_inclus_all %>% 
  select(
    # Publication and study info
    publication_year,
    industry_link,
    ratio_n_analyzed,
    
    # Arm adjustment
    adj_method_bin,
    n_adj_vars,
    
    # RWD arm settings
    qualif_ctrl___1,
    qualif_ctrl___2,
    qualif_ctrl___4,
    ratio_centers,
    decalage,
    diff_start_enroll,
    rwd_strategies,
    
    # stratification variable
    rwd_source_grouped
  )

df_tab_6$n_adj_vars <- as.numeric(df_tab_6$n_adj_vars)



# Tables ------------------------------------------------------------------

tab_6_source <- tbl_summary(df_tab_6,
                          by = rwd_source_grouped,
                          type = list(c(
                            industry_link,
                            rwd_strategies,
                            adj_method_bin) ~ "categorical",
                            c(publication_year,
                              decalage,
                              n_adj_vars,
                              ratio_centers,
                              diff_start_enroll,
                              ratio_n_analyzed) ~ "continuous",
                            c(qualif_ctrl___1,
                              qualif_ctrl___2,
                              qualif_ctrl___4) ~ "dichotomous"),
                          label = list(industry_link ~ "Industry link",
                                       ratio_n_analyzed ~ "Ratio of analyzed patients (control/experimental)",
                                       rwd_strategies ~ "Multiple therapeutical strategies in the control arm",
                                       adj_method_bin ~ "Arm adjustment method",
                                       qualif_ctrl___1 ~ "    Real-World",
                                       qualif_ctrl___2 ~ "    Historical",
                                       qualif_ctrl___4 ~ "    Other",
                                       ratio_centers ~ "Ratio of participating centers (control/experimental)",
                                       publication_year ~ "Publication year",
                                       decalage ~ "Enrollment time gap",
                                       diff_start_enroll ~ "Enrollment start lag",
                                       n_adj_vars ~ "No. of planned adjustment covariates"
                          ),
                          digits = list(c(all_categorical(), 
                                          all_dichotomous(),
                                          all_continuous()) ~ 0),
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



# Convert to flextable
ft <- as_flex_table(tab_6_source)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = paste(tables.res.folder, "Word", "06tab6_source.docx", sep = "/"))

rm(ft, doc)

