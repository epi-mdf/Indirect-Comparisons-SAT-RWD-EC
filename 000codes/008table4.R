#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 008table4.R
#' Matylde Diouf
#' 08/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Table 4: Arms exchangeability and statistical methods, stratified 
#' by arm adjustment method.
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


df_tab_4 <- df_inclus_all %>% 
  select(
    # Statistical features
    nsn_justif,
    multiple_endpoints,
    endpoint_categ___1,
    endpoint_categ___3,
    
    # Adjustment covariates
    n_adj_vars,
    type_adj_var,
    justif_adj_vars,
    
    # Comparability assessment
    balance_plan,
    overlap_plan,
    baseline_comp_meth_fact,
    
    # Results robustness
    sens_analysis_bin,
    bias_discussion, # bias acknowledgment in discussion
    
    # Stratification var
    adj_method_bin
  )

df_tab_4$n_adj_vars <- as.numeric(df_tab_4$n_adj_vars)



# Tables ------------------------------------------------------------------

### 1. Adjustment method #######################################################

tab_4_stat <- tbl_summary(df_tab_4,
                          by = adj_method_bin,
                          type = list(c(
                            nsn_justif,
                            balance_plan,
                            overlap_plan,
                            baseline_comp_meth_fact) ~ "categorical",
                            c(type_adj_var, justif_adj_vars,
                              sens_analysis_bin,
                              bias_discussion,
                              endpoint_categ___1,
                              endpoint_categ___3,
                              multiple_endpoints) ~ "dichotomous",
                            n_adj_vars ~ "continuous"),
                          label = list(
                            multiple_endpoints ~ "Multiple outcomes",
                            endpoint_categ___1 ~ "    TTE criteria",
                            endpoint_categ___3 ~ "    Tolerance / Safety criteria",
                            n_adj_vars ~ "Number of planned variables",
                            nsn_justif ~ "Sample size rationale",
                            balance_plan ~ "Balance assessment planned",
                            overlap_plan ~ "Overlap assessment planned",
                            type_adj_var ~ "Description of variables used",
                            justif_adj_vars ~ "Justification for variables used",
                            baseline_comp_meth_fact ~ "Baseline comparison method",
                            sens_analysis_bin ~ "Sensitivity analyses planned",
                            bias_discussion ~ "Bias acknowledgment"
                          ),
                          digits = list(c(all_categorical(), all_dichotomous(),
                                          all_continuous()) ~ 0),
) %>% 
  
  modify_footnote(everything() ~ NA) %>% # remove footnote n(%), median (IQR)
  
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
ft <- as_flex_table(tab_4_stat)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = paste(tables.res.folder, "Word", "04tab4_stat.docx", sep = "/"))

rm(ft, doc)

