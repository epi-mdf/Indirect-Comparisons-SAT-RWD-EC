#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 005table1.R
#' Matylde Diouf
#' 08/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Table 1: Single-arm trial and experimental arm features, 
#' stratified by adjustment method 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



df_tab_1 <- df_inclus_all %>% 
  select(
    ## infos trial
    publication_year_15,
    phase,
    platform_bin,
    og_monocenter,
    population___ict, 
    population___4,
    population___5,
    population___6,
    pathology_onco_fact,
    
    ## infos bras
    exp_first_enroll,
    og_study,
    og_start,
    n_post_exp,
    ratio_n_exp,
    og_center_number,
    
    ## stratification var
    adj_method_bin)



# Tables ------------------------------------------------------------------

### 1. Adjustment method #######################################################

tab_1_stat <- tbl_summary(df_tab_1,
                          by = adj_method_bin,
                          type = list(c(phase,
                                        og_start,
                                        pathology_onco_fact) ~ "categorical",
                                      c(n_post_exp,
                                        ratio_n_exp,
                                        og_center_number,
                                        exp_first_enroll) ~ "continuous",
                                      c(population___ict, 
                                        population___4,
                                        population___5,
                                        population___6,
                                        platform_bin,
                                        og_monocenter,
                                        og_study,
                                        publication_year_15) ~ "dichotomous"),
                          
                          label = list(phase ~ "Phase",
                                       exp_first_enroll ~ "Year of first enrollment",
                                       publication_year_15 ~ "Publication year >= 2015",
                                       og_study ~ "Arm derived from previously published study",
                                       platform_bin ~ "Trial registration",
                                       og_monocenter ~ "Single-center study",
                                       population___ict ~ "    Infants, children or adolescents", 
                                       population___4 ~ "    Adults",
                                       population___5 ~ "    Elderly",
                                       population___6 ~ "    Not reported",
                                       pathology_onco_fact ~ "Pathology type",
                                       og_start ~ "Index date definition",
                                       n_post_exp ~ "No. of analyzed patients",
                                       ratio_n_exp ~ "Ratio of analyzed to included patients",
                                       og_center_number ~ "Number of participating centers"),
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
ft <- as_flex_table(tab_1_stat)

# Create a new Word document
doc <- read_docx()

# Add the flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
print(doc, target = paste(tables.res.folder, "Word", "01tab1_stat.docx", sep = "/"))

rm(ft, doc)
