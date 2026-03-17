#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 002data_management.R
#' Matylde Diouf
#' 26/02/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Data management, organized by forms.
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





# General -----------------------------------------------------------------


# Delete excluded instances
## We chose to mark as "complete" the ID form when the paper is included
## in the analysis
# str(df$id_complete) # int non string

df_inclus <- df %>%
  filter(id_complete == 2)

# df_diff_inclus <- df_diff %>% 
#   filter(id_complete == 2)


id_paper_min <- 5
id_paper_max <- grep("paper_complete", colnames(df))
id_exp_min <- id_paper_max + 1
id_exp_max <- grep("experimental_arm_complete", colnames(df))
id_ctrl_min <- id_exp_max + 1
id_ctrl_max <- grep("control_arm_complete", colnames(df))
id_stat_min <- id_ctrl_max + 1
id_stat_max <- grep("statistical_methods_indirect_comparison_complete", colnames(df))
id_res_min <- id_stat_max + 1
id_res_max <- grep("results_complete", colnames(df))


# Organize columns by forms to create associated df
id_form <- colnames(df[, c(1, 4)])
paper_form <- colnames(df[, id_paper_min:id_paper_max])
exp_form <- colnames(df[, id_exp_min:id_exp_max])
control_form <- colnames(df[, id_ctrl_min:id_ctrl_max])
stat_form <- colnames(df[id_stat_min:id_stat_max])
results_form <- colnames(df[, id_res_min:id_res_max])


# Create "paper form" df for included instances
df_inclus_paper <- df_inclus %>% 
  select(record_id, all_of(paper_form))
# str(df_inclus_paper)


# Create "exp arm form" df for included instances
df_inclus_exp <- df_inclus %>% 
  select(record_id, all_of(exp_form))
# str(df_inclus_exp)


# Create "control arm form" df for included instances
df_inclus_control_tmp <- df %>% 
  filter(record_id %in% df_inclus$record_id & redcap_repeat_instrument == "control_arm") %>%
  select(record_id, all_of(control_form))
# record no 57: 2 control groups
# record no 70: 2 control groups
# str(df_inclus_control)

# Pool (compute median) repeated instances
df_inclus_control <- df_inclus_control_tmp %>%
  select(-notes_control) %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  group_by(record_id) %>%
  dplyr::summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) 


# Create "stat arm form" df for included instances
df_inclus_stat <- df_inclus %>% 
  select(record_id, all_of(stat_form))
# str(df_inclus_stat)


# Create "results form" df for included instances
df_inclus_results_tmp <- df %>% 
  filter(record_id %in% df_inclus$record_id & redcap_repeat_instrument == "results") %>%
  select(record_id, all_of(results_form))

# Pool (compute median) repeated instances
df_inclus_results <- df_inclus_results_tmp %>%
  select(-notes_results) %>% 
  mutate(across(everything(), ~ as.numeric(.))) %>% 
  group_by(record_id) %>%
  dplyr::summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))

df_pca <- reduce(list(df_inclus_paper, df_inclus_exp, 
                      df_inclus_control, df_inclus_stat,
                      df_inclus_results),
                 left_join, by = "record_id")



# JCR ---------------------------------------------------------------------


# Issue with some articles: the - is lacking in issn
df$issn[which(df$issn == "0011393X")] <- "0011-393X"
df$issn[which(df$issn == "10532498")] <- "1053-2498"
df$issn[which(df$issn == "23272236")] <- "2327-2236"

# Cols of interest
jcr_keep <- jcr %>%
  select(-c(Edition, Total.Citations, X..of.Citable.OA, Category, JIF.Quartile))

jcr_keep <- jcr_keep[!duplicated(jcr_keep),]


# To later add JIF and JCI metrics to df_inclus_paper
jcr_keep2 <- jcr_keep %>%
  select(eISSN, ISSN, X2023.JIF, X2023.JCI)
colnames(jcr_keep2)[1] <- "issn"




# ICD11 -------------------------------------------------------------------

icd <- icd_raw


# encoding issue for certain strings
icd$Title <- iconv(icd$Title, from = "latin1", to = "UTF-8", sub = "")


# Remove leading dashes 
icd$Title <- gsub("^[^A-Za-z0-9]+", "", icd$Title) 

icd <- icd %>% 
  select(Code, BlockId, Title, ClassKind, ChapterNo) %>%
  rename(icd11 = Code) 


# Extract ICD vectors and split cells with multiple codes
icd11_inclus <- df_inclus_exp$icd10
icd11_inclus <- unlist(strsplit(icd11_inclus, split = ";", fixed = T))


# Transform to 4 digits icd code + remove dots and white spaces
icd11_inclus <- gsub(".", "", icd11_inclus, fixed = T)
icd11_inclus <- gsub(" ", "", icd11_inclus, fixed = T)
icd11_4_inclus <- str_sub(string = icd11_inclus, 
                          start = 1, end = 4)


# Add column to dataframe
icd11_inclus_df <- data.frame(icd11 = icd11_4_inclus)


# Add title column to dataframe
icd11_inclus_df <- icd11_inclus_df %>% 
  left_join(icd, by = "icd11") %>% 
  select(-BlockId)


# Add chapter titles
icd11_inclus_df <- icd11_inclus_df %>% 
  mutate(chap = ChapterNo) %>% 
  select(-ChapterNo) %>% 
  left_join(icd_chap, by = "chap")


# Counts for codes and chapters 
icd11_inclus_df$icd11 <- as.factor(icd11_inclus_df$icd11)
icd11_inclus_df$chap <- as.factor(icd11_inclus_df$chap)

icd11_inclus_df <- 
  icd11_inclus_df %>% 
  group_by(icd11) %>% 
  mutate(count_code = n()) %>% 
  ungroup() %>% 
  group_by(chap_title) %>% 
  mutate(count_chap = n()) %>% 
  ungroup()
  

# Remove duplicates and order
icd11_inclus_df <- icd11_inclus_df %>% 
  distinct(icd11, Title, count_code, count_chap, ClassKind, chap, chap_title, 
           .keep_all = T) %>% 
  arrange(desc(count_code)) %>% 
  select(icd11, Title, count_code, chap, chap_title, count_chap, ClassKind)


# Split dataframe into icd11 dataframe and chapter icd11 dataframe
icd11_inclus_codes_df <- icd11_inclus_df %>% 
  select(icd11, Title, count_code)


icd11_inclus_chap_df <- icd11_inclus_df %>% 
  select(chap, chap_title, count_chap, ClassKind) %>% 
  distinct(chap, chap_title, count_chap, ClassKind, 
           .keep_all = T) %>% 
  arrange(desc(count_chap))


# Merge df with icd11 df
df_inclus_exp <- df_inclus_exp %>% 
  mutate(icd11 = str_sub(icd10,
                         start = 1, end = 4)) %>% 
  mutate(icd11 = gsub(".", "", icd11, fixed = T)) %>% 
  mutate(icd11 = gsub(" ", "", icd11, fixed = T)) %>% 
  left_join(icd11_inclus_df, by = "icd11")


# Make table with ATC, title and pathology as coded in Redcap
tmp <- df_inclus_exp[, c("record_id", "icd11", "pathology")]

icd11_patho_inclus <- tmp %>% left_join(icd11_inclus_codes_df, by = "icd11")





# ATC ---------------------------------------------------------------------

# Keep columns of interest
atc <- atc_raw[, 1:2] 


# Extract ICD vectors and split cells with multiple codes
atc_inclus <- df_inclus_exp$atc
atc_inclus <- unlist(strsplit(atc_inclus, split = ";", fixed = T))


# Transform to 4 digits icd code + remove dots and white spaces
atc_inclus <- gsub(".", "", atc_inclus, fixed = T)
atc_inclus <- gsub(" ", "", atc_inclus, fixed = T)
atc_4_inclus <- str_sub(string = atc_inclus, 
                        start = 1, end = 3)

# Add column to dataframe
atc_inclus_df <- data.frame(atc_code = atc_4_inclus)


# Add title column to dataframe
atc_inclus_df <- atc_inclus_df %>% 
  left_join(atc, by = "atc_code") 


# Counts for codes and chapters 
atc_inclus_df$atc_code <- as.factor(atc_inclus_df$atc_code)

atc_inclus_df <- 
  atc_inclus_df %>% 
  group_by(atc_code) %>% 
  mutate(count_code_atc = n()) %>% 
  ungroup()


# Remove duplicates and order
atc_inclus_df <- atc_inclus_df %>% 
  distinct(atc_code, atc_name, count_code_atc, 
           .keep_all = T) %>% 
  arrange(desc(count_code_atc)) 


# Merge df with atc df
df_inclus_exp <- df_inclus_exp %>% 
  mutate(atc_code = str_sub(atc,
                            start = 1, end = 3)) %>% 
  mutate(atc_code = gsub(".", "", atc_code, fixed = T)) %>% 
  mutate(atc_code = gsub(" ", "", atc_code, fixed = T)) %>% 
  left_join(atc_inclus_df, by = "atc_code")




# Paper form --------------------------------------------------------------

df_inclus_paper <- df_inclus_paper %>%
  mutate(author_industry = author_industry %>%
           factor() %>%
           fct_recode("Yes" = "1",
                      "No" = "0",
                      "Not reported" = "2") %>%
           fct_relevel("Yes", "No", "Not reported")) %>% 
  mutate(authors_cro = authors_cro %>%
           factor() %>%
           fct_recode("Yes" = "1",
                      "No" = "0",
                      "Not reported" = "2") %>%
           fct_relevel("Yes", "No", "Not reported")) %>% 
  mutate(competing_interest = competing_interest %>%
           factor() %>%
           fct_recode("Yes" = "1",
                      "No" = "0",
                      "Not reported" = "2") %>%
           fct_relevel("Yes", "No", "Not reported"))


df_inclus_paper <- df_inclus_paper %>% 
  mutate(continent_first_author = case_when(
    
    # Northern america
    country_first_author == "canada" | 
      country_first_author == "usa" ~ "Northern America",
    
    # Europe
    country_first_author == "france" |
      country_first_author == "germany" |
      country_first_author == "italy" |
      country_first_author == "russia" |
      country_first_author == "spain" |
      country_first_author == "sweden" |
      country_first_author == "switzerland" |
      country_first_author == "uk" ~ "Europe",
    
    # East Asia
    country_first_author == "china" |
      country_first_author == "hong kong" |
      country_first_author == "japan" |
      country_first_author == "south korea" ~ "Eastern Asia",
    
    
    # central and south america
    country_first_author == "argentina" |
      country_first_author == "brazil" |
      # Oceania
      country_first_author == "australia" |
      # South East Asia
      country_first_author == "indonesia" |
      country_first_author == "thailand" |
      # middle east
      country_first_author == "israel" ~ "Other")) %>% 
  
  mutate(continent_last_author = case_when(
    # Northern america
    country_last_author == "canada" | 
      country_last_author == "usa" ~ "Northern America",
    
    # Europe
    country_last_author == "france" |
      country_last_author == "germany" |
      country_last_author == "italy" |
      country_last_author == "russia" |
      country_last_author == "spain" |
      country_last_author == "sweden" |
      country_last_author == "switzerland" |
      country_last_author == "uk" ~ "Europe",
    
    # East Asia
    country_last_author == "china" |
      country_last_author == "hong kong" |
      country_last_author == "japan" |
      country_last_author == "south korea" ~ "Eastern Asia",
    
    
    # central and south america
    country_last_author == "argentina" |
      country_last_author == "brazil" |
      # Oceania
      country_last_author == "australia" |
      # South East Asia
      country_last_author == "indonesia" |
      country_last_author == "thailand" |
      # middle east
      country_last_author == "israel" ~ "Other"))



df_inclus_paper <- df_inclus_paper %>% 
  mutate(promoter_fact = case_when(
  # Hard coded
    promoter == "alexion astrazeneca rare disease" | 
      promoter == "janssen" |  
      promoter == "bristol myers squibb" |
      promoter == "roche" | 
      promoter == "janssen asia pacific" | 
      promoter == "eli lilly" | 
      promoter == "gsk" | 
      promoter == "hengrui medicine" |  
      promoter == "Tokyo Medical and Dental University" | 
      promoter == "takeda" |  
      promoter == "bayer healthcare sas" | 
      promoter == "amgen" | 
      promoter == "merck & co" | 
      promoter == "pfizer" | 
      promoter == "biomarin pharmaceutical" | 
      promoter == "GSK" ~ "Private (pharmaceutical industry)",
    
    promoter == "Linkoeping University" |
      promoter == "ministry of health of russian federation" |
      promoter == "M.D. Anderson Cancer Center" |
      promoter == "Chinese PLA General Hospital" |
      promoter == "Seoul National University Hospital" |
      promoter == "Bone Marrow Transplantation Center of the First Affiliated Hospital" |
      promoter == "Hadassah Medical Organization" |
      promoter == "Beijing Ditan Hospital" |
      promoter == "Marcelo Iastrebner" |
      promoter == "University of Kansas Medical Center" |
      promoter == "Albert Einstein College of Medicine" |
      promoter == "Qilu Hospital of Shandong University" |
      promoter == "Institute for Advancement of Clinical and Translational Research of Kyoto University"
    ~ "Public",
    
    promoter == "NR" ~ "Not reported"
  ) %>% 
    
    fct_relevel("Private (pharmaceutical industry)",
                "Public",
                "Not reported"))

# Create 5-year categories for barplot bins
df_inclus_paper <- df_inclus_paper %>% 
  mutate(publication_year_20 = 
           cut(publication_year, 
               breaks = seq(1985, 2030, by = 20),
               include.lowest = T, 
               right = FALSE)) 

# Paper inclusion from inception to 2024 --> rename last category
df_inclus_paper$publication_year_20 <- fct_recode(df_inclus_paper$publication_year_20,
                                                  "[1985,2004]" = "[1.98e+03,2e+03)",
                                                  "[2005, 2024]" = "[2e+03,2.02e+03]"
)

df_inclus_paper <- df_inclus_paper %>% 
  mutate(publication_year_15 = ifelse(publication_year >= 2015, 1, 0))


# Agregate "funding" columns into one
df_inclus_paper <- df_inclus_paper %>% 
  mutate(funding =
           case_when(
             funding___1 == 1 & funding___2 == 0 & funding___5 == 0 ~ "Public",
             funding___1 == 0 & funding___2 == 1 ~ "Private (pharmaceutical industry)",
             funding___1 == 1 & funding___2 == 1 & funding___5 == 0 ~ "Public and private (pharmaceutical industry)",
             funding___4 == 1 ~ "Not reported",
             .default = "Other") %>% 
           fct_relevel("Private (pharmaceutical industry)",
                       "Public and private (pharmaceutical industry)",
                       "Public",
                       "Other",
                       "Not reported"))

df_inclus_paper <- df_inclus_paper %>% 
  mutate(funding_grouped = case_when(
    funding == "Private (pharmaceutical industry)" | 
      funding == "Public and private (pharmaceutical industry)" ~ "Pharma involvement",
    funding == "Not reported" ~ "Not reported",
    TRUE ~ "No pharma involvment"
  ) %>% 
    fct_relevel("Pharma involvement", "No pharma involvment", "Not reported"))


# Join df and jcr by issn
df_inclus_paper <- df_inclus_paper %>%
  left_join(jcr_keep2, by = "issn")
# Creates duplicates for some reason --> remove those
df_inclus_paper <- df_inclus_paper[!duplicated(df_inclus_paper),]

# Rename "issn" of jcr to "eISSN" and "ISSN" to "issn"
jcr_keep2 <- 
  jcr_keep2 %>% 
  mutate(eISSN = issn) %>% 
  select(-issn) %>% 
  mutate(issn = ISSN) 


# Some references in df show issn instead of eissn
df_inclus_paper <- df_inclus_paper %>%
  left_join(jcr_keep2, by = "issn") %>%
  mutate(X2023.JIF = coalesce(X2023.JIF.x, X2023.JIF.y)) %>%
  mutate(X2023.JCI = coalesce(X2023.JCI.x, X2023.JCI.y)) %>%
  mutate(ISSN = coalesce(ISSN.x, ISSN.y)) %>%
  select(-X2023.JIF.x, -X2023.JIF.y,
         -X2023.JCI.x, -X2023.JCI.y,
         -ISSN.x, -ISSN.y) 


# Variable for industry link, summary of authors_industry, funding and promoter
df_inclus_paper <- df_inclus_paper %>% 
  mutate(industry_link = case_when(
    # Any clear industry involvement
    funding %in% c("Private (pharmaceutical industry)", "Public and private (pharmaceutical industry)") |
      promoter_fact == "Private (pharmaceutical industry)" |
      author_industry == "Yes" ~ "Yes",
    
    # All clearly non-industry
    funding %in% c("Public", "Other") &
      promoter_fact == "Public" &
      author_industry == "No" ~ "No",
    
    # All info not reported
    funding == "Not reported" &
      promoter_fact == "Not reported" &
      author_industry == "Not reported" ~ "NR",
    
    # Any other mix (e.g. partly reported, inconclusive)
    TRUE ~ "Inconclusive"
    
  ) %>% 
    fct_relevel("Yes", "No", "Inconclusive", "NR")) 


# Convert columns to factor
fact_paper <- c(
  # "publication_year", 
  "author_industry", "authors_cro", "competing_interest",
  "country_first_author", "country_last_author",
  "promoter_fact", "funding", "continent_first_author", "continent_last_author",
  "funding___1", "funding___2", "publication_year_20",
  "funding___3", "funding___4")
df_inclus_paper[fact_paper] <- lapply(df_inclus_paper[fact_paper],
                                      function(x) as.factor(x))
rm(fact_paper)



# Experimental arm form ---------------------------------------------------

df_inclus_exp$platform_bin <- 1 - df_inclus_exp$platform___4



df_inclus_exp$pathology <- str_to_lower(df_inclus_exp$pathology)

# # Creation of the pathology.csv file
# # Create a vector containing only the text
# bs <- df_inclus_exp %>% separate_rows(pathology, sep = ";")
# 
# text <- bs$pathology ; rm(bs)
# 
# 
# # Export to csv to recode
# write.csv(data.frame(pathology = text),
#           file = "000data/pathology.csv",
#           row.names = F)

# Remove separated lines (same study but more than one patho) because same patho category
tmp <- pathology_recoded$pathology_recoded[-c(4, 21, 28:30)]
df_inclus_exp$pathology_recoded <- tmp ; rm(tmp)

df_inclus_exp <- df_inclus_exp %>% 
  mutate(pathology_recoded_fact = case_when(
    pathology_recoded == "Hematology" ~ "Hematology",
    pathology_recoded == "Pneumology" ~ "Pneumology",
    pathology_recoded == "COVID19" ~ "COVID19",
    pathology_recoded == "Gastroenterology" ~ "Gastroenterology",
    .default = "Other"
  ))
# change levels order
df_inclus_exp$pathology_recoded_fact <- factor(df_inclus_exp$pathology_recoded_fact,
                                               levels = c("Hematology",
                                                          "Pneumology",
                                                          "COVID19",
                                                          "Gastroenterology",
                                                          "Other"))

# Remove separated lines (same study but more than one patho) because same patho category
tmp <- pathology_recoded$pathology_onco_bin[-c(4, 21, 28:30)]
df_inclus_exp$pathology_onco_bin <- tmp ; rm(tmp)
df_inclus_exp <- df_inclus_exp %>% 
  mutate(pathology_onco_bin = fct_recode(as.factor(df_inclus_exp$pathology_onco_bin),
                                         "Oncology" = "1",
                                         "Other" = "0"))
# change levels order
df_inclus_exp$pathology_onco_bin <- factor(df_inclus_exp$pathology_onco_bin,
                                           levels = c("Oncology",
                                                      "Other"))

df_inclus_exp <- df_inclus_exp %>% 
  mutate(pathology_onco_fact = case_when(
    pathology_onco_bin == "Oncology" & pathology_recoded == "Hematology" ~ "Haemato Oncology",
    pathology_onco_bin == "Oncology" & pathology_recoded != "Hematology" ~ "Solid tumor",
    TRUE ~ "Not oncology"
  ) %>% 
    fct_relevel(c("Solid tumor", "Haemato Oncology", "Not oncology")))


df_inclus_exp <- df_inclus_exp %>%
  mutate(phase = case_when(
    phase___1 == 1 | phase___2 == 1 ~ "Phase I and/or II",
    # phase___3 == 1 ~ "Phase III",
    phase_other %in% c("pilot",
                       "pilot study",
                       "preliminary study") ~ "Pilot study",
    phase___4 == 1 ~ "Not reported",
    TRUE ~ "Other"
  ) %>% 
    fct_relevel(c("Pilot study",
                  "Phase I and/or II",
                  # "Phase III",
                  "Other",
                  "Not reported")))



# Population
df_inclus_exp <- df_inclus_exp %>% 
  mutate(population___ict = ifelse( # infants children teens
    population___1 == 1 | population___2 == 1 | population___3 == 1, 1, 0
  ))

df_inclus_exp$og_study <- 
  fct_recode(as.factor(df_inclus_exp$og_study),
             "No" = "2",
             "Yes" = "1")

df_inclus_exp$og_design <-
  fct_recode(as.factor(df_inclus_exp$og_design),
             "Single-arm" = "1",
             "More than one arm" = "2",
             "Not reported" = "3")


df_inclus_exp <- df_inclus_exp %>% 
  mutate(justif_sat = case_when(
    justif_sat_def___1 == 1 | justif_sat_def___2 == 1 ~ "Rare and/or severe disease",
    justif_sat_def___3 == 1 ~ "Pediatric population",
    justif_sat_def___4 == 1 ~ "Not reported",
    justif_sat_def___5 == 1 ~ "Other"
    
  ))

df_inclus_exp <- df_inclus_exp %>% 
  mutate(og_start = case_when(
    og_fu_start == 2 ~ "Treatment initiation",
    og_fu_start == 3 ~ "Not reported",
    TRUE ~ "Other"
  ) %>% 
    fct_relevel(c("Treatment initiation",
                  "Other",
                  "Not reported")))


# Convert column to numeric and change NR to NA for table display
df_inclus_exp <- df_inclus_exp %>% 
  mutate(og_center_number = as.numeric(ifelse(df_inclus_exp$og_center_number == "NR", 
                                              NA, df_inclus_exp$og_center_number)))

df_inclus_exp$og_monocenter <- ifelse(df_inclus_exp$og_center_number > 1, 0, 1)

df_inclus_exp <- df_inclus_exp %>% 
  mutate(ctrl_number_centers = df_inclus_control$ctrl_number_centers,
    ratio_centers = ctrl_number_centers / og_center_number) %>% 
  select(-ctrl_number_centers)



# Convert columns to factor
fact_exp <- c(
  "og_study", "og_design", "exp_arm_constr", "phase",
  "pathology_onco_bin",
  "justif_sat", "og_start")
df_inclus_exp[fact_exp] <- lapply(df_inclus_exp[fact_exp],
                                  function(x) as.factor(x))
rm(fact_exp)




# Control arm form --------------------------------------------------------

# Convert column to numeric and change NR to NA for table display
df_inclus_control <- df_inclus_control %>% 
  mutate(nb_ctrl_comp_ind = as.numeric(ifelse(df_inclus_control$nb_ctrl_comp_ind == "NR", 
                                              NA, df_inclus_control$nb_ctrl_comp_ind)))

df_inclus_control <- df_inclus_control %>% 
  mutate(ctrl_number_centers = as.numeric(ifelse(df_inclus_control$ctrl_number_centers == "NR", 
                                                 NA, df_inclus_control$ctrl_number_centers)))


df_inclus_control$same_center <-
  fct_recode(as.factor(df_inclus_control$same_center),
             "Yes" = "1",
             "No" = "2",
             "Not reported" = "3")


df_inclus_control <- df_inclus_control %>% 
  mutate(ctrl_same_select_crit = case_when(
    ctrl_same_select_crit %in% 1:2 ~ "Yes or Partially",
    ctrl_same_select_crit == 3 ~ "No",
    TRUE ~ "Unclear or not reported"
  ) %>% 
    fct_relevel(c("Yes or Partially", 
                  "No", 
				  "Unclear or not reported")))


df_inclus_control$ctrl_units <-
  fct_recode(as.factor(df_inclus_control$ctrl_units),
             "Patients" = "1",
             "Lines of treatment" = "2")


df_inclus_control$ctrl_fu_start_def <-
  fct_recode(as.factor(df_inclus_control$ctrl_fu_start_def),
             "Yes" = "1",
             "Not reported" = "2",
             "Unclear" = "3")


df_inclus_control <- df_inclus_control %>% 
  mutate(ctrl_exp_fu_start_eq = 
           case_when(
             ctrl_exp_fu_start_eq == 1 ~ "Yes",
             ctrl_exp_fu_start_eq == 0 ~ "No",
             is.na(ctrl_exp_fu_start_eq) ~ "Index date not reported in controls"
           ) %>% 
           fct_relevel("Yes", "No", "Index date not reported in controls"))



df_inclus_control$rwd_descr <- df_inclus_results$rwd_descr %>%
  factor() %>%
  fct_recode("Yes" = "1", 
             "No" = "0") %>%
  fct_relevel("Yes", "No")


## condition: "if described, several ttt included in control arm"
df_inclus_results <- df_inclus_results %>%
  mutate(rwd_strategies = case_when(
    rwd_strategies == "1" ~ "Yes",
    rwd_strategies == "2" ~ "No",
    TRUE ~ "Strategy not described"
  ) %>%
    factor() %>%
    fct_relevel("Yes", "No", "Strategy not described"))

df_inclus_control$rwd_strategies <- df_inclus_results$rwd_strategies


# Delete variables in results df to avoid duplicates when merging 
df_inclus_results <- df_inclus_results %>% 
  select(-rwd_descr, -rwd_strategies)


df_inclus_control <- df_inclus_control %>% 
  mutate(ctrl_source = case_when(
    ctrl_source == 1 ~ "Unique",
    ctrl_source == 2 ~ "Multiple",
    ctrl_source == 3 ~ "Not reported"
  ))


df_inclus_control <- df_inclus_control %>% 
  mutate(qualif_ctrl___4 = case_when(
    qualif_ctrl___3 == 1 | 
      qualif_ctrl___4 == 1 ~ 1,
    TRUE ~ 0)) %>% 
  select(-qualif_ctrl___3)


df_inclus_control <- df_inclus_control %>% 
  mutate(justif_rwd_def = case_when(
    justif_rwd_def == 1 ~ "No relevant comparator",
    justif_rwd_def == 2 ~ "Different standards of care",
    justif_rwd_def == 3 ~ "Not reported",
    justif_rwd_def == 4 ~ "Other",
    justif_rwd_def == 5 ~ "Not reported",
    justif_rwd_def == 6 ~ "Comparison to SOC/RW treatments",
    justif_rwd_def == 7 ~ "Not reported"
  ) %>% 
    fct_relevel("Comparison to SOC/RW treatments",
                "Different standards of care",
                "No relevant comparator",
                "Other",
                "Not reported"
    ))


df_inclus_control <- df_inclus_control %>% 
  mutate(rwd_source = case_when(
    ctrl_type_source___1 == 0 & ctrl_type_source___2 == 0 & (ctrl_type_source___3 == 1 | 
                                                               ctrl_type_source___4 == 1) & 
      ctrl_type_source___5 == 0 & ctrl_type_source___7 == 0
    ~ "EHR or EMR",
    ctrl_type_source___1 == 0 & ctrl_type_source___2 == 0 & ctrl_type_source___3 == 0 & 
      ctrl_type_source___4 == 0 & ctrl_type_source___5 == 1 & ctrl_type_source___7 == 0
    ~ "Claims only",
    ctrl_type_source___1 == 1 & ctrl_type_source___2 == 0 & ctrl_type_source___3 == 0 & 
      ctrl_type_source___4 == 0 & ctrl_type_source___5 == 0 & ctrl_type_source___7 == 0
    ~ "Register only",
    ctrl_type_source___1 == 0 & ctrl_type_source___2 == 1 & ctrl_type_source___3 == 0 & 
      ctrl_type_source___4 == 0 & ctrl_type_source___5 == 0 & ctrl_type_source___7 == 0
    ~ "Cohort only",
    ctrl_type_source___6 == 1 ~ "Not reported",
    .default = "Other" # mix de sources
  ))

# Relevel
df_inclus_control$rwd_source <- factor(df_inclus_control$rwd_source,
                                       levels = c("EHR or EMR",
                                                  "Cohort only",
                                                  "Register only",
                                                  "Claims only",
                                                  "Not reported",
                                                  "Other")) # mix de sources

df_inclus_control <- df_inclus_control %>%
  mutate(rwd_source_grouped = case_when(
    ctrl_type_source___6 == 1 ~ "Not reported",
    ctrl_type_source___3 == 1 | ctrl_type_source___4 == 1 | ctrl_type_source___5 == 1 ~ "EHR/EMR or Claims",
    ctrl_type_source___1 == 1 | ctrl_type_source___2 == 1 ~ "Cohort or Register",
    ctrl_type_source___7 == 1 ~ "Other", # original other
    TRUE ~ "Not reported" 
  ) %>% 
    fct_relevel(c("EHR/EMR or Claims", "Cohort or Register", "Other", "Not reported")))


df_inclus_control <- df_inclus_control %>% 
  mutate(ctrl_type_source___1 = fct_recode(as.factor(ctrl_type_source___1),
                                           "No" = "0",
                                           "Yes" = "1")) %>% 
  mutate(ctrl_type_source___2 = fct_recode(as.factor(ctrl_type_source___2),
                                           "No" = "0",
                                           "Yes" = "1")) %>% 
  mutate(ctrl_type_source___3 = fct_recode(as.factor(ctrl_type_source___3),
                                           "No" = "0",
                                           "Yes" = "1")) %>% 
  mutate(ctrl_type_source___4 = fct_recode(as.factor(ctrl_type_source___4),
                                           "No" = "0",
                                           "Yes" = "1")) %>% 
  mutate(ctrl_type_source___5 = fct_recode(as.factor(ctrl_type_source___5),
                                           "No" = "0",
                                           "Yes" = "1")) %>% 
  mutate(ctrl_type_source___6 = fct_recode(as.factor(ctrl_type_source___6),
                                           "No" = "0",
                                           "Yes" = "1")) %>% 
  mutate(ctrl_type_source___7 = fct_recode(as.factor(ctrl_type_source___7),
                                           "No" = "0",
                                           "Yes" = "1"))


df_inclus_control$decalage <- 
  as.numeric(df_inclus_results$exp_first_enroll) - 
  as.numeric(df_inclus_results$ctrl_last_enroll)

df_inclus_control$diff_start_enroll <- 
  as.numeric(df_inclus_results$ctrl_first_enroll) -
  as.numeric(df_inclus_results$exp_first_enroll)

df_inclus_control$rp_overlap <- case_when(
  df_inclus_control$decalage < 0 ~ "Yes",
  df_inclus_control$decalage >= 0 | is.na(df_inclus_control$decalage) ~ "No or NR"
)

df_inclus_control$rp_select_crit <- df_inclus_control$ctrl_same_select_crit


df_inclus_control$rp_t0_eq <- case_when(
  df_inclus_control$ctrl_exp_fu_start_eq == "Yes" ~ "Yes",
  df_inclus_control$ctrl_exp_fu_start_eq %in% c("No", 
                                                "Index date not reported in controls") ~ "No or NR",
)


# Convert columns to factor
fact_control <- c(
  "same_center", "ctrl_same_select_crit", "ctrl_units", "ctrl_fu_start_def",
  "ctrl_exp_fu_start_eq", "rwd_descr", "rwd_strategies", "ctrl_source",
  # "qualif", 
  "rwd_source",
  "ctrl_type_source___1", "ctrl_type_source___2",
  "ctrl_type_source___3", "ctrl_type_source___4", "ctrl_type_source___5",
  "ctrl_type_source___6", "ctrl_type_source___7",
  "justif_rwd_def", "rp_overlap", "rp_select_crit", "rp_t0_eq"
)

df_inclus_control[fact_control] <- lapply(df_inclus_control[fact_control],
                                          function(x) as.factor(x))
rm(fact_control)





# Stats and results forms -------------------------------------------------


# We added balance and overlap related variables for studies with naive
# comparison design --> enter info a posteriori
recordid_naive <- df_inclus_stat$record_id[df_inclus_stat$planned_adj == 0]
df_inclus_stat$balance_plan_meth___6[df_inclus_stat$record_id %in% recordid_naive] <- 1
df_inclus_stat$overlap_plan_meth___3[df_inclus_stat$record_id %in% recordid_naive] <- 1
df_inclus_results$balance_check[df_inclus_stat$record_id %in% recordid_naive] <- 3
df_inclus_results$overlap_check[df_inclus_stat$record_id %in% recordid_naive] <- 3
df_inclus_results$balance_meth_where___3[df_inclus_stat$record_id %in% recordid_naive] <- 1


df_inclus_stat <- df_inclus_stat %>% 
  mutate(nsn_justif = nsn_justif %>% 
           factor() %>% 
           fct_recode("Power computation" = "1",
                      "Convenience sample" = "2",
                      "Not reported" = "3",
                      "Other" = "4") %>% 
           fct_relevel("Power computation",
                       "Convenience sample",
                       "Other",
                       "Not reported")) 


df_inclus_stat <- df_inclus_stat %>% 
  mutate(estimand = case_when(
    estimand == 1 ~ "ATE",
    estimand == 2 ~ "ATT",
    estimand == 3 ~ "ATC",
    estimand == 4 ~ "Not reported",
    estimand == 5 ~ "Other"
  ))


df_inclus_stat$rp_estimand <- ifelse(df_inclus_stat$estimand == "Not reported", "No", "Yes")


df_inclus_stat <- df_inclus_stat %>% 
  mutate(estimand_method = case_when(
    adj_method == 5 | weight_type_other == "entropy balancing" ~ "Unclear", # Unclear in the article
    adj_method == 1 | weight_type == 2 ~ "ATT",
    weight_type == 1 ~ "ATE",
    weight_type == 3 ~ "ATO",
    TRUE ~ "N/A" # unadjusted
  ) %>% 
    fct_relevel(c("ATE", "ATT", "ATO", "Unclear", "N/A")))


df_inclus_stat <- df_inclus_stat %>% 
  mutate(adj_method3 = case_when(
    adj_method == 1 ~ "Matching",
    weight_type == 2 ~ "IPTW (ATT weights)",
    weight_type == 1 ~ "IPTW (ATE weights)",
    weight_type == 3 ~ "IPTW (ATO weights)",
    weight_type_other == "entropy balancing" ~ "Entropy balancing",
    adj_method == 5 ~ "Outcome regression",
    TRUE ~ "N/A"
  ) %>% 
    fct_relevel(c("Matching",
                  "IPTW (ATT weights)",
                  "IPTW (ATE weights)",
                  "IPTW (ATO weights)",
                  "Entropy balancing",
                  "Outcome regression",
                  "N/A"
    )))


df_inclus_stat <- df_inclus_stat %>%
# Hard coded
  mutate(prim_end_categ = case_when(
    def_prim_endpoint == "N/A" | def_prim_endpoint == "NA"  ~ "N/A", # those not defining a primary endpoint
    
    # safety
    def_prim_endpoint == "safety" |
      def_prim_endpoint == "incidence of hypersensitivity reactions" |
      def_prim_endpoint == "cumulative number of adverse events" ~ "Safety",
    
    # # response rate
    # def_prim_endpoint == "overall response rate" |
    #   def_prim_endpoint == "confirmed overall response rate" ~ "Response",
    
    # tte
    def_prim_endpoint == "time to first relapse" |
      def_prim_endpoint == "6 months progression free survival" |
      def_prim_endpoint == "overall survival" |
      def_prim_endpoint == "cumulative incidence of aml and overall survival" |
      def_prim_endpoint == "time to discharge" |
      def_prim_endpoint == "overall survival up to day 28" |
      def_prim_endpoint == "progression free survival" |
      def_prim_endpoint == "incidence of delayed bleeding" |
      def_prim_endpoint == "time to medical treatment failure" |
      def_prim_endpoint == "incidence of acute rejection" ~ "Time-to-event",
    
    # biological/physiological
    def_prim_endpoint == "median viral clearance time" |
      def_prim_endpoint == "dose of vasopressor required at several time points" |
      def_prim_endpoint == "change from baseline in hba1c" |
      def_prim_endpoint == "mean eGFR" |
      def_prim_endpoint == "change from baseline to end of 2d week in easi score" |
      def_prim_endpoint == "difference in number of cd34 cells  on days 4 and 5 post mobilization" |
      def_prim_endpoint == "change of pao2 fio2 at days 1 and 7" ~ "Biological or physiological",
    
    # other
    .default = "Other"
  ) %>% 
    fct_relevel("Time-to-event",
                # "Response",
                "Biological or physiological",
                "Safety",
                "Other",
                "N/A"))

df_inclus_stat$rp_outcome <- ifelse(df_inclus_stat$prim_end_categ == "N/A", "No", "Yes")


# Variable n post adjustment whatever the adjustment method (or absence of)
# employed
df_inclus_results <- df_inclus_results %>% 
  mutate(n_post_exp = 
           rowSums(df_inclus_results[, c("n_naive_exp", "n_match_exp", "n_weight_exp")], na.rm = T))  %>% 
  mutate(n_post_ctrl = 
           rowSums(df_inclus_results[, c("n_naive_ctrl", "n_match_ctrl", "n_weight_ctrl")], na.rm = T))

df_inclus_results$n_post_exp <- ifelse(df_inclus_results$n_post_exp == 0,
                                       NA, df_inclus_results$n_post_exp) 
df_inclus_results$n_post_ctrl <- ifelse(df_inclus_results$n_post_ctrl == 0,
                                        NA, df_inclus_results$n_post_ctrl)


df_inclus_results <- df_inclus_results %>% 
  mutate(ratio_n_exp = n_post_exp/n_included_exp,
         ratio_n_ctrl = n_post_ctrl/n_included_control)


df_inclus_results <- df_inclus_results %>% 
  mutate(ratio_n_included = n_included_control/n_included_exp,
         ratio_n_analyzed = n_post_ctrl/n_post_exp)



df_inclus_stat <- df_inclus_stat %>%
  mutate(
    type_adj_var = case_when(
      type_adj_vars___3 == 1 | planned_adj == "0" ~ "No",         
      planned_adj == "1" & type_adj_vars___3 == 0 ~ "Yes",
      TRUE ~ NA_character_
    ),
    justif_adj_vars = case_when(
      justif_adj_vars___3 == 1 | planned_adj == "0" ~ "No",
      planned_adj == "1" & justif_adj_vars___3 == 0 ~ "Yes",
      TRUE ~ NA_character_
    ),
    # Then convert to ordered factor if needed
    type_adj_var = factor(type_adj_var, levels = c("Yes", "No")),
    justif_adj_vars = factor(justif_adj_vars, levels = c("Yes", "No"))
  )



df_inclus_stat$multiple_endpoints <- ifelse(df_inclus_stat$endpoint_type == 2, "Yes", "No")


df_inclus_stat <- df_inclus_stat %>% 
  mutate(type_adj_vars___1 = fct_recode(as.factor(type_adj_vars___1),
                                        "No" = "0",
                                        "Yes" = "1")) %>% 
  mutate(type_adj_vars___3 = fct_recode(as.factor(type_adj_vars___3),
                                        "No" = "0",
                                        "Yes" = "1")) %>% 
  mutate(type_adj_vars___4 = fct_recode(as.factor(type_adj_vars___4), 
                                        "No" = "0",
                                        "Yes" = "1"))
# == "related to TTT assignment" == "instrumental variable


# Because of the Redcap condition on planned_adj, all naives have "No" for
# type_adj_var == "NR" --> recode
df_inclus_stat$type_adj_vars___3[df_inclus_stat$planned_adj == 0] <- "Yes"


df_inclus_stat <- df_inclus_stat %>% 
  mutate(justif_adj_vars___1 = fct_recode(as.factor(justif_adj_vars___1),
                                          "No" = "0",
                                          "Yes" = "1")) %>% 
  mutate(justif_adj_vars___2 = fct_recode(as.factor(justif_adj_vars___2),
                                          "No" = "0",
                                          "Yes" = "1")) %>% 
  mutate(justif_adj_vars___3 = fct_recode(as.factor(justif_adj_vars___3),
                                          "No" = "0",
                                          "Yes" = "1")) %>% 
  mutate(justif_adj_vars___4 = fct_recode(as.factor(justif_adj_vars___4),
                                          "No" = "0",
                                          "Yes" = "1"))
# 1 other == forward selection for regression model


# Because of the Redcap condition on planned_adj, all naives have "No" for
# type_adj_var == "NR" --> recode
df_inclus_stat$justif_adj_vars___3[df_inclus_stat$planned_adj == 0] <- "Yes"


# Create a single variable for baseline group comparison methods
tmp <- df_inclus_results[, c("baseline_comp_meth___1", "baseline_comp_meth___2",
                             "baseline_comp_meth___3",
                             "baseline_comp_meth___5", "baseline_comp_meth___6")] # no "other"

method_vars <- c("baseline_comp_meth___1", "baseline_comp_meth___2",
                 "baseline_comp_meth___5", "baseline_comp_meth___6")
method_labels <- c("Table", "SMD", "p-value", "No comparison")

df_inclus_results$baseline_comp_meth <- get_collapse_dummies(tmp, 
                                                             method_vars, 
                                                             method_labels)
rm(method_vars, method_labels)


df_inclus_results <- df_inclus_results %>% 
  mutate(baseline_comp_meth_fact = case_when(
    baseline_comp_meth == "Table + SMD" | 
      baseline_comp_meth == "Table + SMD + p-value" ~ "Table + SMD (+/_ p-value)",
    baseline_comp_meth == "Table + p-value" ~ "Table + p-value",
    TRUE ~ "Other" 
  ) %>% 
    fct_relevel(c("Table + SMD (+/_ p-value)",
                  "Table + p-value",
                  "Other")))



df_inclus_stat <- df_inclus_stat %>% 
  mutate(baseline_comp_meth___1 = fct_recode(as.factor(df_inclus_results$baseline_comp_meth___1),
                                             "No" = "0",
                                             "Yes" = "1")) %>% 
  mutate(baseline_comp_meth___2 = fct_recode(as.factor(df_inclus_results$baseline_comp_meth___2),
                                             "No" = "0",
                                             "Yes" = "1")) %>% 
  mutate(baseline_comp_meth___5 = fct_recode(as.factor(df_inclus_results$baseline_comp_meth___5),
                                             "No" = "0",
                                             "Yes" = "1")) %>% 
  mutate(baseline_comp_meth___6 = fct_recode(as.factor(df_inclus_results$baseline_comp_meth___6),
                                             "No" = "0",
                                             "Yes" = "1"))

# Remove cols from results df to avoid merging conflicts
df_inclus_results <- df_inclus_results %>% 
  select(-c(grep("baseline_comp_meth___", colnames(df_inclus_results))))


df_inclus_stat <- df_inclus_stat %>% 
  mutate(balance_plan = case_when(
    balance_plan_meth___1 == 1 | balance_plan_meth___2 == 1 |
      balance_plan_meth___3 == 1 | balance_plan_meth___4 == 1 |
      balance_plan_meth___5 == 1 | balance_plan_meth___7 == 1 ~ "Yes",
    balance_plan_meth___6 == 1 ~ "Not reported"
  ) %>% 
    fct_relevel("Yes", "Not reported")) %>% 
  mutate(overlap_plan = case_when(
    overlap_plan_meth___2 == 1 | overlap_plan_meth___4 == 1 ~ "Yes",
    overlap_plan_meth___3 == 1 ~ "Not reported"
  ) %>% 
    fct_relevel("Yes", "Not reported"))


df_inclus_stat <- df_inclus_stat %>% 
  mutate(balance_check = fct_recode(as.factor(df_inclus_results$balance_check),
                                    "Yes" = "1",
                                    "No" = "2",
                                    "Not reported" = "3")) %>% 
  mutate(overlap_check = fct_recode(as.factor(df_inclus_results$overlap_check),
                                    "Yes" = "1",
                                    "No" = "2",
                                    "Not reported" = "3"))

# Remove cols from results df to avoid merging conflicts
df_inclus_results <- df_inclus_results %>% 
  select(-c(balance_check, overlap_check))


df_inclus_stat <- df_inclus_stat %>% 
  mutate(endpoint_type = fct_recode(as.factor(df_inclus_stat$endpoint_type),
                                    "Unique" = "1",
                                    "Multiple" = "2",
                                    "Not reported" = "3")) %>% 
  mutate(multiplicity = fct_recode(as.factor(df_inclus_stat$multiplicity),
                                   "Yes" = "1",
                                   "No" = "0") %>% 
           fct_relevel("Yes", "No"))


df_inclus_stat <- df_inclus_stat %>% 
  mutate(adj_method_all = case_when(
    planned_adj == 0 ~ "Naive analysis",
    adj_method == 1 ~ "Matching",
    adj_method %in% c(2,7) ~ "Weighting",
    adj_method == 3 ~ "MAIC",
    adj_method == 4 ~ "STC",
    adj_method == 5 ~ "Regression",
    adj_method == 6 ~ "Not reported"
    # ,    adj_method == 7 ~ "Other" # == entropy balancing
  ) %>% 
    fct_relevel(c("Matching",
                  "Weighting",
                  "Regression",
                  "Naive analysis")))


df_inclus_stat <- df_inclus_stat %>% 
  mutate(adj_method_bin = case_when(
    adj_method_all == "Matching" | adj_method_all == "Weighting" |
      adj_method_all == "MAIC" | adj_method_all == "STC" |
      adj_method_other == "entropy balancing" ~ "Balancing-based methods",
    adj_method_all == "Naive analysis" | 
      adj_method_all == "Regression" ~ "Non-balancing methods"
  ))



df_inclus_stat <- df_inclus_stat %>% 
  mutate(sens_analysis_bin = case_when(
    sens_analysis_type___1 == 1 |
      sens_analysis_type___2 == 1 | 
      sens_analysis_type___3 == 1 |
      sens_analysis_type___4 == 1 |
      sens_analysis_type___5 == 1 |
      sens_analysis_type___7 == 1 ~ "Yes",
    sens_analysis_type___6 == 1 ~ "No"
  ) %>% 
    fct_relevel("Yes", "No")) 


df_inclus_stat <- df_inclus_stat %>% 
  mutate(
    sens_an_adj_model = ifelse(sens_analysis_type___1 == 1 |
                                 sens_analysis_type___2 == 1 |
                                 sens_analysis_type___3 == 1,
                               1, 0),
    sens_an_analyis_model = sens_analysis_type___4,
    
    sens_an_estimand = sens_analysis_type___5,
    
    sens_an_pop = ifelse(sens_analysis_type___8 == 1 |
                           sens_analysis_type___9 == 1,
                         1, 0),
    
    sens_an_index_date = sens_analysis_type___10,
    
    sens_an_no = sens_analysis_type___6,
    
    sens_an_other = sens_analysis_type___7
  )


df_inclus_stat <- df_inclus_stat %>% 
  mutate(sens_analysis_detail = case_when(
    sens_analysis_type___1 == 1 ~ "Other adjustment method",
    sens_analysis_type___2 == 1 | 
      sens_analysis_type___3 == 1 ~ "Adjustment variable added/removed",
    sens_analysis_type___4 == 1 ~ "Other analysis model",
    sens_analysis_type___5 == 1 ~ "Other estimand",
    sens_analysis_type___6 == 1 ~ "Not reported",
    sens_analysis_type___7 == 1 ~ "Other",
    sens_analysis_type___8 == 1 ~ "Change in inclusion criteria for control arm",
    sens_analysis_type___9 == 1 ~ "Subgroup analysis",
    sens_analysis_type___10 == 1 ~ "Change in index date"
  ))


df_inclus_stat$sign_results_eval <- fct_recode(as.factor(df_inclus_results$res_sign_eval),
                                               "Yes" = "1",
                                               "No" = "2",
                                               "Unclear" = "3")

df_inclus_stat$sens_analyses_sign <- fct_recode(as.factor(df_inclus_results$sens_analyses_sign),
                                                "Yes (all endpoints)" = "1",
                                                "Yes (some endpoints)" = "2",
                                                "Yes (all endpoints tested)" = "3",
                                                "No" = "4",
                                                "Not reported" = "5")

# Add new factor level "No SA planned" 
levels(df_inclus_stat$sens_analyses_sign) <- c(levels(df_inclus_stat$sens_analyses_sign), "No sensitivity analysis planned")
df_inclus_stat$sens_analyses_sign[df_inclus_stat$sens_analysis_bin == "Not reported"] <-
  "No sensitivity analysis planned"


df_inclus_results <- df_inclus_results %>% 
  select(-sens_analyses_sign)

df_inclus_stat$res_sign_eval_prim <- df_inclus_results$res_sign_eval_prim
df_inclus_stat$res_sign_eval <- df_inclus_results$res_sign_eval

df_inclus_stat <- df_inclus_stat %>%
  mutate(prim_endp_sign = case_when(
    def_prim_endpoint == "N/A" ~ "No PE defined",
    res_sign_eval_prim == 1 ~ "Positive",
    res_sign_eval_prim == 2 | res_sign_eval == 2 ~ "Negative",
    res_sign_eval == 3 & def_prim_endpoint != "N/A" ~ "Unclear",
    .default = "PB"
  ) %>% 
    fct_relevel("Positive", "Negative", "Unclear", "No PE defined"))


fact_stat <- c("prim_end_categ", "type_adj_vars___1", "type_adj_vars___3", 
               "justif_adj_vars___1", "justif_adj_vars___2", 
               "justif_adj_vars___3", "justif_adj_vars___4",
               "baseline_comp_meth___1", "baseline_comp_meth___2",
               "baseline_comp_meth___5", "baseline_comp_meth___6",
               "balance_plan", "overlap_plan", "balance_check",
               "overlap_check", "endpoint_type", "multiplicity", "adj_method_bin",
               "sens_analysis_bin", "estimand", "rp_estimand", "rp_outcome",
               "prim_endp_sign")

df_inclus_stat[fact_stat] <- lapply(df_inclus_stat[fact_stat],
                                    function(x) as.factor(x))
rm(fact_stat)



# Merge all into clean df -------------------------------------------------

df_inclus_all <- reduce(list(df_inclus_paper, df_inclus_exp, 
                             df_inclus_control, df_inclus_stat,
                             df_inclus_results),
                        left_join, by = "record_id")


rm(list = c("df_inclus_control_tmp", "df_inclus_results_tmp",
            "df_inclus_paper", "df_inclus_exp", "tmp",
            "df_inclus_control", "df_inclus_stat", "df_inclus_results",
            "atc_4_inclus", "atc_inclus", "icd11_4_inclus",
            "icd11_inclus", "recordid_naive",
            grep("^id_", ls(), value = T),
            grep("_form$", ls(), value = T)))