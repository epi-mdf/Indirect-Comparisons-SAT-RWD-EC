#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 010discrepancies.R
#' Matylde Diouf
#' 08/07/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Cross validation of the databases.
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




# Import sylvie data and dictionnary
sylvie <- read.csv(paste(data.folder, "sylvie_050625.csv", sep = "/"))
sylvie_dict <- read.csv(paste(data.folder, "dict_sylvie_090525.csv", sep = "/"))

# Rename variables
sylvie_dict <- sylvie_dict %>% 
  mutate(ref_variable = Variable...Field.Name) %>% 
  select(Number, ref_variable, Form.Name)


# Retreive included record_id in sylvie
included_titles <- df_inclus$title
sylvie_included_id <- sylvie$record_id[sylvie$title %in% included_titles]
sylvie_inclus <- sylvie %>%
  filter(record_id %in% sylvie_included_id & !is.na(id_complete))


# Data management to re-construct sylvie without the repeated instances
id_paper_min <- 5
id_paper_max <- grep("paper_complete", colnames(sylvie))
id_exp_min <- id_paper_max + 1
id_exp_max <- grep("experimental_arm_complete", colnames(sylvie))
id_ctrl_min <- id_exp_max + 1
id_ctrl_max <- grep("control_arm_complete", colnames(sylvie))
id_stat_min <- id_ctrl_max + 1
id_stat_max <- grep("statistical_methods_indirect_comparison_complete", colnames(sylvie))
id_res_min <- id_stat_max + 1
id_res_max <- grep("results_complete", colnames(sylvie))


# Organize columns by forms to create associated sylvie
id_form <- colnames(sylvie[, c(1, 4)])
paper_form <- colnames(sylvie[, id_paper_min:id_paper_max])
exp_form <- colnames(sylvie[, id_exp_min:id_exp_max])
control_form <- colnames(sylvie[, id_ctrl_min:id_ctrl_max])
stat_form <- colnames(sylvie[id_stat_min:id_stat_max])
results_form <- colnames(sylvie[, id_res_min:id_res_max])



# Create "paper form" sylvie for included instances
sylvie_inclus_paper <- sylvie_inclus %>% 
  select(record_id, all_of(paper_form))
# str(sylvie_inclus_paper)


# Create "exp arm form" sylvie for included instances
sylvie_inclus_exp <- sylvie_inclus %>% 
  select(record_id, all_of(exp_form))
# str(sylvie_inclus_exp)


# Create "control arm form" sylvie for included instances
sylvie_inclus_control <- sylvie %>% 
  filter(record_id %in% sylvie_included_id & redcap_repeat_instrument == "control_arm") %>%
  select(record_id, all_of(control_form))


# Create "stat arm form" sylvie for included instances
sylvie_inclus_stat <- sylvie_inclus %>% 
  select(record_id, all_of(stat_form))
# str(sylvie_inclus_stat)


# Create "results form" sylvie for included instances
sylvie_inclus_results <- sylvie %>% 
  filter(record_id %in% sylvie_included_id & redcap_repeat_instrument == "results") %>%
  select(record_id, all_of(results_form))


sylvie_inclus <- reduce(list(sylvie_inclus_paper, sylvie_inclus_exp, 
                      sylvie_inclus_control, sylvie_inclus_stat,
                      sylvie_inclus_results),
                 left_join, by = "record_id")

# Clean env
rm(list = c(grep("^id_", ls(), value = T),
            grep("_form$", ls(), value = T),
            grep("^sylvie_inclus_", ls(), value = T)))





discrepancies <- get_diff(sylvie_inclus = sylvie_inclus,
                          me = df_pca,
                          n_first = 44,
                          n_last = 55,
                          sylvie_dict = sylvie_dict,
                          df_inclus = df_inclus)
# save the csv in xlsm to keep row colors!!!

