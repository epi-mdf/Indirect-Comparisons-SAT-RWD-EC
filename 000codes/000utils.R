#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 000utils.R
#' Matylde Diouf
#' 26/02/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Functions developed for all the project's codes.
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


round_any <- plyr::round_any

'%!in%' <- function(x,y)!('%in%'(x,y))


get_map_density <- function(df, 
                            variable,
                            col_high = "#FF8C00", 
                            col_low = "lemonchiffon", 
                            col_dot = "red",
                            type) {
  #' Function to produce world map density of author country.
  #' Returns ggplot object with map.
  #' 
  #' df = dataframe with country information
  #' var = variable from df with country information
  #' col_high = color for highest density
  #' col_low = color for lowest density
  #' col_dot = color for density point
  #' type = A or B for figure
  
  
  
  ### Data management ##########################################################
  
  ##############################################################################
  # # Get unique country instances 
  # sort(unique(c(df$country_first_author, df$country_last_author)))
  # /!\ Countries name and associated sov_a3 have to be manually
  # changed each time a new study with an unused country is added.
  ##############################################################################
  
  
  # Get proper column in df
  df$var <- df[, variable]
  
  
  # Standardize country names for merging
  country <- df %>%
    select(var) %>%
    # Recode country names using countries110$sov_a3
    # ==  three-letter ISO 3166-1 alpha-3 code for the sovereign country 
    # that controls a given geographic region
    mutate(var = recode(var,
                        "argentina" = "ARG",
                        "australia" = "AU1",
                        "brazil" = "BRA",
                        "canada" = "CAN",
                        "china" = "CH1",
                        "france" = "FR1",
                        "germany" = "DEU",
                        # lo siento
                        "hong kong" = "CH1",
                        "indonesia" = "IDN",
                        "israel" = "IS1",
                        "italy" = "ITA",
                        "japan" = "JPN",
                        "russia" = "RUS",
                        "south korea" = "KOR",
                        "spain" = "ESP",
                        "sweden" = "SWE",
                        "switzerland" = "CHE",
                        "thailand" = "THA",
                        "uk" = "GB1",
                        "usa" = "US1"))
  
  
  # Get unique country list (remove empty entries)
  highlight_countries <- unique(na.omit(country$var))
  
  
  
  ### Compute country density ##################################################
  
  country_density <- country %>%
    count(var, name = "density")
  
  # Remove potential empty rows
  country_density <- filter(country_density, var != "")
  
  
  
  ### Load & filter world map data #############################################
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  
  # Remove overseas territories
  mainland_map <- world %>%
    filter(admin == sovereignt | admin %in% c("France")) %>%
    filter(!name %in% c("French Guiana", "Réunion", "Guadeloupe", "Martinique", 
                        "Mayotte", "New Caledonia", "French Polynesia",
                        "Saint Pierre and Miquelon", "Wallis and Futuna",        
                        "Saint Martin", "Saint Barthelemy", "French Southern and Antarctic Lands"))
  
  # Merge density data with world map
  map_data <- mainland_map %>%
    left_join(country_density, by = c("sov_a3" = "var"))
  
  
  
  ### Compute correct centroids ################################################
  
  centroids <- map_data %>%
    mutate(centroid = st_centroid(geometry)) %>%
    st_as_sf()
  
  # Manually fix france's centroid
  # Problem with centroid location, moved towards outre mer
  france_centroid <- data.frame(
    sov_a3 = "FR1",
    long = 2.2137,   # Fixed longitude
    lat = 46.2276    # Fixed latitude
  )
  
  # Extract correct centroids for all other selected countries
  highlighted_centroids <- centroids %>%
    filter(sov_a3 %in% highlight_countries) %>%
    mutate(long = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2])
  
  # Replace France centroid manually
  highlighted_centroids <- highlighted_centroids %>%
    mutate(long = ifelse(sov_a3 == "FR1", france_centroid$long, long),
           lat = ifelse(sov_a3 == "FR1", france_centroid$lat, lat))
  
  
  
  ### Plot ######################################################################
  
  gg <- ggplot() +
    # Base map with country density
    geom_sf(data = map_data, aes(fill = density), color = "grey60", size = 0.1) +
    scale_fill_gradient(low = col_low, high = col_high, na.value = "#F0FFFF", name = "Density") +
    
    # Add proportional red dots for selected countries
    geom_point(data = highlighted_centroids, 
               aes(x = long, y = lat, size = density), 
               color = col_dot, alpha = 0.7) +
    
    # Add labels for selected countries
    geom_text(data = highlighted_centroids, 
              aes(x = long, y = lat, label = paste0(iso_a2_eh, "(", density, ")")), 
              color = "black", size = 2.5, fontface = "bold", nudge_y = 0.5) +
    
    # Adjust dot size legend
    scale_size_continuous(name = "Density", range = c(2, 10)) + 
    guides(size = "none") +  # Remove size legend for dots
    
    # Formatting
    theme_minimal() +
    
    
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(t = -5),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(0, -5, 0, -5)  # trim outer whitespace
    ) +
    
    
    labs(title = NULL,
         subtitle = type,
         y = NULL,
         x = NULL)
  
  
  return(gg)
}


get_stacked_barplot <- function(df) {
  #' Function to produce a stacked barplot of study primary
  #' endpoint over time.
  #' Returns ggplot object.
  
  
  # Create dataset for plotting with 2 year-bins variable
  plot_data <- df %>%
    mutate(
      period = cut(publication_year,
                   breaks = seq(1987, 2025, by = 2),
                   right = FALSE)
    ) %>%
    count(period, prim_endp_sign) %>%
    group_by(period) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  # Count the number of studies per 2-year bin periods
  totals <- plot_data %>%
    summarise(total = sum(n), .by = period)
  
  # Plot
  gg <- ggplot(plot_data, aes(period, prop, fill = prim_endp_sign)) +
    geom_col() +
    geom_label(
      data = totals,
      aes(period, 1.03, label = total),
      inherit.aes = FALSE,
      fill = "white",
      color = "black",
      size = 3
    ) +
    scale_fill_manual(
      values = c(
        "Positive" = "#1b9e77",
        "Negative" = "#d95f02",
        "Unclear" = "#7570b3",
        "No PE defined" = "grey"
      )
    ) +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1.08)
    ) +
    labs(
      x = "Publication period",
      y = "Trial outcome according to evaluators",
      fill = "Primary endpoint"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(gg)
}




get_enrollment_period <- function(strata,
                                  nn,
                                  title) {
  
  #' Function to produce Gantt chart of enrollment timeline
  #' in experimental and control arm, stratified on the
  #' adjustment method (binary variable)
  #' + add primary endpoint (positive/negative/unclear/not defined)
  #' strata = character level of adj_method_bin
  #' nn = "A" or "B" for printing
  #' title = obsolete? 
  
  
  # Select needed columns
  tmp <- df_inclus_all %>%
    select(record_id, exp_first_enroll, exp_last_enroll,
           ctrl_first_enroll, ctrl_last_enroll, adj_method_bin,
           prim_endp_sign) %>%
    
    # Remove studies with 0 date defined for both group
    filter(
      adj_method_bin == strata,
      !(is.na(exp_first_enroll) & is.na(exp_last_enroll) &
          is.na(ctrl_first_enroll) & is.na(ctrl_last_enroll))
    )
  
  tmp <- tmp %>%
    mutate(exp_first_enroll = as.integer(exp_first_enroll)) %>%
    mutate(exp_last_enroll = as.integer(exp_last_enroll)) %>%
    mutate(ctrl_first_enroll = as.integer(ctrl_first_enroll)) %>%
    mutate(ctrl_last_enroll = as.integer(ctrl_last_enroll))
  
  
  # Experimental arm data
  exp_df <- tmp %>%
    select(record_id, adj_method_bin, start = exp_first_enroll, end = exp_last_enroll) %>%
    mutate(arm = "exp", fill_category = "exp")
  
  # Control arm data
  ctrl_df <- tmp %>%
    select(record_id, adj_method_bin, start = ctrl_first_enroll, end = ctrl_last_enroll) %>%
    mutate(arm = "ctrl", fill_category = "ctrl")
  
  # Combine both
  tmp_long <- bind_rows(exp_df, ctrl_df) %>%
    # Add 1 year for visibility for studies with year_start = year_end
    mutate(end = ifelse(!is.na(start) & start == end, end + 1, end)) %>%
    # Sort according to record_id
    arrange(record_id)
  
  
  # Order by start date for visibility
  tmp_long <- tmp_long %>%
    group_by(record_id) %>%
    mutate(min_start = if (all(is.na(start))) NA_real_ else min(start, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(record_id = reorder(factor(record_id), min_start))
  
  
  # Separate rows: complete periods vs. start-only or end-only
  periods <- tmp_long %>% filter(!is.na(start) & !is.na(end))
  starts_only <- tmp_long %>% filter(!is.na(start) & is.na(end)) %>% mutate(type = "start")
  ends_only   <- tmp_long %>% filter(is.na(start) & !is.na(end)) %>% mutate(type = "end")
  
  # Force levels to collect the legends
  starts_only$type <- factor(starts_only$type, levels = c("start", "end"))
  ends_only$type   <- factor(ends_only$type, levels = c("start", "end"))
  
  
  
  # Overlapping periods for plotting
  overlaps <- tmp_long %>%
    group_by(record_id) %>%
    summarize(
      overlap_start = max(start),
      overlap_end = min(end)
    ) %>%
    mutate(
      overlap_start = ifelse(overlap_start < overlap_end, overlap_start, NA),
      overlap_end = ifelse(overlap_start < overlap_end, overlap_end, NA),
      fill_category = "overlap"
    )
  
  
  # Create the "primary endpoint significance" points
  outcome_pts <- tmp %>%
    mutate(
      record_id = factor(record_id, levels = levels(tmp_long$record_id)),
      outcome_x = 2025 # Plot all squares on x=2025 for alignment when combining both plots
    ) %>%
    filter(!is.na(prim_endp_sign))
  
  
  # Plot
  gantt <- ggplot(tmp_long, aes(y = factor(record_id),
                                xmin = start,
                                xmax = end,
                                fill = arm)) +
    geom_rect(aes(ymin = as.numeric(factor(record_id)) - 0.4,
                  ymax = as.numeric(factor(record_id)) + 0.4),
              color = "black",
              alpha = 0.6,
              linewidth = 0.5) +
    
    # Points for start-only or end-only dates
    geom_point(data = starts_only,
               aes(x = start, y = record_id, fill = arm, shape = type),
               size = 3,
               alpha = 0.8,
               color = "black",    # border
               stroke = 1, 
               show.legend = c(fill = F)) +  # to avoid arm legend printed twice   
    
    geom_point(data = ends_only,
               aes(x = end, y = record_id, fill = arm, shape = type),
               size = 3,
               alpha = 0.8,
               color = "black",    # border
               stroke = 1, 
               show.legend = c(fill = F)) +  # to avoid arm legend printed twice      
    
    
    # Dashed segments for overlap
    geom_rect_pattern(data = overlaps,
                      aes(xmin = overlap_start, xmax = overlap_end,
                          ymin = as.numeric(factor(record_id)) - 0.4,
                          ymax = as.numeric(factor(record_id)) + 0.4,
                          fill = fill_category),
                      pattern = "stripe",    # Crosshatch pattern
                      pattern_angle = 45,       # Angle of the stripes
                      pattern_density = 0.08,    # Reduced density
                      pattern_spacing = 0.01,    # Increased spacing
                      pattern_fill = "black", 
                      pattern_color = "black", 
                      alpha = 0.3, # Transparency for the overlap region
                      inherit.aes = F) +
    
    # Fix x-axis range so it spans the corpus's publication year span
    scale_x_continuous(limits = c(min(c(df_inclus_all$exp_first_enroll, 
                                        df_inclus_all$ctrl_first_enroll), na.rm = T) - 1,
                                  max(c(df_inclus_all$exp_last_enroll, 
                                        df_inclus_all$ctrl_last_enroll), na.rm = T) + 5)) +
    
    
    
    scale_fill_manual(
      values = c("exp" = "tomato1", "ctrl" = "dodgerblue1", "overlap" = "darkviolet"),
      name = "Arm",
      labels = c("Experimental", "Control", "Overlap"),
      breaks = c("exp", "ctrl", "overlap")
    ) +

    
    # Manual scale for shape (incomplere dates)
    scale_shape_manual(
      values = c("start" = 23, "end" = 21),
      name = "Incomplete enrollment\n period definition",
      labels = c("Start date only", "End date only"),
      drop = F
    ) +
    
    # Plot the primary endpoint points
    geom_point(data = outcome_pts,
               aes(x = outcome_x, y = factor(record_id), color = prim_endp_sign),
               shape = 15,
               size = 3,
               inherit.aes = FALSE) +
    
    scale_color_manual(
      values = c(
        "Positive" = "#1b9e77",
        "Negative" = "#d95f02",
        "Unclear" = "#7570b3",
        "No PE defined" = "grey"
      ),
      name = "Primary endpoint",
      drop = FALSE
    ) +
    
    labs(
      x = "Year",
      y = "Record ID",
      title = NULL,
      subtitle = nn
    ) +
    
    theme_minimal() +
    
    guides(
      fill = guide_legend(
        override.aes = list(
          # Updated pattern properties in the legend to match the plot
          pattern = c("none", "none", "stripe"),
          pattern_density = c(NA, NA, 0.08),
          pattern_spacing = c(NA, NA, 0.02),
          pattern_fill = c(NA, NA, "black"),
          pattern_color = c(NA, NA, "black"),
          fill = c("tomato1", "dodgerblue1", "slateblue3"), # on change la couleur à cause de la transparence
          alpha = 0.8,
          color = c("black", "black", "black"), # black lining for all squares
          shape = NA_real_,  # prevents dot shape from being rendered
          size = NA_real_    # prevents point size
        )
      )
    )
  
  return(gantt)
}



get_collapse_dummies <- function(df, dummy_vars, labels = NULL, none_label = "Not reported") {
  if (is.null(labels)) labels <- dummy_vars  # fallback to names if no labels
  
  # Collapse dummies row-wise into combined string of labels
  combined <- apply(df[, dummy_vars], 1, function(x) {
    if (sum(x, na.rm = TRUE) == 0) {
      return(none_label)
    }
    paste(labels[which(x == 1)], collapse = " + ")
  })
  
  return(factor(combined))
}



get_diff <- function(sylvie_inclus, me, n_first, n_last, 
                     df_inclus, sylvie_dict) {
  
  #' Function to produce a spreadsheet of differences for cross validation 
  #' of data entry.
  
  #' No repeated instances allowed for the dataset.
  #'
  #' sylvie_inclus = sylvie's raw dataset
  #' me = df_DIFF_inclus (raw dataset of included records)
  #' n_first, n_last = indexes for subsetting the datasets doni doni
  #' df_inclus = dataset of included instances to filter the datasets on included titles
  #' sylvie_dict = sylvie's dictionnary because the variables differ a little
  
  
  # Error catch
  if(
    n_first > nrow(sylvie_inclus) | n_last > nrow(sylvie_inclus) |
    n_first > nrow(me) | n_last > nrow(me))
    stop("Indexes 'n_first' or 'n_last' out of range")
  
  if(n_last < n_first) 
    stop("Index 'n_last' smaller than 'n_first'")
  
  if (anyDuplicated(me$title) | anyDuplicated(sylvie_inclus$title)) {
    warning("Duplicated titles detected — join may result in duplication.")
  }
  
  
  # # The record id do not match between the two datasets because of the deleted
  # # instances in redcap --> we have to filter sylvie's rows on paper title
  # # not on record_id
  # included_titles <- df_inclus$title
  # 
  # sylvie_included_id <- sylvie$record_id[sylvie$title %in% included_titles]
  # 
  # sylvie_inclus <- sylvie %>%
  #   filter(record_id %in% sylvie_included_id & !is.na(id_complete))
  # 
  # # # We indeed retrieved the same papers
  # # all.equal(sort(sylvie_inclus$title), sort(df_pca$title))
  
  
  # Rename record_id cols in all datasets
  sylvie_inclus <- sylvie_inclus %>% 
    mutate(record_id_sylvie = record_id)
  
  me <- me %>% 
    mutate(record_id_me = record_id)
  
  
  # Add new record_id cols in each datasets
  me <- me %>%
    left_join(sylvie_inclus %>% select(title, record_id_sylvie), 
              by = "title")
  
  sylvie_inclus <- sylvie_inclus %>% 
    left_join(me %>% select(title, record_id_me), 
              by = "title")
  
  
  
  # Subsets of dataframes for given records, BASED ON SYLVIE'S RECORD IDS
  me_n <- me[n_first:n_last, ]
  sylvie_n <- sylvie_inclus[sylvie_inclus$record_id_sylvie %in% 
                              me_n$record_id_sylvie, ]
  
  
  # Select cols to keep for comparison 
  # a <- sapply(results_form, function(x) paste0('"', x, '"'))
  # cat(a, sep = ", ")
  cols_to_keep_id <- c("record_id_sylvie", "record_id_me")
  
  cols_to_keep_paper <- c("title")
  
  cols_to_keep_exp <- c("og_study", "exp_arm_constr", "exp_arm_constr_other", "exp_arm_select___1", 
                        "exp_arm_select___2", "exp_arm_select___3", "exp_arm_select___4", "exp_arm_select___5", 
                        "exp_arm_select_other", "phase___1", "phase___2", "phase___3", "phase___4", "phase___5", 
                        "phase_other", "pathology", "intervention", "platform___1", "platform___2", "platform___3", "platform___4", "platform___5",
                        "platform_other", "og_center_number", "og_design", "justif_sat_def___1", "justif_sat_def___2", "justif_sat_def___3", 
                        "justif_sat_def___4", "justif_sat_def___5", "justif_sat_def_other", "og_design_ctrl", 
                        "og_design_comp___1", "og_design_comp___2", "og_design_comp___3", "og_design_comp___4", "og_design_comp___5", 
                        "og_design_comp___6", "og_design_comp___7", "og_design_compo_other", "og_fu_start", "og_fu_start_other")
  
  
  
  cols_to_keep_ctrl <- c("nb_ctrl_comp_ind", "nb_rct_cmp_ind", "ctrl_source", "ctrl_type_source___1", 
                         "ctrl_type_source___2", "ctrl_type_source___3", "ctrl_type_source___4", 
                         "ctrl_type_source___5", "ctrl_type_source___6", "ctrl_type_source___7", "ctrl_type_source_other", 
                         "ctrl_number_centers", "same_center", "ctrl_units", "ctrl_same_select_crit", "ctrl_select_crit___1", 
                         "ctrl_select_crit___2", "ctrl_select_crit___3", "ctrl_select_crit___4", "ctrl_select_crit___5", 
                         "ctrl_select_crit___6", "ctrl_select_crit_other", "ctrl_fu_start_def", 
                         "ctrl_exp_fu_start_eq", "qualif_ctrl___1", "qualif_ctrl___2", "qualif_ctrl___3", 
                         "qualif_ctrl___4", "qualif_ctrl_other", 
                         # "justif_rwd_def", 
                         "justif_rwd_def_other", 
                         "ctrl_ttt_selection_descr")
  
  # setdiff(cols_to_keep_ctrl, names(sylvie_inclus[, id_ctrl_min:id_ctrl_max])) # == "justif_rwd_def"
  
  
  cols_to_keep_stat <- c("estimand", "estimand_other", "endpoint_type", "def_prim_endpoint", "endpoint_categ___1", 
                         "endpoint_categ___2", "endpoint_categ___3", "endpoint_categ___4", "nsn_justif", "nsn_justif_other", 
                         "alpha", "power", "nsn_planned_exp", "nsn_planned_ctrl", "planned_adj", "adj_method", 
                         "adj_method_other", "n_adj_vars", "type_adj_vars___1", "type_adj_vars___2", "type_adj_vars___3", 
                         "type_adj_vars___4", "type_adj_vars_other", "justif_adj_vars___1", "justif_adj_vars___2", "justif_adj_vars___3", 
                         "justif_adj_vars___4", "justif_adj_other", "adj_vars_add_justif___1", "adj_vars_add_justif___2", 
                         "adj_vars_add_justif___3", "adj_vars_add_justif_other", "matching_ratio", "matching_method", 
                         "matching_meth_other", "matching_algo___1", "matching_algo___2", "matching_algo___3", "matching_algo___4", 
                         "matching_algo___5", "matching_algo_other", "caliper_length", "match_replacement", "weight_type", 
                         "weight_type_other", "iptw_stab", "balance_plan_meth___1", "balance_plan_meth___2", 
                         "balance_plan_meth___3", "balance_plan_meth___4", "balance_plan_meth___5", 
                         "balance_plan_meth___6", "balance_plan_meth___7", "balance_plan_meth_other", "smd_thresh", 
                         "overlap_plan_meth___1", "overlap_plan_meth___2", "overlap_plan_meth___3", "overlap_plan_meth___4", 
                         "overlap_plan_meth_other", "sens_analysis_type___1", "sens_analysis_type___2", 
                         "sens_analysis_type___3", "sens_analysis_type___4", "sens_analysis_type___5", 
                         "sens_analysis_type___6", "sens_analysis_type___7", "sens_analysis_type_other")
  
  cols_to_keep_res <- c("rwd_descr", "rwd_strategies", "exp_first_enroll", "exp_last_enroll", "ctrl_first_enroll", 
                        "ctrl_last_enroll", "med_fu_global", "med_fu_exp", "med_fu_control", "n_included_exp", 
                        "n_included_control", "baseline_comp_meth___1", "baseline_comp_meth___2", "baseline_comp_meth___3", 
                        "baseline_comp_meth___4", "baseline_comp_meth___5", "baseline_comp_meth___6", "baseline_comp_meth___7", 
                        "baseline_comp_meth_other", "n_naive_exp", "n_naive_ctrl", "res_unadjusted", "n_match_exp", 
                        "n_match_ctrl", "n_weight_exp", "n_weight_ctrl", "number_eff_adj", "adj_var_exclu_causes___1", 
                        "adj_var_exclu_causes___2", "adj_var_exclu_causes___3", "adj_var_exclu_causes___4", "adj_var_exclu_causes___5",
                        "adj_var_exclu_causes___6", "adj_var_exclu_other", "balance_check", "balance_proof", "overlap_check", "overlap_proof",
                        "balance_meth_where___1", "balance_meth_where___2", "balance_meth_where___3", "res_diff_how", "res_diff_how_other", 
                        "res_diff_adjust", "res_sign_eval", "sign_results", "bias_discussion")
  
  cols_to_keep <- c(cols_to_keep_id, cols_to_keep_paper,
                    cols_to_keep_exp, cols_to_keep_ctrl,
                    cols_to_keep_stat, cols_to_keep_res)
  
  
  
  # # Get common columns between the 2 datasets
  # common_cols <- intersect(names(sylvie_n), names(me_n))
  # 
  # cols_to_del <- c(
  #   # "redcap_repeat_instrument", "redcap_repeat_instance",
  #                  "icd10", "atc",
  #                  "filler", grep("complete", colnames(sylvie_n), value = T),
  #                  grep("notes", colnames(sylvie_n), value = T))
  # # cols_to_del <- which(colnames(sylvie_n) %in% cols_to_del)
  # 
  
  
  # Convert to long format
  sylvie_n_long <- sylvie_n %>% 
    # select(all_of(setdiff(common_cols, cols_to_del))) %>% 
    select(all_of(cols_to_keep)) %>% 
    mutate(across(-c(record_id_sylvie, record_id_me, title), as.character)) %>% 
    mutate(across(everything(), str_to_lower)) %>% 
    mutate(across(everything(), ~na_if(., ""))) %>%  
    pivot_longer(-c(record_id_sylvie, record_id_me, title), 
                 names_to = "variable", values_to = "value_sylvie_n")
  
  
  me_n_long <- me_n %>% 
    # select(all_of(setdiff(common_cols, cols_to_del))) %>% 
    select(all_of(cols_to_keep)) %>% 
    mutate(across(-c(record_id_sylvie, record_id_me, title), as.character)) %>% 
    mutate(across(everything(), str_to_lower)) %>% 
    mutate(across(everything(), ~na_if(., ""))) %>%  
    pivot_longer(-c(record_id_sylvie, record_id_me, title), 
                 names_to = "variable", values_to = "value_me_n")
  
  
  
  # Join datasets
  comparison <- full_join(me_n_long, sylvie_n_long, 
                          by = c("record_id_sylvie", "record_id_me", 
                                 "title", "variable"))
  
  
  # Discrepancies dataset
  discrepancies <- comparison %>%
    filter(is.na(value_me_n) != is.na(value_sylvie_n) | value_me_n != value_sylvie_n) %>%
    arrange(as.numeric(record_id_sylvie)) %>% 
    
    
    # !!!!!!!!!!!!!!!!!!!!!
    # filter paper variables
    filter(variable %in% sylvie_dict$ref_variable[sylvie_dict$Form.Name != "paper"])
  
  # Add variable number to refer to codebook
  discrepancies <- discrepancies %>%
    mutate(ref_variable = str_remove(variable, "___\\d+$")) %>% 
    left_join(sylvie_dict, by = "ref_variable") 
  
  discrepancies <- discrepancies %>% 
    mutate(variable_number = Number) %>% 
    select(record_id_sylvie, record_id_me, title, variable_number, 
           variable, value_me_n, value_sylvie_n)
  
  
  
  # Add comment columns and write file
  discrepancies$comment_matylde <- ""
  discrepancies$comment_sylvie <- ""
  write.csv(discrepancies,
            file = paste0("06_discrepancies/discrepancies_", n_first, "_", n_last, ".csv"),
            row.names = F)
  
  return(discrepancies)
}

