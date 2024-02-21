# server shiny ----
server <- function(input, output, session) {

  # cdm snapshot ----
  output$tbl_cdm_snaphot <- renderText(kable(cdm_snapshot) %>%
                                          kable_styling("striped", full_width = F) )
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(cdm_snapshot)
      gtsave(x, file)
    }
  )
  
  # code use ----
  getCodeUse <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    code_use <- code_use %>% 
      select(c("cdm_name", "codelist_name",
               "group_name", 
               "strata_name", "strata_level",
               "standard_concept_name", "standard_concept_name",
               "source_concept_name",  "source_concept_id" ,   "domain_id",
               "variable_name", "estimate")) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate)
    names(code_use)<-stringr::str_replace_all(names(code_use), "_", " ")
    code_use
      
  })
  
  output$dt_code_use  <- DT::renderDataTable({
    table_data <- getCodeUse()
    
    datatable(table_data, 
              filter = "top",
              rownames= FALSE) 
  })

  # cohort_count ----
  get_cohort_count <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    working_cohort_count <- cohort_count  %>% 
      filter(variable_name %in%
               c("number_records", "number_subjects")) %>% 
      filter(additional_level == "overall") %>% 
      filter(cdm_name %in% input$cd_cdm) %>% 
      filter(strata_level %in%  input$cd_cohort) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate_value) %>% 
      select(cdm_name, strata_level,
             number_records,
             number_subjects)  %>% 
      pivot_wider(names_from = cdm_name, 
                  values_from = c(number_records, 
                                  number_subjects),
                  names_glue = "{cdm_name}: {.value}",
                  names_vary = "slowest")
    if(isFALSE(input$cd_cc_records)){
      working_cohort_count<-working_cohort_count %>%
      select(!matches("number_record"))
    }
    
    if(isFALSE(input$cd_cc_subjects)) {
      working_cohort_count<-working_cohort_count %>%
        select(!matches("number_subj"))
    }
    
    working_cohort_count
  })
  
  output$dt_cohort_count  <- DT::renderDataTable({
    table_data <- get_cohort_count()
    
    datatable(table_data, rownames= FALSE) 
  })  
 
  # index_codes ----
  get_index_codes <- reactive({
    
    validate(
      need(input$cd_cdm != "", "Please select a database")
    )
    validate(
      need(input$cd_cohort != "", "Please select a cohort")
    )
    
    index_codes <- index_codes %>% 
    filter(cdm_name %in% input$cd_cdm,
           cohort_name %in%  input$cd_cohort,
           group_name %in%  input$cd_index_group_name,
           strata_name %in%  input$cd_index_strata_name) %>% 
      select(c("cdm_name", "cohort_name" ,
               "group_name", 
               "strata_name", "strata_level",
               "standard_concept_name", "standard_concept_id",
               "source_concept_name",  "source_concept_id" ,   "domain_id",
               "variable_name", "estimate")) %>% 
      pivot_wider(names_from = variable_name, 
                  values_from = estimate)
    
    names(index_codes)<-stringr::str_replace_all(names(index_codes), "_", " ")
    
    index_codes
  })
  
  output$dt_index_codes  <- DT::renderDataTable({
    table_data <- get_index_codes()
    datatable(table_data, rownames= FALSE) 
  })   
  
  
  
  # rp_incidence_estimates ----
  ### get estimates
  getrp_incidence <- reactive({
    rp_incidence %>%
      filter(cdm_name %in% input$rp_incidence_estimates_cdm_name) %>%
      filter(outcome_cohort_name %in% input$rp_incidence_estimates_outcome_cohort_name) %>%
      # filter(denominator_target_cohort_name %in% input$rp_incidence_estimates_denominator_target_cohort_name) %>%
      filter(denominator_age_group %in% input$rp_incidence_estimates_denominator_age_group) %>%
      filter(denominator_sex %in% input$rp_incidence_estimates_denominator_sex) %>%
      filter(denominator_days_prior_observation %in% input$rp_incidence_estimates_denominator_days_prior_observation) %>%
      # filter(denominator_start_date %in% input$rp_incidence_estimates_denominator_start_date) %>%
      # filter(denominator_end_date %in% input$rp_incidence_estimates_denominator_end_date) %>%
      # filter(analysis_outcome_washout %in% input$rp_incidence_estimates_analysis_outcome_washout) %>%
      # filter(analysis_repeated_events %in% input$rp_incidence_estimates_analysis_repeated_events) %>%
      # filter(analysis_complete_database_intervals %in% input$rp_incidence_estimates_analysis_complete_database_intervals) %>%
      # filter(analysis_min_cell_count %in% input$rp_incidence_estimates_analysis_min_cell_count) %>%
      # filter(analysis_interval %in% input$rp_incidence_estimates_analysis_interval) %>%
      filter(as.Date(incidence_start_date) %in% as.Date(input$rp_incidence_estimates_incidence_start_date)) %>%
      mutate(
        person_years = round(suppressWarnings(as.numeric(person_years))),
        person_days = round(suppressWarnings(as.numeric(person_days))),
        n_events = round(suppressWarnings(as.numeric(n_events))),
        incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys)), 2),
        incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower)), 2),
        incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)), 2)
      )
  })
  ### download table
  output$rp_incidence_estimates_download_table <- downloadHandler(
    filename = function() {
      "rp_incidenceTable.csv"
    },
    content = function(file) {
      write_csv(getrp_incidence(), file)
    }
  )
  ### table estimates
  output$rp_incidence_estimates_table <- renderDataTable({
    table <- getrp_incidence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    table <- table %>%
      mutate(incidence_100000_pys = paste0(
        incidence_100000_pys, " (", incidence_100000_pys_95CI_lower, " to ",
        incidence_100000_pys_95CI_upper, " )"
      )) %>%
      select(cdm_name, outcome_cohort_name, denominator_target_cohort_name, denominator_age_group, denominator_sex, 
             denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, 
             analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, 
             incidence_start_date, n_events, n_persons, person_years, incidence_100000_pys)
    datatable(
      table,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  ### make plot
  plot_rp_incidence <- reactive({
    table <- getrp_incidence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))
    plotIncidence(
      table,
      x = input$rp_incidence_estimates_plot_x,
      ylim = c(0, NA),
      facet = input$rp_incidence_estimates_plot_facet,
      colour = input$rp_incidence_estimates_plot_colour,
      colour_name = paste0(input$rp_incidence_estimates_plot_colour, collapse = "; "),
      ribbon = FALSE
    )
  })
  ### download plot
  output$rp_incidence_estimates_download_plot <- downloadHandler(
    filename = function() {
      "rp_incidencePlot.png"
    },
    content = function(file) {
      ggsave(
        file,
        plot_rp_incidence(),
        width = as.numeric(input$rp_incidence_estimates_download_width),
        height = as.numeric(input$rp_incidence_estimates_download_height),
        dpi = as.numeric(input$rp_incidence_estimates_download_dpi),
        units = "cm"
      )
    }
  )
  ### plot
  output$rp_incidence_estimates_plot <- renderPlotly({
    plot_rp_incidence()
    
    table <- getrp_incidence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))
    plotIncidence(
      table,
      x = input$rp_incidence_estimates_plot_x,
      ylim = c(0, NA),
      facet = input$rp_incidence_estimates_plot_facet,
      colour = input$rp_incidence_estimates_plot_colour,
      colour_name = paste0(input$rp_incidence_estimates_plot_colour, collapse = "; "),
      ribbon = FALSE
    )
    
    
  })
  
  
  # rp_prevalence_estimates ----
  ### get estimates
  getrp_prevalence <- reactive({
    rp_prevalence %>%
      filter(cdm_name %in% input$rp_prevalence_estimates_cdm_name) %>%
      filter(outcome_cohort_name %in% input$rp_prevalence_estimates_outcome_cohort_name) %>%
      # filter(denominator_target_cohort_name %in% input$rp_prevalence_estimates_denominator_target_cohort_name) %>%
      filter(denominator_age_group %in% input$rp_prevalence_estimates_denominator_age_group) %>%
      filter(denominator_sex %in% input$rp_prevalence_estimates_denominator_sex) %>%
      filter(denominator_days_prior_observation %in% input$rp_prevalence_estimates_denominator_days_prior_observation) %>%
      # filter(denominator_start_date %in% input$rp_prevalence_estimates_denominator_start_date) %>%
      # filter(denominator_end_date %in% input$rp_prevalence_estimates_denominator_end_date) %>%
      # filter(analysis_type %in% input$rp_prevalence_estimates_analysis_type) %>%
      # filter(analysis_outcome_lookback_days %in% input$rp_prevalence_estimates_analysis_outcome_lookback_days) %>%
      # filter(analysis_time_point %in% input$rp_prevalence_estimates_analysis_time_point) %>%
      # filter(analysis_complete_database_intervals %in% input$rp_prevalence_estimates_analysis_complete_database_intervals) %>%
      # filter(analysis_full_contribution %in% input$rp_prevalence_estimates_analysis_full_contribution) %>%
      # filter(analysis_min_cell_count %in% input$rp_prevalence_estimates_analysis_min_cell_count) %>%
      # filter(analysis_interval %in% input$rp_prevalence_estimates_analysis_interval) %>%
      # filter(prevalence_start_date %in% input$rp_prevalence_estimates_prevalence_start_date) %>%
      mutate(
        n_cases = round(suppressWarnings(as.numeric(n_cases))),
        n_population = round(suppressWarnings(as.numeric(n_population))),
        prevalence = round(suppressWarnings(as.numeric(prevalence)), 4),
        prevalence_95CI_lower = round(suppressWarnings(as.numeric(prevalence_95CI_lower)), 4),
        prevalence_95CI_upper = round(suppressWarnings(as.numeric(prevalence_95CI_upper)), 4)
      )
  })
  ### download table
  output$rp_prevalence_estimates_download_table <- downloadHandler(
    filename = function() {
      "rp_prevalenceTable.csv"
    },
    content = function(file) {
      write_csv(getrp_prevalence(), file)
    }
  )
  ### table estimates
  output$rp_prevalence_estimates_table <- renderDataTable({
    table <- getrp_prevalence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    table <- table %>%
      mutate(`prevalence (%)` = paste0(
        100 * prevalence, " (", 100 * prevalence_95CI_lower, " to ",
        100 * prevalence_95CI_upper, " )"
      )) %>%
      select(cdm_name, outcome_cohort_name, 
             denominator_target_cohort_name, 
             denominator_age_group, denominator_sex, 
             denominator_days_prior_observation, 
             denominator_start_date, denominator_end_date, 
             analysis_type, 
             analysis_time_point, 
             analysis_complete_database_intervals, 
             analysis_full_contribution, 
             analysis_min_cell_count, analysis_interval, 
             prevalence_start_date, n_cases, n_population,
             "prevalence (%)")
    datatable(
      table,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(scrollX = TRUE, scrollCollapse = TRUE)
    )
  })
  ### make plot
  plotrp_prevalence <- reactive({
    table <- getrp_prevalence()
    validate(need(nrow(table) > 0, "No results for selected inputs"))
    class(table) <- c("PrevalenceResult", "IncidencePrevalenceResult", class(table))
    plotPrevalence(
      table,
      x = input$rp_prevalence_estimates_plot_x,
      ylim = c(0, NA),
      facet = input$rp_prevalence_estimates_plot_facet,
      colour = input$rp_prevalence_estimates_plot_colour,
      colour_name = paste0(input$rp_prevalence_estimates_plot_colour, collapse = "; "),
      ribbon = FALSE
    )
  })
  ### download plot
  output$rp_prevalence_estimates_download_plot <- downloadHandler(
    filename = function() {
      "rp_prevalencePlot.png"
    },
    content = function(file) {
      ggsave(
        file,
        plotrp_prevalence(),
        width = as.numeric(input$rp_prevalence_estimates_download_width),
        height = as.numeric(input$rp_prevalence_estimates_download_height),
        dpi = as.numeric(input$rp_prevalence_estimates_download_dpi),
        units = "cm"
      )
    }
  )
  ### plot 
  output$rp_prevalence_estimates_plot <- renderPlotly({
    plotrp_prevalence()
  })
  # rp_patient_characteristics ----
  get_rp_patient_characteristics <- reactive({
    
    validate(
      need(input$rp_chars_cdm != "", "Please select a database")
    )
    validate(
      need(input$rp_chars_cohort != "", "Please select a cohort")
    )
    
    rp_patient_characteristics <- rp_patient_characteristics %>% 
      filter(cdm_name %in% input$rp_chars_cdm) %>% 
      filter(group_level %in%  input$rp_chars_cohort)
              
    rp_patient_characteristics
  })
  
  output$gt_rp_patient_characteristics  <- render_gt({
    get_rp_patient_characteristics()|> 
      formatEstimateValue(
        decimals = c(integer = 0, numeric = 2, percentage = 1, proportion = 3),
        decimalMark = ".",
        bigMark = ",")  |>
      formatEstimateName(
      estimateNameFormat = c("N (%)" = "<count> (<percentage>%)",
                             "N" = "<count>",
                             "Mean (SD)" = "<mean> (<sd>)"),
      keepNotFormatted = FALSE) |>
      dplyr::select(-c("result_type", "package_name", "package_version", 
                       "group_name", "additional_name", "additional_level",
                       "estimate_type")) |>
      formatTable(header = c("cdm_name", "group_level"),
                  delim = "\n", 
                  includeHeaderName = FALSE,
                  includeHeaderKey = TRUE) |>
      gtTable(
        delim = "\n",
        style = "default",
        na = "-",
        # title = "My first gt table with visOmopResults!",
        # groupNameCol = "group_level",
        groupNameAsColumn = FALSE,
        # groupOrder = c("cohort1", "cohort2"),
        colsToMergeRows = "all_columns"
      )
    
  })   
  
  
  
  
  
  
  # rp_large_scale_characteristics ----
  get_rp_large_scale_characteristics <- reactive({
    
    validate(
      need(input$rp_chars_cdm != "", "Please select a database")
    )
    validate(
      need(input$rp_chars_cohort != "", "Please select a cohort")
    )
    
    rp_large_scale_characteristics <- rp_large_scale_characteristics %>% 
      filter(cdm_name %in% input$rp_chars_cdm,
             group_level %in%  input$rp_chars_cohort,
             variable_level %in%  input$rp_chars_index_time_window,
             table_name %in%  input$rp_chars_lsc_domain) %>% 
      select(!c("result_type","group_name", "estimate_type",
                "strata_name", "strata_level",
                "type", "analysis")) %>% 
      pivot_wider(names_from = estimate_name, 
                  values_from = estimate_value) %>% 
      rename("concept_id" = "concept",
             "concept_name" = "variable_name",
             "time_window" = "variable_level",
             "domain" = "table_name") %>% 
      relocate("time_window", .after = "domain") %>% 
      mutate(percentage = round(percentage, 2)) %>% 
      mutate(count_percentage = paste0(count, " (", percentage, "%)"))
    names(rp_large_scale_characteristics)<-stringr::str_replace_all(names(rp_large_scale_characteristics), "_", " ")
    
    rp_large_scale_characteristics
  })
  
  output$dt_rp_large_scale_characteristics  <- DT::renderDataTable({
    table_data <- get_rp_large_scale_characteristics()
    datatable(table_data, rownames= FALSE) 
  })   

  
  
  
  # rp_rt_day ------
  get_rp_rt_day <- reactive({
    
    validate(
      need(input$rp_rt_surv_cdm != "", "Please select a database")
    )
    validate(
      need(input$rp_rt_surv_cohort != "", "Please select a cohort")
    )
    
    survial_at_time_point(as.character(input$rp_rt_day_choice)) %>% 
      filter(cdm_name %in% input$rp_rt_surv_cdm) %>% 
      filter(group_level %in% input$rp_rt_surv_cohort) %>% 
      filter(strata_name %in% input$rp_rt_surv_strata_name) %>% 
      filter(strata_level %in% input$rp_rt_surv_strata_level)
    
  })
  
  output$dt_rp_rt_day  <- DT::renderDataTable({
    table_data <- get_rp_rt_day()
    datatable(table_data, rownames= FALSE) 
  })  
  
  

  # survival plot -----
  get_surv_plot_data <- reactive({ })
  
  plot_rp_rt_surv <- renderPlotly({
    CohortSurvival::plotCumulativeIncidence(plot_data)
  })
}

