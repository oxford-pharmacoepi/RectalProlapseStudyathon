# load packages -----
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(IncidencePrevalence)
library(snakecase)
library(visOmopResults)

# theme -----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#aa42f5", #  "#D8DEE9",
    dark_hover_bg = "#4a067a", #"#81A1C1",
    dark_color ="white"# "#2E3440"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)
# functions ----
nice.num3<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
nice.num1<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

# read results from data folder ----
results<-list.files(here("data"), recursive = TRUE,
                    full.names = TRUE)

# cdm snapshot ------
cdm_snapshot_files<-results[stringr::str_detect(results, ".csv")]
cdm_snapshot_files<-results[stringr::str_detect(results, "cdm_snapshot")]
cdm_snapshot <- list()
for(i in seq_along(cdm_snapshot_files)){
  cdm_snapshot[[i]]<-readr::read_csv(cdm_snapshot_files[[i]], 
                                     show_col_types = FALSE) %>% 
    select("cdm_name", "person_count", "observation_period_count" ,
           "vocabulary_version")
}
cdm_snapshot <- dplyr::bind_rows(cdm_snapshot)
cdm_snapshot <- cdm_snapshot %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version")

# cohort_count -----
cohort_count_files<-results[stringr::str_detect(results, ".csv")]
cohort_count_files<-results[stringr::str_detect(results, "cohort_count")]
cohort_count <- list()
for(i in seq_along(cohort_count_files)){
  cohort_count[[i]]<-readr::read_csv(cohort_count_files[[i]], 
                                 show_col_types = FALSE) 
}
cohort_count <- dplyr::bind_rows(cohort_count)


# index_codes -----
index_codes_files<-results[stringr::str_detect(results, ".csv")]
index_codes_files<-results[stringr::str_detect(results, "index_codes")]
index_codes <- list()
for(i in seq_along(index_codes_files)){
  index_codes[[i]]<-readr::read_csv(index_codes_files[[i]], 
                                    show_col_types = FALSE) 
}
if(length(index_codes_files) >=1){
  index_codes <- dplyr::bind_rows(index_codes) %>% 
    filter(!is.na(estimate)) 
}


# rectal prolapse patient_characteristics -----
rp_patient_characteristics_files<-results[stringr::str_detect(results, ".csv")]
rp_patient_characteristics_files<-results[stringr::str_detect(results, "rectal_prolapse_patient_characteristics")]
rp_patient_characteristics <- list()
for(i in seq_along(rp_patient_characteristics_files)){
  rp_patient_characteristics[[i]]<-readr::read_csv(rp_patient_characteristics_files[[i]], 
                                     show_col_types = FALSE) 
}
rp_patient_characteristics <- dplyr::bind_rows(rp_patient_characteristics)


# rectal prolapse large_scale_characteristics -----
rp_large_scale_characteristics_files<-results[stringr::str_detect(results, ".csv")]
rp_large_scale_characteristics_files<-results[stringr::str_detect(results, "large_scale_characteristics")]
rp_large_scale_characteristics <- list()
for(i in seq_along(rp_large_scale_characteristics_files)){
  rp_large_scale_characteristics[[i]]<-readr::read_csv(rp_large_scale_characteristics_files[[i]], 
                                     show_col_types = FALSE) 
}
rp_large_scale_characteristics <- dplyr::bind_rows(rp_large_scale_characteristics)
rp_large_scale_characteristics <- rp_large_scale_characteristics %>% 
  mutate(variable_level = if_else(variable_level == "-inf to 0", 
                                  "Any time prior to 0", variable_level))
rp_large_scale_characteristics <- visOmopResults::splitAdditional(rp_large_scale_characteristics)


# rp_large_scale_characteristics$variable <- CodelistGenerator:::tidyWords(rp_large_scale_characteristics$variable)


# rectal prolapse incidence -----
rp_incidence_files<-results[stringr::str_detect(results, ".csv")]
rp_incidence_files<-rp_incidence_files[stringr::str_detect(rp_incidence_files, "rectal_prolapse_incidence")]
rp_incidence_files<-rp_incidence_files[stringr::str_detect(rp_incidence_files, "attrition", negate = TRUE)]
rp_incidence <- list()
for(i in seq_along(rp_incidence_files)){
  rp_incidence[[i]]<-readr::read_csv(rp_incidence_files[[i]], 
                                                   show_col_types = FALSE) 
}
rp_incidence <- dplyr::bind_rows(rp_incidence)



# rectal prolapse prevalence -----
rp_prevalence_files<-results[stringr::str_detect(results, ".csv")]
rp_prevalence_files<-rp_prevalence_files[stringr::str_detect(rp_prevalence_files, "rectal_prolapse_prevalence")]
rp_prevalence_files<-rp_prevalence_files[stringr::str_detect(rp_prevalence_files, "attrition", negate = TRUE)]
rp_prevalence <- list()
for(i in seq_along(rp_prevalence_files)){
  rp_prevalence[[i]]<-readr::read_csv(rp_prevalence_files[[i]], 
                                     show_col_types = FALSE) 
}
rp_prevalence <- dplyr::bind_rows(rp_prevalence)



# rectal prolapse to rectopexy complications: survival_estimates ----
rp_rt_survival_estimates_files<-results[stringr::str_detect(results, ".csv")]
rp_rt_survival_estimates_files<-rp_rt_survival_estimates_files[stringr::str_detect(rp_rt_survival_estimates_files, 
                                                                                   "rectal_prolapse_survival_estimates")]
rp_rt_survival_estimates <- list()
for(i in seq_along(rp_rt_survival_estimates_files)){
  rp_rt_survival_estimates[[i]]<-readr::read_csv(rp_rt_survival_estimates_files[[i]], 
                                              show_col_types = FALSE)
  if(nrow(rp_rt_survival_estimates[[i]]) ==0){
    rp_rt_survival_estimates[[i]] <- NULL
  }
}
rp_rt_survival_estimates <- dplyr::bind_rows(rp_rt_survival_estimates)


# rectal prolapse to rectopexy complications: survival_events ----
rp_rt_survival_events_files<-results[stringr::str_detect(results, ".csv")]
rp_rt_survival_events_files<-rp_rt_survival_events_files[stringr::str_detect(rp_rt_survival_events_files, "rectal_prolapse_survival_events")]
rp_rt_survival_events <- list()
for(i in seq_along(rp_rt_survival_events_files)){
  rp_rt_survival_events[[i]]<-readr::read_csv(rp_rt_survival_events_files[[i]], 
                                           show_col_types = FALSE) 
}
rp_rt_survival_events <- dplyr::bind_rows(rp_rt_survival_events)

# rectal prolapse to rectopexy complications: survival_summary ----
rp_rt_survival_summary_files<-results[stringr::str_detect(results, ".csv")]
rp_rt_survival_summary_files<-rp_rt_survival_summary_files[stringr::str_detect(rp_rt_survival_summary_files, 
                                                                               "rectal_prolapse_survival_summary")]
rp_rt_survival_summary <- list()
for(i in seq_along(rp_rt_survival_summary_files)){
  rp_rt_survival_summary[[i]]<-readr::read_csv(rp_rt_survival_summary_files[[i]], 
                                            show_col_types = FALSE) 
}
rp_rt_survival_summary <- dplyr::bind_rows(rp_rt_survival_summary)
# rp_rt_survival_summary <- visOmopResults::splitAdditional(rp_rt_survival_summary)

# combined survival results: 365 days -----
rp_rt_survival_estimates_2 <-  visOmopResults::splitAdditional(rp_rt_survival_estimates) %>% 
  filter(estimate_type == "Cumulative failure probability") %>% 
  pivot_wider(names_from = estimate_name,
              values_from = estimate_value) %>% 
  mutate(cumulative_incidence = paste0(round(estimate,2)*100, "% (",
                                       round(estimate_95CI_lower*100,2), "% to ",
                                      round(estimate_95CI_upper,2), "%)"))


rp_rt_survival_summary_2 <-  visOmopResults::splitAdditional(rp_rt_survival_events) %>% 
  pivot_wider(names_from = estimate_name,
              values_from = estimate_value) %>%
  select(!"estimate_type")

rp_rt_survival_at_time_points <-rp_rt_survival_summary_2 %>% 
  left_join(rp_rt_survival_estimates_2)

survial_at_time_point <- function(days){
  rp_rt_survival_at_time_points %>% 
    filter(time %in%  c(days)) %>% 
    select(c("cdm_name","group_level","strata_name","strata_level",       
             "variable_level", "time" ,
             "n_risk", "n_events",    
             "cumulative_incidence")) %>% 
    mutate(cumulative_incidence = if_else(is.na(n_events),
                                            NA, cumulative_incidence))
}
