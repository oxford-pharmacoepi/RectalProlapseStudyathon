# start -----
start_time <- Sys.time()
# cdm reference ----
cli::cli_text("- Creating CDM reference ({Sys.time()})")
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_schema,
                                  write_schema = c(schema = write_schema,
                                                   prefix = study_prefix),
                                  cdm_name = db_name)


# cdm snapshot ----
cli::cli_text("- Getting cdm snapshot ({Sys.time()})")
write_csv(snapshot(cdm), here("results", paste0(
  "cdm_snapshot_", cdmName(cdm), ".csv"
)))

# import concepts ------
cli::cli_text("- Importing concepts ({Sys.time()})")
rp_cs <- codesFromConceptSet(
  path = here("ConceptSets", "rectal_prolapse"),
  cdm = cdm)
rt_cs <- codesFromConceptSet(
  path = here("ConceptSets", "rectopexy"),
  cdm = cdm)

study_cs <- list_flatten(list(rp_cs, rt_cs))

# instantiate cohorts -------
cli::cli_text("- Instantiating rectal prolapse cohorts ({Sys.time()})")
cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                              conceptSet = rp_cs,
                                              limit = "first",
                                              end = "observation_period_end_date",
                                              name = "rectal_prolapse",
                                              overwrite = TRUE)
# another table with only those present during the study period
# will use later for characterisation and survival
cdm[["rectal_prolapse_study"]] <- cdm[["rectal_prolapse"]] %>%
  dplyr::compute(name = "rectal_prolapse_study",
                 temporary = FALSE,
                 overwrite = TRUE) %>%
  dplyr::filter(cohort_start_date >= as.Date("2013-01-01") &
                  cohort_start_date <= as.Date("2022-12-31")) %>%
  CDMConnector::recordCohortAttrition(reason =
                                        "cohort entry between 2013-01-01 to 2022-12-31") %>%
  addAge() %>%
  filter(age >= 18)%>%
  dplyr::compute(name = "rectal_prolapse_study",
                 temporary = FALSE,
                 overwrite = TRUE) %>%
  record_cohort_attrition("Age 18 or over")

attr(cdm[["rectal_prolapse_study"]], "cohort_set")<- attr(cdm[["rectal_prolapse_study"]],
                                                                 "cohort_set") %>%
  dplyr::mutate(cohort_name= paste0(cohort_name, "_study"))

cli::cli_text("- Instantiating rectopexy cohorts ({Sys.time()})")
cdm <- CDMConnector::generateConceptCohortSet(cdm,
                                              conceptSet = rt_cs,
                                              limit = "first",
                                              end = "observation_period_end_date",
                                              name = "rectopexy",
                                              overwrite = TRUE)

# single cohort table for diagnostics
cdm <- omopgenerics::bind(cdm$rectal_prolapse,
                          cdm$rectal_prolapse_study,
                   cdm$rectopexy,
                   name = "study_cohorts")
attr(cdm$study_cohorts, "cohort_attrition") <- attr(cdm$study_cohorts, "cohort_attrition") %>%
  select(!any_of(c("limit", "prior_observation",
            "future_observation", "end")))

# cohort counts ----
cli::cli_text("- Getting cohort counts ({Sys.time()})")

cohort_counts <- summary(cdm$study_cohorts) %>%
  suppress(minCellCount = 5)
write_csv(cohort_counts,
          here("results", paste0(
            "cohort_count_", cdmName(cdm), ".csv"
          )))

# index events  ----
cli::cli_text("- Getting index event codes ({Sys.time()})")
index_codes<- list()
non_empty_cohorts <- sort(cohort_count(cdm[["study_cohorts"]]) %>%
                            filter(number_records > 0) %>%
                            pull("cohort_definition_id"))

for(i in seq_along(non_empty_cohorts)){
  working_cohort_id <- non_empty_cohorts[i]
  working_cohort <- settings(cdm[["study_cohorts"]]) %>%
    filter(cohort_definition_id == working_cohort_id) %>%
    pull("cohort_name")
  cli::cli_text("-- For {working_cohort} ({i} of {length(non_empty_cohorts)})")

  working_cs <- study_cs[str_replace(working_cohort, "_study", "")]


  index_codes[[i]] <- summariseCohortCodeUse(working_cs,
                                             cohortTable = "study_cohorts",
                                             cohortId = working_cohort_id,
                                             timing = "entry",
                                             cdm = cdm) %>%
    mutate(cohort_name = working_cohort)

}
index_codes <- bind_rows(index_codes) %>%
  mutate(cdm_name = db_name)
write_csv(index_codes,
          here("results", paste0(
            "index_codes_", cdmName(cdm), ".csv"
          )))

# incidence and prevalence -----
cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator",
                                    ageGroup = list(c(18,150),
                                                    c(18,24),
                                                    c(25,34),
                                                    c(35,44),
                                                    c(45,54),
                                                    c(55,64),
                                                    c(65,74),
                                                    c(75,84),
                                                    c(85,94),
                                                    c(95,150)),
                                    cohortDateRange = as.Date(c("2013-01-01",
                                                                "2022-12-31")),
                                    sex = c("Both", "Male", "Female"),
                                    daysPriorObservation = c(0, 365))

rp_inc_gpop <- estimateIncidence(cdm,
                                 denominatorTable = "denominator",
                                 outcomeTable = "rectal_prolapse",
                                 interval = "years",
                                 outcomeWashout = Inf,
                                 completeDatabaseIntervals = TRUE)

write_csv(rp_inc_gpop,
          here("results", paste0(
            "rectal_prolapse_incidence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(attrition(rp_inc_gpop),
          here("results", paste0(
            "rectal_prolapse_incidence_attrition_general_population_", cdmName(cdm), ".csv"
          )))

prev_gpop <- estimatePeriodPrevalence(cdm,
                                      denominatorTable = "denominator",
                                      outcomeTable = "rectal_prolapse",
                                      interval = "years",
                                      completeDatabaseIntervals = TRUE,
                                      fullContribution = TRUE)
write_csv(prev_gpop,
          here("results", paste0(
            "rectal_prolapse_prevalence_general_population_", cdmName(cdm), ".csv"
          )))
write_csv(attrition(prev_gpop),
          here("results", paste0(
            "rectal_prolapse_prevalence_attrition_general_population_", cdmName(cdm), ".csv"
          )))

# cohort characteristics ----
cli::cli_text("- Getting patient characteristics ({Sys.time()})")
rp_chars <- PatientProfiles::summariseCharacteristics(cdm$rectal_prolapse_study,
                                                      ageGroup = list(c(18,24),
                                                                      c(25,34),
                                                                      c(35,44),
                                                                      c(45,54),
                                                                      c(55,64),
                                                                      c(65,74),
                                                                      c(75,84),
                                                                      c(85,94),
                                                                      c(95,150)))


write_csv(rp_chars,
          here("results", paste0(
            "rectal_prolapse_patient_characteristics_", cdmName(cdm), ".csv"
          )))


# large scale characteristics ----
cli::cli_text("- Getting large scale characteristics ({Sys.time()})")
rp_lsc <- PatientProfiles::summariseLargeScaleCharacteristics(cdm$rectal_prolapse_study,
                                                              eventInWindow = c("condition_occurrence",
                                                                                "drug_exposure",
                                                                                "procedure_occurrence",
                                                                                "observation"),
                                                              window = list(c(-Inf, 0),
                                                                            c(0, 0)))
write_csv(rp_lsc,
          here("results", paste0(
            "large_scale_characteristics_", cdmName(cdm), ".csv"
          )))

# risk of rectopexy ------
cdm$rectal_prolapse_study <- cdm$rectal_prolapse_study %>%
  addDemographics(ageGroup = list(
    c(18,24),
    c(25,34),
    c(35,44),
    c(45,54),
    c(55,64),
    c(65,74),
    c(75,84),
    c(85,94),
    c(95,150)))

surv <- estimateSingleEventSurvival(cdm = cdm,
                                    targetCohortTable = "rectal_prolapse_study",
                                    outcomeCohortTable = "rectopexy",
                                    strata = list(c("age_group"),
                                                  c("sex"),
                                                  c("age_group", "sex")),
                                    estimateGap = 1,
                                    eventGap = 365)

write_csv(surv,
          here("results", paste0(
            "rectal_prolapse_survival_estimates_", cdmName(cdm), ".csv"
          )))
write_csv(attr(surv, "events"),
          here("results", paste0(
            "rectal_prolapse_survival_events_", cdmName(cdm), ".csv"
          )))
write_csv(attr(surv, "summary"),
          here("results", paste0(
            "rectal_prolapse_survival_summary_", cdmName(cdm), ".csv"
          )))



# zip all results -----
cli::cli_text("- Zipping results ({Sys.time()})")
files_to_zip <- list.files(here("results"))
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        db_name)]
files_to_zip <- files_to_zip[str_detect(files_to_zip,
                                        ".csv")]

zip::zip(zipfile = file.path(paste0(
  here("results"), "/results_", db_name, ".zip"
)),
files = files_to_zip,
root = here("results"))

dur <- abs(as.numeric(Sys.time() - start_time, units = "secs"))
cli::cli_alert_success("Cohort diagnostics finished")
cli::cli_alert_success(glue::glue(
  "Study code ran in {floor(dur/60)} min and {dur %% 60 %/% 1} sec"
))
