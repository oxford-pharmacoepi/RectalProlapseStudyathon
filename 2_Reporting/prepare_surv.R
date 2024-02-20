

file <- "D:/github/Rectopexy/RectopexyStudyathon/2_Reporting/data/results_AurumHESFull2309 (2)/rectal_prolapse_survival_estimates_AurumHESFull2309.csv" 
file <- "D:/github/Rectopexy/RectopexyStudyathon/2_Reporting/data/results_CPRD_Gold/rectal_prolapse_survival_estimates_CPRD_Gold.csv" 
file <- "D:/github/Rectopexy/RectopexyStudyathon/2_Reporting/data/results_Pharmetrics/rectal_prolapse_survival_estimates_Pharmetrics.csv" 


working <- read_csv(file)
table(working$outcome)
table(working$group_level)
working <- working %>% 
  filter(outcome %in% c("rectopexy_broad_cs"),
         group_level %in% c("rectopexy_broad_cs"))
write_csv(working, file = file)
