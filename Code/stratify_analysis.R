##################################
###### Sensitivity analysis ######
###  Stratify by health status ###
##################################

# Part 1: Get data ready

# Split main data into required parts
poor_health <- all_data[all_data$imp_general_health == "Fair/Poor",] # Subset data for fair/poor health status
wgt_poor <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = poor_health) # Define survey design
good_health <- all_data[all_data$imp_general_health == "Good",] # Repeat for good health status
wgt_good <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = good_health)

# Part 2: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
stratmodel_any_a_f <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
strat_table2 <- tidy(stratmodel_any_a_f, conf.int = TRUE) # Save key results
strat_table2 <- strat_table2[2,] # Extract only healthcare disruption variable
strat_table2$model <- "All ACS - poor" # Note model name

stratmodel_any_a_g <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_any_a_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "All ACS - good" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

# Model b: Any ambulatory care sensitive acute conditions
stratmodel_any_b_f <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_any_b_f, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - poor" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

stratmodel_any_b_g <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_any_b_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - good" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
stratmodel_any_c_f <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_any_c_f, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - poor" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

stratmodel_any_c_g <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_any_c_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - good" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

# Model d: Any ambulatory care sensitive conditions
stratmodel_any_d_f <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_any_d_f, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - poor" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

stratmodel_any_d_g <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_any_d_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - good" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
stratmodel_any_e_f <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_any_e_f, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - poor" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

stratmodel_any_e_g <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_any_e_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - good" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

# Model f: Any admission
stratmodel_any_f_f <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_any_f_f, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - poor" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

stratmodel_any_f_g <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health 
hold <- tidy(stratmodel_any_f_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - good" # Note model name
strat_table2 <- rbind(strat_table2, hold) # Join to main table

write.csv(strat_table2, "../../outputs/regression_model1_strat.csv") # Save



# Part 3: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
stratmodel_type_a_f <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
strat_table3 <- tidy(stratmodel_type_a_f, conf.int = TRUE) # Save key results
strat_table3 <- strat_table3[2:4,] # Extract only healthcare disruption variable
strat_table3$model <- "All ACS - poor" # Note model name

stratmodel_type_a_g <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_type_a_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "All ACS - good" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

# Model b: Acute ambulatory care sensitive acute conditions
stratmodel_type_b_f <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_type_b_f, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - poor" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

stratmodel_type_b_g <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_type_b_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - good" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
stratmodel_type_c_g <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_type_c_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - poor" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

stratmodel_type_c_g <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_type_c_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - good" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

# Model d: Vaccine preventable ambulatory care sensitive conditions
stratmodel_type_d_f <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_type_d_f, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - poor" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

stratmodel_type_d_g <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_type_d_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - good" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
stratmodel_type_e_f <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_type_e_f, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - poor" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

stratmodel_type_e_g <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_type_e_g, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(stratmodel_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - good" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

# Model f: Any hospital admission
stratmodel_type_f_f <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel_type_f_f, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - poor" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

stratmodel_type_f_g <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel_type_f_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - good" # Note model name
strat_table3 <- rbind(strat_table3, hold) # Join to main table

write.csv(strat_table3, file = "S:/LLC_0009/outputs/regression_model2_strat.csv") # Save


## Part 4: Falsification tests #

# Model a: Any disruption
stratmodel8a_f <- svyglm(vaccinated ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
strat_table6 <- tidy(stratmodel8a_f, conf.int = TRUE) # Save key results
strat_table6 <- strat_table6[2,] # Extract only healthcare disruption variable
strat_table6$model <- "Any - poor" # Note model name

stratmodel8a_g <- svyglm(vaccinated ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel8a_g, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any - good" # Note model name
strat_table6 <- rbind(strat_table6, hold) # Join to main table

# Model b: Disruption by type
stratmodel8b_f <- svyglm(vaccinated ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = poor_health, family = "binomial", design = wgt_poor) # Poor health
hold <- tidy(stratmodel8b_f, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - poor" # Note model name
strat_table6 <- rbind(strat_table6, hold) # Join to main table

stratmodel8b_g <- svyglm(vaccinated ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(cohort) + imp_quan_cci, data = good_health, family = "binomial", design = wgt_good) # Good health
hold <- tidy(stratmodel8b_g, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - good" # Note model name
strat_table6 <- rbind(strat_table6, hold) # Join to main table

write.csv(strat_table6, "../../outputs/falsification_test_strat.csv") # Save
gc()


