###
### Set base case parameters
###

# set number of runs for psa
n_runs <- 100

# Set demographic parameters
n <- 32000  #population size

# time horizon (years)
t_horizon <- 20

# discount
discount <- 0.03

# default testing and medication use patterns
screen_dur_default <- 5 # years of screening
p_new_rx_default <- 0.005 # annual probability of a new rx for one of the included drugs
test_rate_default <- 0.1 # annual probability of testing for patients in the selected age range
start_age_default <- 55 # age at start of screening

# calculated from race-specific prevalence of CYP2C19 variants
p_clo_a <- 0.1298 #prevalence of clopidogrel variant (poor metabolizers) among Asian (E. Asian) patients
p_clo_b <- 0.0405 #prevalence of clopidogrel variant (poor metabolizers) among Black (African American) patients
p_clo_l <- 0.0114 #prevalence of clopidogrel variant (poor metabolizers) among Latinx (Latino) patients
p_clo_w <- 0.2722 #prevalence of clopidogrel variant (poor metabolizers) among White (European) patients
p_clo_o <- 0.1061 #prevalence of clopidogrel variant (poor metabolizers) among other race (average of all races) patients

# calculated from race-specific prevalence of CYP2C9 variants
p_sim_a <- 0.1575 #prevalence of simvastatin variant (poor or medium metabolizers) among Asian (E. Asian) patients
p_sim_b <- 0.2413 #prevalence of simvastatin variant (poor or medium metabolizers) among Black (African American) patients
p_sim_l <- 0.2542 #prevalence of simvastatin variant (poor or medium metabolizers) among Latinx (Latino) patients
p_sim_w <- 0.3708 #prevalence of simvastatin variant (poor or medium metabolizers) among White (European) patients
p_sim_o <- 0.2599 #prevalence of simvastatin variant (poor or medium metabolizers) among other (average of all races) race patients

# assumes that CYP2C9 and VKORC1 are independent 
p_war_a <- 0.9006 #prevalence of warfarin variant among Asian (E. Asian) patients
p_war_b <- 0.3392 #prevalence of warfarin variant among Black (African American) patients
p_war_l <- 0.6005 #prevalence of warfarin variant among Latinx (Americas) patients
p_war_w <- 0.6303 #prevalence of warfarin variant among White (Caucasian) patients
p_war_o <- 0.5743 #prevalence of warfarin variant among other (average of all races) race patients


###
### load model 
###

source("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/precise_value_r_model.R") 
source("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/make_inputs.R") 

###
### run psa
###

# create output data frame
output <- data.frame(run = seq(1,n_runs),
                     costs = rep(0,n_runs),
                     qalys = rep(0, n_runs))

# run psa
for(i in 1:n_runs) {
  
  # get plan demographics
  plan_demographics <- prop.table(table(sample(c("a", "b", "l", "w", "o"),
                                               n,
                                               replace = TRUE,
                                               prob = c(0.06, 0.13, 0.18, 0.60, 0.03))))
  p_a <- plan_demographics["a"] #percentage of population Asian
  p_b <- plan_demographics["b"] #percentage of population Black
  p_l <- plan_demographics["l"] #percentage of population Latinx
  p_w <- plan_demographics["w"] #percentage of population White
  p_o <- plan_demographics["o"] #percentage of population other race
  
  # probability of plan modification based on alert / no alert
  p_change_alert_default <- runif(1, 0.8, 1) # uniform distribution between 0.8 and 1
  p_change_no_alert <- runif(1, 0, 0.2) # uniform distribution between 0 and 
  
  # qalys and costs of changing pgx
  qaly_change_clo <- runif(1, 0.02, 0.08)
  cost_change_clo <- runif(1, 1065, 1465)
  qaly_change_sim <- 0  # simvastatin no longer included
  cost_change_sim <- 0  # as of 6/22
  qaly_change_war <- runif(1, 0.005, 0.015)
  cost_change_war <- runif(1, -250, 150)
  
  # costs associated with alert
  start_up_cost_default <- runif(1, 2000, 6000)
  maint_cost_default <- runif(1, 50, 150)
  
  # testing and medication use patterns
  screen_dur_current <- round(runif(1, 3, 7),0) # years of screening
  p_new_rx_current <- runif(1, 0.0025, 0.0075) # annual probability of a new rx for one of the included drugs
  test_rate_current <- runif(1, 0.075, 0.125) # annual probability of testing for patients in the selected age range
  start_age_current <- round(runif(1, 50, 60),0) # age at start of screening
  
  print(paste0(screen_dur_current, " ", test_rate_current, " ", start_age_current))
  
  # recreate datasets with test and treat info
  make_test_pattern()
  make_treat_prob()
  
  # run model
  total <- precise_value()[[2]]
  output[i, "costs"] <- sum(total$alert_cost) - sum(total$no_alert_cost)
  output[i, "qalys"] <- sum(total$alert_qaly) - sum(total$no_alert_qaly)
}

###
### plot diagram
###

library(ggplot2)
library(tidyverse)
library(scales)

ggplot(output, aes(x = qalys, y = costs)) +
  geom_point() +
  #stat_ellipse(type = "t", linetype = 2) +
  xlab("Incremental QALYs") + 
  scale_y_continuous(name = "Incremental Costs",
                     labels = dollar_format()) +
  theme_bw(base_size = 18)

###
### reset datasets with defaults
###

# testing and medication use patterns
screen_dur_current <- screen_dur_default
p_new_rx_current <- p_new_rx_default
test_rate_current <- test_rate_default
start_age_current <- start_age_default

# recreate datasets with test and treat info
make_test_pattern()
make_treat_prob()