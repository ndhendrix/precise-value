###
### Set base case parameters
###

# set number of runs for psa
n_runs <- 100

# Set demographic parameters
n <- 25000  #population size

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
p_clo_a <- 0.5394 
p_clo_b <- 0.3900
p_clo_w <- 0.3818 

##CYP2C9 variant: poor metabolizer
p_2c9_a<-0.0217
p_2c9_b<-0.0053
p_2c9_w<-0.0141

##CYP4F2 variant: decreased function
p_4f2_a<-0.3128
p_4f2_b<-0.0758
p_4f2_w<-0.3188

##VKORC1 variant
p_vko_a<-0.5174
p_vko_b<-0.1159
p_vko_w<-0.5329

# CYP2C9, VKORC1, CYP4F2. assumes independence
p_war_a <- 1-(1-p_2c9_a)*(1-p_4f2_a)*(1-p_vko_a)
p_war_b <- 1-(1-p_2c9_b)*(1-p_4f2_b)*(1-p_vko_b)
p_war_w <- 1-(1-p_2c9_w)*(1-p_4f2_w)*(1-p_vko_w)

###
### load model 
###

source("precise_value_r_model.R") 
source("make_inputs.R") 

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
  plan_demographics <- prop.table(table(sample(c("a", "b", "w"),
                                               n,
                                               replace = TRUE,
                                               prob = c(0.0603, 0.1369, 0.8028))))
  p_a <- plan_demographics["a"] #percentage of population Asian
  p_b <- plan_demographics["b"] #percentage of population Black
  p_w <- plan_demographics["w"] #percentage of population White

  # probability of plan modification based on alert / no alert
  p_change_alert_default <- runif(1, 0.8, 1) # uniform distribution between 0.8 and 1
  p_change_no_alert <- runif(1, 0, 0.2) # uniform distribution between 0 and 
  
  # qalys and costs of changing pgx
  qaly_change_clo <- runif(1, 0.02, 0.08)
  cost_change_clo <- runif(1, 1065, 1465)
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
  theme_bw(base_size = 30)

###
### reset datasets with defaults
###

# testing and medication use patterns
screen_dur_current <- screen_dur_default
p_new_rx_current <- p_new_rx_default
test_rate_current <- test_rate_default
start_age_current <- start_age_default

# # recreate datasets with test and treat info
make_test_pattern()
make_treat_prob()
