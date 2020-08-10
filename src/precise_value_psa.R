###
### Set base case parameters
###

# set number of runs for psa
n_runs <- 100

# default testing and medication use patterns
screen_dur_default <- screen_dur # years of screening
p_new_rx_default <- p_new_rx # annual probability of a new rx for one of the included drugs
test_rate_default <- test_rate # annual probability of testing for patients in the selected age range
start_age_default <- start_age # age at start of screening

###
### load model 
###

source(here("src", "precise_value_r_model.R"))
source(here("src", "make_inputs.R"))

###
### run psa
###

# create output data frame
output <- data.frame(run = seq(1,n_runs),
                     costs = rep(0,n_runs),
                     qalys = rep(0, n_runs))

# set default demographic values
p_a_default <- p_a
p_b_default <- p_b
p_l_default <- p_l
p_w_default <- p_w
p_o_default <- p_o

# run psa
for(i in 1:n_runs) {
  
  # get plan demographics
  plan_demographics <- prop.table(table(sample(c("a", "b", "l", "w", "o"),
                                               n,
                                               replace = TRUE,
                                               prob = c(p_a_default, 
                                                        p_b_default, 
                                                        p_l_default, 
                                                        p_w_default, 
                                                        p_o_default))))
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