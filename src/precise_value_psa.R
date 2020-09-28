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
# p_l_default <- p_l
p_w_default <- p_w
# p_o_default <- p_o

# run psa
for(i in 1:n_runs) {
  
  # get plan demographics
  plan_demographics <- prop.table(table(sample(c("a", "b", "w"),
                                               n,
                                               replace = TRUE,
                                               prob = c(p_a_default, 
                                                        p_b_default,
                                                        p_w_default))))
  p_a <- plan_demographics["a"] #percentage of population Asian
  p_b <- plan_demographics["b"] #percentage of population Black
  # p_l <- plan_demographics["l"] #percentage of population Latinx
  p_w <- plan_demographics["w"] #percentage of population White
  # p_o <- plan_demographics["o"] #percentage of population other race
  
  # probability of regimen change based on alert: beta distribution. Mean=0.30, CI: 0.1-0.5
  p_change_alert_default <- rbeta(1,4.25,12.76)
  
  # probability of regimen change based on no alert: beta distribution. Mean=0.1, CI: 0 - 0.2 
  p_change_no_alert <- rbeta(1,3.36,30.22)
  
  # QALYs of PGx-clopidogrel: beta distribution. Mean=0.05, CI: 0.04-0.08
  qaly_change_clo <- rbeta(1,22.76, 432.43)
  
  # QALYs of PGx-warfarin: beta distribution. Mean=0.008, CI: 0.005-0.011
  qaly_change_war <- rbeta(1,27.09,3359.35)
  
  # Cost of PGx-clopidogrel: normal distribution. Mean=1700, CI: 1500-1900
  cost_change_clo <- rnorm(1,1500,102.04)

  # Cost of PGx-warfarin: normal distribution. Mean=-150, CI: -350-50
  cost_change_war <- rnorm(1, -150, 102.04)
  
  # start-up cost: normal distribution. Mean=4000, CI: 2000-6000
  start_up_cost_default <- rnorm(1, 4000, 1020.41)
  
  # annual maintenance cost: normal distribution. Mean=100, CI: 50-150
  maint_cost_default <- rnorm(1, 100, 25.51)
  
  # years of screening: log-normal distribution. Mean=5, CI: 3-7
  screen_dur_current <- round(rlnorm(1, 1.61,0.22),0)
  
  # test rate: beta distribution. Mean=0.1, CI: 0.075-0.125
  test_rate_current <- rbeta(1, 55.22, 496.97)
  
  # start age for testing: log normal distribution. Mean=55, CI: 50-60
  start_age_current <-round(rlnorm(1, 4, 0.05),0)
  
  # probability of getting a new drug of interest. Mean=0.005, CI: 0.0025-0.0075
  p_new_rx_current <- rbeta(1, 15.28, 3041.63)
  
  # probability of benefiting from PGx for warfarin
  p_eligible<-rbeta(1, 8.44, 4.15)
  
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

# library(ggplot2)
# library(tidyverse)
# library(scales)

ggplot(output, aes(x = qalys, y = costs)) +
  geom_point() +
  #stat_ellipse(type = "t", linetype = 2) +
  xlab("Incremental QALYs") + 
  scale_y_continuous(name = "Incremental Costs",
                     labels = dollar_format()) +
  theme_bw(base_size = 12)

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