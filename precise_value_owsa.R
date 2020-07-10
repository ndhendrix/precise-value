###
### Set base case parameters
###

# Set demographic parameters
n <- 32000  #population size

# racial demographics from overall US demographics
p_a <- 0.06 #percentage of population Asian
p_b <- 0.13 #percentage of population Black
p_l <- 0.18 #percentage of population Latinx
p_w <- 0.60 #percentage of population White
p_o <- 0.03 #percentage of population other race

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

discount <- 0.03

# probability of plan modification based on alert / no alert
p_change_alert_default <- 0.9
p_change_alert <- p_change_alert_default
p_change_no_alert <- 0.1

# qalys and costs of changing pgx
qaly_change_clo <- 0.05
cost_change_clo <- 1265
qaly_change_sim <- 0  # simvastatin no longer included
cost_change_sim <- 0  # as of 6/22
qaly_change_war <- 0.01
cost_change_war <- -50

# time horizon (years)
t_horizon <- 20

# costs associated with alert
start_up_cost_default <- 4000
maint_cost_default <- 100
start_up_cost <- start_up_cost_default
maint_cost <- maint_cost_default

# testing and medication use patterns
screen_dur <- 5 # years of screening
p_new_rx <- 0.005 # annual probability of a new rx for one of the included drugs
test_rate <- 0.1 # annual probability of testing for patients in the selected age range
start_age <- 55 # age at start of screening
screen_dur_current <- screen_dur
p_new_rx_current <- p_new_rx
test_rate_current <- test_rate
start_age_current <- start_age

###
### load model and get base case ICER
###

source("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/precise_value_r_model.R") 
total <- precise_value()[[2]] 

base_icer <-  round((sum(total$alert_cost) - sum(total$no_alert_cost))/(sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

###
### set low and high values
###

screen_dur_lo <- 2 # years of screening
screen_dur_hi <- 8
p_new_rx_lo <- 0.0025 # annual probability of a new rx for one of the included drugs
p_new_rx_hi <- 0.0075 
test_rate_lo <- 0.05 # annual probability of testing for patients in the selected age range
test_rate_hi <- 0.15
start_up_lo <- 2000 # start-up costs for alerts
start_up_hi <- 6000 
start_age_lo <- 50 # age at start of screening
start_age_hi <- 60
p_reg_change_lo <- 0.8 # probability of changing regimen when alerted
p_reg_change_hi <- 1
maint_cost_lo <- 50 # annual maintenance cost for alerting program
maint_cost_hi <- 150

###
### get values from model for one-ways
###

# create output data frame
output <- data.frame(parameter = c("Screening duration",
                                   "Probability of new rx",
                                   "Testing rate",
                                   "Start-up cost",
                                   "Age at screening start",
                                   "Probability of regimen change",
                                   "Maintenance cost"),
                     lo_icer = rep(0,7),
                     hi_icer = rep(0,7))

# source data input functions for creation of data frames for age distribution, testing patterns, and treatment patterns
source("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/make_inputs.R")

# get screening duration low value
screen_dur_current <- screen_dur_lo
make_test_pattern() # update data source with new data
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Screening duration", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                       (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get screening duration high value
screen_dur_current <- screen_dur_hi
make_test_pattern() 
total <- precise_value()[[2]] 
output[output$parameter == "Screening duration", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                       (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case screen_dur value
screen_dur_current <- screen_dur
make_test_pattern()



# get probability of new rx low value
p_new_rx_current <- p_new_rx_lo
make_treat_prob() # update data source with new data
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Probability of new rx", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                       (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get probability of new rx high value
p_new_rx_current <- p_new_rx_hi
make_treat_prob() 
total <- precise_value()[[2]] 
output[output$parameter == "Probability of new rx", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                       (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case probability of new rx value
p_new_rx_current <- p_new_rx
make_treat_prob()



# get test rate low value
test_rate_current <- test_rate_lo
make_test_pattern() # update data source with new data
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Testing rate", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                          (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get test rate high value
test_rate_current <- test_rate_hi
make_test_pattern()
total <- precise_value()[[2]] 
output[output$parameter == "Testing rate", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                          (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for test rate value
test_rate_current <- test_rate
make_test_pattern()



# get start up cost low value
start_up_cost <- start_up_lo
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Start-up cost", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                 (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get start up cost high value
start_up_cost <- start_up_hi
total <- precise_value()[[2]] 
output[output$parameter == "Start-up cost", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                 (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case start up cost value
start_up_cost <- start_up_cost_default



# get screening age low value
start_age_current <- start_age_lo
make_test_pattern() # update data source with new data
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Age at screening start", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                       (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get screening age high value
start_age_current <- start_age_hi
make_test_pattern() 
total <- precise_value()[[2]] 
output[output$parameter == "Age at screening start", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                       (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case screen_age value
start_age_current <- start_age
make_test_pattern()



# get p_change_alert low value
p_change_alert <- p_reg_change_lo
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Probability of regimen change", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get p_change_alert high value
p_change_alert <- p_reg_change_hi
total <- precise_value()[[2]] 
output[output$parameter == "Probability of regimen change", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case p_change_alert value
p_change_alert <- p_change_alert_default



# get maint_cost low value
maint_cost <- maint_cost_lo
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Maintenance cost", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get maint_cost high value
maint_cost <- maint_cost_hi
total <- precise_value()[[2]] 
output[output$parameter == "Maintenance cost", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case maint_cost value
maint_cost <- maint_cost_default


###
### plot tornado diagram
###

library(ggplot2)
library(tidyverse)
library(scales)

plot_output <- output %>%
  mutate(distance = (abs(hi_icer - base_icer) + abs(lo_icer - base_icer))/2) %>%
  gather("type", "value", 2:3)

plot_output$parameter <- reorder(plot_output$parameter,
                                 plot_output$distance,
                                 FUN = max)

plot_output <- plot_output %>%
  mutate(ymax = ifelse(value > base_icer, value, base_icer),
         ymin = ifelse(value < base_icer, value, base_icer))

width = 0.95

ggplot() +
  geom_rect(data = plot_output,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(parameter) - width / 2,
                xmax = as.numeric(parameter) + width / 2,
                fill = type)) +
  theme_bw(base_size = 18) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_icer) +
  scale_y_continuous(name = "Incremental Cost-Effectiveness Ratio Versus No Alert",
                     labels = dollar_format()) +
  scale_x_continuous(breaks = c(1:nrow(plot_output[plot_output$type == "lo_icer",])),
                     labels = levels(plot_output$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()
