n <- 250000  #change.population size

# racial demographics from overall US demographics
p_a <- 0.0603
p_b <- 0.1369 #percentage of population Black
p_w <- 0.8028#percentage of population Latinx

# calculated from race-specific prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer
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

# setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value")
# source("precise_value_r_model.R")
precise_value()
return_list <- precise_value()
outcomes <- return_list[[1]]
total <- return_list[[2]]

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
source("make_inputs.R")

# get screening duration low value
screen_dur_current <- screen_dur_lo #low value=2
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
  theme_bw(base_size = 30) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_icer) +
  scale_y_continuous(name = "ICER: Alert VS No Alert",
                     labels = dollar_format()) +
  scale_x_continuous(breaks = c(1:nrow(plot_output[plot_output$type == "lo_icer",])),
                     labels = levels(plot_output$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()
