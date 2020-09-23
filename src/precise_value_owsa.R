
# probability of plan modification based on alert / no alert
p_change_alert_default <- p_change_alert
p_change_no_alert_default<-p_change_no_alert
start_up_cost_default <- start_up_cost
maint_cost_default <- maint_cost
screen_dur_current <- screen_dur
p_new_rx_current <- p_new_rx
test_rate_current <- test_rate
start_age_current <- start_age
p_eligible_default<-p_eligible

###
### load model and get base case ICER
###

source(here("src", "precise_value_r_model.R")) 
source(here("src", "precise_value_base_values.R"))
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
p_change_alert_lo <- 0.1 # probability of changing regimen when alerted
p_change_alert_hi <- 0.5
p_change_no_alert_lo<-0 # probability of changing regimen when not alerted
p_change_no_alert_hi<-0.2
maint_cost_lo <- 50 # annual maintenance cost for alerting program
maint_cost_hi <- 150
p_eligible_lo<-0.50 # % of people on warfarin who can benefit from PGx
p_eligible_hi<-0.75

###
### get values from model for one-ways
###

# create output data frame
output <- data.frame(parameter = c("Screening duration",
                                   "Probability of new rx",
                                   "Testing rate",
                                   "Start-up cost",
                                   "Age at screening start",
                                   "Probability of regimen change with an alert",
                                   "Probability of regimen change without an alert",
                                   "Maintenance cost",
                                   "Proportion of people on warfarin benefit from PGx"),
                     lo_icer = rep(0,9),
                     hi_icer = rep(0,9))

# source data input functions for creation of data frames for age distribution, testing patterns, and treatment patterns
source(here("src", "make_inputs.R"))


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
p_change_alert <- p_change_alert_lo
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Probability of regimen change with an alert", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get p_change_alert high value
p_change_alert <- p_change_alert_hi
total <- precise_value()[[2]] 
output[output$parameter == "Probability of regimen change with an alert", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case p_change_alert value
p_change_alert <- p_change_alert_default


# get p_change_no_alert low value
p_change_no_alert <- p_change_no_alert_lo
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Probability of regimen change without an alert", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get p_change_alert high value
p_change_no_alert <- p_change_no_alert_hi
total <- precise_value()[[2]] 
output[output$parameter == "Probability of regimen change without an alert", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                                  (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case p_change_alert value
p_change_no_alert <- p_change_no_alert_default


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



# get p_eligible low value
p_eligible <- p_eligible_lo
total <- precise_value()[[2]] # run model and take second entry in list of results
output[output$parameter == "Proportion of people on warfarin benefit from PGx", "lo_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                     (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# get p_eligible high value
p_eligible <- p_eligible_hi
total <- precise_value()[[2]] 
output[output$parameter == "Proportion of people on warfarin benefit from PGx", "hi_icer"] <- round((sum(total$alert_cost) - sum(total$no_alert_cost))/
                                                                     (sum(total$alert_qaly) - sum(total$no_alert_qaly)),2)

# reset data for base case maint_cost value
p_eligible <- p_eligible_default



###
### plot tornado diagram
###

# library(ggplot2)
# library(tidyverse)
# library(scales)

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

owsa_plot <- ggplot() +
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
