###
### Set default values
###

#1. Regimen change with an alert.
p_change_alert_default <- p_change_alert

#2. Regimen change without an alert.
p_change_no_alert_default<-p_change_no_alert

#3. risk of getting clopidogrel. 
rr_new_rx_clo_default<-rr_new_rx_clo

#4. risk of getting warfarin. 
rr_new_rx_war_default<-rr_new_rx_war

#5. start-up costs
start_up_cost_default <- start_up_cost

#6. maintenance costs
maint_cost_default <- maint_cost

#7. probability of benefiting from PGx for warfarin
p_eligible_default<-p_eligible

###
### load model and get base case ICER
###

source(here("src", "precise_value_r_model.R")) 
source(here("src", "precise_value_base_values.R"))
CEA_results_discounted_OWSA<- precise_value()[[2]] 

base_icer<-round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/(sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
base_nalerts<-sum(CEA_results_discounted_OWSA$alert_n)
base_totalcostsperalert<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)
base_admincostsperalert<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/base_nalerts
base_medicalcostsperalert<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/base_nalerts


###
### set low and high values. Update: 01/07/2021. 
###

#1. Regimen change with an alert. Range: 0.15-0.5, Base: 0.25. 
p_change_alert_lo <- 0.15
p_change_alert_hi <- 0.5

#2. Regimen change without an alert. Range: 0-0.2, Base: 0.1. 
p_change_no_alert_lo<-0
p_change_no_alert_hi<-0.2

#3. risk of getting clopidogrel. Range: 0.5-1.5, Base: 1
rr_new_rx_clo_lo<-0.5
rr_new_rx_clo_hi<-1.5

#4. risk of getting warfarin Range: 0.5-1.5, Base: 1
rr_new_rx_war_lo<-0.5
rr_new_rx_war_hi<-1.5

#5. Start-up costs. Range: 2000-6000, Base: 4000
start_up_lo <- 2000
start_up_hi <- 6000 

#6. Maintenance costs. Range: 50-150, Base: 100
maint_cost_lo <- 50
maint_cost_hi <- 150

#7. Probability of benefiting from PGx for warfarin. Range: 0.50-0.75, Base: 0.67. 
p_eligible_lo<-0.50
p_eligible_hi<-0.75


###
### get values from model for one-ways
###

# create output data frame
OWSA_output <- data.frame(parameter = c("Probability of regimen change with an alert",
                                        "Probability of regimen change without an alert",
                                        "Risk of initiating clopidogrel for ACS",
                                        "Risk of initiating warfarin for AF",
                                        "One-time start-up cost for alerts",
                                        "Annual maintenance cost for alerts",
                                        "Proportion of people on warfarin benefit from PGx"),
                     lo_icer = rep(0,7),
                     hi_icer = rep(0,7),
                     lo_nalerts=rep(0,7),
                     hi_nalerts=rep(0,7),
                     lo_totalcostsperalert=rep(0,7),
                     hi_totalcostsperalert=rep(0,7),
                     lo_admincostsperalert=rep(0,7),
                     hi_admincostsperalert=rep(0,7),
                     lo_medicalcostsperalert=rep(0,7),
                     hi_medicalcostsperalert=rep(0,7))

# source data input functions for creation of data frames for age distribution, testing patterns, and treatment patterns
source(here("src", "make_inputs.R"))

###########################################
# 1. Probability of regimen change with an alert
##1-1.  Probability of regimen change with an alert - lo
p_change_alert <- p_change_alert_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                       (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##1-2.  Probability of regimen change with an alert - hi
p_change_alert <- p_change_alert_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                       (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case p_change_alert value
p_change_alert <- p_change_alert_default

###########################################
# 2. Probability of regimen change without an alert
##2-1. Probability of regimen change without an alert - lo
p_change_no_alert <- p_change_no_alert_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                          (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##2-2. Probability of regimen change without an alert - hi
p_change_no_alert <- p_change_no_alert_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                          (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Probability of regimen change without an alert","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case p_change_alert value
p_change_no_alert <- p_change_no_alert_default


###########################################
# 3. Risk of initiating clopidogrel for ACS
##3-1. Risk of initiating clopidogrel for ACS - lo
rr_new_rx_clo<-rr_new_rx_clo_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                          (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##3-2. Risk of initiating clopidogrel for ACS - hi
rr_new_rx_clo<-rr_new_rx_clo_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                          (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating clopidogrel for ACS","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset base case value
rr_new_rx_clo <- rr_new_rx_clo_default



###########################################
# 4. Risk of initiating warfarin for AF
##4-1. Risk of initiating warfarin for AF - lo
rr_new_rx_war<-rr_new_rx_war_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                  (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##4-2. Risk of initiating warfarin for AF - hi
rr_new_rx_war<-rr_new_rx_war_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                  (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Risk of initiating warfarin for AF","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset base case value
rr_new_rx_war <- rr_new_rx_war_default


###########################################
###5. One-time start-up cost for alerts
##5-1. One-time start-up cost for alerts - lo
start_up_cost <- start_up_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                   (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##5-2. One-time start-up cost for alerts - hi
start_up_cost <- start_up_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                    (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="One-time start-up cost for alerts","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case start up cost value
start_up_cost <- start_up_cost_default

###########################################
###6. Annual maintenance cost for alerts
##6-1. Annual maintenance cost for alerts - lo
maint_cost <- maint_cost_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                    (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##6-2. Annual maintenance cost for alerts - hi
maint_cost <- maint_cost_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                    (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Annual maintenance cost for alerts","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case maint_cost value
maint_cost <- maint_cost_default

###########################################
###7. Proportion of people on warfarin benefit from PGx
##7-1. Proportion of people on warfarin benefit from PGx - lo
p_eligible<-p_eligible_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                             (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##7-2. Proportion of people on warfarin benefit from PGx - hi
p_eligible<-p_eligible_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                          (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Proportion of people on warfarin benefit from PGx","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case p_change_alert value
p_eligible<-p_eligible_default


#######################################
### plot tornado diagram - ICER
#######################################

ICER_OWSA <- OWSA_output[,1:3] %>%
  mutate(distance = (abs(hi_icer - base_icer) + abs(lo_icer - base_icer))/2) %>%
  gather("type", "value", 2:3)

ICER_OWSA$parameter <- reorder(ICER_OWSA$parameter,
                               ICER_OWSA$distance,
                                    FUN = max)

ICER_OWSA <- ICER_OWSA %>%
  mutate(ymax = ifelse(value > base_icer, value, base_icer),
         ymin = ifelse(value < base_icer, value, base_icer))

width = 0.95

plot_ICER_OWSA<- ggplot() +
  geom_rect(data = ICER_OWSA,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(parameter) - width / 2,
                xmax = as.numeric(parameter) + width / 2,
                fill = type)) +
  theme_bw(base_size = 10) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_icer) +
  scale_y_continuous(name = "Incremental Cost-Effectiveness Ratio PGx-CDS Alerts Versus No Alert",
                     labels = dollar_format()) +
  scale_x_continuous(breaks = c(1:nrow(ICER_OWSA[ICER_OWSA$type == "lo_icer",])),
                     labels = levels(ICER_OWSA$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()

ggsave("OWSA_ICER.png",plot=plot_ICER_OWSA,dpi=600,
       width=10)

#######################################
### plot tornado diagram - n alerts
#######################################
NAlerts_OWSA <- OWSA_output[,c(1,4,5)] %>%
  mutate(distance = (abs(hi_nalerts - base_nalerts) + abs(lo_nalerts - base_nalerts))/2) %>%
  gather("type", "value", 2:3)

NAlerts_OWSA$parameter <- reorder(NAlerts_OWSA$parameter,
                                  NAlerts_OWSA$distance,
                                  FUN = max)

NAlerts_OWSA <- NAlerts_OWSA %>%
  mutate(ymax = ifelse(value > base_nalerts, value, base_nalerts),
         ymin = ifelse(value < base_nalerts, value, base_nalerts))

width = 0.95

plot_NAlerts_OWSA<- ggplot() +
  geom_rect(data = NAlerts_OWSA,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(parameter) - width / 2,
                xmax = as.numeric(parameter) + width / 2,
                fill = type)) +
  theme_bw(base_size = 10) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_nalerts) +
  scale_y_continuous(name = "Number of Alerts PGx-CDS Alerts Versus No Alert") +
  scale_x_continuous(breaks = c(1:nrow(NAlerts_OWSA[NAlerts_OWSA$type == "lo_nalerts",])),
                     labels = levels(NAlerts_OWSA$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()

ggsave("OWSA_NAlerts.png",plot=plot_NAlerts_OWSA,
       dpi=600,width=10)

###############################################
### plot tornado diagram - total costs per alert
###############################################
TotalCostsPerAlert_OWSA <- OWSA_output[,c(1,6,7)] %>%
  mutate(distance = (abs(hi_totalcostsperalert - base_totalcostsperalert) + abs(lo_totalcostsperalert - base_totalcostsperalert))/2) %>%
  gather("type", "value", 2:3)

TotalCostsPerAlert_OWSA$parameter <- reorder(TotalCostsPerAlert_OWSA$parameter,
                                  TotalCostsPerAlert_OWSA$distance,
                                  FUN = max)

TotalCostsPerAlert_OWSA <- TotalCostsPerAlert_OWSA %>%
  mutate(ymax = ifelse(value > base_totalcostsperalert, value, base_totalcostsperalert),
         ymin = ifelse(value < base_totalcostsperalert, value, base_totalcostsperalert))

width = 0.95

plot_TotalCostsPerAlert_OWSA<- ggplot() +
  geom_rect(data = TotalCostsPerAlert_OWSA,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(parameter) - width / 2,
                xmax = as.numeric(parameter) + width / 2,
                fill = type)) +
  theme_bw(base_size = 10) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_totalcostsperalert) +
  scale_y_continuous(name = "Total Costs per Alert PGx-CDS Alerts Versus No Alert",
                     labels = dollar_format())+
  scale_x_continuous(breaks = c(1:nrow(TotalCostsPerAlert_OWSA[TotalCostsPerAlert_OWSA$type == "lo_totalcostsperalert",])),
                     labels = levels(TotalCostsPerAlert_OWSA$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()

ggsave("OWSA_TotalCostsPerAlert.png",plot=plot_TotalCostsPerAlert_OWSA,
       dpi=600,width=10)

###############################################
### plot tornado diagram - admin costs per alert
###############################################
AdminCostsPerAlert_OWSA <- OWSA_output[,c(1,8,9)] %>%
  mutate(distance = (abs(hi_admincostsperalert - base_admincostsperalert) + abs(lo_admincostsperalert - base_admincostsperalert))/2) %>%
  gather("type", "value", 2:3)

AdminCostsPerAlert_OWSA$parameter <- reorder(AdminCostsPerAlert_OWSA$parameter,
                                             AdminCostsPerAlert_OWSA$distance,
                                             FUN = max)

AdminCostsPerAlert_OWSA <- AdminCostsPerAlert_OWSA %>%
  mutate(ymax = ifelse(value > base_admincostsperalert, value, base_admincostsperalert),
         ymin = ifelse(value < base_admincostsperalert, value, base_admincostsperalert))

width = 0.95

plot_AdminCostsPerAlert_OWSA<- ggplot() +
  geom_rect(data = AdminCostsPerAlert_OWSA,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(parameter) - width / 2,
                xmax = as.numeric(parameter) + width / 2,
                fill = type)) +
  theme_bw(base_size = 10) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_admincostsperalert) +
  scale_y_continuous(name = "Admin Costs per Alert PGx-CDS Alerts Versus No Alert",
                     labels = dollar_format())+
  scale_x_continuous(breaks = c(1:nrow(AdminCostsPerAlert_OWSA[AdminCostsPerAlert_OWSA$type == "lo_admincostsperalert",])),
                     labels = levels(AdminCostsPerAlert_OWSA$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()

ggsave("OWSA_AdminCostsPerAlert.png",plot=plot_AdminCostsPerAlert_OWSA,
       dpi=600,width=10)

#####################################################
### plot tornado diagram - medical costs per alert
#####################################################
MedicalCostsPerAlert_OWSA <- OWSA_output[,c(1,10,11)] %>%
  mutate(distance = (abs(hi_medicalcostsperalert - base_medicalcostsperalert) + abs(lo_medicalcostsperalert - base_medicalcostsperalert))/2) %>%
  gather("type", "value", 2:3)

MedicalCostsPerAlert_OWSA$parameter <- reorder(MedicalCostsPerAlert_OWSA$parameter,
                                               MedicalCostsPerAlert_OWSA$distance,
                                             FUN = max)

MedicalCostsPerAlert_OWSA <- MedicalCostsPerAlert_OWSA %>%
  mutate(ymax = ifelse(value > base_medicalcostsperalert, value, base_medicalcostsperalert),
         ymin = ifelse(value < base_medicalcostsperalert, value, base_medicalcostsperalert))

width = 0.95

plot_MedicalCostsPerAlert_OWSA<- ggplot() +
  geom_rect(data = MedicalCostsPerAlert_OWSA,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(parameter) - width / 2,
                xmax = as.numeric(parameter) + width / 2,
                fill = type)) +
  theme_bw(base_size = 10) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_medicalcostsperalert) +
  scale_y_continuous(name = "Medical Costs per Alert PGx-CDS Alerts Versus No Alert",
                     labels = dollar_format())+
  scale_x_continuous(breaks = c(1:nrow(MedicalCostsPerAlert_OWSA[MedicalCostsPerAlert_OWSA$type == "lo_medicalcostsperalert",])),
                     labels = levels(MedicalCostsPerAlert_OWSA$parameter)) +
  scale_fill_discrete(labels = c("High value", "Low value")) +
  coord_flip()

ggsave("OWSA_MedicalCostsPerAlert.png",plot=plot_MedicalCostsPerAlert_OWSA,
       dpi=600,width=10)
