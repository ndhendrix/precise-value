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


###########################################
# 1. Probability of regimen change with an alert
##1-1.  Probability of regimen change with an alert - lo
p_change_alert <- p_change_alert_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$name=="Probability of regimen change with an alert","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                  (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)

# ##(2) the number of alerts
# OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)
# 
# ##(3) total costs per alert
# OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)
# 
# ##(4) admin costs per alert
# OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)
# 
# ##(5) medical costs per alert
# OWSA_output[OWSA_output$parameter=="Probability of regimen change with an alert","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##1-2.  Probability of regimen change with an alert - hi
p_change_alert <- p_change_alert_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$name=="Probability of regimen change with an alert","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                  (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$name=="Probability of regimen change with an alert","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

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
OWSA_output[OWSA_output$name=="Probability of regimen change without an alert","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Probability of regimen change without an alert","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Risk of initiating clopidogrel for ACS","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Risk of initiating clopidogrel for ACS","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Risk of initiating warfarin for AF","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Risk of initiating warfarin for AF","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="One-time start-up cost for alerts","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="One-time start-up cost for alerts","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Annual maintenance cost for alerts","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Annual maintenance cost for alerts","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Proportion of people on warfarin benefit from PGx","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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
OWSA_output[OWSA_output$name=="Proportion of people on warfarin benefit from PGx","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
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

###########################################
###8. QALY payoff from PGx testing for clopidogrel
##8-1. QALY payoff from PGx testing for clopidogrel - lo
qaly_change_clo<-qaly_change_clo_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                        (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##8-2. QALY payoff from PGx testing for clopidogrel - hi
qaly_change_clo<-qaly_change_clo_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$name=="QALY payoff from PGx testing for clopidogrel","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                   (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for clopidogrel","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case p_change_alert value
qaly_change_clo<-qaly_change_clo_default

###########################################
###9. QALY payoff from PGx testing for warfarin
##9-1. QALY payoff from PGx testing for warfarin - lo
qaly_change_war<-qaly_change_war_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                     (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##9-2. QALY payoff from PGx testing for warfarin - hi
qaly_change_war<-qaly_change_war_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                     (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="QALY payoff from PGx testing for warfarin","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case p_change_alert value
qaly_change_war<-qaly_change_war_default

###########################################
###10.. Cost payoff from PGx testing for clopidogrel
##10-1. Cost payoff from PGx testing for clopidogrel- lo
qaly_change_war<-qaly_change_war_lo
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","lo_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                        (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","lo_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","lo_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","lo_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","lo_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)


##9-2. Cost payoff from PGx testing for clopidogrel - hi
qaly_change_war<-qaly_change_war_hi
CEA_results_discounted_OWSA <- precise_value()[[2]]

##(1) ICER
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","hi_icer"] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                        (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
##(2) the number of alerts
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","hi_nalerts"]<-sum(CEA_results_discounted_OWSA$alert_n)

##(3) total costs per alert
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","hi_totalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_total)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

##(4) admin costs per alert
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","hi_admincostsperalert"]<-sum(CEA_results_discounted_OWSA$alert_cost_admin)/sum(CEA_results_discounted_OWSA$alert_n)

##(5) medical costs per alert
OWSA_output[OWSA_output$parameter=="Cost payoff from PGx testing for clopidogrel","hi_medicalcostsperalert"]<-(sum(CEA_results_discounted_OWSA$alert_cost_medical)-sum(CEA_results_discounted_OWSA$no_alert_cost))/sum(CEA_results_discounted_OWSA$alert_n)

# reset data for base case p_change_alert value
qaly_change_war<-qaly_change_war_default