###
### Set base case parameters
###
#1-3. Unchanged.
#4. p_change_alert
p_change_alert_default<-p_change_alert
#5. p_change_no_alert
p_change_no_alert_default<-p_change_no_alert
#6. qaly_change_clo
qaly_change_clo_default<-qaly_change_clo
#7. qaly_change_war
qaly_change_war_default<-qaly_change_war
#8. cost_change_clo
cost_change_clo_default<-cost_change_clo
#9. cost_change_war
cost_change_war_default<-cost_change_war
#10. start_up_cost
start_up_cost_default<-start_up_cost
#11. maint_cost
maint_cost_default<-maint_cost
#12-14. Unchanged
#15. rr_new_rx_clo
rr_new_rx_clo_default<-rr_new_rx_clo
#16. rr_new_rx_war
rr_new_rx_war_default<-rr_new_rx_war
#17. p_eligible
p_eligible_default<-p_eligible

# set number of runs for psa
n_runs <- 1000

###
### load model 
###

source(here("src", "precise_value_r_model.R"))
source(here("src", "make_inputs.R"))

###
### run psa
###

# create output data frame
PSA_output <- data.frame(run = seq(1,n_runs),
                         p_a=rep(0,n_runs), #unchanged
                         p_b=rep(0,n_runs), #unchanged
                         p_w=rep(0,n_runs), #unchanged
                         p_change_alert=rep(0,n_runs),
                         p_change_no_alert=rep(0,n_runs),
                         qaly_change_clo=rep(0,n_runs),
                         qaly_change_war=rep(0,n_runs),
                         cost_change_clo=rep(0,n_runs),
                         cost_change_war=rep(0,n_runs),
                         start_up_cost=rep(0,n_runs),
                         maint_cost=rep(0,n_runs),
                         screen_dur=rep(0,n_runs), #unchanged
                         test_rate=rep(0,n_runs), #unchanged
                         start_age=rep(0,n_runs), #unchanged
                         rr_new_rx_clo=rep(0,n_runs),
                         rr_new_rx_war=rep(0,n_runs),
                         p_eligible=rep(0,n_runs),
                     inc_costs = rep(0,n_runs),
                     inc_qalys = rep(0, n_runs),
                     ICER=rep(0,n_runs))


# run psa
set.seed(123)
for(i in 1:n_runs) {
  
  #1. Asian - Removed from PSA. Date: 01/07/2021. 
  p_a <- 0.0603 #percentage of population Asian
  PSA_output[i,2]<-p_a
  
  #2. Black - Removed from PSA. Date: 01/07/2021. 
  p_b <- 0.1369 #percentage of population Black
  PSA_output[i,3]<-p_b
  
  #3. White - Removed from PSA. Date: 01/07/2021. 
  p_w <- 0.8028 #percentage of population White
  PSA_output[i,4]<-p_w
  
  # p_a <- rbeta(1,1.2523,19.5178) #Asian:  6%, range: 0-20%, Beta distribution
  # p_b <- rbeta(1,2.6249,16.5492) #Black: 14%, range:0-30%, Beta distribution
  # p_w <- 1-p_a-p_b               #White: 80%, range: 50-100%, Beta distribution

  #4. Probability of regimen change based on alert: beta distribution. Mean=0.25, CI: 0.1-0.5
  p_change_alert <- rbeta(1,4.25,12.76)
  PSA_output[i,5]<-p_change_alert
  
  #5. Probability of regimen change based on no alert: beta distribution. Mean=0.1, CI: 0 - 0.2 
  p_change_no_alert <- rbeta(1,3.36,30.22)
  PSA_output[i,6]<-p_change_no_alert
  
  #6. QALYs of PGx-clopidogrel: beta distribution. Mean=0.179, CI: 0.10-0.25. #Updated: 01/23/2021. 
  qaly_change_clo <- rbeta(1,17.79, 81.58)
  PSA_output[i,7]<-qaly_change_clo
  
  #7. QALYs of PGx-warfarin: beta distribution. Mean=0.008, CI: 0.005-0.011
  qaly_change_war <- rbeta(1,27.09,3359.35)
  PSA_output[i,8]<-qaly_change_war
  
  #8. Cost of PGx-clopidogrel: normal distribution. Mean=1972, CI: 1500-2500. #Updated: 01/23/2021. 
  cost_change_clo <- rnorm(1,1972,269.39)
  PSA_output[i,9]<-cost_change_clo
  
  #9. Cost of PGx-warfarin: normal distribution. Mean=-165, CI: -365, 35. #Updated: 01/23/2021. 
  cost_change_war <- rnorm(1, -165, 102.04)
  PSA_output[i,10]<-cost_change_war
  
  #10. Start-up cost: normal distribution. Mean=4000, CI: 2000-6000
  start_up_cost <- rnorm(1, 4000, 1020.41)
  PSA_output[i,11]<-start_up_cost
  
  #11. Annual maintenance cost: normal distribution. Mean=100, CI: 50-150
  maint_cost <- rnorm(1, 100, 25.51)
  PSA_output[i,12]<-maint_cost
  
  #12. years of screening: log-normal distribution. Mean=10, CI: 5-15. Removed from PSA. Date: 01/07/2021. 
  screen_dur <- 10
  PSA_output[i,13]<-screen_dur
  # screen_dur_current <- round(rlnorm(1, 2.30,0.28),0)
  
  #13. test rate: beta distribution. Mean=0.1, CI: 0.075-0.125. Removed from PSA. Date: 01/07/2021. 
  test_rate <- 0.2 
  PSA_output[i,14]<-test_rate
  # test_rate_current <- rbeta(1, 55.22, 496.97)
  
  #14. start age for testing: log normal distribution. Mean=55, CI: 50-60. Removed from PSA. Date: 01/07/2021. 
  start_age <- 55 # age at start of screening
  PSA_output[i,15]<-start_age
  # start_age_current <-round(rlnorm(1, 4, 0.05),0)
  
  #15. probability of getting clopidogrel. Added to PSA. Date: 01/07/2021.
  rr_new_rx_clo<-rlnorm(1, 0, 0.28)
  PSA_output[i,16]<-rr_new_rx_clo
  
  #16. probability of getting warfarin Added to PSA. Date: 01/07/2021.
  rr_new_rx_war<-rlnorm(1, 0, 0.28)
  PSA_output[i,17]<-rr_new_rx_war
  
  #17. probability of benefiting from PGx for warfarin
  p_eligible<-rbeta(1,8.44,4.15)
  PSA_output[i,18]<-p_eligible
  
  # recreate datasets with test and treat info
  make_test_pattern()
  make_treat_prob()
  
  # run model
  CEA_results_discounted_PSA <- precise_value()[[2]]
  PSA_output[i, "inc_costs"] <- sum(CEA_results_discounted_PSA$alert_cost_total) - sum(CEA_results_discounted_PSA$no_alert_cost)
  PSA_output[i, "inc_qalys"] <- sum(CEA_results_discounted_PSA$alert_qaly) - sum(CEA_results_discounted_PSA$no_alert_qaly)
  PSA_output[i, "ICER"] <- (sum(CEA_results_discounted_PSA$alert_cost_total) - sum(CEA_results_discounted_PSA$no_alert_cost))/(sum(CEA_results_discounted_PSA$alert_qaly) - sum(CEA_results_discounted_PSA$no_alert_qaly))
}

View(PSA_output)
###
### plot diagram
###
##1. PSA plot
PSA_ICER<-ggplot(PSA_output, aes(x = inc_qalys, y = inc_costs)) +
  geom_point() +
  #stat_ellipse(type = "t", linetype = 2) +
  xlab("Incremental QALYs") + 
  scale_y_continuous(name = "Incremental Costs",
                     labels = dollar_format()) +
  theme_bw(base_size = 10)

ggsave("PSA_ICER.png",plot=PSA_ICER,
       dpi=600,width=10)

##2. CEAC curve
WTP<-seq(50000,500000,10000)
length(WTP)

CEAC_data<-data.frame(matrix(NA, nrow=46,ncol=3))
CEAC_data[,1]<-WTP
colnames(CEAC_data)<-c("WTP","No Alerts","PGx-CDS Alerts")

for (i in 1:46){
  wtp_temp<-WTP[i]
  temp<-PSA_output %>% mutate(Strategy=ifelse(ICER>=wtp_temp,"No Alerts","Alerts"))
  CEAC_data[i,3]<-prop.table(table(temp$Strategy))["Alerts"]
  CEAC_data[i,2]<-1-CEAC_data[i,3]}

CEAC_plot<-ggplot(CEAC_data, aes(x = WTP, y = `PGx-CDS Alerts`)) +
  geom_point() +
  #geom_smooth(stat="smooth",method="loess")+
  theme_bw(base_size=10)+
  xlab("Wilingness to pay threshold")+
  ylab("Probability that PGx-CDS alert was cost-effective, %")+
  scale_x_continuous(labels = dollar_format())+
  scale_y_continuous(labels=scales::percent_format())

ggsave("CEAC_plot.png",plot=CEAC_plot,
       dpi=600,width=10)


###
### reset datasets with defaults
###

#1-3. Unchanged.
#4. p_change_alert
p_change_alert<-p_change_alert_default
#5. p_change_no_alert
p_change_no_alert<-p_change_no_alert_default
#6. qaly_change_clo
qaly_change_clo<-qaly_change_clo_default
#7. qaly_change_war
qaly_change_war<-qaly_change_war_default
#8. cost_change_clo
cost_change_clo<-cost_change_clo_default
#9. cost_change_war
cost_change_war<-cost_change_war_default
#10. start_up_cost
start_up_cost<-start_up_cost_default
#11. maint_cost
maint_cost<-maint_cost_default
#12-14. Unchanged
#15. rr_new_rx_clo
rr_new_rx_clo<-rr_new_rx_clo_default
#16. rr_new_rx_war
rr_new_rx_war<-rr_new_rx_war_default
#17. p_eligible
p_eligible<-p_eligible_default

# recreate datasets with test and treat info
make_test_pattern()
make_treat_prob()