###
### Set base case parameters
###
#1-3. Unchanged: 3 races. 

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

#10. start_up_cost_work_hour. Updated: 03/06/2021
start_up_cost_work_hour_default<-start_up_cost_work_hour
#11. start_up_cost_salary. Updated: 03/06/2021
start_up_cost_salary_default<-start_up_cost_salary
#12. maint_cost_proportion. Updated: 03/06/2021
maint_cost_proportion_default<-maint_cost_proportion

#13-15. Unchanged: testing patterns (age range, duration, testing rate)

#16. rr_new_rx_clo
rr_new_rx_clo_default<-rr_new_rx_clo
#17. rr_new_rx_war
rr_new_rx_war_default<-rr_new_rx_war
#18. p_eligible
p_eligible_default<-p_eligible

# set number of runs for psa
n_runs <- 5000

###
### load model 
###

source(here("src", "precise_value_r_model.R"))
source(here("src", "make_inputs.R"))

###
### run psa
###

# create output data frame. Updated: 03/06/2021
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
                         start_up_cost_work_hour=rep(0,n_runs), #. Updated: 03/06/2021
                         start_up_cost_salary=rep(0,n_runs),    #. Updated: 03/06/2021
                         maint_cost_proportion=rep(0,n_runs),   #. Updated: 03/06/2021
                         screen_dur=rep(0,n_runs), #unchanged
                         test_rate=rep(0,n_runs), #unchanged
                         start_age=rep(0,n_runs), #unchanged
                         rr_new_rx_clo=rep(0,n_runs),
                         rr_new_rx_war=rep(0,n_runs),
                         p_eligible=rep(0,n_runs),
                         inc_costs = rep(0,n_runs),
                         inc_qalys = rep(0, n_runs),
                         ICER=rep(0,n_runs))
View(PSA_output)

# run psa. Updated: 03/06/2021
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

  #4. Probability of regimen change based on alert: beta distribution. Mean=0.25, CI: 0.20-0.5.Updated: 02/20/2021
  p_change_alert <- rbeta(1, 7.75,23.26)
  PSA_output[i,5]<-p_change_alert
  
  #5. Probability of regimen change based on no alert: beta distribution. Mean=0.1, CI: 0 - 0.15. Updated: 02/20/2021
  p_change_no_alert <- min(rbeta(1,6.05,54.42),p_change_alert-0.01)
  PSA_output[i,6]<-p_change_no_alert
  
  #6. QALYs of PGx-clopidogrel: beta distribution. Mean=0.179, CI: 0.10-0.25. #Updated: 01/23/2021. 
  qaly_change_clo <- rbeta(1,17.79, 81.58)
  PSA_output[i,7]<-qaly_change_clo
  
  #7. QALYs of PGx-warfarin: beta distribution. Mean=0.008, CI: 0.005-0.011
  qaly_change_war <- rbeta(1,27.09,3359.35)
  PSA_output[i,8]<-qaly_change_war
  
  #8. Cost of PGx-clopidogrel: normal distribution. Mean=7043, CI: 5000-10000. #Updated: 02/19/2021
  cost_change_clo <- rnorm(1,7043,1509)
  PSA_output[i,9]<-cost_change_clo
  
  #9. Cost of PGx-warfarin: normal distribution. Mean=-165, CI: -365, 35. #Updated: 01/23/2021. 
  cost_change_war <- rnorm(1, -165, 102.04)
  PSA_output[i,10]<-cost_change_war
  
  #10. start_up_cost_work_hour. log-normal distribution. Mean=200, CI: 50-500. #Updated: 03/06/2021
  start_up_cost_work_hour<-rlnorm(1, 5.30, 0.59)
  PSA_output[i,11]<-start_up_cost_work_hour
  
  #11. start_up_cost_salary. #Updated: 03/06/2021
  start_up_cost_salary<-rlnorm(1, 4.61, 0.28)
  PSA_output[i,12]<-start_up_cost_salary
  
  #12. maint_cost_proportion. beta distribution. Mean=0.2, CI: 0.1-0.3. #Updated: 03/06/2021
  maint_cost_proportion <- rbeta(1,12.09, 48.37)
  PSA_output[i,13]<-maint_cost_proportion
  
  #13. years of screening: log-normal distribution. Mean=10, CI: 5-15. Removed from PSA. Date: 01/07/2021. 
  screen_dur <- 10
  PSA_output[i,14]<-screen_dur
  # screen_dur_current <- round(rlnorm(1, 2.30,0.28),0)
  
  #14. test rate: beta distribution. Mean=0.1, CI: 0.075-0.125. Removed from PSA. Date: 01/07/2021. 
  test_rate <- 0.2 
  PSA_output[i,15]<-test_rate
  # test_rate_current <- rbeta(1, 55.22, 496.97)
  
  #15. start age for testing: log normal distribution. Mean=55, CI: 50-60. Removed from PSA. Date: 01/07/2021. 
  start_age <- 55 # age at start of screening
  PSA_output[i,16]<-start_age
  # start_age_current <-round(rlnorm(1, 4, 0.05),0)
  
  #16. probability of getting clopidogrel. Added to PSA. Date: 01/07/2021.
  rr_new_rx_clo<-rlnorm(1, 0, 0.28)
  PSA_output[i,17]<-rr_new_rx_clo
  
  #17. probability of getting warfarin Added to PSA. Date: 01/07/2021.
  rr_new_rx_war<-rlnorm(1, 0, 0.28)
  PSA_output[i,18]<-rr_new_rx_war
  
  #18. probability of benefiting from PGx for warfarin
  p_eligible<-rbeta(1,8.44,4.15)
  PSA_output[i,19]<-p_eligible
  
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
  geom_point(size=0.1) +
  #stat_ellipse(type = "t", linetype = 2) +
  xlab("Incremental QALYs") + 
  scale_y_continuous(name = "Incremental Costs, $",
                     labels = dollar_format(),
                     breaks=seq(0,10000000, 1000000),
                     expand = c(0, 0))+
  scale_x_continuous(breaks = seq(0,300,25),
                     expand = c(0, 0))+
  theme_bw(base_size = 10)+
  geom_abline(intercept=0,  #100,000 WTP
              slope=100000,
              color="red",
              linetype="dashed")+
  geom_abline(intercept=0,  #50,000 WTP
              slope=50000,
              color="blue",
              linetype="dashed")+
  theme(panel.grid.minor = element_blank())
  
  

ggsave("PSA_ICER.png",plot=PSA_ICER,
       dpi=600,width=10)

##2. CEAC curve
WTP<-seq(25000,500000,10000)
length(WTP)

CEAC_data<-data.frame(matrix(NA, nrow=length(WTP),ncol=3))
CEAC_data[,1]<-WTP
colnames(CEAC_data)<-c("WTP","No Alerts","PGx-CDS Alerts")

for (i in 1:length(WTP)){
  wtp_temp<-WTP[i]
  temp<-PSA_output %>% mutate(Strategy=ifelse(ICER>=wtp_temp,"No Alerts","Alerts"))
  CEAC_data[i,3]<-prop.table(table(temp$Strategy))["Alerts"]
  CEAC_data[i,2]<-1-CEAC_data[i,3]}

View(CEAC_data)

CEAC_data_long<-CEAC_data %>% 
  pivot_longer(!WTP,
               names_to = "strategy",
               values_to="percent")
View(CEAC_data_long)

CEAC_data_long_plot<-CEAC_data_long %>%
  filter(WTP<=300000)

CEAC_plot<-ggplot(CEAC_data_long_plot, aes(x = WTP,y=percent,group=strategy,color=strategy)) +
  geom_line()+
  theme_bw(base_size=10)+
  xlab("Wilingness to pay threshold. $ per QALY gained")+
  ylab("Probability that PGx-CDS alert was cost-effective, %")+
  scale_x_continuous(labels = dollar_format(),
                     breaks=seq(0,300000,50000),
                     expand = c(0, 0))+
  scale_y_continuous(labels=scales::percent_format(),
                     breaks=seq(0,1.1,0.2),
                     expand = c(0, 0))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        panel.background = element_blank(),
        panel.grid.minor = element_blank())
  


ggsave("CEAC_plot.png",plot=CEAC_plot,
       dpi=600,width=10)

