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


##########################################
##Set input dataframe. Updated: 03/06/2021

OWSA_input<-data.frame(matrix(ncol=5,
                              nrow=12))
colnames(OWSA_input)<-c("Name",
                        "Parameter",
                        "Default",
                        "Low",
                        "High")
##1. Name. Updated: 03/06/2021
name = c("Probability of regimen change with an alert",
         "Probability of regimen change without an alert",
         "Risk of initiating clopidogrel for ACS",
         "Risk of initiating warfarin for AF",
         "Number of hours needed to develop the alerting system",
         "Hourly pay to health informaticians to develop the alerting system",
         "Proportion of initial development cost as annual maintenance cost",
         "Proportion of people on warfarin benefit from PGx",
         "QALY payoff from PGx testing for clopidogrel",
         "QALY payoff from PGx testing for warfarin",
         "Cost payoff from PGx testing for clopidogrel",
         "Cost payoff from PGx testing for warfarin")

OWSA_input$Name<-name

##2. Parameter. Updated: 03/06/2021
parameter<-c("p_change_alert",
            "p_change_no_alert",
            "rr_new_rx_clo",
            "rr_new_rx_war",
            "start_up_cost_work_hour",
            "start_up_cost_salary",
            "maint_cost_proportion",
            "p_eligible",
            "qaly_change_clo",
            "qaly_change_war",
            "cost_change_clo",
            "cost_change_war")

OWSA_input$Parameter<-parameter

##3. Default. Updated: 03/06/2021
default<-c(p_change_alert,
           p_change_no_alert,
           rr_new_rx_clo,
           rr_new_rx_war,
           start_up_cost_work_hour,
           start_up_cost_salary,
           maint_cost_proportion,
           p_eligible,
           qaly_change_clo,
           qaly_change_war,
           cost_change_clo,
           cost_change_war)

OWSA_input$Default<-default

##4. Low and High
#(1). Regimen change with an alert. Range: 0.20-0.5, Base: 0.25. 
p_change_alert_lo <- 0.20
p_change_alert_hi <- 0.5

#(2). Regimen change without an alert. Range: 0-0.15, Base: 0.1. 
p_change_no_alert_lo<-0
p_change_no_alert_hi<-0.15

#(3). risk of getting clopidogrel. Range: 0.5-1.5, Base: 1
rr_new_rx_clo_lo<-0.5
rr_new_rx_clo_hi<-1.5

#4. risk of getting warfarin Range: 0.5-1.5, Base: 1
rr_new_rx_war_lo<-0.5
rr_new_rx_war_hi<-1.5

#(5). start_up_cost_work_hour. Range: 50-500, Base: 200. Updated: 03/06/2021
start_up_cost_work_hour_lo <- 50
start_up_cost_work_hour_hi <- 500

#(6). start_up_cost_salary. Range: 50-150. Base: 100. Updated: 03/06/2021
start_up_cost_salary_lo <- 50
start_up_cost_salary_hi <- 150

#(7). maint_cost_proportion. Range: 0.1-0.3. Base: 0.2. 
maint_cost_proportion_lo<-0.1
maint_cost_proportion_hi<-0.3

#(8). Probability of benefiting from PGx for warfarin. Range: 0.50-0.75, Base: 0.67. 
p_eligible_lo<-0.50
p_eligible_hi<-0.75

#(9). QALY payoff from PGx testing for clopidogrel. Range: 0.10-0.25, Base: 0.179 
qaly_change_clo_lo<-0.10
qaly_change_clo_hi<-0.25

#(10). QALY payoff from PGx testing for warfarin. RANGE: 0.005-0.011, Base: 0.008
qaly_change_war_lo<-0.005
qaly_change_war_hi<-0.011

#(11). Cost payoff from PGx testing for clopidogrel. Range: 5000-10,000, Base: 7043. 
cost_change_clo_lo<-5000
cost_change_clo_hi<-10000

#(12). Cost payoff from PGx testing for warfarin. Range: -365, 35. Base: -165. 
cost_change_war_lo<--365
cost_change_war_hi<-35


low<-c(p_change_alert_lo,
       p_change_no_alert_lo,
       rr_new_rx_clo_lo,
       rr_new_rx_war_lo,
       start_up_cost_work_hour_lo,
       start_up_cost_salary_lo,
       maint_cost_proportion_lo,
       p_eligible_lo,
       qaly_change_clo_lo,
       qaly_change_war_lo,
       cost_change_clo_lo,
       cost_change_war_lo)

high<-c(p_change_alert_hi,
       p_change_no_alert_hi,
       rr_new_rx_clo_hi,
       rr_new_rx_war_hi,
       start_up_cost_work_hour_hi,
       start_up_cost_salary_hi,
       maint_cost_proportion_hi,
       p_eligible_hi,
       qaly_change_clo_hi,
       qaly_change_war_hi,
       cost_change_clo_hi,
       cost_change_war_hi)


OWSA_input$Low<-low
OWSA_input$High<-high
View(OWSA_input)



###
### get values from model for one-ways
###

# create output data frame. Updated: 03/06/2021
OWSA_output_loop <- data.frame(name,
                          parameter,
                     lo_icer = rep(0,12),
                     hi_icer = rep(0,12),
                     lo_nalerts=rep(0,12),
                     hi_nalerts=rep(0,12),
                     lo_totalcostsperalert=rep(0,12),
                     hi_totalcostsperalert=rep(0,12),
                     lo_admincostsperalert=rep(0,12),
                     hi_admincostsperalert=rep(0,12),
                     lo_medicalcostsperalert=rep(0,12),
                     hi_medicalcostsperalert=rep(0,12))

# source data input functions for creation of data frames for age distribution, testing patterns, and treatment patterns
source(here("src", "make_inputs.R"))

###########################################
##LOW: 
for (i in 1:12){
  temp=OWSA_input$Low[i] #take one variable's low value
  initial_values[i]=temp #change the initial value to be the low value
  
  #Get results:
  CEA_results_discounted_OWSA <- precise_value_OWSA(initial_default=initial_values)[[2]]
  
  #ICER: 
  OWSA_output_loop[i,3] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                                                                                                         (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)

  #Reset default values:
  initial_values[i]=OWSA_input$Default[i]
  p_change_alert=initial_values[1]
  p_change_no_alert=initial_values[2]
  rr_new_rx_clo=initial_values[3]
  rr_new_rx_war=initial_values[4]
  
  start_up_cost_work_hour=initial_values[5]
  start_up_cost_salary=initial_values[6]
  maint_cost_proportion=initial_values[7]
  
  p_eligible=initial_values[8]
  qaly_change_clo=initial_values[9]
  qaly_change_war=initial_values[10]
  cost_change_clo=initial_values[11]
  cost_change_war=initial_values[12]
}

View(OWSA_output_loop)
View(CEA_results_discounted_OWSA)
###########################################
##HIGH:
for (i in 1:12){
  temp=OWSA_input$High[i]
  initial_values[i]=temp
  CEA_results_discounted_OWSA <- precise_value_OWSA(initial_default=initial_values)[[2]]
  
  #ICER: 
  OWSA_output_loop[i,4] <- round((sum(CEA_results_discounted_OWSA$alert_cost_total) - sum(CEA_results_discounted_OWSA$no_alert_cost))/
                              (sum(CEA_results_discounted_OWSA$alert_qaly) - sum(CEA_results_discounted_OWSA$no_alert_qaly)),2)
  
  #Reset default values:
  initial_values[i]=OWSA_input$Default[i]
  p_change_alert=initial_values[1]
  p_change_no_alert=initial_values[2]
  rr_new_rx_clo=initial_values[3]
  rr_new_rx_war=initial_values[4]
  
  start_up_cost_work_hour=initial_values[5]
  start_up_cost_salary=initial_values[6]
  maint_cost_proportion=initial_values[7]
  
  p_eligible=initial_values[8]
  qaly_change_clo=initial_values[9]
  qaly_change_war=initial_values[10]
  cost_change_clo=initial_values[11]
  cost_change_war=initial_values[12]
}

View(OWSA_output_loop)

#######################################
### plot tornado diagram - ICER
#######################################

ICER_OWSA <- OWSA_output_loop[,c(1,3,4)] %>%
  mutate(distance = (abs(hi_icer - base_icer) + abs(lo_icer - base_icer))/2) %>%
  gather("type", "value", 2:3)

# View(ICER_OWSA)

ICER_OWSA$name <- reorder(ICER_OWSA$name,
                          ICER_OWSA$distance,
                          FUN = max)


ICER_OWSA <- ICER_OWSA %>%
  mutate(ymax = ifelse(value > base_icer, value, base_icer),
         ymin = ifelse(value < base_icer, value, base_icer))

#plot:
width = 0.95

plot_ICER_OWSA<- ggplot() +
  geom_rect(data = ICER_OWSA,
            aes(ymax = ymax,
                ymin = ymin,
                xmin = as.numeric(name) - width / 2,
                xmax = as.numeric(name) + width / 2,
                fill = type)) +
  theme_bw(base_size = 10) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = base_icer) +
  scale_y_continuous(name = "Incremental Cost-Effectiveness Ratio PGx-CDS Alerts Versus No Alert",
                     labels = dollar_format()) +
  scale_x_continuous(breaks = c(1:nrow(ICER_OWSA[ICER_OWSA$type == "lo_icer",])),
                     labels = levels(ICER_OWSA$name)) +
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
