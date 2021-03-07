# Set demographic parameters
n <- 500000  #population size. Updated: 500,000, Date: 01/07/2021

# racial demographics from overall US demographics
p_a <- 0.0603 #percentage of population Asian
p_b <- 0.1369 #percentage of population Black
p_w <- 0.8028 #percentage of population White

# Variant prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer. Confirmed: 01/23/2021. 
p_clo_a <- 0.5394
p_clo_b <- 0.3840
p_clo_w <- 0.3818

# # Variant prevalence ratio of CYP2C19 variants: White as reference. 01/23/2021
# rr_clo_aw<-p_clo_a/p_clo_w
# rr_clo_bw<-p_clo_b/p_clo_w
# rr_clo_ww<-p_clo_w/p_clo_w

# ##CYP2C9 variant: poor metabolizer
# p_2c9_a<-0.0217
# p_2c9_b<-0.0053
# p_2c9_w<-0.0141
# 
# ##CYP4F2 variant: decreased function
# p_4f2_a<-0.3128
# p_4f2_b<-0.0758
# p_4f2_w<-0.3188
# 
# ##VKORC1 variant
# p_vko_a<-0.5174
# p_vko_b<-0.1159
# p_vko_w<-0.5329
# 
# # CYP2C9, VKORC1, CYP4F2. assumes independence
# p_war_a <- 1-(1-p_2c9_a)*(1-p_4f2_a)*(1-p_vko_a)
# p_war_b <- 1-(1-p_2c9_b)*(1-p_4f2_b)*(1-p_vko_b)
# p_war_w <- 1-(1-p_2c9_w)*(1-p_4f2_w)*(1-p_vko_w)

# % of people on warfarin will be eligible to benefit from PGx. 
p_war <- 1 #09/22: we don't model warfarin variant anymore. 
p_eligible<-0.67 

discount <- 0.03

# probability of plan modification based on alert
p_change_alert <- 0.25

# probability of plan modification based on no alert
p_change_no_alert <- 0.1

# qalys and costs of pgx-cds
qaly_change_clo <- 0.179 #Updated on 01/23/2021: account for variant prevalence for White. 
cost_change_clo <- 7043 #Updated on 01/23/2021: account for variant prevalence for White, adjust inflation by CPI. 
# qaly_change_sim <- 0  # simvastatin no longer included
# cost_change_sim <- 0  # as of 6/22
qaly_change_war <- 0.008
cost_change_war <- -165 #Updated on 01/23/2021: adjust inflation by CPI

# ADE of PGx-CDS for clopidogrel (updated: 11/21/2020). #Updated on 01/23/2021. (1) not adjust 1-year risk, (2) account for variant prevalence in White.
NonFatalMI_change_clo<--0.0286 #PGx: risk reduction
StentThrombosis_change_clo<--0.015 #PGx: risk reduction
NonFatalIntracranial_change_clo<-0.0007 #PGx: risk increase
NonFatalExtracranial_change_clo<-0.0011 #PGx: risk increase
CABGBleeding_change_clo<-0.0004 #PGx: risk increase
MinorBleeding_change_clo<-0.0039  #PGx: risk increase
CABGRevascularization_change_clo<--0.0021#PGx: risk reduction
PCIRevascularization_change_clo<--0.0175 #PGx: risk reduction
CVDeath_change_clo<--0.0232 #PGx: risk reduction
NONCVDeath_change_clo<--0.0029 #PGx: risk reduction

# ADE of PGx-CDS for warfarin (updated: 11/21/2020)
Bleeding_change_war<--0.007 #PGx: risk reduction
Clot_change_war<--0.002 #PGx: risk reduction

# time horizon (years)
t_horizon <- 20

# start-up cost of alerting system: Updated: 03/06/2021
start_up_cost_work_hour<-200
start_up_cost_salary<-100
start_up_cost <- start_up_cost_work_hour*start_up_cost_salary

# maintenance cost of alerting system: Updated: 03/06/2021
maint_cost_proportion<-0.20
maint_cost <- maint_cost_proportion*start_up_cost

# testing and medication use patterns
screen_dur <- 10 # age duration
test_rate <- 0.2 # annual probability of PGx. Updated: 20%, Date: 01/07/2021.
start_age <- 55 # age at start of screening


# get a new drug by age groups (12/30 updates)
p_new_rx_clo_18_24 <- 0.000003
p_new_rx_clo_25_34 <- 0.000021
p_new_rx_clo_35_44 <- 0.000173
p_new_rx_clo_45_49 <- 0.000457
p_new_rx_clo_50_54 <- 0.000775
p_new_rx_clo_55_59 <- 0.001160
p_new_rx_clo_60_64 <- 0.001637
p_new_rx_clo_65_69 <- 0.002344
p_new_rx_clo_70_74 <- 0.003454
p_new_rx_clo_75_79 <- 0.004334
p_new_rx_clo_80_84 <- 0.004971
p_new_rx_clo_85_100<- 0.004706
rr_new_rx_clo<-1 #new. 01/07/2021

# get a new drug by age groups (12/30 updates)
p_new_rx_war_18_24 <- 0.000005
p_new_rx_war_25_34 <- 0.000012
p_new_rx_war_35_44 <- 0.000039
p_new_rx_war_45_49 <- 0.000094
p_new_rx_war_50_54 <- 0.000173
p_new_rx_war_55_59 <- 0.000333
p_new_rx_war_60_64 <- 0.000601
p_new_rx_war_65_69 <- 0.001184
p_new_rx_war_70_74 <- 0.002170
p_new_rx_war_75_79 <- 0.003191
p_new_rx_war_80_84 <- 0.004050
p_new_rx_war_85_100<- 0.003816
rr_new_rx_war<-1 #new. 01/07/2021

#used for one-way sensitivity analysis (2020/02/20 update)
initial_values=c(0.25,0.1,1,1,4000,100,0.67,0.179,0.008,7043,-165)
