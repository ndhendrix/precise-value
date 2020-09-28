# Set demographic parameters
n <- 250000  #population size

# racial demographics from overall US demographics
p_a <- 0.0603 #percentage of population Asian
p_b <- 0.1369 #percentage of population Black
p_w <- 0.8028 #percentage of population White

# calculated from race-specific prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer
p_clo_a <- 0.5394 
p_clo_b <- 0.3900
p_clo_w <- 0.3818 

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

# qalys and costs of changing pgx
qaly_change_clo <- 0.05
cost_change_clo <- 1700
# qaly_change_sim <- 0  # simvastatin no longer included
# cost_change_sim <- 0  # as of 6/22
qaly_change_war <- 0.008
cost_change_war <- -150

# time horizon (years)
t_horizon <- 20

# costs associated with alert
start_up_cost <- 4000
maint_cost <- 100

# testing and medication use patterns
screen_dur <- 5 # years of screening
p_new_rx <- 0.005 # annual probability of a new rx for one of the included drugs
test_rate <- 0.1 # annual probability of testing for patients in the selected age range
start_age <- 55 # age at start of screening