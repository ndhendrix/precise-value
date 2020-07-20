# Set demographic parameters
n <- 320000  #population size

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
p_change_alert <- 0.9
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
start_up_cost <- 4000
maint_cost <- 100

# testing and medication use patterns
screen_dur <- 5 # years of screening
p_new_rx <- 0.005 # annual probability of a new rx for one of the included drugs
test_rate <- 0.1 # annual probability of testing for patients in the selected age range
start_age <- 55 # age at start of screening