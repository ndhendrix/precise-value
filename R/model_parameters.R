# Fixed model parameters
# Last updated: 01/12/21

#discount
discount <- 0.03

## Benefit pattern. Joyce updated on 01/12/2021
# Prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer. 
p_clo_a <- 0.5394 
p_clo_b <- 0.3900
p_clo_w <- 0.3818
# Prevalence of CYP2C9, CYP4F2, VKORC1 variants. Updated: 09/22/2020: we don't model warfarin variant anymore. 
p_war <- 1 #09/22: we don't model warfarin variant anymore. 
# Probability being eligible to benefit from PGx. Updated: 09/22/2020. Identified from Kimmel 2013. 
p_eligible <- 0.67 

##Provider behavior. Joyce updated on 01/12/2021
# probability of regimen change with alert
p_change_alert <- 0.25
# probability of regimen change without alert
p_change_no_alert <- 0.1

##Payoffs. Joyce updated on 01/12/2021
# payoffs: qalys and costs of PGx. Identified from literature review. 
qaly_change_clo <- 0.05/0.28 #Updated on 01/23/2021: account for variant prevalence for White. 
cost_change_clo <- 1972/0.28 #Updated on 01/23/2021: account for variant prevalence for White, adjust inflation by CPI. 
qaly_change_war <- 0.008
cost_change_war <- -165 #Updated on 01/23/2021: adjust inflation by CPI

# payoffs: ADEs of PGx. Identified from literature review. - Clopidogrel
# Updated on 01/23/2021. (1) not adjust 1-year risk, (2) account for variant prevalence in White
NonFatalMI_change_clo <- -0.008/0.28 #PGx: risk reduction
StentThrombosis_change_clo <- -0.0042/0.28 #PGx: risk reduction
NonFatalIntracranial_change_clo <- 0.0002/0.28 #PGx: risk increase
NonFatalExtracranial_change_clo <- 0.00032/0.28 #PGx: risk increase
CABGBleeding_change_clo <- 0.0001/0.28 #PGx: risk increase
MinorBleeding_change_clo <- 0.0011/0.28  #PGx: risk increase
CABGRevascularization_change_clo <- -0.0006/0.28 #PGx: risk reduction
PCIRevascularization_change_clo <- -0049/0.28 #PGx: risk reduction
CVDeath_change_clo <- -0.0065/0.28 #PGx: risk reduction
NONCVDeath_change_clo <- -0.0008/0.28 #PGx: risk reduction

# payoffs: ADEs of PGx. Identified from literature review. - Warfarin 
Bleeding_change_war <- -0.007 #PGx: risk reduction
Clot_change_war <- -0.002 #PGx: risk reduction

##Risk of getting clopidogrel for ACS. Joyce updated on 01/12/2021
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
rr_new_rx_clo <- 1

##Risk of getting warfarin for AF. Joyce updated on 01/12/2021
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
p_new_rx_war_85_100 <- 0.003816
rr_new_rx_war <- 1

