# Set demographic parameters

n <- 250000  #change.population size

# racial demographics from overall US demographics
p_a <- 0.0603 #percentage of population Asian
p_b <- 0.1369#percentage of population Black
p_w <- 0.8028#percentage of population White

# calculated from race-specific prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer
p_clo_a <- 0.5394 
p_clo_b <- 0.3900
p_clo_w <- 0.3818 

##CYP2C9 variant: poor metabolizer
p_2c9_a<-0.0217
p_2c9_b<-0.0053
p_2c9_w<-0.0141

##CYP4F2 variant: decreased function
p_4f2_a<-0.3128
p_4f2_b<-0.0758
p_4f2_w<-0.3188

##VKORC1 variant
p_vko_a<-0.5174
p_vko_b<-0.1159
p_vko_w<-0.5329

# CYP2C9, VKORC1, CYP4F2. assumes independence
p_war_a <- 1-(1-p_2c9_a)*(1-p_4f2_a)*(1-p_vko_a)
p_war_b <- 1-(1-p_2c9_b)*(1-p_4f2_b)*(1-p_vko_b)
p_war_w <- 1-(1-p_2c9_w)*(1-p_4f2_w)*(1-p_vko_w)


discount <- 0.03

# probability of plan modification based on alert / no alert
p_change_alert <- 0.9
p_change_no_alert <- 0.1

# qalys and costs of changing pgx
qaly_change_clo <- 0.05
cost_change_clo <- 1265
qaly_change_war <- 0.01
cost_change_war <- -50

# time horizon (years)
t_horizon <- 20

# costs associated with alert
start_up_cost <- 4000
maint_cost <- 100

# setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value")
# source("precise_value_r_model.R")
precise_value()
return_list <- precise_value()
outcomes <- return_list[[1]]
total <- return_list[[2]]

cat(paste0("ICER = ", round((sum(total$alert_cost) - sum(total$no_alert_cost))/(sum(total$alert_qaly) - sum(total$no_alert_qaly)),2),
           "\nCost per alert = ", round((sum(total$alert_cost) - sum(total$no_alert_cost))/sum(total$alert_n),2),
           "\nn alert = ",sum(total$alert_n),
           "\nadmin cost per alert = ", round(sum(outcomes$alert_cost)/sum(total$alert_n),2),
           "\nreg change cost per alert = ", round((sum(total$alert_cost) - sum(outcomes$alert_cost) - 
                                                      sum(total$no_alert_cost))/sum(total$alert_n),2)))
