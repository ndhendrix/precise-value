# This program performs the cost-effectiveness analysis for alerting on pharmacogenomic interactions.

# The three target drugs tested are clopidogrel, simvastatin, and warfarin.

# Model programmed by Nathaniel Hendrix (nhendrix@uw.edu)
precise_value <- function(){
  p_clo <- p_clo_a*p_a + p_clo_b*p_b + p_clo_w*p_w  #population prevalence of clopidogrel variant
  p_war <- 1                                        #population prevalence of warfarin variant. 09/22: we dont need variant prevalence. 
  
  # read input documents
  #setwd(here())
  ages <- read.csv(here("inputs", "plan_age_pattern.csv"))
  test <- read.csv(here("inputs", "test_pattern.csv"))
  drug <- read.csv(here("inputs", "new_rx_pattern.csv"))
  
  # get population by year
  n_age <- data.frame(ages = ages$ages)
  for(i in 1:t_horizon){
    temp_col <- n * ages$p
    n_age$temp_col <- temp_col
    names(n_age)[ncol(n_age)] <- paste0("y", i)
  }
  
  # get probability of new clopidogrel rx by year
  p_new_clo <- data.frame(ages = drug$ages)
  for(i in 1:t_horizon){
    temp_col <- drug$c * rr_new_rx_clo
    p_new_clo$temp_col <- temp_col
    names(p_new_clo)[ncol(p_new_clo)] <- paste0("y", i)
  }
  
  # get probability of new simvastatin rx by year
  # p_new_sim <- data.frame(ages = drug$ages)
  # for(i in 1:t_horizon){
  #   temp_col <- drug$s
  #   p_new_sim$temp_col <- temp_col
  #   names(p_new_sim)[ncol(p_new_sim)] <- paste0("y", i)
  # }
  
  # get probability of new warfarin rx by year
  p_new_war <- data.frame(ages = drug$ages)
  for(i in 1:t_horizon){
    temp_col <- drug$w * rr_new_rx_war
    p_new_war$temp_col <- temp_col
    names(p_new_war)[ncol(p_new_war)] <- paste0("y", i)
  }
  
  # calculate benefit of clopidogrel alert
  n_test <-n_age[,-1] * test[,-1] #number tested 
  n_var <- n_test * p_clo #number tested positive 
  n_rx <- n_var * p_new_clo[,-1] #number get clopidogrel
  
  clo_outcomes <- data.frame(year = seq(1, t_horizon),
                             clo_n_alert = apply(n_rx, 
                                                 2,                   #operate on columns
                                                 function(x) sum(x))) #calculate the total. 
  #QALYs
  clo_outcomes$clo_noalert_q <- clo_outcomes$clo_n_alert * p_change_no_alert * qaly_change_clo
  clo_outcomes$clo_alert_q   <- clo_outcomes$clo_n_alert * p_change_alert * qaly_change_clo
  #Costs - drug and treatment costs, not having added in start-up or maintenance costs yet. 
  clo_outcomes$clo_noalert_c <- clo_outcomes$clo_n_alert * p_change_no_alert * cost_change_clo
  clo_outcomes$clo_alert_c   <- clo_outcomes$clo_n_alert * p_change_alert * cost_change_clo
  #Non fatal MI
  clo_outcomes$clo_noalert_NonfatalMI  <- clo_outcomes$clo_n_alert * p_change_no_alert * NonFatalMI_change_clo
  clo_outcomes$clo_alert_NonfatalMI    <- clo_outcomes$clo_n_alert * p_change_alert * NonFatalMI_change_clo
  #Stent thrombosis
  clo_outcomes$clo_noalert_StentThrombosis <- clo_outcomes$clo_n_alert * p_change_no_alert * StentThrombosis_change_clo
  clo_outcomes$clo_alert_StentThrombosis   <- clo_outcomes$clo_n_alert * p_change_alert * StentThrombosis_change_clo
  #Non fatal intracranial 
  clo_outcomes$clo_noalert_NonFatalIntracranial <- clo_outcomes$clo_n_alert * p_change_no_alert * NonFatalIntracranial_change_clo
  clo_outcomes$clo_alert_NonFatalIntracranial   <- clo_outcomes$clo_n_alert * p_change_alert * NonFatalIntracranial_change_clo
  #Non fatal extracranial
  clo_outcomes$clo_noalert_NonFatalExtracranial  <- clo_outcomes$clo_n_alert * p_change_no_alert * NonFatalExtracranial_change_clo
  clo_outcomes$clo_alert_NonFatalExtracranial    <- clo_outcomes$clo_n_alert * p_change_alert * NonFatalExtracranial_change_clo
  #CABC Bleeding
  clo_outcomes$clo_noalert_CABGBleeding   <- clo_outcomes$clo_n_alert * p_change_no_alert * CABGBleeding_change_clo
  clo_outcomes$clo_alert_CABGBleeding     <- clo_outcomes$clo_n_alert * p_change_alert * CABGBleeding_change_clo
  #Minor bleeding  
  clo_outcomes$clo_noalert_MinorBleeding  <- clo_outcomes$clo_n_alert * p_change_no_alert * MinorBleeding_change_clo
  clo_outcomes$clo_alert_MinorBleeding    <- clo_outcomes$clo_n_alert * p_change_alert * MinorBleeding_change_clo
  #CABG revascularization
  clo_outcomes$clo_noalert_CABGRevascularization <- clo_outcomes$clo_n_alert * p_change_no_alert * CABGRevascularization_change_clo
  clo_outcomes$clo_alert_CABGRevascularization <- clo_outcomes$clo_n_alert * p_change_alert * CABGRevascularization_change_clo
  #PCI revascularization
  clo_outcomes$clo_noalert_PCIRevascularization  <- clo_outcomes$clo_n_alert * p_change_no_alert * PCIRevascularization_change_clo
  clo_outcomes$clo_alert_PCIRevascularization  <- clo_outcomes$clo_n_alert * p_change_alert * PCIRevascularization_change_clo
  #CV death
  clo_outcomes$clo_noalert_CVDeath               <- clo_outcomes$clo_n_alert * p_change_no_alert * CVDeath_change_clo
  clo_outcomes$clo_alert_CVDeath               <- clo_outcomes$clo_n_alert * p_change_alert * CVDeath_change_clo
  #Non CV death
  clo_outcomes$clo_noalert_NONCVDeath            <- clo_outcomes$clo_n_alert * p_change_no_alert * NONCVDeath_change_clo
  clo_outcomes$clo_alert_NONCVDeath            <- clo_outcomes$clo_n_alert * p_change_alert * NONCVDeath_change_clo
  
  # # calculate benefit of simvastatin alert
  # n_test <- n_age * test #number tested by age / year
  # n_var <- n_test * p_sim #number tested positive for variant
  # n_rx <- n_var * p_new_sim
  # sim_outcomes <- data.frame(year = seq(1, t_horizon),
  #                            sim_n_alert = apply(n_rx[,2:ncol(n_rx)],
  #                                                2,
  #                                                function(x) sum(x)))
  # sim_outcomes$sim_alert_q <- sim_outcomes$sim_n_alert * p_change_alert * qaly_change_sim
  # sim_outcomes$sim_alert_c <- sim_outcomes$sim_n_alert * p_change_alert * cost_change_sim
  # sim_outcomes$sim_noalert_q <- sim_outcomes$sim_n_alert * p_change_no_alert * qaly_change_sim
  # sim_outcomes$sim_noalert_c <- sim_outcomes$sim_n_alert * p_change_no_alert * cost_change_sim
  
  # calculate benefit of warfarin alert
  n_test <- n_age[,-1] * test[,-1] #number tested by age / year
  n_var <- n_test * p_war * p_eligible #number people who are eligible to benefit
  n_rx <- n_var * p_new_war[,-1]
  
  war_outcomes <- data.frame(year = seq(1, t_horizon),
                             war_n_alert = apply(n_rx,
                                                 2,
                                                 function(x) sum(x)))
  #QALYs
  war_outcomes$war_noalert_q        <- war_outcomes$war_n_alert * p_change_no_alert * qaly_change_war
  war_outcomes$war_alert_q        <- war_outcomes$war_n_alert * p_change_alert * qaly_change_war
  #Costs
  war_outcomes$war_noalert_c        <- war_outcomes$war_n_alert * p_change_no_alert * cost_change_war
  war_outcomes$war_alert_c        <- war_outcomes$war_n_alert * p_change_alert * cost_change_war
  #Bleeding
  war_outcomes$war_noalert_bleeding <- war_outcomes$war_n_alert * p_change_no_alert * Bleeding_change_war
  war_outcomes$war_alert_bleeding <- war_outcomes$war_n_alert * p_change_alert * Bleeding_change_war
  #Clots
  war_outcomes$war_noalert_clot     <- war_outcomes$war_n_alert * p_change_no_alert * Clot_change_war
  war_outcomes$war_alert_clot     <- war_outcomes$war_n_alert * p_change_alert * Clot_change_war
  
  # combine drug-specific benefit calculations
  outcomes <- merge(clo_outcomes, war_outcomes, by = "year")
  
  # add start-up and maintenance costs of alert program
  outcomes$admin_alert_cost <- start_up_cost
  outcomes$admin_alert_cost[2:nrow(outcomes)] <- maint_cost
  
  # Discounting for costs and QALYs only, not ADEs. 
  # The number of alerts: 2 for clopidogrel, 27 for warfarin. 
  # QALYs: 3, 4 for clopidogrel, 28, 29 for warfarin.
  # Costs: 5, 6 for clopidogrel, 30, 31 for warfarin. & 36 for admin alert costs. 
  CEAoutcomes_nodiscount <- outcomes[,c(1,2,27,3:6,28:31,36)] #select CEA outcomes (costs and QALYs)
  CEAoutcomes_discount   <- outcomes[,c(1,2,27,3:6,28:31,36)] #select CEA outcomes (costs and QALYs)
  CLO_ADE<-outcomes[,c(1,2,7:26)]          
  WAR_ADE<-outcomes[,c(1,27,32:35)]  
  
  #discount: CEAoutcomes_discount
  for(i in 4:12) {
    CEAoutcomes_discount[,i] <- mapply(function(x,y) x * (1 / (1 + discount)^(y - 1)),
                                       x = CEAoutcomes_discount[,i],
                                       y = CEAoutcomes_discount$year)}
  
  #calculate the total costs and QALYs
  CEA_results_discounted<-data.frame(year = CEAoutcomes_discount$year,
                                     alert_n = rowSums(CEAoutcomes_discount[,c(2,3)]),
                                     no_alert_qaly = rowSums(CEAoutcomes_discount[,c(4,8)]),
                                     alert_qaly = rowSums(CEAoutcomes_discount[,c(5,9)]),
                                     no_alert_cost = rowSums(CEAoutcomes_discount[,c(6,10)]),
                                     alert_cost_total = rowSums(CEAoutcomes_discount[,c(7,11,12)]),
                                     alert_cost_medical=rowSums(CEAoutcomes_discount[,c(7,11)]),
                                     alert_cost_admin=CEAoutcomes_discount[,12])
  CEA_results_undiscounted<-data.frame(year = CEAoutcomes_nodiscount$year,
                                       alert_n = rowSums(CEAoutcomes_nodiscount[,c(2,3)]),
                                       no_alert_qaly = rowSums(CEAoutcomes_nodiscount[,c(4,8)]),
                                       alert_qaly = rowSums(CEAoutcomes_nodiscount[,c(5,9)]),
                                       no_alert_cost = rowSums(CEAoutcomes_nodiscount[,c(6,10)]),
                                       alert_cost_total = rowSums(CEAoutcomes_nodiscount[,c(7,11,12)]),
                                       alert_cost_medical=rowSums(CEAoutcomes_nodiscount[,c(7,11)]),
                                       alert_cost_admin=CEAoutcomes_nodiscount[,12])
  
  #ADE results
  ADE_results_clo<-data.frame(noalert_NonFatalMI=double(),
                              alert_NonFatalMI=double(),
                              noalert_StentThrombosis=double(),
                              alert_StentThrombosis=double(),
                              noalert_NonFatalIntracranial=double(),
                              alert_NonFatalIntracranial=double(),
                              noalert_NonFatalExtracranial=double(),
                              alert_NonFatalExtracranial=double(),
                              noalert_CABGBleeding=double(),
                              alert_CABGBleeding=double(),
                              noalert_MinorBleeding=double(),
                              alert_MinorBleeding=double(),
                              noalert_CABGRevascularization=double(),
                              alert_CABGRevascularization=double(),
                              noalert_PCIRevascularization=double(),
                              alert_PCIRevascularization=double(),
                              noalert_CVDeath=double(),
                              alert_CVDeath=double(),
                              noalert_NonCVDeath=double(),
                              alert_NonCVDeath=double())
  
  ADE_results_war<-data.frame(noalert_Bleeding=double(),
                              alert_Bleeding=double(),
                              noalert_Clot=double(),
                              alert_Clot=double())
  for (i in seq(1:20)){
    a<-i+2
    ADE_results_clo[1,i]<-sum(CLO_ADE[,a])}
  
  for (i in seq(1:4)){
    a<-i+2
    ADE_results_war[1,i]<-sum(WAR_ADE[,a])
  }
  
  
  results_list<-list(outcomes,
                     CEA_results_discounted,
                     CEA_results_undiscounted,
                     ADE_results_clo,
                     ADE_results_war)
  names(results_list)<-c("All","CEA_results_discounted","CEA_results_undiscounted","ADE_results_clo","ADE_results_war")
  return(results_list)
}
