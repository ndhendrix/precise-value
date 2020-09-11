# This program performs the cost-effectiveness analysis for alerting on pharmacogenomic interactions.

# Model programmed by Nathaniel Hendrix (nhendrix@uw.edu)
precise_value <- function(){
  p_clo <- p_clo_a*p_a + p_clo_b*p_b + p_clo_w*p_w  #population prevalence of clopidogrel variant
  p_war <- p_war_a*p_a + p_war_b*p_b + p_war_w*p_w  #population prevalence of warfarin variant
  
  # read input documents
  # setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/")
  ages <- read.csv("plan_age_pattern.csv")
  test <- read.csv("test_pattern.csv")
  drug <- read.csv("new_rx_pattern.csv")
  
  # get population by year
  n_age <- data.frame(ages = ages$ages)
  for(i in 1:t_horizon){
    temp_col <- n * ages$p
    n_age$temp_col <- temp_col
    names(n_age)[ncol(n_age)] <- paste0("y", i)
  }
  ##n_age: the total number of people given age (=the total pop size *age distribution prob)
  
  # get probability of new clopidogrel rx by year
  p_new_clo <- data.frame(ages = drug$ages)
  for(i in 1:t_horizon){
    temp_col <- drug$c
    p_new_clo$temp_col <- temp_col
    names(p_new_clo)[ncol(p_new_clo)] <- paste0("y", i)
  }
  ##p_new_clo: the probability of having clopidogrel given age (=prob in the drug input file)
  
  # get probability of new warfarin rx by year
  p_new_war <- data.frame(ages = drug$ages)
  for(i in 1:t_horizon){
    temp_col <- drug$w
    p_new_war$temp_col <- temp_col
    names(p_new_war)[ncol(p_new_war)] <- paste0("y", i)
  }
  ##p_new_war: the probability of having warfarin given age (=prob in the drug input file)
  
  # calculate benefit of clopidogrel alert
  ##step 1: the number of people being tested given age (=the number of people given age * the prob being tested given age)
  n_test <- n_age * test #number tested by age / year
  ##step 2: the number of people with CYP2C19 poor metabolizer variants (=step 1 * population-averaged variant distribution)
  n_var <- n_test * p_clo #number tested positive for variant
  ##step 3: the number of people getting clopidogrel (=step 2 * the prob of getting clopidogrel)
  n_rx <- n_var * p_new_clo
  
    clo_outcomes <- data.frame(year = seq(1, t_horizon),
                             clo_n_alert = apply(
                               n_rx[,2:ncol(n_rx)],
                                                 2, #2: margin=column
                                                 function(x) sum(x))
                             )
    
  clo_outcomes$clo_alert_q <- clo_outcomes$clo_n_alert * p_change_alert * qaly_change_clo
  clo_outcomes$clo_alert_c <- clo_outcomes$clo_n_alert * p_change_alert * cost_change_clo
  clo_outcomes$clo_noalert_q <- clo_outcomes$clo_n_alert * p_change_no_alert * qaly_change_clo
  clo_outcomes$clo_noalert_c <- clo_outcomes$clo_n_alert * p_change_no_alert * cost_change_clo
  
    # calculate benefit of warfarin alert
  n_test <- n_age * test #number tested by age / year
  n_var <- n_test * p_war #number tested positive for variant
  n_rx <- n_var * p_new_war
  war_outcomes <- data.frame(year = seq(1, t_horizon),
                             war_n_alert = apply(n_rx[,2:ncol(n_rx)],
                                                 2,
                                                 function(x) sum(x)))
  war_outcomes$war_alert_q <- war_outcomes$war_n_alert * p_change_alert * qaly_change_war
  war_outcomes$war_alert_c <- war_outcomes$war_n_alert * p_change_alert * cost_change_war
  war_outcomes$war_noalert_q <- war_outcomes$war_n_alert * p_change_no_alert * qaly_change_war
  war_outcomes$war_noalert_c <- war_outcomes$war_n_alert * p_change_no_alert * cost_change_war
  
  # combine drug-specific benefit calculations
  outcomes <- merge(clo_outcomes, war_outcomes, by = "year")
  
  # add costs of alert program
  outcomes$alert_cost <- start_up_cost
  outcomes$alert_cost[2:nrow(outcomes)] <- maint_cost
  
  # implement discounting
  for(i in c(3:6,8:11)) {
    outcomes[,i] <- mapply(function(x,y) x * (1 / (1 + discount)^(y - 1)),
                           x = outcomes[,i],
                           y = outcomes$year)
  }
  total <- data.frame(year = outcomes$year,
                       alert_n = rowSums(outcomes[,c(2,7)]),
                       alert_qaly = rowSums(outcomes[,c(3,8)]),
                       alert_cost = rowSums(outcomes[,c(4,9,12)]),
                       no_alert_qaly = rowSums(outcomes[,c(5,10)]),
                       no_alert_cost = rowSums(outcomes[,c(6,11)]))
   
  return(list(outcomes,total))
}