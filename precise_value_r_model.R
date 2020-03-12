# This program performs the cost-effectiveness analysis for alerting on pharmacogenomic interactions.
# It assumes that patients are proactively genotyped at age 50, and compares a scenario in which
# alerts are shown to prescribers when they attempt to order a drug with a pharmacogenomic interaction
# to a scenario in which the genomic data are available in patient data but do not pop up at the time
# of prescribing

# The three target drugs tested are clopidogrel, simvastatin, and warfarin.

# Model programmed by Nathaniel Hendrix (nhendrix@uw.edu)

# Set demographic parameters
start_age <- 50
time_horizon <- 10 #in years
n <- 100 #population size
p_b <- 0.33 #percentage of population Black
p_w <- 0.33 #percentage of population White
p_o <- 0.34 #percentage of population other race
p_clo_b <- 0.183 #prevalence of clopidogrel variant (poor metabolizers) among Black patients
p_clo_w <- 0.146 #prevalence of clopidogrel variant (poor metabolizers) among White patients
p_clo_o <- 0.290 #prevalence of clopidogrel variant (poor metabolizers)among other race patients
p_sim_b <- 0.039 #prevalence of simvastatin variant (poor or medium metabolizers) among Black patients
p_sim_w <- 0.311 #prevalence of simvastatin variant (poor or medium metabolizers) among White patients
p_sim_o <- 0.243 #prevalence of simvastatin variant (poor or medium metabolizers) among other race patients
p_war_b <- 0.1 #prevalence of warfarin variant among Black patients
p_war_w <- 0.1 #prevalence of warfarin variant among White patients
p_war_o <- 0.1 #prevalence of warfarin variant among other race patients
discount <- 0.03

# Set program costs
alert_pgm_cost <- 4000 #cost of setting up alert program (in dollars)
alert_mtn_cost <- 100 #annual alert maintenance cost (in dollars)

# Set program probabilities
# Refers to the probability of switching from target drug to alternative given alert or no alert
clo_alert_pr <- 0.9
clo_no_alert_pr <- 0.1
sim_alert_pr <- 0.9
sim_no_alert_pr <- 0.1
war_alert_pr <- 0.9
war_no_alert_pr <- 0.1

# Set incremental drug costs and QALYs
# These detail the incremental costs and QALYs of **switching** from target drug to alternative drug.
# Thus, e.g., of switching from clopidogrel to prasugrel.
# Note that warfarin involves switching to an alternative dosing schedule, not an alternative drug.
clo_inc_cost <- 1100
clo_inc_qaly <- 0.03
sim_inc_cost <- 200
sim_inc_qaly <- 0.02
war_inc_cost <- 300
war_inc_qaly <- 0.01

# Get US life tables
# To determine whether patient receives rx for target drug before death
setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/R model")
life_table <- read.csv("ssa_life_table_2016.csv")
names(life_table)[1] <- "age"
life_table_trim <- life_table[life_table$age >= start_age,]
life_table_trim$m_death_inv <- 1 - life_table_trim$m_death
life_table_trim$f_death_inv <- 1 - life_table_trim$f_death
m_death_cum <- numeric() # cumulative risk of death by age for men
for (i in 1:nrow(life_table_trim)) {
  m_death_cum <- append(m_death_cum,
                        1 - prod(life_table_trim[1:i,"m_death_inv"]))
}
f_death_cum <- numeric() # cumulative risk of death by age for women
for (i in 1:nrow(life_table_trim)) {
  f_death_cum <- append(f_death_cum,
                        1 - prod(life_table_trim[1:i,"f_death_inv"]))
}
life_table_trim$m_death_cum <- m_death_cum
life_table_trim$f_death_cum <- f_death_cum

# Set time to medications
# **NOTE** These aren't real values
clo_shape <- 2
clo_scale <- 20
clo_p <- ecdf(rweibull(10000, clo_shape, clo_scale))
clo_cum <- clo_p(seq(0,120-start_age,by=1))
sim_shape <- 3
sim_scale <- 21
sim_p <- ecdf(rweibull(10000, sim_shape, sim_scale))
sim_cum <- sim_p(seq(0,120-start_age,by=1))
war_shape <- 4
war_scale <- 22
war_p <- ecdf(rweibull(10000, war_shape, war_scale))
war_cum <- war_p(seq(0,120-start_age,by=1))

# Make population
# This function uses demographic info set above to create demographic profiles for your population.
# It determines gender, race and variant status for each person. Assumes 50/50 gender split.
make_pop <- function() {
  gender_num <- runif(n)
  race_num <- runif(n)
  clo_num <- runif(n)
  sim_num <- runif(n)
  war_num <- runif(n)
  clo_rx_num <- runif(n)
  sim_rx_num <- runif(n)
  war_rx_num <- runif(n)
  life_num <- runif(n)
  pop_out <- data.frame(id = seq(1:n),
                        gender = ifelse(gender_num < 0.5, "M", "F"),
                        race = ifelse(race_num < p_b, "B",
                                      ifelse(race_num > p_b + p_w, "O", "W")))
  pop_out$clo_var <- (pop_out$race == "B" & clo_num < p_clo_b | 
                        pop_out$race == "W" & clo_num < p_clo_w | 
                        pop_out$race == "O" & clo_num < p_clo_o)
  pop_out$sim_var <- (pop_out$race == "B" & sim_num < p_sim_b | 
                        pop_out$race == "W" & sim_num < p_sim_w | 
                        pop_out$race == "O" & sim_num < p_sim_o)
  pop_out$war_var <- (pop_out$race == "B" & war_num < p_war_b | 
                        pop_out$race == "W" & war_num < p_war_w | 
                        pop_out$race == "O" & war_num < p_war_o)
  pop_out$t_death <- mapply(function(w,x) ifelse(w == "M",
                                                 nrow(life_table_trim[life_table_trim$m_death_cum < x,]),
                                                 nrow(life_table_trim[life_table_trim$f_death_cum < x,])),
                            w = pop_out$gender,
                            x = life_num)
  pop_out$t_clo <- mapply(function(w,x) ifelse(w,
                                               length(clo_cum[clo_cum < x]),
                                               NA),
                          w = pop_out$clo_var,
                          x = clo_rx_num)
  pop_out$t_sim <- mapply(function(w,x) ifelse(w,
                                               length(sim_cum[sim_cum < x]),
                                               NA),
                          w = pop_out$sim_var,
                          x = sim_rx_num)
  pop_out$t_war <- mapply(function(w,x) ifelse(w,
                                               length(war_cum[war_cum < x]),
                                               NA),
                          w = pop_out$war_var,
                          x = war_num)
  return(pop_out)
}

# This function produces a chronological history of the screening program for the time horizon selected.
# It assumes that a new cohort of patients will be genotyped each year and added to the population 
# who are eligible for pharmacogenomic alerts.
make_hist <- function() {
  set.seed(98405)
  hist_out <- data.frame(year = seq(1,time_horizon),
                         living_pop = rep(0,time_horizon),
                         clo_alert_n = rep(0,time_horizon),
                         clo_alert_qaly = rep(0,time_horizon),
                         clo_alert_cost = rep(0,time_horizon),
                         clo_no_alert_qaly = rep(0,time_horizon),
                         clo_no_alert_cost = rep(0,time_horizon),
                         sim_alert_n = rep(0,time_horizon),
                         sim_alert_qaly = rep(0,time_horizon),
                         sim_alert_cost = rep(0,time_horizon),
                         sim_no_alert_qaly = rep(0,time_horizon),
                         sim_no_alert_cost = rep(0,time_horizon),
                         war_alert_n = rep(0,time_horizon),
                         war_alert_qaly = rep(0,time_horizon),
                         war_alert_cost = rep(0,time_horizon),
                         war_no_alert_qaly = rep(0,time_horizon),
                         war_no_alert_cost = rep(0,time_horizon))
  for(i in 1:time_horizon) {
    temp_pop <- make_pop()
    temp_hist <- data.frame(year = seq(1,time_horizon),
                            living_pop = rep(0,time_horizon),
                            clo_alert_n = rep(0,time_horizon),
                            clo_alert_qaly = rep(0,time_horizon),
                            clo_alert_cost = rep(0,time_horizon),
                            clo_no_alert_qaly = rep(0,time_horizon),
                            clo_no_alert_cost = rep(0,time_horizon),
                            sim_alert_n = rep(0,time_horizon),
                            sim_alert_qaly = rep(0,time_horizon),
                            sim_alert_cost = rep(0,time_horizon),
                            sim_no_alert_qaly = rep(0,time_horizon),
                            sim_no_alert_cost = rep(0,time_horizon),
                            war_alert_n = rep(0,time_horizon),
                            war_alert_qaly = rep(0,time_horizon),
                            war_alert_cost = rep(0,time_horizon),
                            war_no_alert_qaly = rep(0,time_horizon),
                            war_no_alert_cost = rep(0,time_horizon))
    for(j in i:time_horizon) {
      clo_rand <- runif(nrow(temp_pop))
      sim_rand <- runif(nrow(temp_pop))
      war_rand <- runif(nrow(temp_pop))
      temp_hist[j,"living_pop"] <- nrow(temp_pop[temp_pop$t_death > 0,])
      temp_hist[j,"clo_alert_n"] <- nrow(temp_pop[temp_pop$t_clo == 0 & !is.na(temp_pop$t_clo),])
      temp_hist[j,"clo_alert_qaly"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < clo_alert_pr,
                                                                   clo_inc_qaly,
                                                                   0),
                                              x = temp_pop$t_clo,
                                              y = clo_rand))
      temp_hist[j,"clo_alert_cost"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < clo_alert_pr,
                                                                       clo_inc_cost,
                                                                       0),
                                                  x = temp_pop$t_clo,
                                                  y = clo_rand))
      temp_hist[j,"clo_no_alert_qaly"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < clo_no_alert_pr,
                                                                       clo_inc_qaly,
                                                                       0),
                                                  x = temp_pop$t_clo,
                                                  y = clo_rand))
      temp_hist[j,"clo_no_alert_cost"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < clo_no_alert_pr,
                                                                       clo_inc_cost,
                                                                       0),
                                                  x = temp_pop$t_clo,
                                                  y = clo_rand))
      temp_hist[j,"sim_alert_n"] <- nrow(temp_pop[temp_pop$t_sim == 0 & !is.na(temp_pop$t_sim),])
      temp_hist[j,"sim_alert_qaly"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < sim_alert_pr,
                                                                       sim_inc_qaly,
                                                                       0),
                                                  x = temp_pop$t_sim,
                                                  y = sim_rand))
      temp_hist[j,"sim_alert_cost"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < sim_alert_pr,
                                                                       sim_inc_cost,
                                                                       0),
                                                  x = temp_pop$t_sim,
                                                  y = sim_rand))
      temp_hist[j,"sim_no_alert_qaly"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < sim_no_alert_pr,
                                                                          sim_inc_qaly,
                                                                          0),
                                                     x = temp_pop$t_sim,
                                                     y = sim_rand))
      temp_hist[j,"sim_no_alert_cost"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < sim_no_alert_pr,
                                                                          sim_inc_cost,
                                                                          0),
                                                     x = temp_pop$t_sim,
                                                     y = sim_rand))
      temp_hist[j,"war_alert_n"] <- nrow(temp_pop[temp_pop$t_war == 0 & !is.na(temp_pop$t_war),])
      temp_hist[j,"war_alert_qaly"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < war_alert_pr,
                                                                       war_inc_qaly,
                                                                       0),
                                                  x = temp_pop$t_war,
                                                  y = war_rand))
      temp_hist[j,"war_alert_cost"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < war_alert_pr,
                                                                       war_inc_cost,
                                                                       0),
                                                  x = temp_pop$t_war,
                                                  y = war_rand))
      temp_hist[j,"war_no_alert_qaly"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < war_no_alert_pr,
                                                                          war_inc_qaly,
                                                                          0),
                                                     x = temp_pop$t_war,
                                                     y = war_rand))
      temp_hist[j,"war_no_alert_cost"] <- sum(mapply(function(x,y) ifelse(x == 0 & !is.na(x) & y < war_no_alert_pr,
                                                                          war_inc_cost,
                                                                          0),
                                                     x = temp_pop$t_war,
                                                     y = war_rand))
      temp_pop$t_death <- temp_pop$t_death - 1
      temp_pop$t_clo <- temp_pop$t_clo - 1
      temp_pop$t_sim <- temp_pop$t_sim - 1
      temp_pop$t_war <- temp_pop$t_war - 1
    }
    hist_out <- hist_out + temp_hist
  }
  hist_out$year <- seq(1,time_horizon)
  hist_out$alert_cost <- alert_mtn_cost
  hist_out[1,"alert_cost"] <- alert_pgm_cost
  for(i in c(4:7,9:12,14:18)) {
    hist_out[,i] <- mapply(function(x,y) x * (1 / (1+discount)^y),
                           x = hist_out[,i],
                           y = hist_out$year)
  }
  return(hist_out)
}

example <- make_hist()
example
