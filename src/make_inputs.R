# Testing pattern on initial base case.
make_test_pattern <- function(start_age, test_rate, screen_dur, t_horizon) {
  test_pattern_df <- data.frame(ages = seq(18, 100),
                   y1 = c(rep(0, start_age - 18),
                          rep(test_rate, screen_dur),
                          rep(0, 101 - (start_age + screen_dur))))
  
  for(i in 2:t_horizon) {
    temp_col <- c(0, test_pattern_df[1:82,ncol(test_pattern_df)]) + test_pattern_df$y1
    test_pattern_df$temp_col <- temp_col
    names(test_pattern_df)[ncol(test_pattern_df)] <- paste0("y", i)
  }
  
  test_pattern_df[,2:t_horizon+1] <- apply(test_pattern_df[,2:t_horizon+1],
                     c(1,2),
                     function(x) min(x, 1))
  
  return(test_pattern_df)
  #write.csv(df, here("inputs", "test_pattern.csv"), row.names = FALSE)
  
  #Sys.sleep(0.0001)
}

# New treatment probability by age: updated on 12/30 MS results
make_treat_prob <- function() {
  
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
  
  new_rx_df <- data.frame(ages = seq(18, 100),
                   c = c(rep(p_new_rx_clo_18_24, 7),
                         rep(p_new_rx_clo_25_34,10),
                         rep(p_new_rx_clo_35_44,10),
                         rep(p_new_rx_clo_45_49,5),
                         rep(p_new_rx_clo_50_54,5),
                         rep(p_new_rx_clo_55_59,5),
                         rep(p_new_rx_clo_60_64,5),
                         rep(p_new_rx_clo_65_69,5),
                         rep(p_new_rx_clo_70_74,5),
                         rep(p_new_rx_clo_75_79,5),
                         rep(p_new_rx_clo_80_84,5),
                         rep(p_new_rx_clo_85_100,16)),
                   s = c(rep(0, 37),
                         rep(0, 30),
                         rep(0, 16)),
                   w = c(rep(p_new_rx_war_18_24, 7),
                         rep(p_new_rx_war_25_34,10),
                         rep(p_new_rx_war_35_44,10),
                         rep(p_new_rx_war_45_49,5),
                         rep(p_new_rx_war_50_54,5),
                         rep(p_new_rx_war_55_59,5),
                         rep(p_new_rx_war_60_64,5),
                         rep(p_new_rx_war_65_69,5),
                         rep(p_new_rx_war_70_74,5),
                         rep(p_new_rx_war_75_79,5),
                         rep(p_new_rx_war_80_84,5),
                         rep(p_new_rx_war_85_100,16)))
  
  return(new_rx_df)
  #write.csv(df, here("inputs", "new_rx_pattern.csv"), row.names = FALSE)
}

# Age distribution
make_age_pattern <- function() {
  plan_age_df <- data.frame(ages = seq(18, 100),
                   p = c(rep(0.01608,2), #18-19
                         rep(0.01693,5), #20-24
                         rep(0.01854,5), #25-29
                         rep(0.01747,5), #30-34
                         rep(0.01708,5), #35-39
                         rep(0.01560,5), #40-44
                         rep(0.01621,5), #45-49
                         rep(0.01621,5), #50-54
                         rep(0.01686,5), #55-59
                         rep(0.01640,5), #60-64
                         rep(0.01383,5), #65-69
                         rep(0.01126,5), #70-74
                         rep(0.00745,5), #75-79
                         rep(0.00482,5), #80-84
                         rep(0.001467,16)))
  return(plan_age_df)
  #write.csv(df, here("inputs", "plan_age_pattern.csv"), row.names = FALSE)
}
