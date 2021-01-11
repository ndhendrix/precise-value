# Testing pattern on initial base case: 10% each year while age 55-59
make_test_pattern <- function(start_age, test_rate, screen_dur, t_horizon) {
  test_pattern_df <- data.frame(ages = seq(18, 100),
                   y1 = c(rep(0, start_age - 18),
                          rep(test_rate, screen_dur),
                          rep(0, 101 - (start_age + screen_dur))))
  
  for(i in 2:t_horizon) {
    temp_col <- c(0,test_pattern_df[1:82,ncol(test_pattern_df)]) + test_pattern_df$y1
    test_pattern_df$temp_col <- temp_col
    names(test_pattern_df)[ncol(test_pattern_df)] <- paste0("y", i)
  }
  
  test_pattern_df <- apply(test_pattern_df,
              c(1,2),
              function(x) min(x, 1))
  
  return(as_tibble(test_pattern_df))
  #write.csv(df, here("inputs", "test_pattern.csv"), row.names = FALSE)
  
  #Sys.sleep(0.0001)
}

# New treatment probability by age: 0.5% between ages 55 and 84
make_treat_prob <- function(p_new_rx) {
  new_rx_df <- data.frame(ages = seq(18, 100),
                   c = c(rep(0, 37),
                         rep(p_new_rx, 30),
                         rep(0, 16)),
                   s = c(rep(0, 37),
                         rep(p_new_rx, 30),
                         rep(0, 16)),
                   w = c(rep(0, 37),
                         rep(p_new_rx, 30),
                         rep(0, 16)))
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

