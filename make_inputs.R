
# Testing pattern on initial base case: 10% each year while age 55-59
make_test_pattern <- function() {
  # setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/")
  df <- data.frame(ages = seq(18, 100),
                   y1 = c(rep(0, start_age_current - 18),
                          rep(test_rate_current, screen_dur_current),
                          rep(0, 101 - (start_age_current + screen_dur_current))))
  
  for(i in 2:t_horizon) {
    temp_col <- c(0,df[1:82,ncol(df)]) + df$y1
    df$temp_col <- temp_col
    names(df)[ncol(df)] <- paste0("y", i)
  }
  
  df <- apply(df,
              c(1,2),
              function(x) min(x, 1))
  
  write.csv(df, "test_pattern.csv", row.names = FALSE)
  
  Sys.sleep(0.0001)
}

# New treatment probability by age: 0.5% between ages 55 and 84
make_treat_prob <- function() {
  # setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/")
  df <- data.frame(ages = seq(18, 100),
                   c = c(rep(0, 37),
                         rep(p_new_rx_current, 30),
                         rep(0, 16)),
                   w = c(rep(0, 37),
                         rep(p_new_rx_current, 30),
                         rep(0, 16)))
  
  write.csv(df, "new_rx_pattern.csv", row.names = FALSE)
}

# Age distribution
make_age_pattern <- function() {
  # setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/precise-value/")
  df <- data.frame(ages = seq(18, 100),
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
  write.csv(df, "plan_age_pattern.csv", row.names = FALSE)
}
