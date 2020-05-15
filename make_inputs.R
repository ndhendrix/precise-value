setwd("D:/ndhen/Dropbox/School/RA/2019/Cost-effectiveness model/R model")

# Testing pattern on initial base case: 10% each year while age 55-59

years <- 20

df <- data.frame(ages = seq(18, 100),
                 y1 = c(rep(0, 37),
                        rep(0.1, 5),
                        rep(0, 41)))

for(i in 2:years) {
  temp_col <- c(0,df[1:82,ncol(df)]) + df$y1
  df$temp_col <- temp_col
  names(df)[ncol(df)] <- paste0("y", i)
}

write.csv(df, "test_pattern.csv", row.names = FALSE)

# New treatment probability by age: 0.5% between ages 55 and 84
df <- data.frame(ages = seq(18, 100),
                 c = c(rep(0, 37),
                        rep(0.005, 30),
                        rep(0, 16)),
                 s = c(rep(0, 37),
                       rep(0.005, 30),
                       rep(0, 16)),
                 w = c(rep(0, 37),
                       rep(0.005, 30),
                       rep(0, 16)))

write.csv(df, "new_rx_pattern.csv", row.names = FALSE)

# Age distribution
df <- data.frame(ages = seq(18, 100),
                 p = c(rep(0.01678,2),
                       rep(0.01758,5),
                       rep(0.01883,5),
                       rep(0.01821,5),
                       rep(0.01707,5),
                       rep(0.01603,5),
                       rep(0.01584,5),
                       rep(0.01625,5),
                       rep(0.01702,5),
                       rep(0.01633,5),
                       rep(0.01408,5),
                       rep(0.01134,5),
                       rep(0.00768,5),
                       rep(0.0051,5),
                       rep(0.00309,5),
                       rep(0.00162,5),
                       rep(0.0005,5),
                       0.000077))
write.csv(df, "plan_age_pattern.csv", row.names = FALSE)
