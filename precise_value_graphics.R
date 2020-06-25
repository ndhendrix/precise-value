# makes chart showing sensitivity to changes in population

library(ggplot2)
library(scales)

df <- data.frame(pop = c(10, 32, 100, 320, 
                      1000, 3200, 10000, 32000,
                      100000, 320000, 1000000, 3200000),
                 n_alert = c(0.07, 0.23, 0.71, 2.28,
                             7.14, 22.85, 71.39, 228.46,
                             713.94, 2284.62, 7139.43, 22846.16),
                 icer = c(8514126, 2669937, 863552, 279133, 98495,
                          40053, 21989, 16145, 14338.32,
                          13753.9, 13573.26, 13514.82),
                 admin_cost = c(76089.87, 23778.08, 7608.99, 2377.81,
                                760.90, 237.78, 76.09, 23.78,
                                7.61, 2.38, 0.76, 0.24),
                 regimen_cost = rep(120.73, 12))

# scale for use with secondary axis
df$icer <- df$icer * (22846.16/8514126)
df$admin_cost <- df$admin_cost * (22846.16/8514126)
df$regimen_cost <- df$regimen_cost * (22846.16/8514126)

# make plot
ggplot(df,
       aes(x = pop)) +
  geom_smooth(method = "loess",
              aes(y = n_alert,
                  color = "Number\nof alerts"),
              se = FALSE,
              size = 2) +
  geom_line(aes(y = icer,
                  color = "Incremental cost-\neffectiveness ratio (ICER)"),
              size = 2) +
  geom_smooth(aes(y = admin_cost,
                  color = "Administrative cost\nper alert"),
              method = "loess",
              span = 2,
              se = FALSE,
              size = 2) +
  geom_smooth(aes(y = regimen_cost,
                  color = "Regimen change cost\nper alert"),
              method = "loess",
              span = 2,
              se = FALSE,
              size = 2) +
  scale_y_continuous(name = "Number of alerts",
                     trans = "log10",
                     labels = comma,
                     expand = c(0,0),
                     sec.axis = sec_axis(~.*(8514126/22846.16),
                                         name = "US Dollars",
                                         labels = dollar)) +
  scale_x_continuous(name = "Population",
                     trans = "log10",
                     labels = comma,
                     expand = c(0,0)) +
  scale_color_manual(name = "",
                     breaks = c("Number\nof alerts",
                                "Incremental cost-\neffectiveness ratio (ICER)",
                                "Administrative cost\nper alert",
                                "Regimen change cost\nper alert"),
                     values = c("Number\nof alerts" = "black",
                                "Incremental cost-\neffectiveness ratio (ICER)" = "blue",
                                "Administrative cost\nper alert" = "red",
                                "Regimen change cost\nper alert" = "green")) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
