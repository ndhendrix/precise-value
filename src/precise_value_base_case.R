source(here("src", "precise_value_base_values.R"))
source(here("src", "precise_value_r_model.R"))
return_list <- precise_value()
outcomes <- return_list[[1]]
total <- return_list[[2]]

cat(paste0("ICER = ", round((sum(total$alert_cost) - sum(total$no_alert_cost))/(sum(total$alert_qaly) - sum(total$no_alert_qaly)),2),
           "\nCost per alert = ", round((sum(total$alert_cost) - sum(total$no_alert_cost))/sum(total$alert_n),2),
           "\nn alert = ",sum(total$alert_n),
           "\nadmin cost per alert = ", round(sum(outcomes$alert_cost)/sum(total$alert_n),2),
           "\nreg change cost per alert = ", round((sum(total$alert_cost) - sum(outcomes$alert_cost) - 
                                                      sum(total$no_alert_cost))/sum(total$alert_n),2)))
