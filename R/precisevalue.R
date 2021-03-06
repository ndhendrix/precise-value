# module for Shiny inputs

# Module UI function
precisevalueUI <- function(id, label = "model inputs") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    numericInput(ns("pop_size"), label = "Population size:", value = 250000),
    numericInput(ns("p_w"), label = "Percentage White:", value = 60),
    #numericInput(ns("p_l"), label = "Percentage Latinx:", value = 18),
    numericInput(ns("p_b"), label = "Percentage Black:", value = 13),
    numericInput(ns("p_l"), label = "Percentage Asian:", value = 6),
    sliderInput(ns("start_age"), label = "Starting Age of Screening:",
                min = 0, max = 100, value = 55, step = 1),
    sliderInput(ns("test_rate"), label = "Probability of Testing for Patients in Age Range:",
                min = 0, max = 1, value = 0.1, step = 0.1),
    sliderInput(ns("screen_dur"), label = "Screening Duration:",
                min = 1, max = 50, value = 5, step = 1),
    sliderInput(ns("t_horizon"), label = "Time Horizon:", # limit to 20
                min = 5, max = 20, value = 20, step = 1),
    sliderInput(ns("startup_cost"), label = "Startup Effort (Hours of IT Build Effort):",
                min = 5, max = 1000, value = 200, step = 5),
    sliderInput(ns("maint_cost"), label = "Maintenance Cost (Annual % of Startup Effort):",
                min = 0, max = 50, value = 20, step = 5)
  )
}

# Module server function
precisevalueServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # The user's data, parsed into a data frame. Joyce updated on 01/12/2021
    data <- reactive({
      p_clo <- p_clo_a*(input$p_a/100) + p_clo_b*(input$p_b/100) + 
        p_clo_w*(input$p_w/100)  # population prevalence of clopidogrel variant
      #p_war <- 1                                        # 09/22: we dont need variant prevalence
      # Population age distribution in probabilities. Notes added on: 01/23/2021
      ages <- make_age_pattern() 
      # Testing pattern in probabilities. All capped<= 100%.  Notes added on: 01/23/2021
      test <- make_test_pattern(input$start_age, input$test_rate, 
                                input$screen_dur, input$t_horizon)
      # Risk of getting a new drug. Notes added on: 01/23/2021
      drug <- make_treat_prob()
      
      # get population by year. Joyce updated on 01/12/2021
      n_age <- data.frame(ages = ages$ages)
      for(i in 1:input$t_horizon){
        temp_col <- input$pop_size * ages$p # total population size multiplied by probability in each age group
        n_age$temp_col <- temp_col
        names(n_age)[ncol(n_age)] <- paste0("y", i)
      }
      # Results: the number of people in each age group. 
      
      # get probability of new clopidogrel rx by year. Joyce updated on 01/12/2021
      p_new_clo <- data.frame(ages = drug$ages)
      for(i in 1:input$t_horizon){
        temp_col <- drug$c * rr_new_rx_clo # the probability of getting a drug * a RR parameter to allow for SA
        p_new_clo$temp_col <- temp_col
        names(p_new_clo)[ncol(p_new_clo)] <- paste0("y", i)
      }
      # Results: the probability of getting a new clopidogrel rx by age
      
      # get probability of new warfarin rx by year.Joyce updated on 01/12/2021
      p_new_war <- data.frame(ages = drug$ages)
      for(i in 1:input$t_horizon){
        temp_col <- drug$w * rr_new_rx_war # the probability of getting a drug * a RR parameter to allow for SA
        p_new_war$temp_col <- temp_col
        names(p_new_war)[ncol(p_new_war)] <- paste0("y", i)
      }
      # Results: the probability of getting a new warfarin rx by age.
      
      # STEP 1: CLOPIDOGREL: Notes added on: 01/23/2021
      # (1) get the final number of people who can benefit from alerts and PGx. 
      n_test <- n_age[,-1] * test[,-1] #number tested 
      n_var <- n_test * p_clo #number tested positive 
      n_rx <- n_var * p_new_clo[,-1] #number get clopidogrel
      
      # (2) add the total number of people who can benefit from alerts and PGx across time horizon
      clo_outcomes <- data.frame(year = seq(1, input$t_horizon),
                                 clo_n_alert = apply(n_rx, 
                                                     2,                   #operate on columns
                                                     function(x) sum(x))) #calculate the total. 
      
      # (3) calculate results for costs, qaly and ADE
      # All the results are based on this calculation. Notes added on: 01/23/2021
      # Payoff = the number of alerts fired * prob of changing treatment * payoff 
      
      # QALYs
      clo_outcomes$clo_noalert_q <- clo_outcomes$clo_n_alert * p_change_no_alert * qaly_change_clo
      clo_outcomes$clo_alert_q <- clo_outcomes$clo_n_alert * p_change_alert * qaly_change_clo
      #Costs - drug and treatment costs, not having added in start-up or maintenance costs yet. 
      clo_outcomes$clo_noalert_c <- clo_outcomes$clo_n_alert * p_change_no_alert * cost_change_clo
      clo_outcomes$clo_alert_c <- clo_outcomes$clo_n_alert * p_change_alert * cost_change_clo
      #Non fatal MI
      clo_outcomes$clo_noalert_NonfatalMI <- clo_outcomes$clo_n_alert * p_change_no_alert * NonFatalMI_change_clo
      clo_outcomes$clo_alert_NonfatalMI <- clo_outcomes$clo_n_alert * p_change_alert * NonFatalMI_change_clo
      #Stent thrombosis
      clo_outcomes$clo_noalert_StentThrombosis <- clo_outcomes$clo_n_alert * p_change_no_alert * StentThrombosis_change_clo
      clo_outcomes$clo_alert_StentThrombosis <- clo_outcomes$clo_n_alert * p_change_alert * StentThrombosis_change_clo
      #Non fatal intracranial 
      clo_outcomes$clo_noalert_NonFatalIntracranial <- clo_outcomes$clo_n_alert * p_change_no_alert * NonFatalIntracranial_change_clo
      clo_outcomes$clo_alert_NonFatalIntracranial <- clo_outcomes$clo_n_alert * p_change_alert * NonFatalIntracranial_change_clo
      #Non fatal extracranial
      clo_outcomes$clo_noalert_NonFatalExtracranial  <- clo_outcomes$clo_n_alert * p_change_no_alert * NonFatalExtracranial_change_clo
      clo_outcomes$clo_alert_NonFatalExtracranial <- clo_outcomes$clo_n_alert * p_change_alert * NonFatalExtracranial_change_clo
      #CABC Bleeding
      clo_outcomes$clo_noalert_CABGBleeding <- clo_outcomes$clo_n_alert * p_change_no_alert * CABGBleeding_change_clo
      clo_outcomes$clo_alert_CABGBleeding <- clo_outcomes$clo_n_alert * p_change_alert * CABGBleeding_change_clo
      #Minor bleeding  
      clo_outcomes$clo_noalert_MinorBleeding <- clo_outcomes$clo_n_alert * p_change_no_alert * MinorBleeding_change_clo
      clo_outcomes$clo_alert_MinorBleeding <- clo_outcomes$clo_n_alert * p_change_alert * MinorBleeding_change_clo
      #CABG revascularization
      clo_outcomes$clo_noalert_CABGRevascularization <- clo_outcomes$clo_n_alert * p_change_no_alert * CABGRevascularization_change_clo
      clo_outcomes$clo_alert_CABGRevascularization <- clo_outcomes$clo_n_alert * p_change_alert * CABGRevascularization_change_clo
      #PCI revascularization
      clo_outcomes$clo_noalert_PCIRevascularization <- clo_outcomes$clo_n_alert * p_change_no_alert * PCIRevascularization_change_clo
      clo_outcomes$clo_alert_PCIRevascularization <- clo_outcomes$clo_n_alert * p_change_alert * PCIRevascularization_change_clo
      #CV death
      clo_outcomes$clo_noalert_CVDeath <- clo_outcomes$clo_n_alert * p_change_no_alert * CVDeath_change_clo
      clo_outcomes$clo_alert_CVDeath <- clo_outcomes$clo_n_alert * p_change_alert * CVDeath_change_clo
      #Non CV death
      clo_outcomes$clo_noalert_NONCVDeath <- clo_outcomes$clo_n_alert * p_change_no_alert * NONCVDeath_change_clo
      clo_outcomes$clo_alert_NONCVDeath <- clo_outcomes$clo_n_alert * p_change_alert * NONCVDeath_change_clo
      
      
      # STEP 2: WARFARIN: Notes added on: 01/23/2021
      # (1) get the final number of people who can benefit from alerts and PGx
      
      # number tested by age = the number of people in each age * the probability of getting tested by age.
      n_test <- n_age[,-1] * test[,-1]
      # number tested positive by age = the number of tested by age * the probability of having a warfarin variant * the prob of eligible benefiting
      n_var <- n_test * p_war * p_eligible
      # number of benefit = the number of people test positive * the probability of getting a new prescription
      n_rx <- n_var * p_new_war[,-1]
      
      # (2) add the total number of people who can benefit from alerts and PGx across 20 years
      war_outcomes <- data.frame(year = seq(1, input$t_horizon),
                                 war_n_alert = apply(n_rx,
                                                     2,
                                                     function(x) sum(x)))
      
      # (3) calculate results for costs, qaly and ADE
      # All the results are based on this calculation. Notes added on: 01/23/2021
      # Payoff = the number of alerts fired * prob of changing treatment * payoff 
      #QALYs
      war_outcomes$war_noalert_q <- war_outcomes$war_n_alert * p_change_no_alert * qaly_change_war
      war_outcomes$war_alert_q <- war_outcomes$war_n_alert * p_change_alert * qaly_change_war
      #Costs
      war_outcomes$war_noalert_c <- war_outcomes$war_n_alert * p_change_no_alert * cost_change_war
      war_outcomes$war_alert_c <- war_outcomes$war_n_alert * p_change_alert * cost_change_war
      #Bleeding
      war_outcomes$war_noalert_bleeding <- war_outcomes$war_n_alert * p_change_no_alert * Bleeding_change_war
      war_outcomes$war_alert_bleeding <- war_outcomes$war_n_alert * p_change_alert * Bleeding_change_war
      #Clots
      war_outcomes$war_noalert_clot <- war_outcomes$war_n_alert * p_change_no_alert * Clot_change_war
      war_outcomes$war_alert_clot <- war_outcomes$war_n_alert * p_change_alert * Clot_change_war
      
      
      # STEP 3: Combine results from 2 drugs together
      outcomes <- merge(clo_outcomes, war_outcomes, by = "year")
      
      # STEP 4: Add start-up and maintenance costs
      outcomes$admin_alert_cost <- input$startup_cost*100
      outcomes$admin_alert_cost[2:nrow(outcomes)] <- input$startup_cost*100 * (input$maint_cost/100)
      
      # STEP 5: Discounting for costs and QALYs only, not ADEs. Notes added on: 01/23/2021
      # (1) Select relevant columns. 
      # The number of alerts: 2 for clopidogrel, 27 for warfarin. 
      # QALYs: 3, 4 for clopidogrel, 28, 29 for warfarin.
      # Costs: 5, 6 for clopidogrel, 30, 31 for warfarin. & 36 for admin alert costs. 
      CEAoutcomes_nodiscount <- outcomes[,c(1,2,27,3:6,28:31,36)] #select CEA outcomes (costs and QALYs)
      CEAoutcomes_discount   <- outcomes[,c(1,2,27,3:6,28:31,36)] #select CEA outcomes (costs and QALYs)
      CLO_ADE<-outcomes[,c(1,2,7:26)]          
      WAR_ADE<-outcomes[,c(1,27,32:35)]  
      
      # (2) Discount: CEAoutcomes_discount
      for(i in 4:12) {
        CEAoutcomes_discount[,i] <- mapply(function(x,y) x * (1 / (1 + discount)^(y - 1)),
                                           x = CEAoutcomes_discount[,i],
                                           y = CEAoutcomes_discount$year)}
      
      
      # (3) Calculate the total costs and QALYs
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
      
      
      # (4) ADE results
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
      
      # FINAL STEP: RETURN RESULTS
      results_list<-list(outcomes,
                         CEA_results_discounted,
                         CEA_results_undiscounted,
                         ADE_results_clo,
                         ADE_results_war)
      names(results_list)<-c("All","CEA_results_discounted","CEA_results_undiscounted","ADE_results_clo","ADE_results_war")
      #return(results_list$All)
      #n_alerts<-as.data.frame(sum(outcomes$clo_n_alert+outcomes$war_n_alert))
      list(
        n_alerts = sum(outcomes$clo_n_alert+outcomes$war_n_alert),
        total_cost_no_alert = sum(CEA_results_discounted$no_alert_cost),
        total_cost_alert = sum(CEA_results_discounted$alert_cost_total),
        qaly_no_alert = sum(CEA_results_discounted$no_alert_qaly),
        qaly_alert = sum(CEA_results_discounted$alert_qaly),
        table = outcomes
      )
    })
    
    # Return the reactive that yields the data frame
    return(data)
  }
  )    
}
