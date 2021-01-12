#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(tidyverse)
source(here("src", "make_inputs.R"))

# Module UI function
precisevalueUI <- function(id, label = "model inputs") {
    # `NS(id)` returns a namespace function, which was save as `ns` and will
    # invoke later.
    ns <- NS(id)
    
    tagList(
        numericInput(ns("pop_size"), label = "Population size:", value = 500000), ##Joyce updated on 01/12/2021
        numericInput(ns("p_w"), label = "Percentage White:", value = 60),
        #numericInput(ns("p_l"), label = "Percentage Latinx:", value = 18),
        numericInput(ns("p_b"), label = "Percentage Black:", value = 13),
        numericInput(ns("p_a"), label = "Percentage Asian:", value = 6),
        sliderInput(ns("start_age"), label = "Starting Age of Screening:",
                    min = 0, max = 100, value = 55, step = 1),
        sliderInput(ns("test_rate"), label = "Probability of Testing for Patients in Age Range:",
                    min = 0, max = 1, value = 0.2, step = 0.1),  ##Joyce updated on 01/12/2021
        sliderInput(ns("screen_dur"), label = "Screening Duration:",
                    min = 1, max = 50, value = 10, step = 1), ##Joyce updated on 01/12/2021
        sliderInput(ns("t_horizon"), label = "Time Horizon:",
                    min = 5, max = 85, value = 20, step = 1),
        sliderInput(ns("startup_cost"), label = "Startup Cost:",
                    min = 100, max = 20000, value = 4000, step = 100),
        sliderInput(ns("maint_cost"), label = "Maintenance Cost:",
                    min = 0, max = 50, value = 20, step = 5)
    )
}


# Module server function
precisevalueServer <- function(id) {
    moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            
            ##Model Set-up. Joyce updated on 01/12/2021
            #discount
            discount <- 0.03
            
            ## Benefit pattern. Joyce updated on 01/12/2021
            # Prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer. 
            p_clo_a <- 0.5394 
            p_clo_b <- 0.3900
            p_clo_w <- 0.3818
            # Prevalence of CYP2C9, CYP4F2, VKORC1 variants. Updated: 09/22/2020: we don't model warfarin variant anymore. 
            p_war <- 1 #09/22: we don't model warfarin variant anymore. 
            # Probability being eligible to benefit from PGx. Updated: 09/22/2020. Identified from Kimmel 2013. 
            p_eligible <- 0.67 
            
            ##Provider behavior. Joyce updated on 01/12/2021
            # probability of regimen change with alert
            p_change_alert <- 0.25
            # probability of regimen change without alert
            p_change_no_alert <- 0.1
            
            ##Payoffs. Joyce updated on 01/12/2021
            # payoffs: qalys and costs of PGx. Identified from literature review. 
            qaly_change_clo <- 0.05
            cost_change_clo <- 1700
            qaly_change_war <- 0.008
            cost_change_war <- -150
            # payoffs: ADEs of PGx. Identified from literature review. - Clopidogrel 
            NonFatalMI_change_clo<--0.002 #PGx: risk reduction
            StentThrombosis_change_clo<--0.0011 #PGx: risk reduction
            NonFatalIntracranial_change_clo<-0.000005 #PGx: risk increase
            NonFatalExtracranial_change_clo<-0.0008 #PGx: risk increase
            CABGBleeding_change_clo<-0.000025 #PGx: risk increase
            MinorBleeding_change_clo<-0.00023  #PGx: risk increase
            CABGRevascularization_change_clo<--0.00015#PGx: risk reduction
            PCIRevascularization_change_clo<--0.0016 #PGx: risk reduction
            CVDeath_change_clo<--0.0016 #PGx: risk reduction
            NONCVDeath_change_clo<--0.00013 #PGx: risk reduction
            # payoffs: ADEs of PGx. Identified from literature review. - Warfarin 
            Bleeding_change_war<--0.007 #PGx: risk reduction
            Clot_change_war<--0.002 #PGx: risk reduction
            
            ##Risk of getting clopidogrel for ACS. Joyce updated on 01/12/2021
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
            rr_new_rx_clo<-1
            
            ##Risk of getting warfarin for AF. Joyce updated on 01/12/2021
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
            rr_new_rx_war<-1
            
            
            # The user's data, parsed into a data frame. Joyce updated on 01/12/2021
            dataframe <- reactive({
                p_clo <- p_clo_a*(input$p_a/100) + p_clo_b*(input$p_b/100) + 
                    p_clo_w*(input$p_w/100)  # population prevalence of clopidogrel variant
                p_war <- 1                                        # 09/22: we dont need variant prevalence
                ages <- make_age_pattern()
                test <- make_test_pattern(input$start_age, input$test_rate, 
                                          input$screen_dur, input$t_horizon)
                drug <- make_treat_prob() #Joyce updated on 01/12/2021
                
                # get population by year. Joyce updated on 01/12/2021
                n_age <- data.frame(ages = ages$ages)
                for(i in 1:t_horizon){
                    temp_col <- n * ages$p
                    n_age$temp_col <- temp_col
                    names(n_age)[ncol(n_age)] <- paste0("y", i)
                }
                
                # get probability of new clopidogrel rx by year. Joyce updated on 01/12/2021
                p_new_clo <- data.frame(ages = drug$ages)
                for(i in 1:t_horizon){
                    temp_col <- drug$c * rr_new_rx_clo
                    p_new_clo$temp_col <- temp_col
                    names(p_new_clo)[ncol(p_new_clo)] <- paste0("y", i)
                }
                
                # get probability of new warfarin rx by year.Joyce updated on 01/12/2021
                p_new_war <- data.frame(ages = drug$ages)
                for(i in 1:t_horizon){
                    temp_col <- drug$w * rr_new_rx_war
                    p_new_war$temp_col <- temp_col
                    names(p_new_war)[ncol(p_new_war)] <- paste0("y", i)
                }
                
                # calculate benefit of clopidogrel alert. Joyce updated on 01/12/2021
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
                
                
                # calculate benefit of warfarin alert. Joyce updated on 01/12/2021
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
                
                # add start-up and maintenance costs of alert program. Joyce updated on 01/12/2021
                outcomes$admin_alert_cost <- start_up_cost
                outcomes$admin_alert_cost[2:nrow(outcomes)] <- maint_cost
                
                # Discounting for costs and QALYs only, not ADEs. Joyce updated on 01/12/2021
                # The number of alerts: 2 for clopidogrel, 27 for warfarin. 
                # QALYs: 3, 4 for clopidogrel, 28, 29 for warfarin.
                # Costs: 5, 6 for clopidogrel, 30, 31 for warfarin. & 36 for admin alert costs. 
                CEAoutcomes_nodiscount <- outcomes[,c(1,2,27,3:6,28:31,36)] #select CEA outcomes (costs and QALYs)
                CEAoutcomes_discount   <- outcomes[,c(1,2,27,3:6,28:31,36)] #select CEA outcomes (costs and QALYs)
                CLO_ADE<-outcomes[,c(1,2,7:26)]          
                WAR_ADE<-outcomes[,c(1,27,32:35)]  
                
                #discount: CEAoutcomes_discount. Joyce updated on 01/12/2021
                for(i in 4:12) {
                    CEAoutcomes_discount[,i] <- mapply(function(x,y) x * (1 / (1 + discount)^(y - 1)),
                                                       x = CEAoutcomes_discount[,i],
                                                       y = CEAoutcomes_discount$year)}
                
                
                #calculate the total costs and QALYs. Joyce updated on 01/12/2021
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
                
                
                #ADE results. Joyce updated on 01/12/2021
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
                
                # Return results. Joyce updated on 01/12/2021
                results_list<-list(outcomes,
                                   CEA_results_discounted,
                                   CEA_results_undiscounted,
                                   ADE_results_clo,
                                   ADE_results_war)
                names(results_list)<-c("All","CEA_results_discounted","CEA_results_undiscounted","ADE_results_clo","ADE_results_war")
                return(results_list)
            })
            
            # Return the reactive that yields the data frame
            return(dataframe)
        }
    )    
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            precisevalueUI("model_inputs", "Model Inputs")
        ),
        mainPanel(
            dataTableOutput("table")
        )
    )
)

server <- function(input, output, session) {
    datafile <- precisevalueServer("model_inputs")

    
    output$table <- renderDataTable({
        datafile()
    })
}

shinyApp(ui, server)
