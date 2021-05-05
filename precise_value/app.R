
library(shiny)
library(shinydashboard)
library(knitr)
library(here)
library(tidyverse)
library(DT)
source(here("R", "make_inputs.R"))
source(here("R", "model_parameters.R"))

convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}

# Module UI function
precisevalueUI <- function(id, label = "model inputs") {
    # `NS(id)` returns a namespace function, which was save as `ns` and will
    # invoke later.
    ns <- NS(id)
    
    tagList(
        numericInput(ns("pop_size"), label = "Population size:", value = 500000), ##Joyce updated on 01/12/2021
        numericInput(ns("p_w"), label = "Percentage European Ancestry:", value = 60),
        #numericInput(ns("p_l"), label = "Percentage Latinx:", value = 18),
        numericInput(ns("p_b"), label = "Percentage African Ancestry:", value = 13),
        numericInput(ns("p_a"), label = "Percentage Asian Ancestry:", value = 6),
        sliderInput(ns("start_age"), label = "Starting Age of Screening:",
                    min = 0, max = 80, value = 55, step = 1),
        sliderInput(ns("test_rate"), label = "Percentage of Patients Tested Per Year (% of Population):",
                    min = 0, max = 100, value = 5, step = 1),  ##Joyce updated on 01/12/2021
        sliderInput(ns("screen_dur"), label = "Number of Years Population is Tested:",
                    min = 1, max = 25, value = 10, step = 1), ##Joyce updated on 01/12/2021
        sliderInput(ns("t_horizon"), label = "Pharmacogenomic Alert Duration (Years):",
                    min = 1, max = 25, value = 10, step = 1),
        sliderInput(ns("startup_cost"), label = "Startup Effort (Hours of IT Build Effort):",
                    min = 5, max = 500, value = 200, step = 5),
        sliderInput(ns("maint_cost"), label = "Maintenance Cost (Annual % of Startup Effort):",
                    min = 0, max = 40, value = 20, step = 5)
    )
}


# Module server function
precisevalueServer <- function(id) {
    moduleServer(id, function(input, output, session) {
            
            # The user's data, parsed into a data frame. Joyce updated on 01/12/2021
            data <- reactive({
                p_clo <- p_clo_a*(input$p_a/100) + p_clo_b*(input$p_b/100) + 
                    p_clo_w*(input$p_w/100) + 
                    p_clo_w*((100-(input$p_a + input$p_b + input$p_w))/100)  # population prevalence of clopidogrel variant
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
                #Death:   #Joyce updated: 04/10/2021.
                war_outcomes$war_noalert_death <- war_outcomes$war_n_alert * p_change_no_alert * Death_change_war
                war_outcomes$war_alert_death <- war_outcomes$war_n_alert * p_change_alert * Death_change_war
                
                # STEP 3: Combine results from 2 drugs together
                outcomes <- merge(clo_outcomes, war_outcomes, by = "year")
                
                # STEP 4: Add start-up and maintenance costs
                outcomes$admin_alert_cost <- input$startup_cost*100
                outcomes$admin_alert_cost[2:nrow(outcomes)] <- input$startup_cost*100 * (input$maint_cost/100)
                
                # STEP 5: Discounting for costs and QALYs only, not ADEs. Notes added on: 01/23/2021
                # (1) Select relevant columns. 
                # The number of alerts: 2 for clopidogrel, 27 for warfarin. 
                # QALYs: 3, 4 for clopidogrel, 28, 29 for warfarin.
                # Costs: 5, 6 for clopidogrel, 30, 31 for warfarin. & 38 for admin alert costs. 
                CEAoutcomes_nodiscount <- outcomes[,c(1,2,27,3:6,28:31,38)] #select CEA outcomes (costs and QALYs)
                CEAoutcomes_discount   <- outcomes[,c(1,2,27,3:6,28:31,38)] #select CEA outcomes (costs and QALYs)
                CLO_ADE<-outcomes[,c(1,2,7:26)]          
                WAR_ADE<-outcomes[,c(1,27,32:37)]  
                
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
                                            alert_Clot=double(),
                                            noalert_Death=double(),
                                            alert_Death=double())
                for (i in seq(1:20)){
                    a<-i+2
                    ADE_results_clo[1,i]<-sum(CLO_ADE[,a])}
                
                for (i in seq(1:6)){
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
                ADE_results_clo_noalert <- ADE_results_clo %>% 
                    select(starts_with("noalert")) %>%
                    mutate(total = rowSums(.))
                ADE_results_clo_alert <- ADE_results_clo %>% 
                    select(starts_with("alert")) %>%
                    mutate(total = rowSums(.))
                clo_ae <- c("CV deaths", "Non-fatal MIs", "Stent Thromboses", 
                            "Non-fatal intracranial bleeding", "Non-fatal extracranial bleeding", 
                            "CABG bleeding", "CABG revascularizations",
                            "PCI revascularizations")
                clo_no_alert_freq <- c(sum(ADE_results_clo$noalert_CVDeath),
                                       sum(ADE_results_clo$noalert_NonFatalMI), 
                                       sum(ADE_results_clo$noalert_StentThrombosis), 
                                       sum(ADE_results_clo$noalert_NonFatalIntracranial),
                                       sum(ADE_results_clo$noalert_NonFatalExtracranial),
                                       sum(ADE_results_clo$noalert_CABGBleeding),
                                       sum(ADE_results_clo$noalert_CABGRevascularization),
                                       sum(ADE_results_clo$noalert_PCIRevascularization)
                                       )
                clo_alert_freq <- c(sum(ADE_results_clo$alert_CVDeath),
                                    sum(ADE_results_clo$alert_NonFatalMI), 
                                    sum(ADE_results_clo$alert_StentThrombosis), 
                                    sum(ADE_results_clo$alert_NonFatalIntracranial),
                                    sum(ADE_results_clo$alert_NonFatalExtracranial),
                                    sum(ADE_results_clo$alert_CABGBleeding),
                                    sum(ADE_results_clo$alert_CABGRevascularization),
                                    sum(ADE_results_clo$alert_PCIRevascularization)
                )
                clo_ae_table <- tibble(clo_ae, clo_no_alert_freq, clo_alert_freq)
                colnames(clo_ae_table) <- c("Adverse event", "Events without alerts", "Events with alerts")
                
                war_ae <- c("Deaths", "Clots", "Bleeds")
                war_no_alert_freq <- c(sum(ADE_results_war$noalert_Death),
                                       sum(ADE_results_war$noalert_Clot),
                                       sum(ADE_results_war$noalert_Bleeding))
                war_alert_freq <- c(sum(ADE_results_war$alert_Death),
                                    sum(ADE_results_war$alert_Clot),
                                    sum(ADE_results_war$alert_Bleeding))
                war_ae_table <- tibble(war_ae, war_no_alert_freq, war_alert_freq)
                colnames(war_ae_table) <- c("Adverse event", "Events without alerts", "Events with alerts")

                list(
                    n_alerts = sum(outcomes$clo_n_alert+outcomes$war_n_alert),
                    n_clo_alerts = sum(outcomes$clo_n_alert),
                    n_war_alerts = sum(outcomes$war_n_alert),
                    total_cost_no_alert = sum(CEA_results_discounted$no_alert_cost),
                    total_cost_alert = sum(CEA_results_discounted$alert_cost_total),
                    medical_cost_alert = sum(CEA_results_discounted$alert_cost_medical),
                    admin_cost_alert = sum(CEA_results_discounted$alert_cost_admin),
                    clo_non_fatal_mi_no_alert = sum(ADE_results_clo$noalert_NonFatalMI),
                    clo_non_fatal_mi_alert = sum(ADE_results_clo$alert_NonFatalMI),
                    clo_stent_thrombosis_no_alert = sum(ADE_results_clo$noalert_StentThrombosis),
                    clo_stent_thrombosis_alert = sum(ADE_results_clo$alert_StentThrombosis),
                    clo_cabg_revasc_no_alert = sum(ADE_results_clo$noalert_CABGRevascularization),
                    clo_cabg_revasc_alert = sum(ADE_results_clo$lert_CABGRevascularization),
                    clo_pci_revasc_no_alert = sum(ADE_results_clo$noalert_PCIRevascularization),
                    clo_pci_revasc_alert = sum(ADE_results_clo$alert_PCIRevascularization),
                    clo_non_fatal_ic_bleed_no_alert = sum(ADE_results_clo$noalert_NonFatalIntracranial),
                    clo_non_fatal_ic_bleed_alert = sum(ADE_results_clo$alert_NonFatalIntracranial),
                    clo_non_fatal_ec_bleed_no_alert = sum(ADE_results_clo$noalert_NonFatalExtracranial),
                    clo_non_fatal_ec_bleed_alert = sum(ADE_results_clo$alert_NonFatalExtracranial),
                    clo_cabg_bleed_no_alert = sum(ADE_results_clo$noalert_CABGBleeding),
                    clo_cabg_bleed_alert = sum(ADE_results_clo$alert_CABGBleeding),
                    war_bleed_no_alert = sum(ADE_results_war$noalert_Bleeding),
                    war_bleed_alert = sum(ADE_results_war$alert_Bleeding),
                    war_clot_no_alert = sum(ADE_results_war$noalert_Clot),
                    war_clot_alert = sum(ADE_results_war$alert_Clot),
                    war_death_no_alert = sum(ADE_results_war$noalert_Death),
                    war_death_alert = sum(ADE_results_war$alert_Death),
                    n_clo_no_alert_ade = ADE_results_clo_noalert$total,
                    n_clo_alert_ade = ADE_results_clo_alert$total,
                    qaly_no_alert = sum(CEA_results_discounted$no_alert_qaly),
                    qaly_alert = sum(CEA_results_discounted$alert_qaly),
                    inc_qaly_disc = round(sum(CEA_results_discounted$alert_qaly) - 
                                              sum(CEA_results_discounted$no_alert_qaly), 1),
                    inc_cost_disc = round(sum(CEA_results_discounted$alert_cost_total) - 
                                              sum(CEA_results_discounted$no_alert_cost), 0),
                    icer_discounted = round((sum(CEA_results_discounted$alert_cost_total) - 
                                           sum(CEA_results_discounted$no_alert_cost)) /
                        (sum(CEA_results_discounted$alert_qaly) - 
                             sum(CEA_results_discounted$no_alert_qaly)), 0),
                    table = outcomes,
                    clo_ae_table = clo_ae_table,
                    war_ae_table = war_ae_table
                )
            })

            # Return the reactive that yields the data frame
            return(data)
        }
    )    
}

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "PRECISE Value",
                    titleWidth = 350),
    dashboardSidebar(
        width = 350,
        tags$head(
            tags$style(HTML("
                      .sidebar { height: 95vh; overflow-y: auto; }
                      " )
            )
        ),
        sidebarMenu(
            id = "sbMenu",
            menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
            menuItem("Background information", tabName = "info", icon = icon("table")),
            menuItem("Variable details", tabName = "interp", icon = icon("table")),
            menuItem("Clinical event details", tabName = "ades", icon = icon("table")),
            menuItem("Economic value details", tabName = "value", icon = icon("table")),
            menuItem("Data Selection", tabName = "ds", startExpanded = TRUE,
                     precisevalueUI("model_inputs", "Model Inputs")))
        ),
    dashboardBody(
        tags$head( 
            tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size to 20
        ),
        tabItems(
            tabItem(tabName = "summary",
                    box(title = "Welcome to PRECISE-Value!",
                        solidHeader = F,
                        collapsible = F,
                        width = 12,
                        fluidRow(column(width = 8, textOutput("welcome_text")),
                                 column(width = 4, align = "center",
                                        img(src="DLMP_logo.jpg", width=200)))),
                    h2(
                        fluidRow(
                            valueBoxOutput("n_alerts"),
                            valueBoxOutput("n_clo_alerts"),
                            valueBoxOutput("n_war_alerts")
                        ),
                        fluidRow(
                            box(title = "Yearly Clopidogrel Alerts",
                                plotOutput("clo_alerts_by_year")),
                            box(title = "Yearly Warfarin Alerts",
                                plotOutput("war_alerts_by_year"))
                        ),
                        fluidRow(
                            valueBoxOutput("alert_decreased_clo_deaths"),
                            valueBoxOutput("alert_decreased_clo_acs_events"),
                            valueBoxOutput("alert_clo_bleeding_events")
                            
                        ),
                        fluidRow(
                            valueBoxOutput("alert_decreased_war_deaths"),
                            valueBoxOutput("alert_decreased_war_clots"),
                            valueBoxOutput("alert_war_bleeding_events")
                        ),
                        fluidRow(
                            valueBoxOutput("change_medical_cost"),
                            valueBoxOutput("admin_cost_alert")
                        )
                    )
                    ),
            tabItem(tabName = "info",
                    fluidPage(
                        fluidRow(
                            box(
                                includeMarkdown(here("R", "explanation_markdown.Rmd"))
                            )
                        )
                    )
                    # tags$ul(
                    #     h2("Primer on Economic Evaluation"),
                    #     
                    #     h3("Economic evaluation"),
                    #     tags$li("Economic evaluation uses a mathematical model to compare two or more alternative courses of action in terms of both their costs and effectiveness. "),
                    #     
                    #     h3("Value"),
                    #     tags$li("Trade-off between costs and effectiveness (health outcomes) when comparing multiple health interventions"),
                    #     
                    #     h3("Measures of effectiveness (health outcomes)"),
                    #     tags$li("Quality-adjusted life years (QALYs) that combines (1) quantity of life (survival), and (2)quality of life."),
                    #     tags$li("The number of adverse events."),
                    #     tags$li("Others possible."),
                    #     
                    #     h3("Results of economic evaluation"),
                    #     tags$li("Costs"),
                    #     tags$li("Total costs of each intervention"),
                    #     h5("Incremental costs comparing two interventions, i.e. Cost of intervetnion A - Cost of intervention B"),
                    #     h4("Effectiveness"),
                    #     h5("Total QALYs of each intervention"),
                    #     h5("Incremental effectiveness comparing two interventions, i.e. QALYs of intervention A - QALYs of intervention B"),
                    #     h4("Incremental costs and effectiveness ratio (ICER)=Incremental Costs / Incremental QALYs"),
                    #     
                    #     h3("Decision made after economic evaluation"),
                    #     h4("What is your willingness to pay (WTP) for one unit increase in effectiveness?"),
                    #     h5("Note that US WTP is between $50,000 and $100,000 per QALY gained"),
                    #     h4("If your estimated ICER is below your chosen WTP, consider implementing the tested intervention"),
                    #     h4("If your estimated ICER is above your chosen WTP, consider implementing the alternative intervention")
                    # )
            ),
            tabItem(tabName = "interp",
                    h2(
                        
                    )
            ),
            tabItem(tabName = "value",
                    h2(
                        fluidRow(
                            valueBoxOutput("icer")
                        ),
                        fluidRow(
                            valueBoxOutput("total_cost_alert"),
                            valueBoxOutput("total_cost_no_alert")
                        ),
                        fluidRow(
                            valueBoxOutput("qaly_alert"),
                            valueBoxOutput("qaly_no_alert")
                        )
                    )),
            tabItem(tabName = "ades",
                    h2(
                        fluidRow(
                            column(width = 12,
                                   box(
                                       title = "Clopidogrel adverse events", width = NULL, status = "primary",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput('clo_ae_table'))
                                       )
                                   ) 
                            ),
                        fluidRow(
                            column(width = 12,
                                   box(
                                       title = "Warfarin adverse events", width = NULL, status = "primary",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput('war_ae_table'))
                                       )
                                   )
                            )
                        )
                    )
                )
            )
            )
    
    


server <- function(input, output, session) {
    data <- precisevalueServer("model_inputs")
    
    output$welcome_text <- renderText("This web application estimates the value of 
                                      developing and implementing clinical decision support 
                                      (CDS) alerts for pharmacogenomics (PGx), using a prototype 
                                      decision-analytic model. The model is intended help 
                                      decision makers in Learning Health Systems, who are 
                                      considering using PGx to improve the quality of patient 
                                      care.")
    # output$explanation_markdown <- renderUI({
    #     HTML(markdown::markdownToHTML(knit(here("R", "explanation_markdown.Rmd"), quiet = TRUE)))
    # })
    output$n_alerts <- renderValueBox({
        valueBox(
            round(data()$n_alerts, 0), "Total number of alerts",
            color = "purple"
        )
    })
    output$n_clo_alerts <- renderValueBox({
        valueBox(
            round(data()$n_clo_alerts, 0), "Clopidogrel alerts",
            color = "purple"
        )
    })
    output$n_war_alerts <- renderValueBox({
        valueBox(
            round(data()$n_war_alerts, 0), "Warfarin alerts",
            color = "purple"
        )
    })
    output$clo_alerts_by_year <- renderPlot(
        ggplot(data()$table, aes(x = year, y = round(clo_n_alert, 0))) +
            geom_bar(stat = "identity") +
            theme_bw(base_size = 14) +
            labs(x = "Year", y = "Annual number of alerts")
    )
    output$war_alerts_by_year <- renderPlot(
        ggplot(data()$table, aes(x = year, y = round(war_n_alert, 0))) +
            geom_bar(stat = "identity") +
            theme_bw(base_size = 14) +
            labs(x = "Year", y = "Annual number of alerts")
    )
    output$alert_decreased_clo_deaths <- renderValueBox({
        valueBox(round(-(sum(data()$table$clo_alert_CVDeath))-
                           (sum(data()$table$clo_noalert_CVDeath)), 0),
                 "Deaths prevented by clopidogrel alerts",
                 color = "yellow")
    })
    output$alert_decreased_clo_acs_events <- renderValueBox({
        valueBox(-round((data()$clo_non_fatal_mi_alert+data()$clo_stent_thrombosis_alert+
                           data()$clo_cabg_revasc_alert+data()$clo_pci_revasc_alert)-
                           (data()$clo_non_fatal_mi_no_alert+data()$clo_stent_thrombosis_no_alert+
                                data()$clo_cabg_revasc_no_alert+data()$clo_pci_revasc_no_alert), 0),
                 "Clinical events prevented by clopidogrel alerts",
                 color = "yellow"
        )
    })
    output$alert_clo_bleeding_events <- renderValueBox({
        valueBox(round((data()$clo_non_fatal_ic_bleed_alert+data()$clo_non_fatal_ec_bleed_alert+
                            data()$clo_cabg_bleed_alert)-
                           (data()$clo_non_fatal_ic_bleed_no_alert+data()$clo_non_fatal_ec_bleed_no_alert+
                                data()$clo_cabg_bleed_no_alert), 0),
                 "Change in bleeding events due to clopidogrel alerts",
                 color = "yellow"
        )
    })
    output$alert_decreased_war_deaths <- renderValueBox({
        valueBox(-round((data()$war_death_alert)-(data()$war_death_no_alert), 0),
                 "Decreased deaths due to warfarin alerts",
                 color = "yellow"
        )
    })
    output$alert_decreased_war_clots <- renderValueBox({
        valueBox(-round((data()$war_clot_alert)-(data()$war_clot_no_alert), 0),
                 "Clinical events (clots) prevented due to warfarin alerts",
                 color = "yellow"
        )
    })
    output$alert_war_bleeding_events <- renderValueBox({
        valueBox(round((data()$war_bleed_alert)-(data()$war_bleed_no_alert), 0),
                 "Change in bleeding events due to warfarin alerts",
                 color = "yellow"
        )
    })
    output$alert_decreased_mi <- renderValueBox({
        valueBox(round(-(sum(data()$table$clo_alert_NonfatalMI))-
                     (sum(data()$table$clo_noalert_NonfatalMI)), 0),
                 "Non-fatal MIs prevented by clopidogrel alerts",
                 color = "yellow")
    })
    output$total_cost_no_alert <- renderValueBox({
        valueBox(paste0("$", format(round(data()$total_cost_no_alert, 0), big.mark = ",")), 
                 "Total cost without alerts",
                 color = "purple")
    })
    output$total_cost_alert <- renderValueBox({
        valueBox(paste0("$", format(round(data()$total_cost_alert, 0), big.mark = ",")), 
                 "Total cost with alerts",
                 color = "purple")
    })
    output$change_medical_cost <- renderValueBox({
        valueBox(paste0("$", format(round(data()$medical_cost_alert-data()$total_cost_no_alert, 0), 
                                    big.mark = ",")),
                 "Change in medical costs with alerts",
                 color = "purple")
    })
    output$medical_cost_alert <- renderValueBox({
        valueBox(paste0("$", format(round(data()$medical_cost_alert, 0), big.mark = ",")), 
                 "Cost of medical therapy with alerts",
                 color = "purple")
    })
    output$admin_cost_alert <- renderValueBox({
        valueBox(paste0("$", format(round(data()$admin_cost_alert, 0), big.mark = ",")), 
                 "Build and maintenance cost of alerts",
                 color = "purple")
    })
    output$n_clo_no_alert_ade <- renderValueBox({
        valueBox(round(data()$n_clo_no_alert_ade, 0), 
                 "Clopidogrel adverse drug events without alerts",
                 color = "purple")
    })
    output$n_clo_alert_ade <- renderValueBox({
        valueBox(round(data()$n_clo_alert_ade, 0), 
                 "Clopidogrel adverse drug events with alerts",
                 color = "purple")
    })
    output$icer <- renderValueBox({
        valueBox(paste0("$", format(data()$icer_discounted, big.mark = ",")),
                 "Incremental cost effectiveness ratio (ICER)",
                 color = "purple")
    })
    output$qaly_no_alert <- renderValueBox({
        valueBox(round(data()$qaly_no_alert, 2),
                 "Quality-adjusted life years (QALYs) without alerts",
                 color = "purple")
    })
    output$qaly_alert <- renderValueBox({
        valueBox(round(data()$qaly_alert, 2),
                 "Quality-adjusted life years (QALYs) with alerts",
                 color = "purple")
    })
    output$clo_ae_table <- DT::renderDataTable({
        DT::datatable(data()$clo_ae_table, 
                      options = list(pageLength = 10))
    })
    output$war_ae_table <- DT::renderDataTable({
        DT::datatable(data()$war_ae_table, 
                      options = list(pageLength = 10))
    })

}

shinyApp(ui, server)
