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
        numericInput(ns("pop_size"), label = "Population size:", value = 250000),
        numericInput(ns("p_w"), label = "Percentage White:", value = 60),
        #numericInput(ns("p_l"), label = "Percentage Latinx:", value = 18),
        numericInput(ns("p_b"), label = "Percentage Black:", value = 13),
        numericInput(ns("p_a"), label = "Percentage Asian:", value = 6),
        sliderInput(ns("start_age"), label = "Starting Age of Screening:",
                    min = 0, max = 100, value = 55, step = 1),
        sliderInput(ns("test_rate"), label = "Probability of Testing for Patients in Age Range:",
                    min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput(ns("screen_dur"), label = "Screening Duration:",
                    min = 1, max = 50, value = 5, step = 1),
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
            
            # calculated from race-specific prevalence of CYP2C19 variants: Poor metabolizer and Intermediate metabolizer
            p_clo_a <- 0.5394 
            p_clo_b <- 0.3900
            p_clo_w <- 0.3818
            
            # % of people on warfarin will be eligible to benefit from PGx. 
            p_war <- 1 #09/22: we don't model warfarin variant anymore. 
            p_eligible <- 0.67 
            discount <- 0.03
            
            # probability of plan modification based on alert
            p_change_alert <- 0.25
            
            # probability of plan modification based on no alert
            p_change_no_alert <- 0.1
            
            # qalys and costs of changing pgx
            qaly_change_clo <- 0.05
            cost_change_clo <- 1700
            qaly_change_war <- 0.008
            cost_change_war <- -150
            
            p_new_rx <- 0.005 # annual probability of a new rx for one of the included drugs
            
            # The user's data, parsed into a data frame
            dataframe <- reactive({
                p_clo <- p_clo_a*(input$p_a/100) + p_clo_b*(input$p_b/100) + 
                    p_clo_w*(input$p_w/100)  # population prevalence of clopidogrel variant
                p_war <- 1                                        # 09/22: we dont need variant prevalence
                ages <- make_age_pattern()
                test <- make_test_pattern(input$start_age, input$test_rate, 
                                          input$screen_dur, input$t_horizon)
                drug <- make_treat_prob(0.005) # setting prob of new rx to 0.5%, will need to clean up
                
                # get population by year
                n_age <- data.frame(ages = ages$ages)
                for(i in 1:input$t_horizon){
                    temp_col <- n * ages$p
                    n_age$temp_col <- temp_col
                    names(n_age)[ncol(n_age)] <- paste0("y", i)
                }
                
                # get probability of new clopidogrel rx by year
                p_new_clo <- data.frame(ages = drug$ages)
                for(i in 1:input$t_horizon){
                    temp_col <- drug$c
                    p_new_clo$temp_col <- temp_col
                    names(p_new_clo)[ncol(p_new_clo)] <- paste0("y", i)
                }
                
                # get probability of new warfarin rx by year
                p_new_war <- data.frame(ages = drug$ages)
                for(i in 1:input$t_horizon){
                    temp_col <- drug$w
                    p_new_war$temp_col <- temp_col
                    names(p_new_war)[ncol(p_new_war)] <- paste0("y", i)
                }
                
                # calculate benefit of clopidogrel alert
                n_test <- n_age * test #number tested by age / year
                n_var <- n_test * p_clo #number tested positive for variant
                n_rx <- n_var * p_new_clo
                clo_outcomes <- data.frame(year = seq(1, input$t_horizon),
                                           clo_n_alert = apply(n_rx[,2:ncol(n_rx)],
                                                               2,
                                                               function(x) sum(x)))
                clo_outcomes$clo_alert_q <- clo_outcomes$clo_n_alert * p_change_alert * qaly_change_clo
                clo_outcomes$clo_alert_c <- clo_outcomes$clo_n_alert * p_change_alert * cost_change_clo
                clo_outcomes$clo_noalert_q <- clo_outcomes$clo_n_alert * p_change_no_alert * qaly_change_clo
                clo_outcomes$clo_noalert_c <- clo_outcomes$clo_n_alert * p_change_no_alert * cost_change_clo
                
                # calculate benefit of warfarin alert
                n_test <- n_age * test #number tested by age / year
                n_var <- n_test * p_war * p_eligible #number people who are eligible to benefit
                n_rx <- n_var * p_new_war
                war_outcomes <- data.frame(year = seq(1, input$t_horizon),
                                           war_n_alert = apply(n_rx[,2:ncol(n_rx)],
                                                               2,
                                                               function(x) sum(x)))
                war_outcomes$war_alert_q <- war_outcomes$war_n_alert * p_change_alert * qaly_change_war
                war_outcomes$war_alert_c <- war_outcomes$war_n_alert * p_change_alert * cost_change_war
                war_outcomes$war_noalert_q <- war_outcomes$war_n_alert * p_change_no_alert * qaly_change_war
                war_outcomes$war_noalert_c <- war_outcomes$war_n_alert * p_change_no_alert * cost_change_war
                
                # combine drug-specific benefit calculations
                outcomes <- merge(clo_outcomes, war_outcomes, by = "year")
                
                # add costs of alert program
                outcomes$alert_cost <- input$start_up_cost
                outcomes$alert_cost[2:nrow(outcomes)] <- input$maint_cost
                
                # implement discounting
                for(i in c(3:6,8:11)) {
                    outcomes[,i] <- mapply(function(x,y) x * (1 / (1 + discount)^(y - 1)),
                                           x = outcomes[,i],
                                           y = outcomes$year)
                }
                
                total <- data.frame(year = outcomes$year,
                                    alert_n = rowSums(outcomes[,c(2,7)]),
                                    alert_qaly = rowSums(outcomes[,c(3,8)]),
                                    alert_cost = rowSums(outcomes[,c(4,9,12)]),
                                    no_alert_qaly = rowSums(outcomes[,c(5,10)]),
                                    no_alert_cost = rowSums(outcomes[,c(6,11)]))
                #return_list <- list(outcomes, total)
                #return(list(outcomes,total))
                return(total)
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
