# module for Shiny inputs

# Module UI function
precisevalueUI <- function(id, label = "model inputs") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    numericInput(ns("pop_size"), label = "Population size:", value = 250000),
    numericInput(ns("p_w"), label = "Percentage White:", value = 60),
    numericInput(ns("p_l"), label = "Percentage Latinx:", value = 18),
    numericInput(ns("p_b"), label = "Percentage Black:", value = 13),
    numericInput(ns("p_l"), label = "Percentage Asian:", value = 6),
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
