suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(magrittr)
  library(dplyr)
  library(rlang)
  library(car)
  library(xtable)
  library(performance)
  library(see)
  library(shiny)
  library(shinyWidgets)
})

source("w_global.R")
source("w_source.R")


options(
  shiny.sanitize.errors = FALSE,
  shiny.reactlog = TRUE,
  error = traceback,
  shiny.fullstacktrace = TRUE
)


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  selectInput("workout", "Workout", choices = c("Arms & Chest", "Back & Shoulders", "Legs & Butt", "Core")),
  br(),
  selectInput("place", "Place", choices = c("home", "gym")),
  br(),
  selectInput("exploration", "Exploration", choices = c("yes", "neutral", "no")),
  br(),
  p("Warmups"),
  tableOutput("warmups_table"),
  br(),
  p("Workout"),
  tableOutput("workout_table"),
  br(),
  p("Stretches"),
  tableOutput("stretches_table"),
  br()
  #p("Debug"),
  #tableOutput("debug_exercises")
  
             
    )




server <- function(input, output, session){
  'debug
  input <- list(
    workout = "Legs & Butt",
    place = "gym"
  )
  '
  
  exercises <- exercises %>% filter(skill>=level) %>% separate_rows(type, sep = ",\\s*")
  #df_exercises <- apply_filters(exercises, input, TRUE) #debug
  #df_active_muscles <- apply_filters(muscles, input, FALSE) #debug
  df_all_exercises <- reactive(apply_filters(exercises, input, TRUE))
  df_active_muscles <- reactive(apply_filters(muscles, input, FALSE))
  
  
  df_warmups <- reactive({df_all_exercises() %>% filter(type == "Warmup")})
  df_stretches <- reactive({df_all_exercises() %>% filter(type == "Stretch")})
  df_exercises <- reactive({df_all_exercises() %>% filter(type == "Strength")})
  
  workout_db <- reactive({

    generate_workout(
    df_active_muscles = df_active_muscles(),
    df_exercises = df_exercises(),
    workout = input$workout,
    exploration = input$exploration
  
  )}
  )
  
  workout_stretches <- reactive({
    # Get every muscle targeted by the workout.
    targeted_muscles <- workout_db() %>%
      pull(target_muscle) %>%
      unique() %>%
      na.omit()
    
    lapply(targeted_muscles, function(muscle) {
      # Find stretches that target this muscle
      available <- df_stretches() %>%
        filter(target_muscle == muscle)
      
      preferred <- available
      
      if (input$exploration == "yes") {
        preferred <- available %>%
          filter(enjoyment == "?")
      } else if (input$exploration == "no") {
        preferred <- available %>%
          filter(enjoyment != "?")
      }
      if (nrow(preferred) > 1) {
        available <- preferred
      }
      
      # We want exactly 2 stretches.
      if (nrow(available) >= 2) {
        available %>%
          slice_sample(n = 2)
      } else {
        # Take however many real stretches are available
        real_stretches <- available
        # Calculate how many placeholders we need
        # Create the placeholder rows
        placeholders <- tibble(
          target_muscle = muscle,
          exercise = paste(muscle, " STRETCH needed!")
        )
        bind_rows(
          real_stretches,
          placeholders
        )
      }
    }) %>%
      bind_rows()
  })
  
  workout_warmups <- reactive({
    # Get every muscle targeted by the workout.
    targeted_muscles <- workout_db() %>%
      pull(target_muscle) %>%
      unique() %>%
      na.omit()
    
    lapply(targeted_muscles, function(muscle) {
      # Find stretches that target this muscle
      available <- df_warmups() %>%
        filter(target_muscle == muscle)
      
      preferred <- available
      
      if (input$exploration == "yes") {
        preferred <- available %>%
          filter(enjoyment == "?")
      } else if (input$exploration == "no") {
        preferred <- available %>%
          filter(enjoyment != "?")
      }
      if (nrow(preferred) > 1) {
        available <- preferred
      }
      
      # We want exactly 2 stretches.
      if (nrow(available) >= 2) {
        available %>%
          slice_sample(n = 2)
      } else {
        #print(df_warmups()) #debug
        # Take however many real stretches are available
        real_warmups <- available
        # Calculate how many placeholders we need
        # Create the placeholder rows
        placeholders <- tibble(
          target_muscle = muscle,
          exercise = paste(muscle, " WARMUP needed!")
        )
        # Combine real stretches + placeholders
        bind_rows(
          real_warmups,
          placeholders
        )
      }
    }) %>%
      bind_rows()
  })
  
  output$warmups_table <- renderTable({
    workout_warmups() %>% select(exercise, target_muscle, equipment, force, enjoyment) %>% distinct(exercise, .keep_all = TRUE)
  })
  
  output$stretches_table <- renderTable({
    workout_stretches() %>% select(exercise, target_muscle, equipment, enjoyment) %>% distinct(exercise, .keep_all = TRUE)
  })
    
  output$workout_table <- renderTable({
      workout_db() %>% select(exercise, target_muscle, equipment, mechanics, enjoyment)
    })
  
  output$debug_exercises <- renderTable({
    df_all_exercises() %>% select(exercise,type, equipment, mechanics, target_muscle, enjoyment, muscle_group)
  })
  

}
shinyApp(ui, server)

