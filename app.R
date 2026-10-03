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
  library(DT)
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
  default_workout <- switch(
    weekdays(Sys.Date()),
    "Monday"    = "Legs & Butt",
    "Tuesday"   = "Abs & Hip flexors",
    "Wednesday" = "Legs & Butt",
    "Thursday"  = "Arms & Chest",
    "Friday"    = "Abs & Hip flexors",
    "Saturday"  = "Legs & Butt",
    "Sunday"    = "Back & Shoulders"
  ),
  
  selectInput("workout", "Workout", choices = c("Arms & Chest", "Back & Shoulders", "Legs & Butt", "Core"), selected = default_workout),
  br(),
  selectInput("place", "Place", choices = c("home", "gym")),
  br(),
  selectInput("exploration", "Exploration", choices = c("yes", "neutral", "no"), selected ="neutral"),
  br(),
  downloadButton(
    "download_workout",
    "Save workout"
  ),
  br(),
  p("Workout"),
  DT::DTOutput("workout_table"),
  br(),
  p("Warmups"),
  DT::DTOutput("warmups_table"),
  br(),
  p("Stretches"),
  DT::DTOutput("stretches_table"),
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
  
  ) %>% select(exercise, target_muscle, equipment, mechanics, weight, enjoyment,  notion_url)}
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
      bind_rows() %>% select(exercise, target_muscle, equipment, enjoyment, notion_url) %>% distinct(exercise, .keep_all = TRUE)
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
      bind_rows()%>% select(exercise, target_muscle, equipment, force, enjoyment, notion_url) %>% distinct(exercise, .keep_all = TRUE)
  })
  
  output$warmups_table <- DT::renderDT({
    workout_warmups() %>%
      mutate(
        exercise = sprintf(
          '<a href="%s" target="_blank">%s</a>',
          notion_url,
          exercise
        )
      ) %>%
      select(-notion_url) %>%
      DT::datatable(
        escape = FALSE
      ) 
  })
  
  output$stretches_table <- DT::renderDT({
    workout_stretches() %>%
      mutate(
        exercise = sprintf(
          '<a href="%s" target="_blank">%s</a>',
          notion_url,
          exercise
        )
      ) %>%
      select(-notion_url) %>%
      DT::datatable(
        escape = FALSE
      ) 
  })
    
  output$workout_table <- DT::renderDT({
      workout_db()  %>%
      mutate(
        exercise = sprintf(
          '<a href="%s" target="_blank">%s</a>',
          notion_url,
          exercise
        )
      ) %>%
      select(-notion_url) %>%
      DT::datatable(
        escape = FALSE
      )
    })
  
  output$debug_exercises <- renderTable({
    df_all_exercises() %>% select(exercise,type, equipment, mechanics, target_muscle, enjoyment, muscle_group, notion_url)
  })
  
  #download button
  make_html_table <- function(df, title = NULL) {
    
    # Turn the exercise name into a clickable Notion link.
    # We do this BEFORE converting the data frame to HTML.
    if ("notion_url" %in% names(df) && "exercise" %in% names(df)) {
      
      df <- df %>%
        mutate(
          exercise = ifelse(
            !is.na(notion_url) & notion_url != "",
            sprintf(
              '<a href="%s" target="_blank">%s</a>',
              notion_url,
              htmltools::htmlEscape(exercise)
            ),
            htmltools::htmlEscape(exercise)
          )
        ) %>%
        select(-notion_url)
    }
    
    # Convert the data frame into an HTML table.
    table <- htmltools::tags$table(
      class = "workout-table",
      htmltools::tags$thead(
        htmltools::tags$tr(
          lapply(names(df), function(x) {
            htmltools::tags$th(x)
          })
        )
      ),
      htmltools::tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          htmltools::tags$tr(
            lapply(df[i, ], function(x) {
              htmltools::tags$td(
                htmltools::HTML(as.character(x))
              )
            })
          )
        })
      )
    )
    
    if (!is.null(title)) {
      htmltools::tagList(
        htmltools::tags$h2(title),
        table
      )
    } else {
      table
    }
  }
  
  output$download_workout <- downloadHandler(
    
    filename = function() {
      paste0(
        "workout_",
        format(Sys.Date(), "%Y-%m-%d"),
        ".html"
      )
    },
    
    content = function(file) {
      
      # Grab the CURRENT workout.
      # This is important: whatever is currently displayed
      # in the app is what gets saved.
      workout <- workout_db()
      
      # Grab the current warmups and stretches too.
      warmups <- workout_warmups()
      stretches <- workout_stretches()
      
      
      # Build the HTML document.
      page <- htmltools::tags$html(
        
        htmltools::tags$head(
          htmltools::tags$meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
          ),
          
          htmltools::tags$title(
            paste("Workout", Sys.Date())
          ),
          
          htmltools::tags$style(
            htmltools::HTML("
            body {
              font-family: -apple-system, BlinkMacSystemFont,
                         'Segoe UI', sans-serif;
              max-width: 800px;
              margin: 0 auto;
              padding: 20px;
              line-height: 1.5;
            }

            h1 {
              margin-bottom: 5px;
            }

            h2 {
              margin-top: 30px;
            }

            .workout-table {
              width: 100%;
              border-collapse: collapse;
              margin-bottom: 20px;
            }

            .workout-table th,
            .workout-table td {
              padding: 10px 8px;
              border-bottom: 1px solid #ddd;
              text-align: left;
            }

            .workout-table th {
              font-weight: 600;
            }

            a {
              color: #0066cc;
              text-decoration: underline;
            }
          ")
          )
        ),
        
        htmltools::tags$body(
          
          htmltools::tags$h1("Today's Workout"),
          
          htmltools::tags$p(
            format(Sys.Date(), "%A, %d %B %Y")
          ),
          make_html_table(
            warmups,
            "Warmups"
          ),
          
          make_html_table(
            workout,
            "Workout"
          ),
          
          make_html_table(
            stretches,
            "Stretches"
          )
        )
      )
      
      # Write the finished HTML document to the temporary
      # file that Shiny provided for the download.
      htmltools::save_html(
        page,
        file
      )
    }
  )
    
  
  

}
shinyApp(ui, server)

