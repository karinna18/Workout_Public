apply_filters <- function(df, input, exi){
  workout_groups <- trimws(strsplit(input$workout, "&")[[1]]) 
  df <- df %>% filter(muscle_group %in% workout_groups)
  
  if(input$place == "home" & exi == TRUE) {
    df <- df %>% filter(equipment %in% c("Dumbell", "Bodyweight", "Plate"))
  }
  if(input$place == "gym" & exi==TRUE) {
    df <- df %>% filter(!(type %in% c("Strength","Plyometrics") & equipment %in% c("Dumbell", "Bodyweight")))
  }
  df
}




generate_workout <- function(df_active_muscles, df_exercises, workout, exploration) {
  
  # Store selected exercise row indices to prevent duplicates
  selected_indices <- integer()
  
  # Helper: get available exercises for a muscle
  get_available_exercises <- function(muscle, compound_only = FALSE) {
    
    available <- df_exercises %>%
      mutate(.exercise_index = row_number()) %>%
      filter(
        target_muscle == muscle,
        !.exercise_index %in% selected_indices
      )
    
    if (compound_only) {
      av <- available %>%
        filter(mechanics == "Compound")
      if (nrow(av) != 0) {
        available <- av
      }
    }
    
    available
  }
  
  get_muscles_with_exercises <- function(muscles_df) {
    
    muscles_df %>%
      distinct(muscle, .keep_all = TRUE) %>%
      filter(
        vapply(
          muscle,
          function(m) nrow(get_available_exercises(m, FALSE)) > 0,
          logical(1)
        )
      )
  }
  
  # Helper: randomly select one exercise for a muscle
  pick_exercise <- function(muscle, compound_only = FALSE, exploration = "neutral") {
    
    if (length(exploration) == 0 || is.na(exploration)) {
      exploration <- "neutral"
    }
    
    available <- get_available_exercises(muscle, compound_only)
    
    '
    # If no compound exercises are available, try any exercise 
    if (nrow(available) == 0 && compound_only) { 
      available <- get_available_exercises(muscle, compound_only = FALSE) 
    } 
    ' #should have been fixed above in get available
    # Stop only if no exercises are available at all 
    if (nrow(available) == 0) { 
      stop(paste("No available exercise for muscle:", muscle)) 
    }
    
    preferred <- available

  if (exploration == "yes") {
    preferred <- available %>%
      filter(enjoyment == "?")
  } else if (exploration == "no") {
    preferred <- available %>%
      filter(enjoyment != "?")
  }
  if (nrow(preferred) > 0) {
    available <- preferred
  }

    
    
    chosen <- available %>%
      slice_sample(n = 1)
    #ok but is this actually making sure the new selcted one isnt already in here?
    selected_indices <<- c(
      selected_indices,
      chosen$.exercise_index
    )
    
    chosen %>%
      select(-.exercise_index)
  }
  
  # --------------------------------------------------
  # 1. PRIORITY 1: One compound exercise per muscle
  # --------------------------------------------------
  
  priority1 <- df_active_muscles %>%
    filter(personal_priority == 1)
  
  # Find which priority-1 muscles actually have an available exercise
  priority1_available <- priority1 %>%
    filter(
      vapply(
        muscle,
        function(m) nrow(get_available_exercises(m, compound_only = TRUE)) > 0,
        logical(1)
      )
    )
  #print(paste0("nrow availble priority 1 muscles: ", nrow(priority1_available))) #debug
  if (nrow(priority1_available) == 2) {
    
    # Normal case: one exercise for each priority-1 muscle
    workout_exercises <- lapply(
      priority1_available$muscle,
      pick_exercise,
      compound_only = TRUE,
      exploration = exploration
    )
    
  } else if (nrow(priority1_available) == 1) {
    # Only one muscle has an available exercise
    muscle <- priority1_available$muscle[1]
    
    print(paste0("only ", muscle, " has available exercises")) #debug
    
    # First exercise: compound if possible, otherwise fallback
    first_exercise <- pick_exercise(
      muscle,
      compound_only = TRUE,
      exploration = exploration
    )
    
    # Second exercise: anything else available for the same muscle
    second_exercise <- pick_exercise(
      muscle,
      compound_only = FALSE,
      exploration = exploration
    )
    
    workout_exercises <- list(
      first_exercise,
      second_exercise
    )
    
  } else {
    
    stop("Neither priority-1 muscle has an available exercise.")
  }
  # --------------------------------------------------
  # 2. PRIORITY 2: Pick two distinct muscles
  # --------------------------------------------------
  selected_muscles <- character()
  # We need to select 2 muscles for this part of the workout.
  for (i in 1:2) {
    
    # Randomly decide which priority this slot should come from.
    chosen_priority <- sample(c(1, 2), size = 1, prob = c(0.35, 0.65))
  
    
    candidates <- df_active_muscles %>%
      filter(
        personal_priority == chosen_priority,
        !muscle %in% selected_muscles
      ) %>%
      get_muscles_with_exercises()
    
    # If there is no other eligible priority-1 muscle, we
    # can't use priority 1 again. In that case, try priority 2.
    if (nrow(candidates) == 0) {
      
      candidates <- df_active_muscles %>%
        filter(
          personal_priority != chosen_priority,
          !muscle %in% selected_muscles
        ) %>%
        get_muscles_with_exercises()
    }
    
    if (nrow(candidates) == 0) {
      stop("Not enough unique muscles available from priorities 1 and 2.")
    }
    
    # Randomly choose one muscle from the eligible candidates.
    chosen_muscle <- candidates %>%
      slice_sample(n = 1) %>%
      pull(muscle)
    
    # Remember the muscle so it cannot be selected again
    # during the second iteration of the loop.
    selected_muscles <- c(
      selected_muscles,
      chosen_muscle
    )
    
    chosen_exercise <- pick_exercise(
      chosen_muscle,
      exploration = exploration
    )
    workout_exercises <- c(
      workout_exercises,
      list(chosen_exercise)
    )
  }
  
  # --------------------------------------------------
  # 3. FINAL SELECTION OR CARDIO
  # --------------------------------------------------
  
  special_workout <- workout %in% c(
    "Arms & Chest",
    "Back & Shoulders"
  )
  
  add_cardio <- special_workout && runif(1) < 0.5
  
  if (add_cardio) {
    
    # Add a Cardio row using the same columns as df_exercises
    cardio <- df_exercises[NA_integer_, , drop = FALSE]
    
    cardio$exercise <- "Cardio"
    
    workout_exercises[[length(workout_exercises) + 1]] <- cardio
    
  } else {
    
    # Find priorities with at least one eligible muscle
    # and at least one available exercise
    eligible_priorities <- df_active_muscles %>%
      distinct(personal_priority) %>%
      pull(personal_priority)
    
    # Choose a priority with equal probability
    # regardless of how many muscles belong to it
    chosen_priority <- sample(
      eligible_priorities,
      size = 1,
      prob = eligible_priorities
    )
    
    # Choose a random muscle within that priority
    eligible_muscles <- df_active_muscles %>%
      filter(personal_priority == chosen_priority) %>%
      get_muscles_with_exercises()
    
    chosen_muscle <- eligible_muscles %>%
      slice_sample(n = 1) %>%
      pull(muscle)
    
    final_exercise <- pick_exercise(chosen_muscle, exploration = exploration)
    #im not sure if anywhere is actually making sure no muscle gets picked twice..
    
    workout_exercises[[length(workout_exercises) + 1]] <-
      final_exercise
  }
  
  # --------------------------------------------------
  # 4. RETURN THE FINAL WORKOUT DATABASE
  # --------------------------------------------------
  
  bind_rows(workout_exercises)
}

