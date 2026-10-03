suppressPackageStartupMessages({
library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(janitor)
library(cli)
})

# --- CREDENTIALS ---

NOTION_TOKEN <<- Sys.getenv("NOTION_TOKEN")
MUSCLES_DATABASE_ID <<- "3ed5a4c40d5c803c9be0ed31ca3a1d44"
EXERCISES_DATABASE_ID <<- "3ed5a4c40d5c80cd8ffbe93a0fc52cf9"

if (!dir.exists("www/notion_cache")) {
  dir.create("www/notion_cache", recursive = TRUE)
}

# --- THE API WORKHORSE ---
fetch_notion_db <- function(db_id, last_sync_time = NULL, strdb = "") {
  #
  #
  #cat("\n--- Fetching database:", strdb, "---\n")
  status_msg <- cli_status("Fetching database: {.strong {strdb}}...")
  #
  #
  url <- paste0("https://api.notion.com/v1/databases/", db_id, "/query")
  
  print(url) #debug
  
  
  filter_payload <- list()
  if (!is.null(last_sync_time)) {
    filter_payload <- list(
      filter = list(
        timestamp = "last_edited_time",
        last_edited_time = list(on_or_after = last_sync_time)
      )
    )
  }
  
  pages <- list()
  has_more <- TRUE
  cursor <- NULL
  
  page_count <- 0

  while (has_more) {
    page_count <- page_count + 1
    #
    #
    #cat("\r  > Total pages retrieved:", page_count)
    cli_status_update(id = status_msg, "Fetching {.strong {strdb}}: Page {page_count} retrieved...")
    #
    #
    # Start with the filter (might be empty list)
    body <- filter_payload
    
    # Add cursor if it exists
    if (!is.null(cursor)) body$start_cursor <- cursor
    
    # Create the request
    req <- request(url) %>%
      req_method("POST") %>%
      req_headers(
        `Authorization` = paste("Bearer", NOTION_TOKEN),
        `Notion-Version` = "2022-06-28",
        `Content-Type` = "application/json"
      )
    
    # ONLY add the body if it has actual content
    # This prevents the [ ] vs { } conflict
    if (length(body) > 0) {
      req <- req %>% req_body_json(body)
    }
    
    # Perform
    resp <- req %>% 
      req_perform() %>% 
      resp_body_json()
    
    pages <- c(pages, resp$results)
    has_more <- resp$has_more
    cursor <- resp$next_cursor
  }
  #
  #
  #cat("  ✓ Total items retrieved:", length(pages), "\n")
  cli_status_clear(id = status_msg)
  cli_alert_success("Retrieved {length(pages)} items from {.strong {strdb}}.")
  #
  #
  return(pages)
}

# --- THE DATA PARSER ---
parse_notion_to_df <- function(pages, label = "Pages") {
  #
  #
  cat("  > Parsing raw JSON to data frame...\n")
  #
  #
  map_df(pages, function(p) {
    props <- p$properties
    page_id <- p$id
    
    get_val <- function(x, nm) {
      type <- x$type
      clean_nm <- tolower(nm)
      
      val <- if (type %in% c("title", "rich_text")) {
        if (length(x[[type]]) == 0) NA else x[[type]][[1]]$plain_text
      } else if (type == "number") {
        x$number # Stays as numeric
      } else if (type == "select" || type == "status") {
        x[[type]]$name
      } else if (type == "multi_select") {
        if (length(x$multi_select) == 0) NA else paste(map_chr(x$multi_select, "name"), collapse = ", ")
      } else if (type == "date") {
          list(start = as.character(x$date$start %||% NA), end = as.character(x$date$end %||% NA))
      } else if (type == "formula") {
        # Formulas are the usual culprits. We force them to character to be safe.
        as.character(x$formula[[x$formula$type]] %||% NA)
      } else if (type == "relation") {
        if (length(x$relation) == 0) NA else paste(map_chr(x$relation, "id"), collapse = ",")
      } else if (type == "checkbox") {
        x$checkbox # Returns TRUE/FALSE
      } else if (type == "people") {
        if (length(x$people) == 0) NA else paste(map_chr(x$people, "name"), collapse = ", ")
        
      } else if (type %in% c("url", "email", "phone_number")) {
        x[[type]] %||% NA
        
      } else if (type == "formula") {
        # Formulas can be strings, numbers, or dates; forcing character for bind_rows safety
        as.character(x$formula[[x$formula$type]] %||% NA)
        
      } else if (type == "rollup") {
        # Rollups are lists of values. Like Python, we convert the whole array to a string.
        as.character(jsonlite::toJSON(x$rollup$array))
        
      } else if (type == "files") {
          if (length(x$files) == 0) {
            NA 
          } else {
            # If it's a cover property, we usually want the URL, not just the name
            # We check for 'file$url' (hosted) or 'external$url' (linked)
            urls <- map_chr(x$files, function(f) {
              if (!is.null(f$file$url)) {
                ext <- tools::file_ext(f$name)
                if(ext == "") ext <- "png"
                safe_name <- paste0(page_id, ".", ext)
                local_path <- file.path("www/notion_cache", safe_name)
                request(f$file$url) %>% req_perform(path = local_path)
                if (!file.exists(local_path)) {
                  # cat("\r  > Downloading new cover:", safe_name)
                  request(f$file$url) %>% req_perform(path = local_path)
                }
                return(file.path("notion_cache", safe_name))
              }
              if (!is.null(f$external$url)) return(f$external$url)
              return(f$name) # Fallback to name if it's just a file list
            })
            paste(urls, collapse = ", ")
          }
        
      } else {
        NA
      }
      
      return(val %||% NA)
    }
    
    # Use imap (indexed map) to pass the property name into get_val
    res <- imap(props, get_val) %>% 
      set_names(tolower(names(props)))
    
    
    as_tibble_row(res) %>%
      mutate(id = p$id, last_edited_time = p$last_edited_time)
  }, .progress = paste("Parsing", label))
}

# --- MAIN SYNC EXECUTION ---
sync_data <- function() {
  cache_file <- "notion_cache.rds"
  
  #
  #
  #cat("--- Initializing Sync ---\n")
  cli_h1("Initializing Sync")
  #
  #
  
  if (file.exists(cache_file)) {
    cache <- readRDS(cache_file)
    # Get the latest timestamp across all three dbs to be safe
    all_times <- c(cache$exercises$last_edited_time, cache$muscles$last_edited_time)
    last_sync <- if(length(all_times) > 0) max(all_times, na.rm = TRUE) else NULL
    #
    #
    #cat("  ✓ Cache found. Last sync point:", as.character(last_sync), "\n")
    cli_alert_success("Cache found. Last sync: {.val {as.character(last_sync)}}")
    #
    #
  } else {
    cache <- list(exercises = tibble(), muscles = tibble())
    last_sync <- NULL
    #
    #
    #cat("  ! No cache found. Performing full sync.\n")
    cli_alert_warning("No cache found. Performing full sync.")
    #
    #
  }
  
  # Fetch updates using the correct IDs
  new_e_raw <- fetch_notion_db(EXERCISES_DATABASE_ID, last_sync, "exercises")
  new_m_raw <- fetch_notion_db(MUSCLES_DATABASE_ID, last_sync, "muscles")
  
  
  
  #
  #
  #cat("\n--- Updating Cache Tables ---\n")
  cli_h2("Updating Cache Tables")
  #
  #
  # Update ex
  if (length(new_e_raw) > 0) {
    new_e <- parse_notion_to_df(new_e_raw, "Exercises")
    
    # Check if cache actually has rows before trying to filter
    if (nrow(cache$exercises) > 0) {
      cache$exercises <- bind_rows(
        cache$exercises %>% filter(!id %in% new_e$id), 
        new_e
      )
    } else {
      cache$exercises <- new_e
    }
  }
  # Update Authors
  if (length(new_m_raw) > 0) {
    new_m <- parse_notion_to_df(new_m_raw, "Muscles")
    if (nrow(cache$muscles) > 0) {
      cache$muscles <- bind_rows(cache$muscles %>% filter(!id %in% new_m$id), new_m)
    } else {
      cache$muscles <- new_m
    }
  }
  
  #
  #cat("  ✓ All tables updated locally.\n")
  cli_alert_success("All tables updated locally.")
  #
  #
  saveRDS(cache, cache_file)
  #
  #
  cli_alert_info("Cache saved to {.file {cache_file}}")
  #cat("  ✓ Cache saved to:", cache_file, "\n")
  #
  #
  return(cache)
}

# --- RUN SYNC AND CLEAN ---
all_data <- sync_data()


#
#
cat("\n--- Running Final Data Processing ---\n")
#
#

# Lookups
muscle_lookup <- all_data$muscles %>% select(id, muscle) %>% deframe()


# Final Dataframes for app.R
muscles <<- all_data$muscles %>%
  select(muscle, `muscle group`, `personal priority`, skill) %>%
  rename(muscle_group = `muscle group`, personal_priority = `personal priority`) %>%
  mutate(skill = as.numeric(trimws(sub("-.*", "", skill))),
         personal_priority = as.numeric(trimws(sub("-.*", "", personal_priority)))
         ) %>%
  clean_names()
#
#
cat("  > Processing book relations and metrics...\n")
#
#

exercises <<- all_data$exercises %>% 
  rename(target_muscle = `target muscle`, secondary_muscles = `secondary muscles`, exercise = name) %>%
  clean_names() %>% 
  mutate(
    target_muscle = map_chr(target_muscle, ~ {
      ids <- unlist(strsplit(.x, ","))
      paste(muscle_lookup[ids], collapse = ", ")
    }),
    
    secondary_muscles = map_chr(secondary_muscles, ~ {
      ids <- unlist(strsplit(.x, ","))
      paste(muscle_lookup[ids], collapse = ", ")
    })
    
  ) %>%
  select(exercise, type, equipment, mechanics, level, force, target_muscle, secondary_muscles, enjoyment) %>% 
    separate_rows(target_muscle, sep = ",\\s*") %>% left_join(muscles, by = join_by(target_muscle == muscle)) %>%
  mutate(
    level = as.numeric(trimws(sub("-.*", "", level)))
  ) 

#
#
cat("✓ Sync and processing complete!\n\n")
#
#

