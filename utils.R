# Load data from csv file
marine_data <- read_csv("ships.csv")

# Add a new column for better identification
df <- marine_data %>%
  mutate(ship_codename = paste0(SHIPNAME, " | ", SHIP_ID))

# Get Max Distance between points
get_max_distance <- function(inputdata) {
  
  df1 <- inputdata %>% 
    arrange(desc(DATETIME)) %>% 
    ungroup() %>% 
    slice_max(distance, n = 1)
  
  maxDistance <- paste0(round(df1$distance, 2), " meters")
  
  return(maxDistance)
}

# Get Start Time
get_time1 <- function(inputdata) {
  
  df1 <- inputdata %>% 
    arrange(desc(DATETIME)) %>% 
    ungroup() %>% 
    slice_max(distance, n = 1)
  
  timeWhen1 <- paste0(df1$DATETIME)
  
  return(timeWhen1)
}

# Get End Time
get_time2 <- function(inputdata) {
  
  df1 <- inputdata %>% 
    arrange(desc(DATETIME)) %>% 
    #group_by(ship_codename) %>%
    ungroup() %>% 
    slice_max(distance, n = 1)
  
  timeWhen2 <- paste0(df1$DATETIME2)
  
  return(timeWhen2)
}

# Custom Text Message
custom_ui_message <- function(head, content, icon_name = "inbox",
                              color = "purple", size = "big") {
  div(class = glue::glue("ui icon {size} {color} message"),
      icon(icon_name),
      div(class = "content",
          div(class = "header", head),
          p(content)
      )
  )
}
