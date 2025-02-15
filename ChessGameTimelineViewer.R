library(ggplot2)
library(gridExtra)

# Read PGN files
read_pgn_content <- function(file_name) {
  lines <- readLines(file_name, warn = FALSE)
  single_string <- paste(lines, collapse = " ")  # Use space instead of newline for collapse
  return(single_string)
}

filenames <- "./PGN"
pgn_files <- list.files(path = filenames, pattern = "\\.pgn$", full.names = TRUE)

plot_list <- list()

for(i in 1:length(pgn_files)){
  pgn_file <- pgn_files[i]
  
  file_name <- sub("\\.png$", "", pgn_file)
  file_name <- sub("\\./PGN/", "", file_name)
  
  result_file_name <- paste('./results/', file_name, '.pdf', sep='')
  
  pdf(result_file_name, width = 8, height = 6)
  
  pgn_content <- read_pgn_content(pgn_file)
  
  # Extract times from the PGN file
  times <- regmatches(pgn_content, gregexpr("(?<=\\[%clk ).*?(?=\\]})", pgn_content, perl = TRUE))[[1]]
  
  # Split times for white and black players
  times_white <- c('2:00:00', times[seq_along(times) %% 2 == 1])
  times_black <- c('2:00:00', times[seq_along(times) %% 2 == 0])
  
  # Create empty data frames for White and Black
  df_white <- data.frame(Move = integer(), Start = character(), End = character(), Min_Start = integer(), Min_End = integer(), stringsAsFactors = FALSE)
  df_black <- data.frame(Move = integer(), Start = character(), End = character(), Min_Start = integer(), Min_End = integer(), stringsAsFactors = FALSE)
  
  # Function to convert time string to minutes as an integer
  convert_time_to_minutes_integer <- function(time_str) {
    parts <- unlist(strsplit(time_str, ":"))
    if(length(parts) == 3) {
      return(as.integer(parts[1]) * 60 + as.integer(parts[2]) + as.integer(parts[3]) / 60)
    } else {
      return(NA)  # Return NA for malformed time strings
    }
  }
  
  # Initialize move number
  move_number <- 1
  
  # Iterate over times for White moves
  for (j in 2:length(times_white)) {
    start_min <- convert_time_to_minutes_integer(times_white[j - 1])
    end_min <- convert_time_to_minutes_integer(times_white[j])
    row <- data.frame(Move = move_number, Start = times_white[j - 1], End = times_white[j], Min_Start = start_min, Min_End = end_min, stringsAsFactors = FALSE)
    df_white <- rbind(df_white, row)
    move_number <- move_number + 1
  }
  
  # Initialize move number for Black
  move_number <- 1
  
  # Iterate over times for Black moves
  for (k in 2:length(times_black)) {
    start_min <- convert_time_to_minutes_integer(times_black[k - 1])
    end_min <- convert_time_to_minutes_integer(times_black[k])
    row <- data.frame(Move = move_number, Start = times_black[k - 1], End = times_black[k], Min_Start = start_min, Min_End = end_min, stringsAsFactors = FALSE)
    df_black <- rbind(df_black, row)
    move_number <- move_number + 1
  }
  
  # Calculate durations
  df_white$Duration <- abs(df_white$Min_End - df_white$Min_Start)
  df_black$Duration <- abs(df_black$Min_End - df_black$Min_Start)
  
  # Add Player column
  df_white$Player <- "White"
  df_black$Player <- "Black"
  
  # Combine White and Black data frames
  df_combined <- rbind(df_white, df_black)
  df_combined <- df_combined[order(df_combined$Move), ]  # Sort by Move
  
  # Calculate cumulative time for each player
  df_combined$CumulativeTime <- cumsum(df_combined$Duration)
  
  # Calculate cumulative start and end times
  df_combined$cumulative_start_time <- c(0, head(df_combined$CumulativeTime, -1))
  df_combined$cumulative_end_time <- df_combined$CumulativeTime
  
  # Calculate total time used in the game
  total_time_used <- max(df_combined$CumulativeTime)  # Total time for the game (max cumulative time)
  
  # Filter for moves below 40
  df_combined <- df_combined[df_combined$Move < 40, ]
  
  # Calculate CumulativeTime_Player and CumulativeTime_left_Player
  df_combined$CumulativeTime_Player <- 0
  cumulative_time_white <- 0
  cumulative_time_black <- 0
  
  for (i in 1:nrow(df_combined)) {
    if (df_combined$Player[i] == "White") {
      cumulative_time_white <- cumulative_time_white + df_combined$Duration[i]
      df_combined$CumulativeTime_Player[i] <- cumulative_time_white
    } else if (df_combined$Player[i] == "Black") {
      cumulative_time_black <- cumulative_time_black + df_combined$Duration[i]
      df_combined$CumulativeTime_Player[i] <- cumulative_time_black
    }
  }
  
  # Calculate time left for each player
  df_combined$CumulativeTime_left_Player <- 120 - df_combined$CumulativeTime_Player
  
  # Extract header information
  header_info <- gsub("\\[|\\]", "", regmatches(pgn_content, gregexpr("\\[.*?\\]", pgn_content))[[1]])
  title <- ""
  subtitle <- ""
  result <- ""
  
  # Parse header info
  for (line in header_info) {
    tokens <- strsplit(line, "\\s+", perl = TRUE)[[1]]
    key <- tokens[1]
    value <- paste(tokens[-1], collapse = " ")
    
    if (key == "Event") title <- value
    if (key == "Round") round_number <- value
    if (key == "Result") result <- value
    if (key == "White") white_player <- value
    if (key == "Black") black_player <- value
  }
  
  result <- gsub("\"", "", result)
  results <- unlist(strsplit(result, '-'))
  subtitle <- paste("White:", white_player, "(", results[1], ")  vs.  Black:", black_player, '(', results[2], ')')
  title <- paste(title, "Round", round_number)
  
  df_combined<-df_combined[order(-df_combined$Min_Start),]
  df_combined<-df_combined[order(df_combined$Move),]
  row.names(df_combined)<-1:nrow(df_combined)
  
  # Generate cumulative time plot
  cumulative_plot <- ggplot(df_combined, aes(x = CumulativeTime, y = CumulativeTime_left_Player, color = Player)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("White" = "#cdcdcd", "Black" = "#333333")) +
    labs(title = title, subtitle = subtitle, x = "", y = "Time Remaining (min)") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +  # Red line at start
    geom_vline(xintercept = total_time_used, color = "red", linetype = "dashed") +  # Red line at end based on total time
    theme_minimal() +
    theme(plot.margin = margin(20, 20, 0, 20),
          legend.title = element_blank(),
          panel.background = element_rect(fill = "#f9f9f9"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none") + 
    xlim(0, total_time_used) +  # Dynamically set x-axis limit
    scale_x_continuous(breaks = seq(0, total_time_used, by = 10))  
  cumulative_plot
  
  
  # Generate the timeline plot
  timeline_plot <- ggplot(df_combined, aes(x = cumulative_start_time, xend = cumulative_end_time, y = 0.5, yend = 0.5, color = Player)) +
    geom_segment(size = 25) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +  # Red line at start
    geom_vline(xintercept = total_time_used, color = "red", linetype = "dashed") +  # Red line at end based on total time
    scale_color_manual(values = c("White" = "#cdcdcd", "Black" = "#333333")) +
    labs(x = "Game Time (min)", y = "") +
    xlim(0, total_time_used) +  # Dynamically set x-axis limit
    scale_x_continuous(breaks = seq(0, total_time_used, by = 10)) +
    theme_minimal() +
    theme(plot.margin = margin(0, 20, 10, 38),
          panel.background = element_rect(fill = "#f9f9f9"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
  
  # Combine both plots
  grid_plot <- grid.arrange(cumulative_plot, timeline_plot, ncol = 1, heights = c(6, 2))
  print(grid_plot)
  dev.off()
}
