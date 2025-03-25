# Copyright 2025 Province of British Columbia
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# r/db_module/db_functions.R

#CHECK FOR MISSING ENTRIES FUNCTION
check_for_missing_entries <- function(data, buffer_minutes = 2) {
  missing_entries_info <- data.frame(
    Table = character(),
    MissingDateTime = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
  
  for (table_name in names(data)) {
    df <- data[[table_name]]
    
    if (nrow(df) > 1) {
      # Convert timestamps to POSIXct
      df$DateTimeNum <- as.POSIXct(df$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
      df <- df %>% arrange(DateTimeNum)
      
      # Create sequence of expected hourly timestamps
      # Now we don't subtract an hour when creating the sequence
      sequence_start <- floor_date(min(df$DateTimeNum), "hour")
      sequence_end <- ceiling_date(max(df$DateTimeNum), "hour")
      
      expected_times <- seq(from = sequence_start,
                          to = sequence_end,
                          by = "hour")
      
      # Modified check that doesn't add an hour to the comparison
      is_time_present <- function(time) {
        any(df$DateTimeNum >= time - minutes(buffer_minutes) &
            df$DateTimeNum <= time + minutes(buffer_minutes))
      }
      
      # Find missing timestamps
      missing_times <- expected_times[!sapply(expected_times, is_time_present)]
      
      # Remove the first expected time if it's missing (to avoid false positive)
      if (length(missing_times) > 0 && missing_times[1] == expected_times[1]) {
        missing_times <- missing_times[-1]
      }
      
      if (length(missing_times) > 0) {
        missing_entries_info <- rbind(missing_entries_info, data.frame(
          Table = table_name,
          MissingDateTime = missing_times,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(missing_entries_info) > 0) {
    missing_entries_info
  } else {
    "No missing entries detected."
  }
}
# CHECK FOR BLANKS FUNCTION
check_for_blanks <- function(data_list) {
  # Initialize results dataframe
  blank_info <- data.frame(
    Table = character(),
    Column = character(),
    Row = integer(),
    Issue = character(),
    Value = character(),
    DateTime = character(),
    stringsAsFactors = FALSE
  )
  
  # Helper function to safely convert to numeric
  safe_as_numeric <- function(x) {
    result <- suppressWarnings(as.numeric(x))
    if (all(is.na(result))) return(x)
    return(result)
  }
  
  # Helper function to check for special NA values
  is_special_na <- function(x) {
    special_nas <- c("NA", "N/A", "n/a", "NULL", "null", "<NA>", "#N/A", "NaN", "")
    if (is.character(x)) {
      return(trimws(x) %in% special_nas | trimws(x) == "")
    }
    return(FALSE)
  }
  
  # First pass: identify active sensors for each station
  for (table_name in names(data_list)) {
    data <- data_list[[table_name]]
    if (is.null(data) || nrow(data) == 0) next
    
    # Get all columns that have actual data (excluding standard metadata columns)
    metadata_columns <- c("site_id", "STATION", "FIRE_CENTRE", "FIRE_ZONE", 
                         "LATITUDE", "LONGITUDE", "ELEVATION", "DateTimeNum")
    
    data_columns <- setdiff(names(data), metadata_columns)
    
    # Check each relevant column
    for (col in data_columns) {
      # Convert to numeric if possible
      values <- safe_as_numeric(data[[col]])
      
      # Check each row
      for (row in 1:nrow(data)) {
        value <- values[row]
        
        # Initialize issue as NULL
        issue <- NULL
        
        # Series of checks
        if (is.na(value)) {
          issue <- "NA value"
        } else if (is.nan(value)) {
          issue <- "NaN value"
        } else if (is.character(value) && trimws(value) == "") {
          issue <- "Empty string"
        } else if (is.character(value) && is_special_na(value)) {
          issue <- "Special NA value"
        } else if (is.infinite(value)) {
          issue <- if (value > 0) "Positive infinity" else "Negative infinity"
        } else if (is.character(value) && grepl("^\\s+$", value)) {
          issue <- "Whitespace only"
        }
        
        # Check for invalid numeric values in numeric columns
        if (is.null(issue) && !is.character(values)) {
          numeric_value <- suppressWarnings(as.numeric(value))
          if (is.na(numeric_value) && !is.na(value)) {
            issue <- "Invalid numeric value"
          }
        }
        
        # Add to results if an issue was found
        if (!is.null(issue)) {
          blank_info <- rbind(blank_info, data.frame(
            Table = table_name,
            Column = col,
            Row = row,
            Issue = issue,
            Value = as.character(value),
            DateTime = as.character(data[row, "DateTimeNum"]),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Return results
  if (nrow(blank_info) > 0) {
    # Sort by Table, DateTime, and Column for better readability
    blank_info <- blank_info[order(blank_info$Table, blank_info$DateTime, blank_info$Column), ]
    return(blank_info)
  } else {
    return("No blank or invalid values found in available columns.")
  }
}