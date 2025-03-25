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

# crmp_module/crmp_charts.R

# Create a precipitation plot for CRMP stations
#' Create a plot of cumulative precipitation measurements over time
#'
#' @param WX_stations List of weather station data frames
#' @param selected_station The station identifier selected in the UI
#' @return A plotly object showing precipitation measurements over time
create_precip_plot <- function(WX_stations, selected_station) {
  # Initialize an empty plot with a message for when no data is available
  empty_plot <- plotly::plot_ly() %>% 
    plotly::add_annotations(
      text = "No precipitation data available for the selected station(s)",
      showarrow = FALSE,
      font = list(size = 15)
    )
  
  # Return empty plot if WX_stations is NULL or empty
  if (is.null(WX_stations) || length(WX_stations) == 0) {
    return(empty_plot)
  }
  
  # Return empty plot if "All Stations" is selected
  if (selected_station == "All Stations") {
    return(empty_plot)
  }
  
  # Check if selected station exists
  if (!selected_station %in% names(WX_stations)) {
    return(empty_plot)
  }
  
  # Get data for the selected station
  station_data <- WX_stations[[selected_station]]
  
  # Return empty plot if station data is NULL or empty
  if (is.null(station_data) || nrow(station_data) == 0) {
    return(empty_plot)
  }
  
  # Check for precipitation columns
  precip_sensors <- c("PrecipOP2", "PrecipOP1", "PrecipPC2", "PrecipRIT", "PC", "Pcp_raw")
  available_sensors <- intersect(precip_sensors, names(station_data))
  
  # Return empty plot if no precipitation data is available
  if (length(available_sensors) == 0) {
    return(empty_plot)
  }
  
  # Convert dates if needed
  if (is.character(station_data$DateTimeNum)) {
    station_data$DateTime <- as.POSIXct(station_data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S")
  } else {
    station_data$DateTime <- station_data$DateTimeNum
  }
  
  # Sort data chronologically
  station_data <- station_data[order(station_data$DateTime), ]
  
  # Initialize plot
  p <- plotly::plot_ly()
  has_data <- FALSE
  
  # Define color palette for different sensor types
  sensor_colors <- c(
    "PrecipOP2" = "#1f77b4",  # Blue
    "PrecipOP1" = "#ff7f0e",  # Orange
    "PrecipPC2" = "#2ca02c",  # Green
    "PrecipRIT" = "#d62728",  # Red
    "PC" = "#9467bd",         # Purple
    "Pcp_raw" = "#8c564b"     # Brown
  )
  
  # Add a trace for each available precipitation sensor
  for (sensor in available_sensors) {
    # Skip if no valid data
    if (!any(!is.na(station_data[[sensor]]))) next
    
    # Get user-friendly name for the sensor
    sensor_name <- switch(sensor,
                         "PrecipOP2" = "Optical Precip 2",
                         "PrecipOP1" = "Optical Precip 1",
                         "PrecipPC2" = "Precip Counter 2",
                         "PrecipRIT" = "RIT Precip",
                         "PC" = "Precip Counter",
                         "Pcp_raw" = "Raw Precip",
                         sensor)
    
    # Add trace for this sensor
    p <- p %>% plotly::add_trace(
      data = station_data,
      x = ~DateTime,
      y = as.formula(paste0("~", sensor)),
      type = "scatter",
      mode = "lines+markers",
      name = sensor_name,
      line = list(color = sensor_colors[sensor], width = 2),
      marker = list(size = 4, color = sensor_colors[sensor]),
      hoverinfo = "text",
      text = ~paste(
        "Station:", selected_station,
        "<br>Date:", format(DateTime, "%Y-%m-%d %H:%M"),
        "<br>", sensor_name, ":", round(get(sensor), 2), "mm"
      )
    )
    has_data <- TRUE
  }
  
  # Return empty plot if no valid precipitation data was found
  if (!has_data) {
    return(empty_plot)
  }
  
  # Finalize plot layout
  p <- p %>% plotly::layout(
    title = paste("Cumulative Precipitation -", selected_station),
    xaxis = list(title = "Date & Time"),
    yaxis = list(title = "Precipitation (mm)"),
    hovermode = "closest",
    legend = list(orientation = "h", y = -0.2)
  )
  
  return(p)
}


#' Create a snow depth plot
#' Create a plot of snow depth measurements over time
#'
#' @param WX_stations List of weather station data frames
#' @param selected_station The station identifier selected in the UI
#' @return A plotly object showing snow depth measurements over time
create_sdepth_plot <- function(WX_stations, selected_station) {
  # Initialize an empty plot with a message
  empty_plot <- plotly::plot_ly() %>% 
    plotly::add_annotations(
      text = "No snow depth data available for the selected station(s)",
      showarrow = FALSE,
      font = list(size = 15)
    )
  
  # Handle case when no data is available
  if (is.null(WX_stations) || length(WX_stations) == 0) {
    return(empty_plot)
  }
  
  # Return empty plot if "All Stations" is selected
  if (selected_station == "All Stations") {
    return(empty_plot)
  }
  
  # Determine station to plot
  if (!selected_station %in% names(WX_stations)) {
    return(empty_plot)
  }
  
  # Only plot the selected station
  stations_to_plot <- selected_station
  
  # Initialize plot
  p <- plotly::plot_ly()
  has_data <- FALSE
  
  # Process each selected station
  for (station_name in stations_to_plot) {
    # Skip if station doesn't exist in our data
    if (!station_name %in% names(WX_stations)) next
    
    station_data <- WX_stations[[station_name]]
    
    # Skip if no data available
    if (is.null(station_data) || nrow(station_data) == 0) next
    
    # Check for snow depth columns
    has_sdepth <- "SDepth" %in% names(station_data) && any(!is.na(station_data$SDepth))
    has_sd <- "SD" %in% names(station_data) && any(!is.na(station_data$SD))
    
    # Skip if no snow depth data
    if (!has_sdepth && !has_sd) next
    
    # Convert dates if needed
    if (is.character(station_data$DateTimeNum)) {
      station_data$DateTime <- as.POSIXct(station_data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S")
    } else {
      station_data$DateTime <- station_data$DateTimeNum
    }
    
    # Sort data chronologically
    station_data <- station_data[order(station_data$DateTime), ]
    
    # Add SDepth trace if available
    if (has_sdepth) {
      p <- p %>% plotly::add_trace(
        data = station_data,
        x = ~DateTime,
        y = ~SDepth,
        type = "scatter",
        mode = "lines+markers",
        name = paste(station_name, "- SDepth"),
        marker = list(size = 3),
        hoverinfo = "text",
        text = ~paste(
          "Station:", station_name,
          "<br>Date:", format(DateTime, "%Y-%m-%d %H:%M"),
          "<br>Snow Depth:", round(SDepth, 1), "cm"
        )
      )
      has_data <- TRUE
    }
    
    # Add SD trace if available
    if (has_sd) {
      p <- p %>% plotly::add_trace(
        data = station_data,
        x = ~DateTime,
        y = ~SD,
        type = "scatter",
        mode = "lines+markers",
        name = paste(station_name, "- SD"),
        marker = list(size = 3),
        hoverinfo = "text",
        text = ~paste(
          "Station:", station_name,
          "<br>Date:", format(DateTime, "%Y-%m-%d %H:%M"),
          "<br>Snow Depth:", round(SD, 1), "cm"
        )
      )
      has_data <- TRUE
    }
  }
  
  # Return empty plot if no data was found
  if (!has_data) {
    return(empty_plot)
  }
  
  # Finalize plot layout
  p <- p %>% plotly::layout(
    title = "Snow Depth Measurements",
    xaxis = list(title = "Date & Time"),
    yaxis = list(title = "Snow Depth (cm)"),
    hovermode = "closest",
    legend = list(orientation = "h", y = -0.2)
  )
  
  return(p)
}