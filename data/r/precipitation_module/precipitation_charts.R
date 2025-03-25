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

# r/precipitation_module/precipitation_charts.R

create_rn1_outliers_plot <- function(WX_stations, all_rn1_outliers, selected_station) {
  # Handle edge case: No station selected
  if (is.null(selected_station) || selected_station == "All Stations") {
    return(
      plot_ly() %>%
        add_annotations(
          text = "Please select a specific station to view outliers",
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  # Handle edge case: No data for the selected station
  if (!selected_station %in% names(WX_stations) || 
      is.null(WX_stations[[selected_station]]) || 
      nrow(WX_stations[[selected_station]]) == 0) {
    return(
      plot_ly() %>%
        add_annotations(
          text = paste("No data available for", selected_station),
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  # Get data for the selected station
  station_data <- WX_stations[[selected_station]]
  
  # Check if we have the required columns
  required_cols <- c("DateTimeNum", "Rn_1", "Temp", "Rh")
  if (!all(required_cols %in% colnames(station_data))) {
    missing_cols <- required_cols[!required_cols %in% colnames(station_data)]
    return(
      plot_ly() %>%
        add_annotations(
          text = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  # Ensure data is properly formatted
  station_data <- station_data %>%
    arrange(DateTimeNum) %>%
    mutate(
      Rn_1 = as.numeric(Rn_1),
      Temp = as.numeric(Temp),
      Rh = as.numeric(Rh),
      DateTimeNum = as.POSIXct(DateTimeNum, format = "%Y-%b-%d %H:%M:%S")
    )
  
  # Handle edge case: No outliers for the selected station
  has_outliers <- FALSE
  if (selected_station %in% names(all_rn1_outliers) && 
      !is.null(all_rn1_outliers[[selected_station]]) && 
      nrow(all_rn1_outliers[[selected_station]]) > 0) {
    outliers_data <- all_rn1_outliers[[selected_station]]
    outliers_data$DateTimeNum <- as.POSIXct(outliers_data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S")
    has_outliers <- TRUE
  }
  
  # Define colors from viridis palette
  colors <- viridis(3, begin = 0, end = 0.8)
  
  # Create the plot
  p <- plot_ly() %>%
    # Add temperature line
    add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~Temp,
      type = "scatter",
      mode = "lines",
      name = "Temperature (°C)",
      line = list(color = colors[1], width = 2),
      hoverinfo = "text",
      text = ~paste(
        "Time: ", format(DateTimeNum, "%Y-%m-%d %H:%M"),
        "<br>Temp: ", round(Temp, 1), "°C",
        "<br>RH: ", round(Rh, 1), "%",
        "<br>Precip: ", round(Rn_1, 2), "mm"
      ),
      yaxis = "y"
    ) %>%
    # Add RH line
    add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~Rh,
      type = "scatter",
      mode = "lines",
      name = "Relative Humidity (%)",
      line = list(color = colors[2], width = 2),
      hoverinfo = "text",
      text = ~paste(
        "Time: ", format(DateTimeNum, "%Y-%m-%d %H:%M"),
        "<br>Temp: ", round(Temp, 1), "°C",
        "<br>RH: ", round(Rh, 1), "%",
        "<br>Precip: ", round(Rn_1, 2), "mm"
      ),
      yaxis = "y2"
    ) %>%
    # Add precipitation line
    add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~Rn_1,
      type = "scatter",
      mode = "lines",
      name = "Precipitation (mm)",
      line = list(color = colors[3], width = 2),
      hoverinfo = "text",
      text = ~paste(
        "Time: ", format(DateTimeNum, "%Y-%m-%d %H:%M"),
        "<br>Temp: ", round(Temp, 1), "°C",
        "<br>RH: ", round(Rh, 1), "%",
        "<br>Precip: ", round(Rn_1, 2), "mm"
      ),
      yaxis = "y3"
    )
  
  # Add outliers if they exist
  if (has_outliers) {
    p <- p %>%
      add_trace(
        data = outliers_data,
        x = ~DateTimeNum,
        y = ~Rn_1,
        type = "scatter",
        mode = "markers",
        name = "Outliers",
        marker = list(
          color = "red",
          size = 10,
          symbol = "diamond"
        ),
        hoverinfo = "text",
        text = ~paste(
          "OUTLIER",
          "<br>Time: ", format(DateTimeNum, "%Y-%m-%d %H:%M"),
          "<br>Value: ", round(Rn_1, 2), "mm",
          "<br>Avg: ", round(RunningAvg, 2), "mm",
          "<br>Temp: ", round(Temp, 1), "°C",
          "<br>RH: ", round(Rh, 1), "%"
        ),
        yaxis = "y3"
      )
  }
  
  # Create layout with multiple axes
  p <- p %>%
    layout(
      title = paste("Precipitation Analysis with Outliers -", selected_station),
      xaxis = list(
        title = "Time",
        gridcolor = "rgba(0,0,0,0.1)",
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Temperature (°C)",
        side = "left",
        gridcolor = "rgba(0,0,0,0.1)",
        zeroline = FALSE
      ),
      yaxis2 = list(
        title = "Relative Humidity (%)",
        side = "right",
        overlaying = "y",
        gridcolor = "rgba(0,0,0,0)",
        rangemode = "tozero",
        zeroline = FALSE,
        range = c(0, 100)
      ),
      yaxis3 = list(
        title = "Precipitation (mm)",
        side = "right",
        overlaying = "y",
        anchor = "free",
        position = 0.95,
        gridcolor = "rgba(0,0,0,0)",
        rangemode = "tozero",
        zeroline = FALSE
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.2,
        xanchor = "center"
      ),
      margin = list(
        r = 100,  # Extra right margin for second y-axis
        t = 60,   # Top margin for title
        b = 100   # Bottom margin for legend
      ),
      hovermode = "closest"
    )
  
  return(p)
}