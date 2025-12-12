library(plotly)
library(shiny)
library(ggplot2)
library(dplyr)


ts_plot <- function(select.var) {
  if (select.var == "Annual average temperature anomaly") {
    y.title <- paste0(select.var, " (baseline: start of the record to 1992)\nwith 10-yr moving average")
    y.label <- "Temperature anomaly (ºF / ºC)"
    y.color <- "#FF6600"
    var.name <- "Avg.Temp"
    add_celsius_axis <- TRUE
  } else {
    y.title <- paste0(select.var, " (baseline: start of the record to 1992)\nwith 10-year moving average")
    y.label <- "Precipitation change (%)"
    y.color <- "#0066FF"
    var.name <- "Tot.Prcp"
    add_celsius_axis <- FALSE
  }
  
  hist.anomaly.MA.sum <- read.csv(paste0("./Data/Annual.MA.sum_2024/", var.name, ".csv"))
  hist.anomaly.MA.reshape <- reshape2::melt(hist.anomaly.MA.sum, id.var = "Year", value.name = var.name)
  colnames(hist.anomaly.MA.reshape) <- c("year", "City", "obs")
  hist.anomaly.MA.reshape <- dplyr::filter(hist.anomaly.MA.reshape, !is.na(obs))
  hist.anomaly.MA.reshape$City <- gsub("[.]", " ", hist.anomaly.MA.reshape$City)
  
  hist.anomaly.MA.sql <- highlight_key(hist.anomaly.MA.reshape, ~ City)
  
  # Calculate x axis ticks every 20 years
  year_range <- range(hist.anomaly.MA.reshape$year, na.rm = TRUE)
  x_ticks <- seq(ceiling(year_range[1] / 20) * 20, floor(year_range[2] / 20) * 20, by = 20)
  
  ts.p <- plot_ly(hist.anomaly.MA.sql, color = I(y.color), alpha = 0.75) %>%
    group_by(City) %>%
    add_lines(
      x = ~year, y = ~obs, name = y.label,
      hoverinfo = "text",
      text = ~paste("Year:", year, "<br>City:", City, "<br>Value:", obs)
    )
  
  if (add_celsius_axis) {
    y_range_f <- range(hist.anomaly.MA.reshape$obs, na.rm = TRUE)
    y_ticks_f <- pretty(y_range_f)
    y_ticks_c <- round(y_ticks_f * 5/9, 1)
    y_tick_labels <- paste0(y_ticks_f, "°F\n", y_ticks_c, "°C")
    obs.ts <- ts.p %>% layout(
      title = y.title,
      xaxis = list(title = "", tickvals = x_ticks, ticktext = x_ticks,
                   tickfont  = list(size = 13)),
      yaxis = list(
        title = y.label,
        side = "left",
        tickvals = y_ticks_f,
        ticktext = y_tick_labels,
        tickfont = list(size = 13)
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  } else {
    obs.ts <- ts.p %>% layout(
      title = y.title,
      xaxis = list(title = "", tickvals = x_ticks, ticktext = x_ticks,
                   tickfont  = list(size = 13)),
      yaxis = list(title = y.label,
                   tickfont  = list(size = 13)),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  }
  
  highlight(
    obs.ts, 
    on = "plotly_hover",
    selectize = FALSE, 
    dynamic = FALSE,
    color = "red",
    persistent = FALSE
  )
}
var.list <- c("Annual average temperature anomaly", "Annual total precipitation change")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Make the app title (which is an h2 by default) smaller */
      .title {
        font-size: 0.3em; /* Adjust this value as desired */
        line-height: 1.2;
      }

      /* Make the text/label above the select input bigger */
      .control-label {
        font-size: 1.3em; /* Adjust this value as desired */
        font-weight: bold; /* Optional: Make label bold */
      }

      /* Make the text within the dropdown menu selection bigger */
      .selectize-input {
        font-size: 1.2em;
        padding: 8px 12px;
        height: auto;
        width: 400px !important; /* Set a specific pixel width using CSS */
      }

    "))
  ),
  titlePanel("Historical annual temperature and precipitation changes at selected U.S. cities (interactive)"),
  selectInput("var", "Select the annual climate variable", choices = var.list, selected = var.list[1]),
  shinycssloaders::withSpinner(
    plotlyOutput("plot", width = "100%", height = "60vh"), 
    hide.ui = FALSE, type = 3, color = "#666666", color.background = "#FFFFFF"
  )  
)

server <- function(input, output, session) {

  output$plot <- renderPlotly({
    ts_plot(input$var)})
  
}

shinyApp(ui = ui, server = server)


# Processing new data -----------------------------------------------------


# library(dplyr)
# 
# # Your city list
# city.list <- c("Montgomery.AL", "Mobile.AL", "Phoenix.AZ", "Flagstaff.AZ", "Fort.Smith.AR", "Sacramento.CA",
#                "Los.Angeles.CA", "Fresno.CA", "Eureka.CA", "Grand.Junction.CO", "Denver.CO", "Colorado.Springs.CO",
#                "Tampa.FL", "Pensacola.FL", "Jacksonville.FL", "Washington.DC.DC", "Savannah.GA", "Augusta.GA",
#                "Atlanta.GA", "Boise.ID", "Springfield..IL", "Moline.IL", "Chicago.IL", "Indianapolis.IN",
#                "Evansville.IN", "Cedar.Rapids.IA", "Dubuque.IA", "Des.Moines.IA", "Topeka.KS", "Dodge.City.KS",
#                "Concordia.KS", "Louisville.KY", "Shreveport.LA", "Portland.ME", "Baltimore.MD", "Boston.MA",
#                "Sault.Ste.Marie.MI", "Marquette.MI", "Lansing.MI", "Grand.Rapids.MI", "Detroit.MI", "Minneapolis.MN",
#                "Duluth.MN", "Meridian.MS", "Springfield.MO", "Kansas.City.MO", "Helena.MT", "Havre.MT",
#                "Valentine.NE", "Omaha.NE", "North.Platte.NE", "Winnemucca.NV", "Concord.NH", "Roswell.NM",
#                "Albuquerque.NM", "Rochester.NY", "New.York.NY", "Albany.NY", "Raleigh.NC", "Charlotte.NC",
#                "Williston.ND", "Fargo.ND", "Bismarck.ND", "Columbus.OH", "Oklahoma.OK", "Pittsburgh.PA",
#                "Philadelphia.PA", "Middletown.Harrisburg.PA", "Erie.PA", "Columbia.SC", "Sioux.Falls.SD", "Huron.SD",
#                "Nashville.TN", "Memphis.TN", "Chattanooga.TN", "San.Antonio.TX", "Houston.TX", "Dallas.Fort.Worth.TX",
#                "Corpus.Christi.TX", "Amarillo.TX", "Abilene.TX", "Salt.Lake.City.UT", "Norfolk.VA", "Lynchburg.VA",
#                "Spokane.WA", "Seattle.WA", "Eau.Claire.WI", "Milwaukee.WI", "Madison.WI", "La.Crosse.WI",
#                "Green.Bay.WI", "Lander.WY", "Cheyenne.WY")
# 
# data_dir <- "Data/GHCN-D/Annual_2024/"
# 
# special_cases <- c("Rochester.NY", "Columbus.OH", "Columbia.SC", "Norfolk.VA")
# 
# city_names <- sapply(city.list, function(x) {
#   if (x %in% special_cases) {
#     gsub("[.]", "", x)  # Keep city and state, remove dots
#   } else {
#     gsub("[.]", "", sub("\\.[^.]+$", "", x))  # Remove last .State, then remove all dots
#   }
# })
# all_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
# 
# # Initialize lists to store data
# city_temps <- list()
# city_precip <- list()
# 
# for (i in seq_along(city.list)) {
#   city <- city.list[i]
#   city_name <- city_names[i]
#   file_match <- all_files[grepl(paste0("^", city_name, "\\.csv$"), basename(all_files), ignore.case = TRUE)]
#   if (length(file_match) == 1) {
#     city_data <- read.csv(file_match)
#     # Assign NA where QA fails, keep all years
#     city_data$Avg.Temp.QA <- ifelse(city_data$Miss.Tmax < 10 & city_data$Miss.Tmin < 10, city_data$Avg.Temp, NA)
#     city_data$Tot.Prcp.QA <- ifelse(city_data$Miss.Prcp < 10, city_data$ToT.Prcp, NA)
#     # Store as named vectors (names = years)
#     city_temps[[city]] <- setNames(city_data$Avg.Temp.QA, city_data$Year)
#     city_precip[[city]] <- setNames(city_data$Tot.Prcp.QA, city_data$Year)
#   } else if (length(file_match) > 1) {
#     warning(paste("Multiple files found for", city_name, ":", paste(file_match, collapse = ", ")))
#     city_temps[[city]] <- NULL
#     city_precip[[city]] <- NULL
#   } else {
#     warning(paste("No file found for", city_name))
#     city_temps[[city]] <- NULL
#     city_precip[[city]] <- NULL
#   }
# }
# 
# all_years <- sort(unique(unlist(lapply(city_temps, names))))
# df_temp <- data.frame(Year = as.integer(all_years))
# df_precip <- data.frame(Year = as.integer(all_years))
# for (city in city.list) {
#   df_temp[[city]] <- as.numeric(city_temps[[city]][as.character(df_temp$Year)])
#   df_precip[[city]] <- as.numeric(city_precip[[city]][as.character(df_precip$Year)])
# }
# # For temperature
# baseline_temp <- sapply(city.list, function(city) {
#   city_vec <- df_temp[[city]]
#   year_vec <- df_temp$Year
#   mean(city_vec[!is.na(city_vec) & year_vec <= 1994], na.rm = TRUE)
# })
# 
# # For precipitation
# baseline_precip <- sapply(city.list, function(city) {
#   city_vec <- df_precip[[city]]
#   year_vec <- df_precip$Year
#   mean(city_vec[!is.na(city_vec) & year_vec <= 1994], na.rm = TRUE)
# })
# 
# # Temperature: absolute change
# df_temp_change <- df_temp
# for (city in city.list) {
#   baseline <- baseline_temp[city]
#   df_temp_change[[city]] <- df_temp[[city]] - baseline
# }
# 
# # Precipitation: percent change
# df_precip_change <- df_precip
# for (city in city.list) {
#   baseline <- baseline_precip[city]
#   if (!is.na(baseline) && baseline != 0) {
#     df_precip_change[[city]] <- 100 * (df_precip[[city]] - baseline) / baseline
#   } else {
#     df_precip_change[[city]] <- NA
#   }
# }
# 
# # For temperature
# df_temp_ma <- df_temp_change
# for (city in city.list) {
#   df_temp_ma[[city]] <- zoo::rollapply(df_temp_change[[city]], width = 10, FUN = mean, align = "right", fill = NA, na.rm = FALSE)
# }
# 
# # For precipitation
# df_precip_ma <- df_precip_change
# for (city in city.list) {
#   df_precip_ma[[city]] <- zoo::rollapply(df_precip_change[[city]], width = 10, FUN = mean, align = "right", fill = NA, na.rm = FALSE)
# }
# 
# df_temp_ma[] <- lapply(df_temp_ma, function(x) {
#   x <- round(x, 3)
#   x[is.nan(x)] <- NA
#   x
# })
# 
# df_precip_ma[] <- lapply(df_precip_ma, function(x) {
#   x <- round(x, 1)
#   x[is.nan(x)] <- NA
#   x
# })
# 
# # Optional: Save results
# write.csv(df_temp_ma, "Avg.Temp.csv", row.names = FALSE)
# write.csv(df_precip_ma, "Tot.Prcp.csv", row.names = FALSE)
