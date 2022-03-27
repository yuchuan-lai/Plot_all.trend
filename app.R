library(plotly)

ts_plot <- function(select.var) {
  if (select.var == "Annual average temperature anomaly") {
    y.title <- paste0(select.var, "\napplied with 10-year moving average filter (start of the record to the end of 1992)")
    y.label <- "Temperature anomaly (ºF)"
    y.color <- "#FF6600"
    var.name <- "Avg.Temp"
  } else {
    y.title <- paste0(select.var, "\napplied with 10-year moving average filter (start of the record to the end of 1992)")
    y.label <- "Precipitation anomaly (in.)"
    y.color <- "#0066FF"
    var.name <- "Tot.Prcp"
  }
  
  hist.anomaly.MA.sum <- read.csv(paste0("./Data/Annual.MA.sum_2022/", var.name, ".csv"))[-c(1)]
  hist.anomaly.MA.reshape <- reshape2::melt(hist.anomaly.MA.sum, id.var = "year", value.name = var.name)
  colnames(hist.anomaly.MA.reshape) <- c("year", "City", "obs")
  hist.anomaly.MA.reshape <- filter(hist.anomaly.MA.reshape, is.na(obs) == FALSE)
  hist.anomaly.MA.reshape$City <- gsub("[.]", " ", hist.anomaly.MA.reshape$City)
  
  hist.anomaly.MA.sql <- highlight_key(hist.anomaly.MA.reshape, ~ City)
  plot_ly(hist.anomaly.MA.sql, color = I(y.color), alpha = 0.75) %>% group_by(City) -> ts.p
  ts.p %>% group_by(City) %>% add_lines(x = ~ year, y = ~ obs) -> obs.ts
  obs.ts %>% layout(title = y.title, xaxis = list(title = "Year", dtick = 10), yaxis = list(title =  y.label)) -> obs.ts
  
  highlight(
    obs.ts, 
    on = "plotly_hover",
    selectize = FALSE, 
    dynamic = FALSE,
    color = "red",
    persistent = FALSE
  )
}

var.list <- c("Annual average temperature anomaly", "Annual total precipitation anomaly")

ui <- fluidPage(
  titlePanel("Historical time series of annual temperature and precipitation anomalies at selected U.S. cities (interactive)"),
  
  selectInput("var", "Select the annual climate variable", choices = var.list, selected = var.list[1]),
  plotlyOutput("plot", width = "900px", height = "600px")
  
  
)

server <- function(input, output, session) {

  output$plot <- renderPlotly({
    shinycssloaders::withSpinner(
      ts_plot(input$var),
      hide.ui = FALSE, type = 3, color = "#666666", color.background = "#FFFFFF"
    )
  })
  
}

shinyApp(ui = ui, server = server)

