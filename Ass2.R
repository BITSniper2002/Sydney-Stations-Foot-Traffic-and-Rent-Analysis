# app.R — Sydney Stations Foot Traffic (CSV-backed starter)
# -------------------------------------------------------------------
# This starter app reads your uploaded CSV: entry_exit_sydney_merged.csv
# and builds a clean, extensible dashboard without needing simulated data.
# Later we can add rent, weather, facilities, map, etc.

# ----------------------------
# Packages
# ----------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(janitor)
# (Optional) for cleaning names; install.packages("janitor") if needed
suppressWarnings({
  if (!requireNamespace("janitor", quietly = TRUE)) {
    message("Consider install.packages('janitor') for robust name cleaning")
  }
})

# ----------------------------
# 0) Helpers
# ----------------------------
nice_num <- function(x) label_number_si(accuracy = 0.1)(x)

six_month_slope <- function(d) {
  # slope of last 6 points: d must have columns date, trips
  d <- arrange(d, date)
  if (nrow(d) < 6) return(NA_real_)
  d_tail <- tail(d, 6)
  idx <- seq_len(nrow(d_tail))
  as.numeric(coef(lm(trips ~ idx, data = d_tail))[2])
}

# Compute YoY growth for an aggregate time series (date, trips)
yoy_growth_pct <- function(ts_df) {
  ts_df <- arrange(ts_df, date)
  if (nrow(ts_df) < 13) return(NA_real_)
  last_date <- max(ts_df$date)
  prev_date <- last_date %m-% months(12)
  last_val <- ts_df$trips[ts_df$date == last_date]
  prev_val <- ts_df$trips[ts_df$date == prev_date]
  if (length(last_val) == 0 || length(prev_val) == 0) return(NA_real_)
  (sum(last_val) / sum(prev_val)) - 1
}

# Build a metric-specific dataset from the raw long table
# metric is one of: "Total", "Entries", "Exits"
make_metric_df <- function(d, metric = "Total") {
  d <- d %>% mutate(entry_exit = tolower(entry_exit))
  if (metric == "Entries") {
    d <- d %>% filter(entry_exit == "entry")
  } else if (metric == "Exits") {
    d <- d %>% filter(entry_exit == "exit")
  }
  d %>% group_by(station, date) %>% summarise(trips = sum(trip, na.rm = TRUE), .groups = "drop")
}

# ----------------------------
# 1) Load YOUR dataset
# ----------------------------
# Set this path to where your CSV lives. If it's beside app.R, you can leave as-is.
data_path <- "entry_exit_sydney_merged.csv"

candidate_paths <- c(
  "entry_exit_sydney_merged.csv",
  "./entry_exit_sydney_merged.csv",
  "./datasets/entry_exit_sydney_merged.csv",
  "/mnt/data/entry_exit_sydney_merged.csv"
)
if (!file.exists(data_path)) {
  alt <- candidate_paths[file.exists(candidate_paths)]
  if (length(alt) == 0) {
    stop("Could not find 'entry_exit_sydney_merged.csv'. Tried: ", paste(candidate_paths, collapse = ", "))
  }
  data_path <- alt[[1]]
}

raw_read <- readr::read_csv(data_path, show_col_types = FALSE)
raw_read <- janitor::clean_names(raw_read)
# Clean names if janitor is available
if (requireNamespace("janitor", quietly = TRUE)) {
  raw_read <- janitor::clean_names(raw_read)
} else {
  clean_names_basic <- function(nm) {
    nm <- gsub("[^A-Za-z0-9]+", "_", nm)
    tolower(nm)
  }
  names(raw_read) <- clean_names_basic(names(raw_read))
}

# Expecting columns: station, entry_exit, trip, month, year
required_cols <- c("station", "entry_exit", "trip", "month", "year")
missing <- setdiff(required_cols, names(raw_read))
if (length(missing) > 0) {
  stop(sprintf("Missing required columns in %s: %s", data_path, paste(missing, collapse = ", ")))
}

# Coerce types & build a proper Date for first of month
raw_tbl <- raw_read %>%
  mutate(
    station    = as.character(station),
    entry_exit = as.character(entry_exit),
    trip       = as.numeric(trip),
    month      = as.integer(month),
    year       = as.integer(year),
    date       = as.Date(sprintf("%04d-%02d-01", year, pmax(1, pmin(12, month))))
  ) %>%
  filter(!is.na(trip), !is.na(date), !is.na(station))

# Available date range
date_min <- min(raw_tbl$date, na.rm = TRUE)
date_max <- max(raw_tbl$date, na.rm = TRUE)

# ----------------------------
# 2) UI
# ----------------------------
app_theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- fluidPage(
  theme = app_theme,
  titlePanel("Sydney Stations — Foot Traffic Explorer (CSV Starter)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      dateRangeInput(
        "date_range", "Date range",
        start = date_min, end = date_max,
        min = date_min, max = date_max,
        format = "dd M yyyy", startview = "year"
      ),
      selectInput("metric", "Metric", choices = c("Total", "Entries", "Exits"), selected = "Total"),
      numericInput("top_n", "Top N stations (Overview)", value = 10, min = 1, max = 30, step = 1),
      hr(),
      selectInput("profile_station", "Station profile", choices = sort(unique(raw_tbl$station))),
      selectizeInput("compare_stations", "Compare stations", multiple = TRUE, choices = sort(unique(raw_tbl$station)))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Overview",
                 fluidRow(
                   column(
                     4, bslib::value_box(
                       title = "Total trips (selected)",
                       value = textOutput("vb_total"),
                       showcase = icon("person-walking")
                     )
                   ),
                   column(
                     4, bslib::value_box(
                       title = "YoY growth (latest month)",
                       value = textOutput("vb_yoy"),
                       showcase = icon("chart-line")
                     )
                   ),
                   column(
                     4, bslib::value_box(
                       title = "Stations with ↑ trend (6-mo)",
                       value = textOutput("vb_rising"),
                       showcase = icon("arrow-trend-up")
                     )
                   )
                 ),
                 br(),
                 plotlyOutput("top_bar", height = "420px"),
                 br(),
                 plotlyOutput("agg_ts", height = "300px")
        ),
        tabPanel("Station profile",
                 plotlyOutput("profile_ts", height = "420px"),
                 br(),
                 plotlyOutput("profile_breakdown", height = "300px")
        ),
        tabPanel("Compare",
                 plotlyOutput("cmp_ts", height = "520px")
        ),
        tabPanel("Data",
                 DTOutput("table")
        )
      )
    )
  )
)

# ----------------------------
# 3) SERVER
# ----------------------------
server <- function(input, output, session) {
  
  # Metric-specific dataset (reactive)
  df_metric <- reactive({
    make_metric_df(raw_tbl, input$metric)
  })
  
  # Filtered by date range
  df <- reactive({
    req(input$date_range)
    df_metric() %>% filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  # Also keep an entries/exits breakdown for the Station profile breakdown plot
  df_breakdown <- reactive({
    raw_tbl %>%
      mutate(entry_exit = factor(tolower(entry_exit), levels = c("entry", "exit"))) %>%
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
      group_by(station, date, entry_exit) %>%
      summarise(trips = sum(trip), .groups = "drop")
  })
  
  # Update station choices based on filters
  observe({
    st_choices <- df() %>% distinct(station) %>% arrange(station) %>% pull()
    if (length(st_choices)) {
      updateSelectInput(session, "profile_station", choices = st_choices, selected = st_choices[1])
      updateSelectizeInput(session, "compare_stations", choices = st_choices, server = TRUE)
    }
  })
  
  # Latest date within range
  latest_date <- reactive({ df() %>% summarise(m = max(date)) %>% pull(m) })
  
  # ---------------- KPIs ----------------
  output$vb_total <- renderText({
    d <- df()
    if (nrow(d) == 0 || all(is.na(d$trips))) return("0")
    total <- sum(d$trips, na.rm = TRUE)
    nice_num(total)
  })
  
  output$vb_yoy <- renderText({
    agg <- df() %>% group_by(date) %>% summarise(trips = sum(trips), .groups = "drop")
    g <- yoy_growth_pct(agg)
    if (is.na(g)) "N/A" else percent(g, accuracy = 0.1)
  })
  
  output$vb_rising <- renderText({
    rising <- df() %>%
      arrange(date) %>%
      group_by(station) %>%
      summarise(slope6 = six_month_slope(cur_data_all()), .groups = "drop") %>%
      summarise(n = sum(slope6 > 0, na.rm = TRUE)) %>% pull(n)
    format(rising, big.mark = ",")
  })
  
  # ---------------- Overview: Top stations (latest month) ----------------
  output$top_bar <- renderPlotly({
    ld <- latest_date()
    validate(need(!is.na(ld), "No data in selection."))
    validate(need(is.numeric(input$top_n) && input$top_n > 0 && input$top_n <= 30, "Top N must be between 1 and 30"))
    
    top_df <- df() |>
      filter(date == ld) |>
      group_by(station) |>
      summarise(trips = sum(trips), .groups = "drop") |>
      slice_max(trips, n = max(1, min(30, as.integer(input$top_n))))
    
    if (nrow(top_df) == 0) return(plotly_empty())
    
    plot_ly(top_df,
            x = ~trips,
            y = ~reorder(station, trips),
            type = 'bar',
            orientation = 'h',
            text = ~paste0('<b>', station, '</b><br>Trips (', format(ld, '%b %Y'), '): ', scales::comma(trips)),
            hoverinfo = 'text') |>
      layout(
        xaxis = list(title = paste0(input$metric, ' trips — latest month')),
        yaxis = list(title = NULL),
        margin = list(l = 120, r = 20, t = 20, b = 20)
      )
  })
  
  # ---------------- Overview: Aggregate time series ----------------
  output$agg_ts <- renderPlotly({
    agg <- df() %>% group_by(date) %>% summarise(total_trips = sum(trips), .groups = "drop")
    plot_ly(agg, x = ~date, y = ~total_trips, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = paste0(input$metric, " trips (total)")), xaxis = list(title = NULL))
  })
  
  # ---------------- Station profile: time series ----------------
  output$profile_ts <- renderPlotly({
    req(input$profile_station)
    d <- df() %>% filter(station == input$profile_station)
    plot_ly(d, x = ~date, y = ~trips, type = "scatter", mode = "lines+markers",
            text = ~paste0("<b>", station, "</b><br>",
                           "Trips: ", comma(trips), "<br>",
                           format(date, "%b %Y")), hoverinfo = "text") %>%
      layout(yaxis = list(title = paste0(input$metric, " trips")), xaxis = list(title = NULL))
  })
  
  # ---------------- Station profile: entries vs exits breakdown ----------------
  output$profile_breakdown <- renderPlotly({
    req(input$profile_station)
    d <- df_breakdown() %>% filter(station == input$profile_station)
    if (nrow(d) == 0) return(NULL)
    plot_ly(d, x = ~date, y = ~trips, color = ~entry_exit, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = "Trips (entries vs exits)"), xaxis = list(title = NULL), legend = list(title = list(text = "Type")))
  })
  
  # ---------------- Compare stations ----------------
  output$cmp_ts <- renderPlotly({
    req(input$compare_stations)
    d <- df() %>% filter(station %in% input$compare_stations)
    plot_ly(d, x = ~date, y = ~trips, color = ~station, type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = paste0(input$metric, " trips")), xaxis = list(title = NULL), legend = list(title = list(text = "Station")))
  })
  
  # ---------------- Data table ----------------
  output$table <- renderDT({
    df() %>% arrange(desc(date), desc(trips)) %>%
      mutate(month = format(date, "%b %Y")) %>%
      select(month, station, trips) %>%
      datatable(options = list(pageLength = 25, order = list(list(0, 'desc'))), rownames = FALSE)
  })
}

# ----------------------------
# 4) Run app
# ----------------------------
shinyApp(ui, server)
