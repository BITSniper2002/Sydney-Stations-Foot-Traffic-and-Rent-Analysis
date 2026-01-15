# Sydney Train Usage and Rent Analysis Dashboard

An interactive Shiny web application for analyzing Sydney train station foot traffic patterns and their relationship with rental prices. This dashboard provides comprehensive visualizations and insights into station usage trends, facility information, and rent correlations.

## Features

### 📊 Overview Tab
- **Key Performance Indicators (KPIs)**:
  - Total trips for selected date range
  - Year-over-year growth percentage
  - Number of stations with rising trends (6-month slope)
- **Visualizations**:
  - Top N busiest stations bar chart (latest month)
  - Aggregate time series of total trips across date range
- **Interactive Controls**:
  - Date range selector
  - Metric selection (Total, Entries, Exits)
  - Top N stations selector (1-25)

### ☁️ Wordcloud Tab
- Visual representation of station prominence based on appearance counts
- Adjustable number of stations displayed (10-100)
- Dynamic sizing based on station frequency

### 🚉 Station Profile Tab
- **Individual Station Analysis**:
  - Time series visualization of trips over time
  - Entry vs Exit breakdown comparison
  - Facility information with icon display
- Station selector dropdown

### 🔄 Compare Tab
- **Multi-Station Comparison**:
  - Side-by-side time series comparison (up to 10 stations)
  - Weekly rent comparison table
  - Common and unique facilities analysis
- Clear all button for easy reset

### 💰 Rent vs Trips Tab
- **Correlation Analysis**:
  - Interactive scatter plot (log-scaled Y-axis)
  - Bubble size represents trip volume
  - Color-coded by rent amount
  - Adjustable bubble size scale
- Data table with sortable columns

### 📋 Data Tab
- Full data table with filtering and sorting capabilities
- Time-aware sorting (by date and trips)

## Installation

### Prerequisites
- R (version 4.0 or higher recommended)
- RStudio (optional, but recommended)

### Required R Packages
```r
install.packages(c(
  "shiny",
  "bslib",
  "tidyverse",
  "lubridate",
  "plotly",
  "DT",
  "scales",
  "janitor",
  "wordcloud2",
  "readxl",
  "wordcloud",
  "RColorBrewer"
))
```

### Setup
1. Clone this repository:
```bash
git clone <repository-url>
cd Ass2
```

2. Ensure data files are in the correct location:
   - `./datasets/entry_exit_sydney_merged.csv`
   - `./datasets/locationfacilitydata_filtered.csv`
   - `./datasets/sydney_train_metro_station_counts.csv`
   - `./datasets/rent.xlsx`
   - `./facilities/` directory with facility icon images

3. Run the application:
```r
# In R or RStudio
shiny::runApp("my_app")
```

Or directly:
```r
source("my_app/App.R")
```

## Data Requirements

The application expects the following data files:

### 1. Entry/Exit Data (`entry_exit_sydney_merged.csv`)
Required columns:
- `station`: Station name
- `entry_exit`: Entry/Exit indicator
- `trip`: Number of trips
- `month`: Month (1-12 or month name/abbreviation)
- `year`: Year (1900-2100)

### 2. Facility Data (`locationfacilitydata_filtered.csv`)
Required columns:
- `LOCATION_NAME`: Station name (will be renamed to `station`)
- `FACILITIES`: Pipe-separated list of facilities

### 3. Station Word Count (`sydney_train_metro_station_counts.csv`)
Required columns:
- `station_name`: Station name
- `total_appearance`: Frequency count

### 4. Rent Data (`rent.xlsx`)
Required columns:
- `station`: Station name
- `rent_aud_week`: Weekly rent in AUD

### 5. Facility Icons
Place facility icon images in `./facilities/` directory. Supported facilities include:
- Baby change table
- Bike lockers/racks/shed
- Commuter car park
- Emergency help point
- Mobile phone charging
- Opal card machines (various types)
- Toilets (including wheelchair accessible)
- And more...

## Usage

1. **Launch the application** - The app will open in your default web browser
2. **Select date range** - Use the date picker in the Overview tab to filter data
3. **Choose metric** - Select Total, Entries, or Exits to view different trip types
4. **Explore tabs**:
   - **Overview**: Get high-level insights and trends
   - **Wordcloud**: Visualize station prominence
   - **Station Profile**: Deep dive into individual stations
   - **Compare**: Compare multiple stations side-by-side
   - **Rent vs Trips**: Analyze correlation between rent and foot traffic
   - **Data**: View and export raw data

## Project Structure

```
Ass2/
├── my_app/
│   └── App.R                 # Main Shiny application
├── datasets/
│   ├── entry_exit_sydney_merged.csv
│   ├── locationfacilitydata_filtered.csv
│   ├── sydney_train_metro_station_counts.csv
│   └── rent.xlsx
├── facilities/               # Facility icon images
│   ├── baby_change.png
│   ├── bike_lockers.png
│   └── ...
└── README.md
```

## Key Functions

### Data Processing
- `normalize_entry_exit()`: Standardizes entry/exit indicators
- `parse_month_int()`: Converts various month formats to integers
- `make_metric_df()`: Filters and aggregates data by metric type
- `six_month_slope()`: Calculates 6-month trend slope

### Formatting
- `fmt_big()`: Formats large numbers with K/M/B abbreviations
- `render_facility_icons()`: Displays facility icons from text descriptions

## Technologies Used

- **R Shiny**: Web application framework
- **bslib**: Bootstrap theme customization
- **tidyverse**: Data manipulation and visualization
- **plotly**: Interactive charts
- **DT**: Interactive data tables
- **wordcloud2/wordcloud**: Word cloud generation
- **lubridate**: Date handling
- **readxl**: Excel file reading

## Features Highlights

- ✅ Interactive date range filtering
- ✅ Multiple visualization types (bar charts, time series, scatter plots, word clouds)
- ✅ Real-time KPI calculations
- ✅ Facility icon mapping and display
- ✅ Rent and traffic correlation analysis
- ✅ Multi-station comparison
- ✅ Exportable data tables
- ✅ Responsive Bootstrap UI

## Notes

- The application normalizes station names for matching between datasets
- Date filtering is month-aligned (uses first day of selected months)
- Missing or invalid data is handled gracefully
- Facility icons are displayed when available in the `facilities/` directory


## Author

Zhining Chen/ Qingying Zhang

## Acknowledgments

We are inspired by the following materials:
- [WHO COVID-19 Dashboard](https://data.who.int/dashboards/covid19/cases?n=c)
- [Shiny.paho dashboard1](https://shiny.paho-phe.org/h5n1/)
- [Shiny.paho dashboard2](https://shiny.paho-phe.org/cholera/)
- [ITF dashboard](https://itf-oecd.org/transport-data-dashboard)
- [NSW Rent and sales report - interactive dashboard](https://public.tableau.com/app/profile/dcj.statistics/viz/Rentandsales_16849924917120/Rent?publish=yes)
- [NSW Customer Satisfaction Index dashboard](https://www.transport.nsw.gov.au/data-and-research/data-and-insights/customer-satisfaction-index-dashboard)
- [Rent Map](https://flatmates.com.au/widgets/train-maps/sydney)

Really appreciate them for make their data or works public💗.

