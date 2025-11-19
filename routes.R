# library(readr)
# library(dplyr)
# library(here)
# 
# # Adjust the path if your file is elsewhere
# routes <- read_csv(here("Ass2/datasets/full/routes.txt"), show_col_types = FALSE)
# 
# # Show distinct route_type values (and optionally counts)
# route_desc <- routes %>%
#   distinct(route_desc)
# 
# # Or, to see how many routes per type:
# routes %>%
#   count(route_type, sort = TRUE)
# 
# routes1 <- filter(routes,route_desc %in% c("Sydney Metro Network","Sydney Trains Network"))
library(readr)
library(dplyr)

# 1) Load GTFS components
stops       <- read_csv(here("Ass2/datasets/full/stops.txt"), show_col_types = FALSE)
routes      <- read_csv(here("Ass2/datasets/full/routes.txt"), show_col_types = FALSE)
trips       <- read_csv(here("Ass2/datasets/full/trips.txt"), show_col_types = FALSE)
stop_times  <- read_csv(here("Ass2/datasets/full/stop_times.txt"), show_col_types = FALSE)

# 2) Keep only Sydney Metro and Sydney Trains routes
rail_routes <- routes %>%
  filter(route_desc %in% c("Sydney Metro Network", "Sydney Trains Network")) %>%
  select(route_id, route_desc)

# 3) Get all stop_ids appearing in those routes
rail_stop_ids <- trips %>%
  inner_join(rail_routes, by = "route_id") %>%
  inner_join(stop_times %>% select(trip_id, stop_id), by = "trip_id")

# 4) Count how many times each stop appears across all routes
stop_counts <- rail_stop_ids %>%
  count(stop_id, name = "appearance_count")

# 5) Merge with stops and use parent_station as grouping
rail_stops_parent <- stop_counts %>%
  inner_join(stops %>% select(stop_id, parent_station, stop_name), by = "stop_id") %>%
  mutate(parent_station = ifelse(is.na(parent_station) | parent_station == "", stop_id, parent_station))

# 6) Aggregate by parent_station
station_counts <- rail_stops_parent %>%
  group_by(parent_station) %>%
  summarise(
    total_appearance = sum(appearance_count),
    station_name = first(stop_name),
    .groups = "drop"
  ) %>%
  # keep only the part before comma in station_name
  mutate(
    station_name = trimws(sub(",.*", "", station_name))
  ) %>%
  arrange(desc(total_appearance))


# 7) Save result
write_csv(station_counts, here("Ass2/datasets/sydney_train_metro_station_counts.csv"))

# 8) (Optional preview)
head(station_counts)
