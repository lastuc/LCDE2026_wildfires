#-----------------------------------------------------------------------------#
#                          4b. Extract weekly exposures                        #
#-----------------------------------------------------------------------------#


pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2026_wildfires/"
# pathroot <- ""

# 1. Assign areas, exposure by grid of population for each week

# Read data
pointnuts <- vect(paste0(pathroot, "data/processed/expocentroids_nuts.gpkg"))
dfnuts <- as.data.frame(pointnuts)
population <- rast(paste0(pathroot, "data/processed/population_yearly.tif"))
population <- subset(population, names(population) != "year")
names(population) <- as.character(c(2003:2024))

# Create date sequence
dayseq <- seq.Date(as.Date("2003-01-01"), as.Date("2024-12-31"), by = "1 day")


# Extract geographical IDs, population, calculate weekly average SILAM for each week-cell per year
for(y in 2003:2024){

  print(y)

  # Extract population ----
  ypop <- population[as.character(y)]
  names(ypop) <- "population"
  ypop <- terra::extract(ypop, pointnuts)[-1]

  # Extract SILAM ----
  yindex <- year(dayseq) == y
  ysilam <- rast(paste0(pathroot, "data/raw/SILAM/europePMfire_2003to2024daymean.nc4"), lyrs = yindex)
  names(ysilam) <- as.character(as.Date(time(ysilam)))
  ysilam <- ysilam*1e+9 # change of units
  ysilam <- terra::extract(ysilam, pointnuts)[-1]

  # Merge, reformat ----
  ydata <- cbind(dfnuts, ypop, ysilam)
  ydata <- tidyr::pivot_longer(ydata,
                               cols = -c("GRD_ID","NUTS_0","NUTS_2","NUTS_mort","region","population", "country", "eu"),
                               names_to = "date", values_to = "pm25")
  
  # Calculate weekly average exposure and aggregate to weekly observations ----
  # Ensure the 'date' column is in Date format
  ydata$date <- as.Date(ydata$date)
  
  # Add a column for the week (starting date of the week)
  ydata <- ydata %>%
    mutate(week_start = floor_date(date, unit = "week"))
  
  # Group by geographical ID and week_start, then calculate the mean
  weekly_averages <- ydata %>%
    group_by(GRD_ID, week_start) %>%
    summarise(pm25_weekly_avg = mean(pm25, na.rm = T))
  
  # If needed, merge back with the original data
  ydata_weekly <- inner_join(ydata, weekly_averages, by = c("GRD_ID", "date" = "week_start"))
  
  # Write ----
  readr::write_csv(ydata_weekly, paste0(pathroot, "data/processed/assembled/data_weekly_", y,".csv"))
  rm("ypop", "yindex", "ysilam", "ydata", "weekly_averages", "ydata_weekly")
}

# Clean
rm(list = ls())
