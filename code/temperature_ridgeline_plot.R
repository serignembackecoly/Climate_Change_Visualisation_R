# Ridgelines of temperatures

# Librairies
library(tidyverse)
library(ggridges)
library(data.table)
library(lubridate)
library(ncdf4)
library(R.utils)


# Download the data
url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
download.file(url, destfile = "gistemp250_GHCNv4.nc.gz")
gunzip("gistemp250_GHCNv4.nc.gz")

# Open the netcdf file
nc_data <- nc_open("gistemp250_GHCNv4.nc")

# Extract the coordinates
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
t <- ncvar_get(nc_data, "time")

# # Time conversion
# t_units <- ncatt_get(nc_data, "time", "units")
# t_ustr <- strsplit(t_units$value, " ")
# t_dstr <- strsplit(unlist(t_ustr)[3], "-")
# date <- ymd(t_dstr) + ddays(time)
# date

# Get the variable
temp_anomaly <- ncvar_get(nc_data, "tempanomaly")
dim(temp_anomaly)

# Handle NA value
fillvalue <- ncatt_get(nc_data, "tempanomaly", "_FillValue")
temp_anomaly[temp_anomaly == fillvalue$value] <- NA

# Convert the data to a datatable and Summarising
t_data <- as.data.table(temp_anomaly) %>% 
  as_tibble() %>% 
  select(longitude = V1, latitude = V2, time = V3, temp_diff = value) %>%  # Change the colname
  mutate(longitude = lon[longitude], latitude = lat[latitude],
         time = t[time] + as.Date("1800-01-01"))%>% 
  mutate(year = year(time)) %>% 
  group_by(year, longitude, latitude) %>% 
  summarise(t_diff = mean(temp_diff), .groups = "drop") %>% 
  filter(year >= 1950) %>% 
  group_by(year) %>% 
  mutate(t_ave = mean(t_diff))

# # Save the data into a csv
# csv_fname <- "data/temperature_anomaly_netcdf.csv"
# write.table(t_data, csv_fname, row.names=FALSE, sep=";")


# Visualisation
t_data %>% 
  # filter(year %in% c(1950, 1994, 2012)) %>% 
  ggplot(aes(x = t_diff,
             y = factor(year, levels = seq(2024, 1950, -1)),
             fill = t_ave)) + 
  geom_density_ridges(bandwidth = 0.2,
                      linewidth = 0.1,
                      color = "white") +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red",
                       midpoint = 0, guide = "none") +
  coord_cartesian(xlim = c(-5, 5 )) +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  scale_y_discrete(breaks = seq(1950, 2020, 10)) +
  labs(title = "Land temperature anomaly distribution",
       x = "Temperature anomaly (°C)",
       y = NULL) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "white"),
    axis.ticks.y =  element_blank(),
    axis.ticks.x = element_line(colour = "white"),
    axis.text = element_text(colour = "white"),
    text = element_text(colour = "white")
  )
# Save the figure
ggsave(dpi = 600, filename = "figures/temperature_ridgelines.png",
       height = 6, width = 4)
