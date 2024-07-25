# Libraries
library(tidyverse)
library(scales)
library(glue) # Awesome!

# Read the data
temp_data <- read.csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***")

# Clean and select the data we want
temp_data <- temp_data%>% 
  select(year = Year, temperature_diff = "J.D") %>% 
  drop_na()

# Add the dates
annotation <- temp_data %>% 
  arrange(year) %>% 
  slice(1, n()) %>% 
  mutate(temperature_diff = 0,
         x = year + c(-5, 5))

# Capture the actual value of the maximum temperature increasing
max_t_diff <- round(max(temp_data$temperature_diff),1)

# Visualisation

temp_data %>%
  ggplot(aes(x = year, y = temperature_diff, fill = temperature_diff)) +
  geom_col(show.legend = F) +
  geom_text(data = annotation, aes(x = x, label = year),
            color = "white") +
  geom_text(x = 1880, y = 1, hjust = 0,
            label = glue("Global temperatures have increased by over {max_t_diff}°C since {min(temp_data$year)}"),
            color = "white") + 
  scale_fill_stepsn(colors = c("navy", "white", "red"),
                   values = rescale(c(min(temp_data$temperature_diff),
                                      0,
                                      max(temp_data$temperature_diff)),
                                    ),
                   limits = c(min(temp_data$temperature_diff),
                              max(temp_data$temperature_diff)),
                   n.breaks = 11) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  )

 # Save the figure
ggsave("figures/temperature_bar_plot.png", width = 7, height = 4, dpi = 400)
