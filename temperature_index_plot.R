# Libraries
library(tidyverse)

# Read the data
temp_data <- read.csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***")

# Clean and select the data we want
temp_data <- temp_data%>% 
  select(year = Year, temperature_diff = "J.D") 

# Visualisation of the temperature evolution
temp_data %>% 
  ggplot(aes(x = year, y = temperature_diff)) +
  geom_line(aes(color = "1"), show.legend = F) +
  geom_point(fill = "white", aes(color = "1"),
             show.legend = T, size = 0.5) +
  geom_smooth(se = F, aes(color = "2"), span = 0.15,
              size = 0.5, show.legend = F) +
  scale_x_continuous(breaks = seq(1880, 2020, 20), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0,0)) +
  scale_color_manual(name = NULL,
                     breaks = c(1,2),
                     values = c("gray", "black"),
                     labels = c("Annual mean",
                                "Lowess smoothing"),
                     guide = guide_legend(override.aes = list(shape = 15,
                                                              size = 5))) +
  labs(x = "YEAR", y = "Temperature anomaly (°C)",
       title =  "GLOBAL LAND-OCEAN TEMPERATURE INDEX",
       caption = "Data source: NASA's Goddard Institute for Space Studies (GISS)") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(colour = "red", size = 12, 
                                  face = "bold", margin = margin(b=10)),
        plot.caption  = element_text(size = 4, margin = margin(t=10)),
        legend.position = c(0.15,0.9),
        legend.title = element_text(size = 0),
        legend.key.height = unit(8, "pt"))

# Save the figure

ggsave("figures/temperature_index_plot.png", height = 4, width = 6)
