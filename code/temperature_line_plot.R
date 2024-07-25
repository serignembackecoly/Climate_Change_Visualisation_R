# Temperatures lines plot

# Librairies
library(tidyverse)
library(scales)
library(glue)

# Loading the data
raw_data <- read.csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***")

# Clean and select
temp_data <- raw_data %>%
  select(year = Year, all_of(month.abb)) %>%
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na()

# Some creationof data frame for plot purpose
last_dec <- temp_data %>% 
            filter(month == "Dec") %>% 
            mutate(year = year + 1,
                   month = "last_dec")

next_jan <- temp_data %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1,
         month = "next_jan")

t_data <- bind_rows(last_dec, temp_data, next_jan) %>% 
          mutate(month = factor(month, levels = c("last_dec", month.abb, "next_jan")),
                 month_number = as.numeric(month) - 1,
                 this_year = year == max(year))

annotation <- t_data %>% slice_max(year) %>% slice_max(month_number)

# Visualisation
t_data %>% ggplot(aes(x = month_number, y = t_diff, color = year,
                      group = year, size = this_year)) + 
  geom_line() +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.2, linetype = "dashed") +
  geom_text(data = annotation,
            aes(x = month_number, y = t_diff, label = year),
            size = 5, hjust = 0, nudge_x = 0.03, nudge_y = 0.05) +
  scale_color_viridis_c("", breaks = seq(1880, 2020, 20),
                        guide = guide_colorbar(frame.linewidth = 0.5,
                                               frame.colour = "white")) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  scale_y_continuous(breaks = seq(-0.8,1.4,0.2)) +
  scale_size_manual(values = c(0.15, 1), guide = "none") +
  coord_cartesian(xlim = c(0.5,12.5)) +
  labs(title = "Global temperature change since 1880 by month",
       y = "Temperature change since pre-industrial time (°C)",
       x = NULL) +
  theme(
    panel.background = element_rect(fill = "black", colour = "white", size = 1),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "gray33"),
    plot.title = element_text(colour = "white", size = 14,
                              hjust = 0.5, face  = "bold"),
    axis.title = element_text(colour = "white", size = 12),
    axis.text = element_text(colour = "white", size = 11),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(colour = "white"),
    legend.key.height = unit(55, "pt"),
    axis.ticks.length = unit(-5, "pt"),
    axis.ticks = element_line(colour = "white")
  )

# Save the figure
ggsave("figures/temperature_line_plot.png", height = 4.5, width = 7, dpi = 400)
  