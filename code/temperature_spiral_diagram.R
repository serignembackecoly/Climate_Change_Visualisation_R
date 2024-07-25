# Script for climate spiral diagram

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
  mutate(month = factor(month, levels = c( month.abb)),
         month_number = as.numeric(month)) %>% 
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

t_data <- bind_rows(temp_data, next_jan) %>% 
  mutate(month = factor(month, levels = c(month.abb, "next_jan")),
         month_number = as.numeric(month) - 1)

circles <- tibble(
  x = 12,
  y = c(1.5, 2.0),
  labels = c("1.5°C", "2.0°C")
)
months_labels <- tibble(
  x = 1:12,
  y = 2.5,
  label = month.abb
)
annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

# Visualisation
t_data %>% ggplot(aes(x = month_number, y = t_diff, color = year,
                      group = year)) + 
  geom_point(x=12,y=-1.5,color="black",size=113) +
  geom_hline(yintercept = c(1.5, 2), color = "red") +
  geom_line() +
  geom_label(data = circles,
             aes(x = x, y = y, label = labels),
             color = "red", fill = "black",
             label.size = 0, inherit.aes = F) +
  geom_text(data = months_labels, aes(x = x, y = y, label = label),
            inherit.aes = F, color = "white",
            angle = c(seq(330,0, length.out = 12))) +
  geom_text(aes(x = 3, y = -1.5,
                label = glue("{max(t_data$year)}",)),
            size = 6, show.legend = F)+
  geom_point(data = annotation, aes(x = month_number, y = t_diff),
             size = 2.5) +
  scale_color_viridis_c("", breaks = seq(1880, 2020, 20),
                        guide = "none") +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb) +
  scale_y_continuous(breaks = seq(-2,2.4,0.2),
                     limits = c(-2,2.5), expand = c(0,-0.5)) +
  coord_polar() +
  labs(title = glue("Global temperature change (1880 - {max(t_data$year)})"),
       y = NULL,
       x = NULL) +
  theme(
    panel.background = element_rect(fill = "gray24", colour = "gray24" ),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "gray24", colour = "gray24"),
    plot.title = element_text(colour = "white", size = 14,
                              hjust = 0.5, face  = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Save the figure
ggsave("figures/temperature_spiral_plot.png", height = 4.5, width = 4.5, dpi = 400)
