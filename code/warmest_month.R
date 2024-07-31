# Animation of the warmest month

# Librairies
library(lubridate)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(glue)

# Load data
month_anom <- read.table("data/merra2_seas_anom.txt",
                         skip = 3, header = T) %>% 
  select(month = Month, seas_anom) %>% 
  mutate(month = month.abb[month])

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() %>% 
  inner_join(., month_anom, by = "month") %>% 
  mutate(month_anom = (t_diff + seas_anom) - 0.7,
         month = factor(month, levels = month.abb)) %>% 
  group_by(year) %>% 
  mutate(ave = mean(month_anom)) %>% 
  ungroup() %>% 
  mutate(ave = if_else(year == 2024, max(abs(month_anom)), ave))
# Data for annotation
annotation <- t_data %>% slice_tail(n = 1)

# Visualisation
p1 <- t_data %>% 
  ggplot(aes(x = month, y = month_anom,
             group = year, color = ave)) +
  geom_line() +
  scale_color_gradient2(low = "navy", mid = "white", high = "darkred",
                        midpoint = 0, guide = "none") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-3, 2, 1)) +
  labs(x = NULL,
       y = NULL,
       title = "Temperature anomaly (°C)",
       subtitle = "(Difference between 1950 and 2015 annual mean)"
       ) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.grid.major.y = element_line(colour = "gray",
                                    linetype = "dotted",
                                    linewidth = 0.25),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = "gray", size = 10),
    axis.text = element_text(colour = "black"),
    plot.margin = margin(10, 10, 10, 15)
  )

p2 <- p1 + 
  geom_point(data = annotation,
                aes(x = month, y = month_anom),
                size = 4) +
  geom_text(data = annotation,
            aes(x = 5.7, y = month_anom, label = glue("June {annotation$year}")),
            hjust = 1) 
# Save the figure
ggsave("figures/warmest_month_plot.png", plot = p2, height = 4, width = 6, dpi = 600)

# Create the GIF
p3 <- p1 +
  geom_label(aes(x = 7, y = 0, label = year),
             label.size = 0, fontface = "bold") +
  transition_manual(year, cumulative = T)

animate(p3, width = 6 , height = 4, units = "in", res = 300)
anim_save("figures/animation_warmest_month.gif")
