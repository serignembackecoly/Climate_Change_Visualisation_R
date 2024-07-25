# Warming strip year

# Librairies
library(tidyverse)
library(scales)
library(glue)

# Loading the data
t_data <- read.csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***")

# Clean and select
t_data <- t_data %>% select(year = Year, t_diff = "J.D") %>% drop_na()

# Visualisation

t_data %>% ggplot(aes(x = year, y = 1, fill = t_diff)) +
  geom_tile(show.legend = F)+
  scale_fill_stepsn(colors = c("#08306B",  "white",  "#67000D"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    n.breaks = 25) +
  scale_x_continuous(breaks = seq(1890,2020,30)) +
  theme_void() +
  coord_cartesian(expand = F) +
  labs(title = glue("Global temperature changes ({min(t_data$year)} - {max(t_data$year)})")) +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(colour = "white", face = "bold",
                              margin = margin(t = 5, b = 10,
                                              unit = "pt"),
                              hjust = 0.05),
    axis.text.x = element_text(colour = "white", face = "bold",
                               margin = margin(t = 2, b = 2,
                                               unit = "pt"))
  )

# Save the figure
ggsave("figures/warming_stripes.png", height = 4.5, width = 7, dpi = 400)
