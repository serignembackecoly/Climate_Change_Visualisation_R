# Tornado plot

# Libraries
library(tidyverse)
library(shadowtext)

# Load the data
t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na ="***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na()

# Create datafrome for labels
grid_labels <- tibble(
  x = c(-5, -4, 0, 1),
  y = 2029,
  label = c("+1°C", "0°C", "0°C", "+1°C")
)

year_labels <- tibble(
  x = -2,
  y = c(seq(1880,2000, 20), 2010, 2024),
  label = c(seq(1880,2000, 20), 2010, paste0('2024','*'))
)

asterix <- tibble(
  x = -5.2,
  y = 1940,
  label = "(*) 2024 based on Jan-Jun"
)
# Visualisation
t_data %>%
  filter(month == "Apr" | month == "Oct") %>% 
  pivot_wider(names_from = "month",
              values_from = "t_diff") %>% 
  mutate(ave_t = (Apr + Oct) / 2) %>% 
  ggplot(aes(x = -4-Oct, xend = Apr,
             y = year, yend = year,
             color = ave_t)) +
  geom_vline(xintercept = c(-5, -4, 0, 1),
             color = "gold")+
  geom_segment(linewidth = 1.1, lineend = "round") +
  geom_shadowtext(data = year_labels,
            aes(x = x, y = y,
                label = label), inherit.aes = F,
            color = "cyan") +
  geom_shadowtext(data = asterix,
                  aes(x = x,y = y,label =label),
                  inherit.aes = F,color = "cyan",
                  angle = 90, size = 2) +
  geom_label(data = grid_labels,
             aes(x = x, y = y, label = label), 
             inherit.aes = F, color = "gold",
             label.size = 0, fill = "black") +
  scale_color_gradient2(low = "navy", mid = "white",
                        high = "red", midpoint = 0,
                        guide = "none") +
  scale_y_continuous(limits = c(NA, 2030), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL,
       y = NULL) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", colour = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# Save figure
ggsave("figures/temperature_tornado_plot.png", height = 5, width = 5, dpi = 600)
