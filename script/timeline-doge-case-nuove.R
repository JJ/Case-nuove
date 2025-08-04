library(ggplot2)
load("data/ducali_dogi_data.rda")

# create a timeline with the ducali_dogi_data using different colors for different Types
# The timeline would be a single bar, colored depending of the type of family, with every bar spanning from the year it started (in Year) until the year the next started (also in Year)

# load the library that includes the function "lead"

library(dplyr)
ggplot(ducali_dogi_data, aes(x = Year, y = 1, color = Type)) +
  geom_segment(aes(xend = lead(Year, order_by = Year), yend = 1), linewidth = 10) +
  scale_color_manual(values = c("Ducali"="gold", "Nuovissime" = "blue", "Nuove" = "green", "Apostoliche" = "red", "Evangeliche" = "black")) +
  labs(title = "Timeline of Doge Families in Venice",
       x = "Year",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank())+ylim(c(0.5, 1.5))

