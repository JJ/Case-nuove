devtools::load_all()
library(ggplot2)

load("data/proportions_results_df.rda")

ggplot(proportions_results_df, aes(x = Year)) +
  geom_line(aes(y = ProportionLunghiInFirstCommunity, color = "Lunghi")) +
  geom_line(aes(y = ProportionDucaliInFirstCommunity, color = "Ducali")) +
  geom_point(aes(y = ProportionLunghiInFirstCommunity, color = Type), size = ifelse(proportions_results_df$Type=="Ducali",0,3)) +
  geom_point(aes(y = ProportionDucaliInFirstCommunity, color = Type), size = ifelse(proportions_results_df$Type!="Ducali",0,3)) +
  labs(title = "Proportions of Families in the First Community Over Time",
       x = "Year",
       y = "Proportion",
       color = "Family Type") +
  theme_minimal() +
  scale_color_manual(values = c("Lunghi" = "blue", "Ducali" = "red"))
ggsave("figures/proportions_first_community.png", width = 10, height = 6, dpi = 300)

ggplot(proportions_results_df, aes(x = Year)) +
  geom_line(aes(y = ProportionLunghiOutOfFirstCommunity, color = "Lunghi")) +
  geom_line(aes(y = ProportionDucaliOutOfFirstCommunity, color = "Ducali")) +
  geom_point(aes(y = ProportionLunghiOutOfFirstCommunity, color = Type), size = ifelse(proportions_results_df$Type=="Ducali",0,3)) +
  geom_point(aes(y = ProportionDucaliOutOfFirstCommunity, color = Type), size = ifelse(proportions_results_df$Type!="Ducali",0,3)) +
  labs(title = "Proportions of Families in the First Community Over Time",
       x = "Year",
       y = "Proportion",
       color = "Family Type") +
  theme_minimal() +
  scale_color_manual(values = c("Lunghi" = "blue", "Ducali" = "red"))
ggsave("figures/composition_first_community.png", width = 10, height = 6, dpi = 300)
