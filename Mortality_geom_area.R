ggplot(inc_long2,
       aes(x = year,
           y = incidence,
           fill = region)) +
  
  geom_area(alpha = 0.6) +
  
  facet_wrap(
    ~region,
    ncol = 4
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Malaria incidence evolution",
    x = "Year",
    y = "Incidence"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) 