library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbreak)
library(grid)

data <- read.csv("C:/Users/DELL XPS/Downloads/Tableau_morbidite_mortalite_PNLP25_2001_2025.csv", 
                 header = TRUE, dec = ".", sep = ",")

dataF <- data.frame(data)

data_long <- dataF %>% pivot_longer(
  cols = c(Morbidity, Mortality),
  names_to = "Indicator",
  values_to = "Value"
)

ggplot(data_long,
       aes(x = Year,
           y = Value,
           color = Indicator)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  scale_x_continuous(
    breaks = 2001:2025,
    guide = guide_axis(angle = 90)
  ) +
  
  labs(x = "Year",
       y = "Evolution",
       color = "",
       title = "Evolution of Morbidity and Mortality between : 2001-2005") +
         
  annotate(
           "rect",
           xmin = 2006.5,
           xmax = 2008.5,
           ymin = -Inf,
           ymax = Inf,
           fill = "gold",
           alpha = 0.15
         ) +
         
  annotate(
           "rect",
           xmin = 2008.5,
           xmax = 2010.5,
           ymin = -Inf,
           ymax = Inf,
           fill = "red",
           alpha = 0.15
         ) +
        
  annotate(
           "rect",
           xmin = 2013.2,
           xmax = 2020.8,
           ymin = -Inf,
           ymax = Inf,
           fill = "green",
           alpha = 0.15
         ) +
         
  annotate(
           "rect",
           xmin = 2020.9,
           xmax = 2024.8,
           ymin = -Inf,
           ymax = Inf,
           fill = "purple",
           alpha = 0.15
         ) +
         
    scale_color_manual(
        name = "Indicator",               # Titre de la légende
        values = c(
             "Morbidity" = "royalblue",        # Couleur pour la morbidité
             "Mortality" = "#E41A1C"        # Couleur pour la mortalité
           )
         ) +
theme_minimal()
       