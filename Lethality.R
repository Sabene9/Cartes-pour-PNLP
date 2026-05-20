# =========================================================
# PACKAGES
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbreak)
library(grid)


# Preparation de la base de données
pop <- data.frame(
  region = c("Kedougou","Tambacounda","Kolda","Senegal"),
  pop2020 = c(190509,872155,822003,16705608),
  pop2023 = c(245146,987151,914798,18126390)
)


pop$annual_growth <- (pop$pop2023 - pop$pop2020)/3

pop$pop2019 <- pop$pop2020 - pop$annual_growth
pop$pop2021 <- pop$pop2020 + pop$annual_growth
pop$pop2022 <- pop$pop2020 + 2*pop$annual_growth

cases <- data.frame(
  region = c("Kedougou","Tambacounda","Kolda","Senegal"),
  cases2019 = c(67941,101077,116983,354708),
  cases2020 = c(86449,128541,155967,445313),
  cases2021 = c(105694,133778,181999,536850),
  cases2022 = c(89093,63696,80093,358033)
)

dat <- merge(pop, cases, by="region")

dat$inc2019 <- (dat$cases2019 / dat$pop2019) * 1000
dat$inc2020 <- (dat$cases2020 / dat$pop2020) * 1000
dat$inc2021 <- (dat$cases2021 / dat$pop2021) * 1000
dat$inc2022 <- (dat$cases2022 / dat$pop2022) * 1000

dat$region <- factor(dat$region, levels = c("Kedougou", "Tambacounda", "Kolda", "Senegal"))
# =========================================================
# DONNEES MORTALITE
# =========================================================
mort <- data.frame(
  region = c("Kedougou","Tambacounda","Kolda","Senegal"),
  mort2019 = c(22,37,43,260),
  mort2020 = c(64,58,68,373),
  mort2021 = c(40,63,71,399),
  mort2022 = c(27,13,43,273)
)


let2019 <- (mort$mort2019 / cases$cases2019) * 100
let2020 <- (mort$mort2020 / cases$cases2020) * 100
let2021 <- (mort$mort2021 / cases$cases2021) * 100
let2022 <- (mort$mort2022 / cases$cases2022) * 100

let <- data.frame(
  region = c("Kedougou","Tambacounda","Kolda","Senegal"),
  let2019,let2020,let2021,let2022)

let$region <- factor(let$region, levels = c("Kedougou", "Tambacounda", "Kolda", "Senegal"))

dat2 <- merge(dat, let, by="region")

# =========================================================
# FORMAT LONG
# =========================================================

inc_long <- dat2 %>%
  select(region, starts_with("inc")) %>%
  pivot_longer(
    cols = starts_with("inc"),
    names_to = "year",
    values_to = "incidence"
  ) %>%
  mutate(
    year = as.numeric(gsub("inc","",year))
  )


let_long <- dat2 %>%
  select(region, starts_with("let")) %>%
  pivot_longer(
    cols = starts_with("let"),
    names_to = "year",
    values_to = "lethality"
  ) %>%
  mutate(
    year = as.numeric(gsub("let","",year))
  )

# =========================================================
# COEFFICIENT DE TRANSFORMATION
# =========================================================

coef <- max(inc_long$incidence) /
  max(let_long$lethality)

let_long$let_scaled <-
  let_long$lethality * coef

# =========================================================
# GRAPHIQUE
# =========================================================

p <- ggplot() +
  
  # =====================================================
# DIAGRAMMES EN BATONS (MORTALITE)
# =====================================================

geom_col(
  data = let_long,
  aes(
    x = year,
    y = let_scaled,
    fill = region
  ),
  position = position_dodge(width = 0.2),
  width = 0.18,
  alpha = 0.8
) +
  
  # =====================================================
# LIGNES INCIDENCE
# =====================================================

geom_line(
  data = inc_long,
  aes(
    x = year,
    y = incidence,
    color = region,
    group = region
  ),
  size = 1.3
) +
  
  geom_point(
    data = inc_long,
    aes(
      x = year,
      y = incidence,
      color = region
    ),
    size = 2
  ) +
  
  # =====================================================
# AXES
# =====================================================

scale_y_continuous(
  
  name = "Incidence per 1000 inhabitants",
  
  sec.axis = sec_axis(
    ~ . / coef,
    name = "Lethality"
  )
  
) +
  
  scale_x_continuous(
    breaks = c(2019,2020,2021,2022)
  ) +
  
  # =====================================================
# COULEURS LIGNES
# =====================================================

scale_color_manual(
  name = "Incidence",
  values = c(
    "Senegal" = "#e5383b",
    "Kedougou" = "#3a86ff",
    "Kolda" = "#219ebc",
    "Tambacounda" = "#ffbe0b"
  )
) +
  
  # =====================================================
# COULEURS BATONS
# =====================================================

scale_fill_manual(
  name = "Lethality",
  values = c(
    "Senegal" = "#e5383b",
    "Kedougou" = "#3a86ff",
    "Kolda" = "#219ebc",
    "Tambacounda" = "#ffbe0b"
  )
) +
  
  
  labs(
    title = "Evolution of malaria incidence and lethality (2019–2022)",
    x = "Year"
  ) +
  
  
  theme_bw(base_size = 8) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
    legend.position = "right",
    
    axis.title.y.left = element_text(
      face = "bold"
    ),
    
    axis.title.y.right = element_text(
      face = "bold"
    )
  )


# =========================================================
# TROISIEME AXE VISUEL
# =========================================================

p2 <- p +
  
  annotation_custom(
    
    grob = textGrob(
      "Third visual axis",
      rot = 90,
      gp = gpar(
        fontsize = 12,
        fontface = "bold",
        col = "darkgreen"
      )
    ),
    
    xmin = 2022.55,
    xmax = 2022.55,
    ymin = 50,
    ymax = 350
  )

#Affichage
p2