# =========================================================
# PACKAGES
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbreak)
library(grid)

# =========================================================
# PREPARATION DES DONNEES
# =========================================================

pop <- data.frame(
  region = c("Kedougou","Tambacounda","Kolda","Senegal"),
  pop2020 = c(190509,872155,822003,16705608),
  pop2023 = c(245146,987151,914798,18126390)
)

pop$annual_growth <- (pop$pop2023 - pop$pop2020)/3

pop$pop2019 <- pop$pop2020 - pop$annual_growth
pop$pop2021 <- pop$pop2020 + pop$annual_growth
pop$pop2022 <- pop$pop2020 + 2*pop$annual_growth

# =========================================================
# DONNEES CAS
# =========================================================

cases <- data.frame(
  region = c("Kedougou","Tambacounda","Kolda","Senegal"),
  cases2019 = c(67941,101077,116983,354708),
  cases2020 = c(86449,128541,155967,445313),
  cases2021 = c(105694,133778,181999,536850),
  cases2022 = c(89093,63696,80093,358033)
)

dat <- merge(pop, cases, by="region")

# =========================================================
# CALCUL INCIDENCE
# =========================================================

dat$inc2019 <- (dat$cases2019 / dat$pop2019) * 1000
dat$inc2020 <- (dat$cases2020 / dat$pop2020) * 1000
dat$inc2021 <- (dat$cases2021 / dat$pop2021) * 1000
dat$inc2022 <- (dat$cases2022 / dat$pop2022) * 1000

dat$region <- factor(
  dat$region,
  levels = c("Kedougou", "Tambacounda", "Kolda", "Senegal")
)

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

# =========================================================
# TAUX DE MORTALITE (%)
# =========================================================

mort$mort2019 <- (mort$mort2019 / pop$pop2019) * 100
mort$mort2020 <- (mort$mort2020 / pop$pop2020) * 100
mort$mort2021 <- (mort$mort2021 / pop$pop2021) * 100
mort$mort2022 <- (mort$mort2022 / pop$pop2022) * 100

mort$region <- factor(
  mort$region,
  levels = c("Kedougou", "Tambacounda", "Kolda", "Senegal")
)

# =========================================================
# FUSION
# =========================================================

dat2 <- merge(dat, mort, by="region")

# =========================================================
# FORMAT LONG INCIDENCE
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

# =========================================================
# FORMAT LONG MORTALITE
# =========================================================

mort_long <- dat2 %>%
  select(region, starts_with("mort")) %>%
  pivot_longer(
    cols = starts_with("mort"),
    names_to = "year",
    values_to = "mortality"
  ) %>%
  mutate(
    year = as.numeric(gsub("mort","",year))
  )
# =========================================================
# Senegal
# =========================================================
inc_long2 <- inc_long

mort_long2 <- mort_long

# =========================================================
# COEFFICIENT DE TRANSFORMATION
# =========================================================

coef <- max(inc_long2$incidence) /
  max(mort_long2$mortality)

mort_long2$mortality_scaled <-
  mort_long2$mortality * coef

# =========================================================
# GRAPHIQUE FACETS
# =========================================================

p_facets <- ggplot() +
  
  # =====================================================
# BARRES MORTALITE
# =====================================================

geom_col(
  data = mort_long2,
  aes(
    x = year,
    y = mortality_scaled,
    fill = "Mortality"
  ),
  width = 0.35,
  alpha = 0.7
) +
  
  # =====================================================
# LIGNES INCIDENCE
# =====================================================

geom_line(
  data = inc_long2,
  aes(
    x = year,
    y = incidence,
    color = "Incidence",
    group = region
  ),
  size = 1.2
) +
  
  geom_point(
    data = inc_long2,
    aes(
      x = year,
      y = incidence,
      color = "Incidence"
    ),
    size = 2.5
  ) +
  
  # =====================================================
# FACETS
# =====================================================

facet_wrap(
  ~region,
  ncol = 4
) +
  
  # =====================================================
# AXES
# =====================================================

scale_y_continuous(
  
  name = "Incidence per 1000 inhabitants",
  
  sec.axis = sec_axis(
    ~ . / coef,
    name = "Mortality (%)",
    breaks = seq(0, 0.04, by = 0.005)
  )
  
) +
  
  scale_x_continuous(
    breaks = c(2019,2020,2021,2022)
  ) +
  
  # =====================================================
# COULEURS LEGENDES
# =====================================================

scale_color_manual(
  name = "Indicator",
  values = c(
    "Incidence" = "#1e90ff"
  )
) +
  
  scale_fill_manual(
    name = "Indicator",
    values = c(
      "Mortality" = "#ff0000"
    )
  ) +
  
  # =====================================================
# TITRES
# =====================================================

labs(
  title = "Evolution of malaria incidence and mortality (2019–2022)",
  x = "Year"
) +
  
  # =====================================================
# THEME
# =====================================================

theme_bw(base_size = 10) +
  
  theme(
    
    # TITRE
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 14
    ),
    
    # LEGENDES
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    # TITRES AXES
    axis.title.x = element_text(
      face = "bold",
      size = 11
    ),
    
    axis.title.y.left = element_text(
      face = "bold",
      size = 11
    ),
    
    axis.title.y.right = element_text(
      face = "bold",
      size = 11
    ),
    
    # TEXTE AXES
    axis.text.x = element_text(
      size = 9
    ),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    # FACETS
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    strip.background = element_rect(
      fill = "grey85",
      color = "black"
    ),
    
    # GRILLE
    panel.grid.major = element_line(
      color = "grey85"
    ),
    
    panel.grid.minor = element_blank()
    
  )

# =========================================================
# AFFICHAGE
# =========================================================

p_facets