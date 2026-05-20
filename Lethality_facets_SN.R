# =========================================================
# PACKAGES
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# =========================================================
# FACETS
# =========================================================

p_facets <- ggplot() +
  
  # =====================================================
# BARRES LETHALITE
# =====================================================

geom_col(
  data = let_long,
  aes(
    x = year,
    y = let_scaled,
    fill = "Lethality"
  ),
  width = 0.35,
  alpha = 0.7
) +
  
  # =====================================================
# LIGNES INCIDENCE
# =====================================================

geom_line(
  data = inc_long,
  aes(
    x = year,
    y = incidence,
    color = "Incidence",
    group = region
  ),
  size = 1.2
) +
  
  geom_point(
    data = inc_long,
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
    name = "Lethality (%)"
  )
  
) +
  
  scale_x_continuous(
    breaks = c(2019,2020,2021,2022)
  ) +
  
  # =====================================================
# COULEURS
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
      "Lethality" = "#ff4d4d"
    )
  ) +
  
  # =====================================================
# TITRES
# =====================================================

labs(
  title = "Evolution of malaria incidence and lethality (2019–2022)",
  x = "Year"
) +
  
  # =====================================================
# THEME
# =====================================================

theme_bw(base_size = 8) +
  
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