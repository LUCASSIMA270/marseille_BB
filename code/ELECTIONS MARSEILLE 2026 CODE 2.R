library(tidyverse)
library(stringr)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

set.seed(2026)
N_SIMULATIONS <- 10000

predictions_centrales <- tibble(
  SECTEUR = 1:8,
  G  = c(42, 38, 35, 29, 31, 27, 34, 40),
  RN = c(22, 28, 33, 38, 36, 41, 30, 24),
  C  = c(18, 17, 16, 18, 17, 16, 18, 17),
  D  = c(18, 17, 16, 15, 16, 16, 18, 19)
)

sigma_local    <- 1.5
resultats_mc <- map_dfr(seq_len(N_SIMULATIONS), function(sim_id) {
  choc_nat_G  <- rnorm(1, 0, sigma_national)
  choc_nat_RN <- rnorm(1, 0, sigma_national)
  choc_nat_C  <- rnorm(1, 0, sigma_national)
  choc_nat_D  <- rnorm(1, 0, sigma_national)
  
  predictions_centrales %>%
    mutate(
      G_brut  = G  + choc_nat_G  + rnorm(n(), 0, sigma_local),
      RN_brut = RN + choc_nat_RN + rnorm(n(), 0, sigma_local),
      C_brut  = C  + choc_nat_C  + rnorm(n(), 0, sigma_local),
      D_brut  = D  + choc_nat_D  + rnorm(n(), 0, sigma_local),
      
      G_brut  = pmax(G_brut,  1),
      RN_brut = pmax(RN_brut, 1),
      C_brut  = pmax(C_brut,  1),
      D_brut  = pmax(D_brut,  1),
      
      Total   = G_brut + RN_brut + C_brut + D_brut,
      Pred_G  = G_brut  / Total * 100,
      Pred_RN = RN_brut / Total * 100,
      Pred_C  = C_brut  / Total * 100,
      Pred_D  = D_brut  / Total * 100,
      
      Simulation = sim_id
    ) %>%
    select(SECTEUR, Simulation, Pred_G, Pred_RN, Pred_C, Pred_D)
})

ic_secteurs <- resultats_mc %>%
  group_by(SECTEUR) %>%
  summarise(
    G_med  = median(Pred_G),
    G_lo   = quantile(Pred_G,  0.05),
    G_hi   = quantile(Pred_G,  0.95),
    RN_med = median(Pred_RN),
    RN_lo  = quantile(Pred_RN, 0.05),
    RN_hi  = quantile(Pred_RN, 0.95),
    C_med  = median(Pred_C),
    C_lo   = quantile(Pred_C,  0.05),
    C_hi   = quantile(Pred_C,  0.95),
    D_med  = median(Pred_D),
    D_lo   = quantile(Pred_D,  0.05),
    D_hi   = quantile(Pred_D,  0.95),
    .groups = "drop"
  )

print(ic_secteurs %>%
        transmute(
          SECTEUR,
          Gauche  = paste0(round(G_med,1),  " [", round(G_lo,1),  "–", round(G_hi,1),  "]"),
          RN      = paste0(round(RN_med,1), " [", round(RN_lo,1), "–", round(RN_hi,1), "]"),
          Centre  = paste0(round(C_med,1),  " [", round(C_lo,1),  "–", round(C_hi,1),  "]"),
          Droite  = paste0(round(D_med,1),  " [", round(D_lo,1),  "–", round(D_hi,1),  "]")
        ))

prob_victoire_secteur <- resultats_mc %>%
  mutate(
    Vainqueur = case_when(
      Pred_G  >= Pred_RN & Pred_G  >= Pred_C & Pred_G  >= Pred_D ~ "GAUCHE",
      Pred_RN >= Pred_G  & Pred_RN >= Pred_C & Pred_RN >= Pred_D ~ "RN",
      Pred_C  >= Pred_G  & Pred_C  >= Pred_RN & Pred_C >= Pred_D ~ "CENTRE",
      TRUE ~ "DROITE"
    )
  ) %>%
  group_by(SECTEUR, Vainqueur) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(SECTEUR) %>%
  mutate(Probabilite = round(N / sum(N) * 100, 1)) %>%
  ungroup()

print(
  prob_victoire_secteur %>%
    pivot_wider(id_cols = SECTEUR, names_from = Vainqueur, values_from = Probabilite, values_fill = 0) %>%
    arrange(SECTEUR)
)

print(
  ic_secteurs %>%
    left_join(predictions_centrales, by = "SECTEUR") %>%
    transmute(
      SECTEUR,
      Delta_G  = round(G_med  - G,  2),
      Delta_RN = round(RN_med - RN, 2),
      Delta_C  = round(C_med  - C,  2),
      Delta_D  = round(D_med  - D,  2)
    )
)

ic_plot <- resultats_mc %>%
  pivot_longer(
    cols      = c(Pred_G, Pred_RN, Pred_C, Pred_D),
    names_to  = "Bloc",
    values_to = "Score"
  ) %>%
  mutate(Bloc = recode(Bloc,
                       Pred_G  = "Gauche",
                       Pred_RN = "RN",
                       Pred_C  = "Centre",
                       Pred_D  = "Droite"
  )) %>%
  group_by(SECTEUR, Bloc) %>%
  summarise(
    ymin   = quantile(Score, 0.05),   # IC90 bas
    lower  = quantile(Score, 0.25),   # IC50 bas
    middle = median(Score),           # médiane
    upper  = quantile(Score, 0.75),   # IC50 haut
    ymax   = quantile(Score, 0.95),   # IC90 haut
    .groups = "drop"
  ) %>%
  mutate(
    Bloc    = factor(Bloc, levels = c("Gauche", "RN", "Centre", "Droite")),
    Secteur = factor(SECTEUR)
  )

couleurs_blocs <- c(
  "Gauche" = "#E41A1C",
  "RN"     = "#000080",
  "Centre" = "#D4AC0D",
  "Droite" = "#00BFFF"
)

ic_plot

blocs_liste <- c("Gauche", "RN", "Centre", "Droite")

plots_blocs <- map(blocs_liste, function(bloc_choisi) {
  
  couleur <- couleurs_blocs[bloc_choisi]
  
  ic_plot %>%
    filter(Bloc == bloc_choisi) %>%
    ggplot(aes(y = Secteur)) +
    
    geom_errorbarh(
      aes(xmin = ymin, xmax = ymax),
      height    = 0.25,
      linewidth = 0.7,
      color     = couleur,
      alpha     = 0.9
    ) +
    
    geom_crossbar(
      aes(x = middle, xmin = lower, xmax = upper),
      width     = 0.55,
      linewidth = 0.45,
      fill      = couleur,
      color     = couleur,
      alpha     = 0.6,
      fatten    = 0
    ) +
    
    geom_point(
      aes(x = middle),
      shape = 21,
      size  = 3,
      fill  = "white",
      color = couleur,
      stroke = 1
    ) +
    
    geom_vline(
      xintercept = median(ic_plot$middle[ic_plot$Bloc == bloc_choisi]),
      linetype   = "dashed",
      color      = "grey50",
      linewidth  = 0.5
    ) +
    
    scale_x_continuous(
      limits = c(0, 65),
      breaks = seq(0, 60, 10),
      labels = function(x) paste0(x, "%")
    ) +
    
    scale_y_discrete(limits = rev) +   
    
    labs(
      title = bloc_choisi,
      x     = "Score estimé au 1er tour (%)",
      y     = "Secteur"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 13,
                                        color = couleur, hjust = 0.5),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x       = element_text(size = 9, color = "grey40"),
      axis.title.y       = element_text(size = 9, color = "grey40")
    )
})

p_blocs_combines <- wrap_plots(plots_blocs, ncol = 2) +
  plot_annotation(
    title    = "Intervalles de confiance Monte Carlo par Bloc politique",
    subtitle = "Boîte = IC50 · Moustaches = IC90 · Point = médiane · Tiret = médiane globale du bloc | 10 000 simulations",
    caption  = "Incertitude : sondages ±2,5pp + chocs locaux ±1,5pp",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(color = "grey40", size = 9,  hjust = 0.5),
      plot.caption  = element_text(color = "grey60", size = 8,  hjust = 1)
    )
  )

p_blocs_combines
ggsave("boxplot_4blocs.png", p_blocs_combines, width = 12, height = 10, dpi = 300)

grille_complete <- expand_grid(
  SECTEUR   = 1:8,
  Vainqueur = c("GAUCHE", "RN", "CENTRE", "DROITE")
)

prob_complete <- grille_complete %>%
  left_join(prob_victoire_secteur %>% select(SECTEUR, Vainqueur, Probabilite),
            by = c("SECTEUR", "Vainqueur")) %>%
  replace_na(list(Probabilite = 0)) %>%
  mutate(
    Vainqueur = factor(Vainqueur,
                       levels = c("GAUCHE", "RN", "CENTRE", "DROITE"),
                       labels = c("Gauche", "RN", "Centre", "Droite")),
    Secteur   = factor(SECTEUR),
    Label     = ifelse(Probabilite >= 2, paste0(Probabilite, "%"), "")
  )

couleurs_hist <- c(
  "Gauche" = "#E41A1C",
  "RN"     = "#000080",
  "Centre" = "#D4AC0D",
  "Droite" = "#00BFFF"
)

p_hist <- ggplot(prob_complete,
                 aes(x = Secteur, y = Probabilite, fill = Vainqueur)) +
  
  geom_bar(
    stat     = "identity",
    width    = 0.72,
    color    = "white",
    linewidth = 0.3
  ) +
  
  geom_text(
    aes(label = Label),
    position  = position_stack(vjust = 0.5),
    size      = 3.8,
    fontface  = "bold",
    color     = "white"
  ) +
  
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  annotate("text", x = 8.55, y = 51.5,
           label = "50%", color = "grey40",
           size = 3, hjust = 0, fontface = "italic") +
  
  scale_fill_manual(values = couleurs_hist) +
  scale_y_continuous(
    limits = c(0, 101),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(clip = "off") +
  
  labs(
    title    = "Probabilité de finir en Tête par Secteur au 1er Tour",
    subtitle = "10 000 simulations Monte Carlo",
    x        = "Secteur",
    y        = "Probabilité de finir en tête (%)",
    fill     = "Bloc"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey60", size = 8, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("hist_probabilites_victoire.png", p_hist, width = 10, height = 6, dpi = 300)

p_hist