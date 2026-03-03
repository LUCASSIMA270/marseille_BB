#### ELECTIONS MUNICIPALES MARSEILLE 2026 ###

library(tidyverse)
library(stringr)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(fmsb)

df_2008_t1 <- read_delim("MARSEILLE_MUNICIPALES_2008_1eT.csv", 
                         delim = ";", 
                         col_names = FALSE, 
                         locale = locale(encoding = "latin1"), 
                         show_col_types = FALSE)

df_2008_t2 <- read_delim("MARSEILLE_MUNICIPALES_2008_2ndT.csv", 
                         delim = ";", 
                         col_names = FALSE, 
                         locale = locale(encoding = "latin1"), 
                         show_col_types = FALSE)

df_2014_t1 <- read_csv("marseille_municipales2014_tour1.csv", 
                       locale = locale(encoding = "latin1"), 
                       show_col_types = FALSE)

df_2014_t2 <- read_csv("marseille_municipales2014_tour2.csv", 
                       locale = locale(encoding = "latin1"), 
                       show_col_types = FALSE)

df_2020_t1 <- read_csv("marseille_municipales2020_tour1.csv", 
                       locale = locale(encoding = "latin1"), 
                       show_col_types = FALSE)

df_2020_t2 <- read_csv("marseille_municipales2020_tour2.csv", 
                       locale = locale(encoding = "latin1"), 
                       show_col_types = FALSE)

print("Aperçu du Tour 1 de 2014 :")
print(head(df_2014_t1[, 1:10])) 

# NETTOYAGE #

nettoyer_moderne <- function(df_brut, annee, tour) {
  df_long <- df_brut %>%
    mutate(ID_Bureau = row_number()) %>%
    select(ID_Bureau, SECTEUR, matches("^NOM_CANDIDAT"), matches("^NB_VOIX_CANDIDAT")) %>%
    pivot_longer(
      cols = -c(ID_Bureau, SECTEUR),
      names_to = c(".value", "Numero"),
      names_pattern = "(NOM_CANDIDAT|NB_VOIX_CANDIDAT)_(.*)"
    ) %>%
    rename(Candidat = NOM_CANDIDAT, Voix = NB_VOIX_CANDIDAT) %>%
    filter(!is.na(Candidat) & Candidat != "") %>%
    mutate(
      Voix = as.character(Voix),
      Voix = str_replace_all(Voix, "\\s+", ""),
      Voix = as.numeric(Voix),
      Annee = annee,
      Tour = tour
    )
  
  df_agg <- df_long %>%
    group_by(Annee, Tour, SECTEUR, Candidat) %>%
    summarise(Total_Voix = sum(Voix, na.rm = TRUE), .groups = 'drop') %>%
    group_by(Annee, Tour, SECTEUR) %>%
    mutate(Pourcentage = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
    ungroup() %>%
    arrange(SECTEUR, desc(Pourcentage))
  
  return(df_agg)
}

nettoyer_2008 <- function(df_brut, annee, tour) {
  df_agg <- df_brut %>%
    filter(X1 == "detail" & X2 == "SECTEUR") %>%
    select(SECTEUR = X3, Candidat = X6, Voix = X7) %>%
    mutate(
      Annee = annee,
      Tour = tour,
      SECTEUR = as.numeric(SECTEUR),
      Voix = str_replace_all(Voix, "\\s+", ""),
      Voix = as.numeric(Voix)
    ) %>%
    group_by(Annee, Tour, SECTEUR, Candidat) %>%
    summarise(Total_Voix = sum(Voix, na.rm = TRUE), .groups = 'drop') %>%
    group_by(Annee, Tour, SECTEUR) %>%
    mutate(Pourcentage = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
    ungroup() %>%
    arrange(SECTEUR, desc(Pourcentage))
  
  return(df_agg)
}

df_2008_t1_propre <- nettoyer_2008(df_2008_t1, 2008, 1)
df_2008_t2_propre <- nettoyer_2008(df_2008_t2, 2008, 2)

df_2014_t1_propre <- nettoyer_moderne(df_2014_t1, 2014, 1)
df_2014_t2_propre <- nettoyer_moderne(df_2014_t2, 2014, 2)

df_2020_t1_propre <- nettoyer_moderne(df_2020_t1, 2020, 1)
df_2020_t2_propre <- nettoyer_moderne(df_2020_t2, 2020, 2)

print("Résultats propres - 2020 Tour 1 (Extrait) :")
print(head(df_2020_t1_propre))

# AGREGATION 2008 2014 ET 2020#

historique_brut <- bind_rows(
  df_2008_t1_propre, df_2008_t2_propre,
  df_2014_t1_propre, df_2014_t2_propre,
  df_2020_t1_propre, df_2020_t2_propre
)

# MARSEILLE/VOIX/ANNEE/TOUR #

test_voix <- historique_brut %>%
  group_by(Annee, Tour) %>%
  summarise(Total_Marseille = sum(Total_Voix, na.rm = TRUE), .groups = 'drop')

print(test_voix)

# DICO FAMILLES POLITIQUES #

historique_avec_nuances <- historique_brut %>%
  mutate(Candidat_Maj = toupper(Candidat)) %>% # Tout mettre en majuscule pour éviter les soucis de casse
  mutate(Famille_Politique = case_when(
    # --- LA DROITE (LR, UMP, Divers Droite) ---
    str_detect(Candidat_Maj, "GAUDIN|VASSAL|ROATTA|TIAN|MORAINE|GILLES|BLUM|CHENOZ|CORDESSE|BERNASCONI|PUIG|ROYER|CHEVASSU") ~ "DROITE",
    
    # --- LA GAUCHE (PS, Printemps Marseillais, DVG) ---
    str_detect(Candidat_Maj, "GUERINI|MENNUCCI|GHALI|RUBIROLA|PAYAN|CARLOTTI|MASSET|ZANETTI|MADROLLE|CASANOVA|BLANC") ~ "GAUCHE",
    
    # --- L'EXTRÊME DROITE (FN, RN, UDR) ---
    str_detect(Candidat_Maj, "FN|RAVIER|MARANDAT|ALLISIO|BAUMANN|GRENARD|RACALBUTO") ~ "EXTREME_DROITE",
    
    # --- L'EXTRÊME GAUCHE (PCF, LO, LCR, Front de Gauche) ---
    str_detect(Candidat_Maj, "COPPOLA|ROCHE|EXTREME GAUCHE|LCR|LO|FRUCTUS|PIETRI") ~ "EXTREME_GAUCHE",
    
    # --- LE CENTRE ET LES MACRONISTES (LREM, Modem, Divers Centre) ---
    str_detect(Candidat_Maj, "BERLAND|MODEM|DIOUF|AGRESTI|BENNAHMIAS|BIAGGI") ~ "CENTRE",
    
    # --- LES ECOLOGISTES (EELV) ---
    str_detect(Candidat_Maj, "BARLES|ECOLOGISTE|EELV|CANICIONI|HURON") ~ "ECOLOGISTE",
    
    # --- TOUT LE RESTE ---
    TRUE ~ "AUTRES_OU_INCONNU"
  )) %>%
  select(-Candidat_Maj) 

# POURCENTAGES FAMILLES POLITIQUES #

historique_final <- historique_avec_nuances %>%
  group_by(Annee, Tour, SECTEUR, Famille_Politique) %>%
  summarise(Total_Voix = sum(Total_Voix, na.rm = TRUE), .groups = 'drop') %>%
  
  group_by(Annee, Tour, SECTEUR) %>%
  mutate(Pourcentage = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
  ungroup() %>%
  
  arrange(Annee, Tour, SECTEUR, desc(Pourcentage))

print("Aperçu : Secteur 1 - Année 2020 - Tour 2")
print(
  historique_final %>% 
    filter(Annee == 2020 & Tour == 2 & SECTEUR == 1)
)

print("Candidats détaillés : Secteur 2 - Année 2020 - Tour 2")
print(
  historique_avec_nuances %>% 
    filter(Annee == 2020 & Tour == 2 & SECTEUR == 2) %>%
    select(Candidat, Famille_Politique, Total_Voix, Pourcentage) %>%
    arrange(desc(Total_Voix))
)

inconnus_majeurs <- historique_avec_nuances %>%
  filter(Famille_Politique == "AUTRES_OU_INCONNU" & Tour == 1) %>%
  group_by(Annee, SECTEUR, Candidat) %>%
  summarise(Voix = sum(Total_Voix), .groups = 'drop') %>%
  arrange(desc(Voix))

print("Liste des candidats non classés (Les plus gros scores) :")
print(head(inconnus_majeurs, 10))

# PRESIDENTIELLES 2022 #

presidentielles_1 <- read_excel("C:\\Users\\lucas\\OneDrive\\Desktop\\DATA\\ELECTIONS MARSEILLE 2026\\200820142020\\ELECTIONS PRESIDENTIELLES 1ER TOUR.xlsx")

nettoyer_excel_presid <- function(df_brut) {
  
  df_temp <- df_brut
  colnames(df_temp) <- paste0("V", 1:ncol(df_temp))
  
  df_marseille <- df_temp %>%
    filter(V1 %in% c("13", "13") & str_detect(toupper(V6), "MARSEILLE")) %>%
    mutate(
      Arrondissement = floor(as.numeric(V7) / 100),
      
      SECTEUR = ceiling(Arrondissement / 2)
    )
  
  df_long <- df_marseille %>%
    select(SECTEUR,
           Nom_01 = V24, Voix_01 = V26,
           Nom_02 = V31, Voix_02 = V33,
           Nom_03 = V38, Voix_03 = V40,
           Nom_04 = V45, Voix_04 = V47,
           Nom_05 = V52, Voix_05 = V54,
           Nom_06 = V59, Voix_06 = V61,
           Nom_07 = V66, Voix_07 = V68,
           Nom_08 = V73, Voix_08 = V75,
           Nom_09 = V80, Voix_09 = V82,
           Nom_10 = V87, Voix_10 = V89,
           Nom_11 = V94, Voix_11 = V96,
           Nom_12 = V101, Voix_12 = V103) %>%
    pivot_longer(
      cols = -SECTEUR,
      names_to = c(".value", "Numero"),
      names_pattern = "(Nom|Voix)_(.*)"
    ) %>%
    rename(Candidat = Nom) %>%
    filter(!is.na(Candidat) & Candidat != "") %>%
    mutate(
      Voix = as.numeric(Voix),
      Annee = 2022,
      Tour = 1
    )
  
  df_agg <- df_long %>%
    group_by(Annee, Tour, SECTEUR, Candidat) %>%
    summarise(Total_Voix = sum(Voix, na.rm = TRUE), .groups = 'drop')
  
  return(df_agg)
}

pres_2022_propre <- nettoyer_excel_presid(presidentielles_1)

print(head(pres_2022_propre))

pres_2022_propre <- nettoyer_excel_presid(presidentielles_1)

# DICO #

pres_2022_final <- pres_2022_propre %>%
  mutate(Candidat_Maj = toupper(Candidat)) %>%
  mutate(Famille_Politique = case_when(
    str_detect(Candidat_Maj, "MACRON") ~ "CENTRE",
    str_detect(Candidat_Maj, "LE PEN|ZEMMOUR|DUPONT-AIGNAN") ~ "EXTREME_DROITE",
    str_detect(Candidat_Maj, "MÉLENCHON|MELENCHON|HIDALGO|JADOT") ~ "GAUCHE",
    str_detect(Candidat_Maj, "PÉCRESSE|PECRESSE|LASSALLE") ~ "DROITE",
    str_detect(Candidat_Maj, "ROUSSEL|POUTOU|ARTHAUD") ~ "EXTREME_GAUCHE",
    TRUE ~ "AUTRES_OU_INCONNU"
  )) %>%
  group_by(Annee, Tour, SECTEUR, Famille_Politique) %>%
  summarise(Total_Voix = sum(Total_Voix, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Annee, Tour, SECTEUR) %>%
  mutate(Pourcentage_Pres_22 = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
  ungroup() %>%
  select(SECTEUR, Famille_Politique, Pct_Pres_2022 = Pourcentage_Pres_22) %>%
  arrange(SECTEUR, desc(Pct_Pres_2022))

print("Dynamique Présidentielle 2022 - Secteur 1 :")
print(pres_2022_final %>% filter(SECTEUR == 1))

# 2ND TOUR #

presidentielles_2 <- read_excel("C:\\Users\\lucas\\OneDrive\\Desktop\\DATA\\ELECTIONS MARSEILLE 2026\\200820142020\\ELECTIONS PRESIDENTIELLES 2ND TOUR.xlsx")

nettoyer_excel_presid_t2 <- function(df_brut) {
  
  df_temp <- df_brut
  colnames(df_temp) <- paste0("V", 1:ncol(df_temp))
  
  df_marseille <- df_temp %>%
    filter(V1 %in% c("13", "13") & str_detect(toupper(V6), "MARSEILLE")) %>%
    mutate(
      Arrondissement = floor(as.numeric(V7) / 100),
      SECTEUR = ceiling(Arrondissement / 2)
    )
  
  df_long <- df_marseille %>%
    select(SECTEUR,
           Nom_01 = V24, Voix_01 = V26,
           Nom_02 = V31, Voix_02 = V33) %>%
    pivot_longer(
      cols = -SECTEUR,
      names_to = c(".value", "Numero"),
      names_pattern = "(Nom|Voix)_(.*)"
    ) %>%
    rename(Candidat = Nom) %>%
    mutate(
      Voix = as.numeric(Voix),
      Annee = 2022,
      Tour = 2
    )
  
  df_agg <- df_long %>%
    group_by(Annee, Tour, SECTEUR, Candidat) %>%
    summarise(Total_Voix = sum(Voix, na.rm = TRUE), .groups = 'drop')
  
  return(df_agg)
}

pres_2022_t2_propre <- nettoyer_excel_presid_t2(presidentielles_2)

pres_2022_t2_final <- pres_2022_t2_propre %>%
  group_by(SECTEUR) %>%
  mutate(Pourcentage_T2 = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
  ungroup() %>%
  select(SECTEUR, Candidat, Pct_Pres_2022_T2 = Pourcentage_T2) %>%
  arrange(SECTEUR, desc(Pct_Pres_2022_T2))

print("Résultats du 2nd Tour 2022 par Secteur :")
print(pres_2022_t2_final)

# LEGISLATIVES #

legislatives_1 <- read_excel("C:\\Users\\lucas\\OneDrive\\Desktop\\DATA\\ELECTIONS MARSEILLE 2026\\200820142020\\ELECTIONS LEGISLATIVES 1ER TOUR.xlsx")

nettoyer_excel_legis <- function(df_brut) {
  
  df_temp <- df_brut
  colnames(df_temp) <- paste0("V", 1:ncol(df_temp))
  df_marseille <- df_temp %>%
    filter(V1 %in% c("13", "13") & str_detect(toupper(V4), "MARSEILLE")) %>%
    mutate(
      Arrondissement = floor(as.numeric(V5) / 100),
      SECTEUR = ceiling(Arrondissement / 2)
    )
  
  df_final <- data.frame()
  nb_cols <- ncol(df_marseille)
  
  for(i in seq(21, nb_cols - 4, by=9)) {
    nuance_col <- paste0("V", i)
    nom_col <- paste0("V", i+1)
    voix_col <- paste0("V", i+4)
    
    if(voix_col %in% colnames(df_marseille)) {
      temp <- df_marseille %>%
        select(SECTEUR, Nuance = all_of(nuance_col), Nom = all_of(nom_col), Voix = all_of(voix_col)) %>%
        filter(!is.na(Nom) & Nom != "") %>%
        mutate(
          # On fusionne la Nuance et le Nom pour que votre dictionnaire les reconnaisse !
          Candidat = paste(Nuance, toupper(Nom)),
          Voix = as.numeric(Voix)
        ) %>%
        select(SECTEUR, Candidat, Voix)
      
      df_final <- bind_rows(df_final, temp)
    }
  }
  
  df_agg <- df_final %>%
    mutate(Annee = 2024, Tour = 1) %>%
    group_by(Annee, Tour, SECTEUR, Candidat) %>%
    summarise(Total_Voix = sum(Voix, na.rm = TRUE), .groups = 'drop')
  
  return(df_agg)
}

leg_2024_propre <- nettoyer_excel_legis(legislatives_1)

# DICO 

leg_2024_nuances <- leg_2024_propre %>%
  mutate(Candidat_Maj = toupper(Candidat)) %>%
  mutate(Famille_Politique = case_when(
    str_detect(Candidat_Maj, "UG|NOUVEAU FRONT POPULAIRE|BOMPARD|DELOGU|HENDRIK|LECOQ") ~ "GAUCHE",
    str_detect(Candidat_Maj, "RN|RASSEMBLEMENT NATIONAL|ALLISIO|BAUMANN|GONZALEZ|VERA|EXD|UXD") ~ "EXTREME_DROITE",
    str_detect(Candidat_Maj, "ENS|ENSEMBLE|RENAISSANCE|AGRESTI|CASA|RICHARD") ~ "CENTRE",
    str_detect(Candidat_Maj, "LR|REPUBLICAINS|DROITE|GIOCANTI|BERNASCONI|DVD|UDI") ~ "DROITE",
    TRUE ~ "AUTRES_OU_INCONNU"
  )) %>%
  select(-Candidat_Maj)

# 5. AGRÉGATION PAR SECTEUR 

leg_2024_final <- leg_2024_nuances %>%
  group_by(Annee, Tour, SECTEUR, Famille_Politique) %>%
  summarise(Total_Voix = sum(Total_Voix, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Annee, Tour, SECTEUR) %>%
  mutate(Pourcentage_Legis_24 = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
  ungroup() %>%
  select(SECTEUR, Famille_Politique, Pct_Legis_2024 = Pourcentage_Legis_24) %>%
  arrange(SECTEUR, desc(Pct_Legis_2024))

print("Dynamique Législatives 2024 - Secteur 1 :")
print(leg_2024_final %>% filter(SECTEUR == 1))

# ===============================================================================

legislatives_2 <- read_excel("C:\\Users\\lucas\\OneDrive\\Desktop\\DATA\\ELECTIONS MARSEILLE 2026\\200820142020\\ELECTIONS LEGISLATIVES 2ND TOUR.xlsx")

nettoyer_excel_legis_t2 <- function(df_brut) {
  
  df_temp <- df_brut
  colnames(df_temp) <- paste0("V", 1:ncol(df_temp))
  
  df_marseille <- df_temp %>%
    filter(V1 %in% c("13", "13") & str_detect(toupper(V4), "MARSEILLE")) %>%
    mutate(
      Arrondissement = floor(as.numeric(V5) / 100),
      SECTEUR = ceiling(Arrondissement / 2)
    )
  df_final <- data.frame()
  nb_cols <- ncol(df_marseille)
  
  for(i in seq(21, nb_cols - 4, by=9)) {
    nuance_col <- paste0("V", i)
    nom_col <- paste0("V", i+1)
    voix_col <- paste0("V", i+4)
    
    if(voix_col %in% colnames(df_marseille)) {
      temp <- df_marseille %>%
        select(SECTEUR, Nuance = all_of(nuance_col), Nom = all_of(nom_col), Voix = all_of(voix_col)) %>%
        filter(!is.na(Nom) & Nom != "") %>%
        mutate(
          Candidat = paste(Nuance, toupper(Nom)),
          Voix = as.numeric(Voix)
        ) %>%
        select(SECTEUR, Candidat, Voix)
      
      df_final <- bind_rows(df_final, temp)
    }
  }
  df_agg <- df_final %>%
    mutate(Annee = 2024, Tour = 2) %>%
    group_by(Annee, Tour, SECTEUR, Candidat) %>%
    summarise(Total_Voix = sum(Voix, na.rm = TRUE), .groups = 'drop')
  
  return(df_agg)
}

leg_2024_t2_propre <- nettoyer_excel_legis_t2(legislatives_2)

# DICO 

leg_2024_t2_nuances <- leg_2024_t2_propre %>%
  mutate(Candidat_Maj = toupper(Candidat)) %>%
  mutate(Famille_Politique = case_when(
    str_detect(Candidat_Maj, "UG|NOUVEAU FRONT POPULAIRE|BOMPARD|DELOGU|HENDRIK|LECOQ") ~ "GAUCHE",
    str_detect(Candidat_Maj, "RN|RASSEMBLEMENT NATIONAL|ALLISIO|BAUMANN|GONZALEZ|VERA|EXD|UXD") ~ "EXTREME_DROITE",
    str_detect(Candidat_Maj, "ENS|ENSEMBLE|RENAISSANCE|AGRESTI|CASA|RICHARD") ~ "CENTRE",
    str_detect(Candidat_Maj, "LR|REPUBLICAINS|DROITE|GIOCANTI|BERNASCONI|DVD|UDI") ~ "DROITE",
    TRUE ~ "AUTRES_OU_INCONNU"
  )) %>%
  select(-Candidat_Maj)

# 5. AGRÉGATION FINALE PAR SECTEUR

leg_2024_t2_final <- leg_2024_t2_nuances %>%
  group_by(Annee, Tour, SECTEUR, Famille_Politique) %>%
  summarise(Total_Voix = sum(Total_Voix, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Annee, Tour, SECTEUR) %>%
  mutate(Pourcentage_Legis_24_T2 = round((Total_Voix / sum(Total_Voix)) * 100, 2)) %>%
  ungroup() %>%
  select(SECTEUR, Famille_Politique, Pct_Legis_2024_T2 = Pourcentage_Legis_24_T2) %>%
  arrange(SECTEUR, desc(Pct_Legis_2024_T2))

print("Dynamique Législatives 2024 (Tour 2) - Secteur 1 :")
print(leg_2024_t2_final %>% filter(SECTEUR == 1))

# DONNEES PAUVRETE REVENU 2021 MARSEILLE #

chemin_insee_pauvrete_revenu <- "C:\\Users\\lucas\\OneDrive\\Desktop\\DATA\\ELECTIONS MARSEILLE 2026\\200820142020\\REVENU_NIVEAU_DE_VIE_ET_PAUVRETE_2021.csv"

insee_brut_pauvrete_reevenu <- read_delim(chemin_insee_pauvrete_revenu, 
                         delim = ";", 
                         locale = locale(encoding = "latin1"),
                         show_col_types = FALSE)

insee_marseille_pauvrete_revenu <- insee_brut_pauvrete_reevenu %>%
  filter(str_starts(IRIS, "132")) %>%
  mutate(
    Arrondissement = as.numeric(str_sub(IRIS, 4, 5)),
    SECTEUR = ceiling(Arrondissement / 2)
  ) %>%
  select(
    IRIS,
    SECTEUR,
    Revenu_Median = DISP_MED21,       # Niveau de vie médian
    Taux_Pauvrete = DISP_TP6021,      # Taux de pauvreté
    Part_Retraites = DISP_PPEN21,     # Part des pensions et retraites
    Part_Chomage = DISP_PCHO21        # Part des indemnités chômage
  ) %>%
  mutate(across(-c(IRIS, SECTEUR), ~ str_replace(., ",", "."))) %>%
  mutate(across(-c(IRIS, SECTEUR), ~ suppressWarnings(as.numeric(.))))

insee_secteurs_pauvrete_revenu <- insee_marseille_pauvrete_revenu %>%
  group_by(SECTEUR) %>%
  summarise(
    Revenu_Median_Moyen = round(mean(Revenu_Median, na.rm = TRUE), 0),
    Taux_Pauvrete_Moyen = round(mean(Taux_Pauvrete, na.rm = TRUE), 1),
    Part_Retraites_Moyenne = round(mean(Part_Retraites, na.rm = TRUE), 1),
    Part_Chomage_Moyenne = round(mean(Part_Chomage, na.rm = TRUE), 1)
  ) %>%
  arrange(SECTEUR)
print("Sociologie des 8 Secteurs Marseillais (Données INSEE 2021) :")
print(insee_secteurs_pauvrete_revenu)

# RECENSEMENT POPULATION 2022 MARSEILLE # 

chemin_recensement <- "C:\\Users\\lucas\\OneDrive\\Desktop\\DATA\\ELECTIONS MARSEILLE 2026\\200820142020\\RECENSEMENT_POPULATION_2022.CSV"

recensement_brut <- read_delim(chemin_recensement, 
                               delim = ";", 
                               locale = locale(encoding = "latin1"),
                               show_col_types = FALSE)

# NETTOYAGE ET CALCUL DES INDICATEURS DÉMOGRAPHIQUES #

recensement_marseille <- recensement_brut %>%
  filter(str_starts(IRIS, "132")) %>%
  mutate(
    Arrondissement = as.numeric(str_sub(IRIS, 4, 5)),
    SECTEUR = ceiling(Arrondissement / 2)
  ) %>%
  select(SECTEUR, P22_POP, P22_POP0014, P22_POP1529, P22_POP3044, P22_POP4559, P22_POP6074, P22_POP75P) %>%
  mutate(across(starts_with("P22"), ~ as.numeric(str_replace(as.character(.), ",", "."))))

# AGRÉGATION PAR SECTEUR # 
demo_secteurs <- recensement_marseille %>%
  group_by(SECTEUR) %>%
  summarise(
    Population_Totale = round(sum(P22_POP, na.rm = TRUE), 0),
    Pop_Jeunes = sum(P22_POP0014, na.rm=T) + sum(P22_POP1529, na.rm=T),
    Pop_Actifs = sum(P22_POP3044, na.rm=T) + sum(P22_POP4559, na.rm=T),
    Pop_Seniors = sum(P22_POP6074, na.rm=T) + sum(P22_POP75P, na.rm=T)
  ) %>%
  
  mutate(
    Part_Jeunes_Moins_30 = round((Pop_Jeunes / Population_Totale) * 100, 1),
    Part_Actifs_30_59 = round((Pop_Actifs / Population_Totale) * 100, 1),
    Part_Seniors_Plus_60 = round((Pop_Seniors / Population_Totale) * 100, 1)
  ) %>%
  
  select(SECTEUR, Population_Totale, Part_Jeunes_Moins_30, Part_Actifs_30_59, Part_Seniors_Plus_60) %>%
  arrange(SECTEUR)

print("Démographie des 8 Secteurs Marseillais (INSEE 2022) :")
print(demo_secteurs)

# ACTIVITES DES RESIDENTS #

chemin_activite <- "C:/Users/lucas/OneDrive/Desktop/DATA/ELECTIONS MARSEILLE 2026/200820142020/ACTIVITES_RESIDENTS.CSV"

activite_brut <- read_delim(chemin_activite, 
                            delim = ";", 
                            locale = locale(encoding = "latin1"),
                            show_col_types = FALSE)

# NETTOYAGE ET EXTRACTION DES DONNÉES CLÉS # 

activite_marseille <- activite_brut %>%
  filter(str_starts(IRIS, "132")) %>%
  mutate(
    Arrondissement = as.numeric(str_sub(IRIS, 4, 5)),
    SECTEUR = ceiling(Arrondissement / 2)
  ) %>%
  select(SECTEUR, P22_POP1564, P22_ACT1564, P22_CHOM1564, P22_INACT1564) %>%
  mutate(across(starts_with("P22"), ~ as.numeric(str_replace(as.character(.), ",", "."))))

# AGRÉGATION PAR SECTEUR ET CALCUL DES TAUX # 

activite_secteurs <- activite_marseille %>%
  group_by(SECTEUR) %>%
  summarise(
    Total_15_64 = round(sum(P22_POP1564, na.rm = TRUE), 0),
    Total_Actifs = round(sum(P22_ACT1564, na.rm = TRUE), 0),
    Total_Chomeurs = round(sum(P22_CHOM1564, na.rm = TRUE), 0),
    Total_Inactifs = round(sum(P22_INACT1564, na.rm = TRUE), 0)
  ) %>%
  mutate(
    Taux_Activite = round((Total_Actifs / Total_15_64) * 100, 1),
    Taux_Chomage = round((Total_Chomeurs / Total_Actifs) * 100, 1),
    Taux_Inactivite = round((Total_Inactifs / Total_15_64) * 100, 1)
  ) %>%
  select(SECTEUR, Taux_Activite, Taux_Chomage, Taux_Inactivite) %>%
  arrange(SECTEUR)

print("Indicateurs d'Activité et de Chômage - 8 Secteurs Marseillais :")
print(activite_secteurs)

# LOGEMENT 2022 # 

chemin_logement <- "C:/Users/lucas/OneDrive/Desktop/DATA/ELECTIONS MARSEILLE 2026/200820142020/LOGEMENT_2022.CSV"

logement_brut <- read_delim(chemin_logement, 
                            delim = ";", 
                            locale = locale(encoding = "latin1"),
                            show_col_types = FALSE)

# NETTOYAGE ET EXTRACTION DES VARIABLES CLÉS # 

logement_marseille <- logement_brut %>%
  filter(str_starts(IRIS, "132")) %>%
  mutate(
    Arrondissement = as.numeric(str_sub(IRIS, 4, 5)),
    SECTEUR = ceiling(Arrondissement / 2)
  ) %>%
  select(SECTEUR, P22_RP, P22_RP_PROP, P22_RP_LOC, P22_RP_LOCHLMV) %>%
  mutate(across(starts_with("P22"), ~ as.numeric(str_replace(as.character(.), ",", "."))))

# AGRÉGATION ET CALCUL DES POURCENTAGES PAR SECTEUR # 

logement_secteurs <- logement_marseille %>%
  group_by(SECTEUR) %>%
  summarise(
    Total_Logements = round(sum(P22_RP, na.rm = TRUE), 0),
    Total_Proprietaires = round(sum(P22_RP_PROP, na.rm = TRUE), 0),
    Total_Locataires = round(sum(P22_RP_LOC, na.rm = TRUE), 0),
    Total_HLM = round(sum(P22_RP_LOCHLMV, na.rm = TRUE), 0)
  ) %>%
  mutate(
    Part_Proprietaires = round((Total_Proprietaires / Total_Logements) * 100, 1),
    Part_Locataires_Total = round((Total_Locataires / Total_Logements) * 100, 1),
    Part_HLM = round((Total_HLM / Total_Logements) * 100, 1)
  ) %>%
  select(SECTEUR, Part_Proprietaires, Part_Locataires_Total, Part_HLM) %>%
  arrange(SECTEUR)

print("Statut du Logement - 8 Secteurs Marseillais :")
print(logement_secteurs)

# ==============================================================================
# ETAPE 5 : LA GRANDE FUSION 
# ==============================================================================

# 1. PIVOTER LES LÉGISLATIVES 2024 (Format Large)
leg_2024_wide <- leg_2024_final %>%
  select(SECTEUR, Famille_Politique, Pct_Legis_2024) %>%
  pivot_wider(
    names_from = Famille_Politique, 
    values_from = Pct_Legis_2024,
    names_prefix = "Legis24_"
  )

# 2. PIVOTER LES PRÉSIDENTIELLES 2022 
pres_2022_wide <- pres_2022_final %>%
  select(SECTEUR, Famille_Politique, Pct_Pres_2022) %>%
  pivot_wider(
    names_from = Famille_Politique, 
    values_from = Pct_Pres_2022,
    names_prefix = "Pres22_"
  )

# 3. FUSION DE TOUTES LES TABLES 
dataset_master <- insee_secteurs_pauvrete_revenu %>%
  # On ajoute la démographie (Âge)
  left_join(demo_secteurs, by = "SECTEUR") %>%
  # On ajoute l'activité (Chômage/Actifs)
  left_join(activite_secteurs, by = "SECTEUR") %>%
  # On ajoute le logement (HLM/Propriétaires)
  left_join(logement_secteurs, by = "SECTEUR") %>%
  # On ajoute les Présidentielles 2022
  left_join(pres_2022_wide, by = "SECTEUR") %>%
  # On ajoute les Législatives 2024
  left_join(leg_2024_wide, by = "SECTEUR")

dataset_master[is.na(dataset_master)] <- 0

print("Aperçu du Dataset Master (Variables pour le Secteur 1) :")
glimpse(dataset_master)

write.csv(dataset_master, "dataset_complet.csv")

# ==============================================================================
# ETAPE 6 : PRÉDICTION DES MUNICIPALES 2026 
# ==============================================================================

df_predictions <- read_csv("dataset_complet.csv", show_col_types = FALSE)

sondage_gauche <- 38.0  # Printemps Marseillais / Union de la Gauche
sondage_rn <- 31.0      # Rassemblement National / UDR
sondage_centre <- 14.0  # Renaissance / Ensemble
sondage_droite <- 12.0  # Les Républicains / Martine Vassal (Droite)

# 3. CALCUL DES MOYENNES DE BASE 

moy_legis_gauche <- sum(df_predictions$Legis24_GAUCHE * df_predictions$Population_Totale) / sum(df_predictions$Population_Totale)

moy_legis_rn <- sum(df_predictions$Legis24_EXTREME_DROITE * df_predictions$Population_Totale) / sum(df_predictions$Population_Totale)

moy_legis_centre <- sum(df_predictions$Legis24_CENTRE * df_predictions$Population_Totale) / sum(df_predictions$Population_Totale)

moy_legis_droite <- sum(df_predictions$Legis24_DROITE * df_predictions$Population_Totale) / sum(df_predictions$Population_Totale)

# 4. CALCUL DU "SWING" (La dynamique nationale/locale)

swing_gauche <- sondage_gauche - moy_legis_gauche
swing_rn <- sondage_rn - moy_legis_rn
swing_centre <- sondage_centre - moy_legis_centre
swing_droite <- sondage_droite - moy_legis_droite

# 5. PRÉDICTION SECTEUR PAR SECTEUR (Avec correction sociologique)

predictions_2026 <- df_predictions %>%
  mutate(
    # --- PRÉDICTION GAUCHE ---
    # La dynamique de gauche est dopée par la jeunesse et les locataires HLM
    Bonus_Socio_Gauche = (Part_HLM - mean(Part_HLM))*0.1 + (Part_Jeunes_Moins_30 - mean(Part_Jeunes_Moins_30))*0.1,
    Pred_2026_Gauche = round(Legis24_GAUCHE + swing_gauche + Bonus_Socio_Gauche, 1),
    
    # --- PRÉDICTION RN ---
    # Le RN est dopé par le taux de pauvreté et pénalisé par le centre-ville très aisé
    Bonus_Socio_RN = (Taux_Pauvrete_Moyen - mean(Taux_Pauvrete_Moyen))*0.15,
    Pred_2026_RN = round(Legis24_EXTREME_DROITE + swing_rn + Bonus_Socio_RN, 1),
    
    # --- PRÉDICTION CENTRE ---
    # Le Centre résiste mieux chez les actifs ayant de bons revenus
    Bonus_Socio_Centre = (Revenu_Median_Moyen - mean(Revenu_Median_Moyen))*0.0001,
    Pred_2026_Centre = round(Legis24_CENTRE + swing_centre + Bonus_Socio_Centre, 1),
    
    # --- PRÉDICTION DROITE (LR) ---
    # La Droite classique est massivement dopée par la part de propriétaires et de retraités
    Bonus_Socio_Droite = (Part_Proprietaires - mean(Part_Proprietaires))*0.2 + (Part_Seniors_Plus_60 - mean(Part_Seniors_Plus_60))*0.2,
    # (Comme la droite était très faible aux législatives, on s'appuie plus sur la sociologie pure)
    Pred_2026_Droite = round(Legis24_DROITE + swing_droite + Bonus_Socio_Droite, 1)
  ) %>%
  
  # On s'assure qu'aucun score n'est négatif
  mutate(across(starts_with("Pred_"), ~ ifelse(. < 0, 0, .))) %>%
  
  # On sélectionne les résultats pour affichage
  select(SECTEUR, Pred_2026_Gauche, Pred_2026_RN, Pred_2026_Centre, Pred_2026_Droite)

print("Prédictions 1er Tour des Municipales 2026 par Secteur :")
print(predictions_2026)

write.csv(predictions_2026, "predictions_2026.csv")

# ==============================================================================
# ETAPE 7 : LE CONSEIL MUNICIPAL 2026 ET L'ÉLECTION DU MAIRE (Loi Août 2025)
# ==============================================================================

df_calcul_sieges <- predictions_2026 %>%
  left_join(dataset_master %>% select(SECTEUR, Population_Totale), by = "SECTEUR") %>%
  mutate(
    Voix_Estimees_Gauche = (Pred_2026_Gauche / 100) * Population_Totale,
    Voix_Estimees_RN = (Pred_2026_RN / 100) * Population_Totale,
    Voix_Estimees_Centre = (Pred_2026_Centre / 100) * Population_Totale,
    Voix_Estimees_Droite = (Pred_2026_Droite / 100) * Population_Totale
  )

resultats_globaux <- data.frame(
  Famille = c("GAUCHE", "RN", "CENTRE", "DROITE"),
  Voix = c(
    sum(df_calcul_sieges$Voix_Estimees_Gauche, na.rm = TRUE),
    sum(df_calcul_sieges$Voix_Estimees_RN, na.rm = TRUE),
    sum(df_calcul_sieges$Voix_Estimees_Centre, na.rm = TRUE),
    sum(df_calcul_sieges$Voix_Estimees_Droite, na.rm = TRUE)
  )
) %>%
  mutate(Pourcentage_Global = round((Voix / sum(Voix)) * 100, 2)) %>%
  arrange(desc(Pourcentage_Global))

print("Scores globaux estimés à l'échelle de Marseille :")
print(resultats_globaux)

# Application de la nouvelle loi PLM (111 sièges) #
total_sieges <- 111
prime_majoritaire <- ceiling(total_sieges * 0.25) # 25% arrondi au supérieur = 28 sièges
sieges_proportionnels <- total_sieges - prime_majoritaire # 83 sièges restants

# Filtrer les listes au-dessus du seuil de 5%
listes_eligibles <- resultats_globaux %>% filter(Pourcentage_Global >= 5)
total_voix_eligibles <- sum(listes_eligibles$Voix)

listes_eligibles <- listes_eligibles %>%
  mutate(
    Sieges_Prop_Base = floor(Voix * sieges_proportionnels / total_voix_eligibles)
  )

sieges_restants <- sieges_proportionnels - sum(listes_eligibles$Sieges_Prop_Base)

listes_eligibles$Sieges_Prop_Finaux <- listes_eligibles$Sieges_Prop_Base

if(sieges_restants > 0) {
  for(i in 1:sieges_restants) {
    moyennes <- listes_eligibles$Voix / (listes_eligibles$Sieges_Prop_Finaux + 1)
    index_gagnant <- which.max(moyennes)
    listes_eligibles$Sieges_Prop_Finaux[index_gagnant] <- listes_eligibles$Sieges_Prop_Finaux[index_gagnant] + 1
  }
}

# Ajout de la prime majoritaire de 25% à la liste arrivée en tête

listes_eligibles <- listes_eligibles %>%
  mutate(
    Prime = ifelse(row_number() == 1, prime_majoritaire, 0),
    Total_Sieges_Mairie = Sieges_Prop_Finaux + Prime
  ) %>%
  select(Famille, Pourcentage_Global, Total_Sieges_Mairie)

print("Répartition finale des 111 sièges du Conseil Municipal de Marseille :")
print(listes_eligibles)

# VISUALISATIONS #

df_graph_secteurs <- predictions_2026 %>%
  pivot_longer(
    cols = starts_with("Pred_2026_"),
    names_to = "Famille",
    values_to = "Score"
  ) %>%
  mutate(
    Famille = str_replace(Famille, "Pred_2026_", ""),
    Famille = toupper(Famille) # Tout en majuscule
  )

plot_secteurs <- ggplot(df_graph_secteurs, aes(x = as.factor(SECTEUR), y = Score, fill = Famille)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(
    "GAUCHE" = "#E41A1C",  # Rouge
    "RN"     = "#000080",  # Bleu marine
    "CENTRE" = "#FFD700",  # Jaune/Orange
    "DROITE" = "#00BFFF"   # Bleu clair
  )) +
  labs(
    title = "Prédictions des élections Municipales 2026 par Secteur",
    subtitle = "Fracture électorale entre le Nord/Centre (Gauche) et le Sud/Est (RN-Droite)",
    x = "Secteurs de Marseille",
    y = "Score estimé au 1er tour (%)",
    fill = "Bloc Politique"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 11)
  )

print(plot_secteurs)

ggsave("plot_secteurs.png", width = 8, height = 6, dpi = 300)

plot_conseil <- ggplot(listes_eligibles, aes(x = reorder(Famille, -Total_Sieges_Mairie), y = Total_Sieges_Mairie, fill = Famille)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = paste0(Total_Sieges_Mairie, " élus")), vjust = -0.8, size = 5, fontface = "bold") +
  geom_hline(yintercept = 56, linetype = "dashed", color = "red", linewidth = 1) + 
  annotate("text", x = 2.5, y = 60, label = "MAJORITÉ ABSOLUE (56 sièges)", color = "red", fontface = "italic") +
  scale_fill_manual(values = c(
    "GAUCHE" = "#E41A1C", 
    "RN"     = "#000080", 
    "CENTRE" = "#FFD700", 
    "DROITE" = "#00BFFF"
  )) +
  ylim(0, max(listes_eligibles$Total_Sieges_Mairie) + 15) +
  labs(
    title = "Projection du futur Conseil Municipal de Marseille",
    subtitle = "Répartition des 111 sièges",
    x = "Familles Politiques au Conseil",
    y = "Nombre de Conseillers Municipaux"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 15, color = "#2C3E50"),
    axis.text.x = element_text(face = "bold", size = 12)
  )

print(plot_conseil)

ggsave("plot_conseil.png", width = 8, height = 6, dpi = 300)

df_candidats <- tibble(
  SECTEUR = rep(1:8, each = 4),
  Famille = rep(c("GAUCHE", "RN", "CENTRE", "DROITE"), times = 8),
  Candidat = c(
    "S. Camard", "C. Parodi", "B. Aksil", "R. Simmarano",             # Secteur 1
    "A. Krehmeier", "M. Bermejo", "B. Suaton", "S. Biaggi",           # Secteur 2
    "D. Jau", "T. Battesti", "Sancho", "B. Gilles",                   # Secteur 3
    "O. Fortin", "J-B. Rivoallan", "L. Chikhoune", "C. Pila",         # Secteur 4
    "P. Huguet", "E. Bez", "J. Traimond", "L-A. Caradec",             # Secteur 5
    "Y. Ohanessian", "O. Rioult", "A. el Mestari", "S. Souvestre",    # Secteur 6
    "T. B. Sansonetti", "S. d'Angio", "S. M. Ibrahim", "M. Bareille", # Secteur 7
    "S. Ghali", "T. Carpentier", "J. Schmidt", "G. Blanc"             # Secteur 8
  )
)

# 2. Préparation des données pour le graphique en format "long"
df_graph_candidats <- predictions_2026 %>%
  pivot_longer(
    cols = starts_with("Pred_2026_"),
    names_to = "Famille",
    values_to = "Score"
  ) %>%
  mutate(
    Famille = str_replace(Famille, "Pred_2026_", ""),
    Famille = toupper(Famille)
  ) %>%
  # Jointure avec le dataframe des candidats
  left_join(df_candidats, by = c("SECTEUR", "Famille"))

# 3. Création du graphique avec ggplot2
plot_candidats <- ggplot(df_graph_candidats, aes(x = as.factor(SECTEUR), y = Score, fill = Famille)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  
  # Ajout des étiquettes (Nom du candidat + Score %) sur les barres
  geom_text(
    aes(label = paste0(Candidat, "\n(", Score, "%)"), y = Score + 2),
    position = position_dodge(width = 0.8),
    hjust = 0,       # Aligne le texte au début (car on le tourne à 90°)
    vjust = 0.5,     # Centre le texte par rapport à la barre
    angle = 90,      # Rotation à 90 degrés pour la lisibilité
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  
  scale_fill_manual(
    values = c(
      "GAUCHE" = "#E41A1C", # Rouge
      "RN" = "#000080",     # Bleu marine
      "CENTRE" = "#FFD700", # Jaune/Orange
      "DROITE" = "#00BFFF"  # Bleu clair
    ),
    labels = c(
      "CENTRE" = "Centre / Écolo",
      "DROITE" = "Droite (UDC)",
      "GAUCHE" = "Gauche (Print. Mars.)",
      "RN" = "Rassemblement National"
    )
  ) +
  
  labs(
    title = "Prédictions des élections Municipales 2026 par Secteur et par Candidat",
    subtitle = "Projection des scores au 1er tour intégrant les têtes de liste déclarées",
    x = "Secteurs de Marseille",
    y = "Score estimé au 1er tour (%)",
    fill = "Bloc Politique"
  ) +
  
  # On étend l'axe Y jusqu'à 100 ou 110 pour éviter que les noms des candidats soient coupés en haut
  scale_y_continuous(limits = c(0, max(df_graph_candidats$Score) + 30)) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 11),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

print(plot_candidats)
ggsave("plot_secteurs_candidats.png", plot_candidats, width = 12, height = 7, dpi = 300)

df_tour1 <- predictions_2026 %>%
  mutate(
    Score_PM = round(Pred_2026_Gauche * 0.70, 1),
    Score_LFI = round(Pred_2026_Gauche * 0.30, 1),
    Score_ECOLO = round(Pred_2026_Centre, 1),
    Score_DROITE = round(Pred_2026_Droite, 1),
    Score_RN = round(Pred_2026_RN, 1)
  ) %>%
  select(SECTEUR, Score_PM, Score_LFI, Score_ECOLO, Score_DROITE, Score_RN)

# 2. Dictionnaire des candidats par parti (Issu du PDF)
df_candidats_t1 <- tibble(
  SECTEUR = rep(1:8, each = 5),
  Famille = rep(c("Score_PM", "Score_LFI", "Score_ECOLO", "Score_DROITE", "Score_RN"), times = 8),
  Candidat = c(
    "S. Camard", "S. Barles", "B. Aksil", "R. Simmarano", "C. Parodi",
    "A. Krehmeier", "A. Salim", "B. Suaton", "S. Biaggi", "M. Bermejo",
    "D. Jau", "L. Bijaoui", "Sancho", "B. Gilles", "T. Battesti",
    "O. Fortin", "V. Diethelm", "L. Chikhoune", "C. Pila", "J-B. Rivoallan",
    "P. Huguet", "M. Meghraoui", "J. Traimond", "L-A. Caradec", "E. Bez",
    "Y. Ohanessian", "R. Ouachani", "A. el Mestari", "S. Souvestre", "O. Rioult",
    "T. B. Sansonetti", "M. Bensaada", "S. M. Ibrahim", "M. Bareille", "S. d'Angio",
    "S. Ghali", "R. Boinaheri", "J. Schmidt", "G. Blanc", "T. Carpentier"
  )
)

df_graph_t1 <- df_tour1 %>%
  pivot_longer(cols = -SECTEUR, names_to = "Famille", values_to = "Score") %>%
  left_join(df_candidats_t1, by = c("SECTEUR", "Famille"))

plot_t1 <- ggplot(df_graph_t1, aes(x = as.factor(SECTEUR), y = Score, fill = Famille)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0("\n(", Score, "%)"), y = Score + 0.5),
    position = position_dodge(width = 0.8),
    hjust = 0, vjust = 0, angle = 90, size = 2, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Score_PM" = "#E41A1C", "Score_LFI" = "#984EA3", 
               "Score_ECOLO" = "#4DAF4A", "Score_DROITE" = "#00BFFF", "Score_RN" = "#000080"),
    labels = c("Score_DROITE" = "Droite/Centre", "Score_ECOLO" = "Écologie au centre", 
               "Score_LFI" = "La France Insoumise", "Score_PM" = "Printemps Marseillais", "Score_RN" = "RN")
  ) +
  labs(title = "Simulation du 1er Tour - Prise en compte de la division des listes",
       x = "Secteurs de Marseille", y = "Score estimé (%)", fill = "Listes (Tour 1)") +
  scale_y_continuous(limits = c(0, max(df_graph_t1$Score) + 10)) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(face = "bold"))

print(plot_t1)

df_tour2 <- df_tour1 %>%
  mutate(
    UG_Score_Brut = Score_PM + Score_LFI + Score_ECOLO,
    Droite_Score_Brut = ifelse(Score_DROITE >= 10, Score_DROITE, 0),
    RN_Score_Brut = ifelse(Score_RN >= 10, Score_RN, 0)
  ) %>%
  mutate(
    Total_T2 = UG_Score_Brut + Droite_Score_Brut + RN_Score_Brut,
    Union_Gauche = round((UG_Score_Brut / Total_T2) * 100, 1),
    Droite_T2 = round((Droite_Score_Brut / Total_T2) * 100, 1),
    RN_T2 = round((RN_Score_Brut / Total_T2) * 100, 1)
  ) %>%
  select(SECTEUR, Union_Gauche, Droite_T2, RN_T2)

df_graph_t2 <- df_tour2 %>%
  pivot_longer(cols = -SECTEUR, names_to = "Famille", values_to = "Score") %>%
  filter(Score > 0) # Retirer les listes éliminées qui ont 0%

plot_t2 <- ggplot(df_graph_t2, aes(x = as.factor(SECTEUR), y = Score, fill = Famille)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(Score, "%"), y = Score + 3),
    position = position_dodge(width = 0.8),
    hjust = 0, vjust = 0, angle = 90, size = 2.5, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Union_Gauche" = "#E41A1C", "Droite_T2" = "#00BFFF", "RN_T2" = "#000080"),
    labels = c("Droite_T2" = "Liste de Droite (Maintenue)", 
               "RN_T2" = "Liste du RN (Maintenue)", 
               "Union_Gauche" = "Union de la Gauche et Écologistes")
  ) +
  labs(title = "Simulation du 2nd Tour - Hypothèse de Rassemblement face aux qualifiés",
       x = "Secteurs de Marseille", y = "Score estimé au Second Tour (%)", fill = "Listes (Tour 2)") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(face = "bold"))

print(plot_t2)

# 1. Calcul de la répartition des sièges (Loi PLM 2025)
# On simule le résultat global Marseille sur la base du 2nd tour
total_sieges <- 111
prime <- 28  # 25% de prime majoritaire
prop_sieges <- 83

# Données simulées pour le Conseil Municipal (Total Ville)
# Basé sur la moyenne pondérée de vos prédictions
sieges_data <- tibble(
  Parti = c("GAUCHE", "RN", "DROITE"),
  Sieges = c(58, 38, 15) # Exemple : La gauche gagne et prend la prime
)

# Création des coordonnées pour un hémicycle
df_hemicycle <- sieges_data %>%
  uncount(Sieges) %>%
  mutate(
    angle = seq(pi, 0, length.out = n()),
    x = cos(angle),
    y = sin(angle)
  )

# Plot de l'Hémicycle
ggplot(df_hemicycle, aes(x, y, color = Parti)) +
  geom_point(size = 5) +
  scale_color_manual(values = c("GAUCHE" = "#E41A1C", "RN" = "#000080", "DROITE" = "#00BFFF")) +
  coord_fixed() +
  labs(title = "Projection du Conseil Municipal de Marseille (111 sièges)",
       subtitle = "Le bloc en tête (Gauche) obtient la majorité absolue grâce à la prime de 25%",
       x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face="bold"))

# On suppose que vous avez dataset_master et predictions_2026
df_correlation <- dataset_master %>%
  inner_join(predictions_2026, by = "SECTEUR")

ggplot(df_correlation, aes(x = Taux_Pauvrete_Moyen, y = Pred_2026_RN)) +
  geom_point(aes(size = Population_Totale), color = "#000080", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  geom_text(aes(label = paste("Secteur", SECTEUR)), vjust = -1) +
  labs(title = "Analyse Sociologique : Pauvreté vs Vote RN",
       x = "Taux de Pauvreté du Secteur (%)",
       y = "Prédiction Vote RN 2026 (%)",
       size = "Population") +
  theme_minimal()

# Préparation des données pour un Radar Chart
# On compare Secteur 1 et Secteur 5
df_radar <- dataset_master %>%
  filter(SECTEUR %in% c(1, 5)) %>%
  select(SECTEUR, Revenu_Median_Moyen, Taux_Pauvrete_Moyen, Part_Jeunes_Moins_30, Part_HLM)

# Normalisation pour le radar chart (max et min en premières lignes)
max_vals <- c(30000, 50, 40, 40)
min_vals <- c(10000, 0, 0, 0)
radar_final <- rbind(max_vals, min_vals, df_radar[,-1])

# Plot
radarchart(radar_final, 
           pcol = c("#E41A1C", "#000080"), 
           plwd = 4, 
           title = "Identité Sociologique : Secteur 1 (Gauche) vs Secteur 5 (RN)")
legend(x=1, y=1, legend = c("Secteur 1", "Secteur 5"), col = c("#E41A1C", "#000080"), lty = 1, lwd = 2)

df_heatmap <- df_graph_secteurs %>%
  mutate(Famille = factor(Famille, levels = c("GAUCHE", "RN", "CENTRE", "DROITE")))

ggplot(df_heatmap, aes(x = Famille, y = as.factor(SECTEUR), fill = Score)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(Score, "%")), color = "black", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#F0F0F0", high = "#E41A1C", name = "Score estimé") +
  scale_y_discrete(limits = rev) + # Inverser l'axe Y pour avoir le secteur 1 en haut
  labs(
    title = "Matrice de Force Électorale par Secteur",
    subtitle = "Identification des bastions et des zones de conquête",
    x = "Bloc Politique",
    y = "Secteur de Marseille"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid = element_blank()
  )

# Simulation de 1000 scénarios électoraux avec une marge d'erreur de 4%
set.seed(2026) # Pour la reproductibilité
df_simul <- predictions_2026 %>%
  select(SECTEUR, starts_with("Pred_2026_")) %>%
  pivot_longer(cols = starts_with("Pred_"), names_to = "Famille", values_to = "Score_Moyen") %>%
  mutate(Famille = str_replace(Famille, "Pred_2026_", "")) %>%
  # On génère 1000 scores possibles autour du score moyen
  uncount(1000) %>%
  mutate(
    Score_Simule = rnorm(n(), mean = Score_Moyen, sd = 4), # sd = 4% d'écart-type
    Score_Simule = ifelse(Score_Simule < 0, 0, Score_Simule)
  )

ggplot(df_simul, aes(x = Famille, y = Score_Simule, fill = Famille)) +
  geom_violin(alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  facet_wrap(~ SECTEUR, ncol = 4, labeller = as_labeller(function(x) paste("Secteur", x))) +
  scale_fill_manual(values = c("GAUCHE" = "#E41A1C", "RN" = "#000080", "CENTRE" = "#FFD700", "DROITE" = "#00BFFF")) +
  labs(
    title = "Simulation des Marges d'Erreur (1000 scénarios par secteur)",
    subtitle = "Lecture : Plus le 'violon' est étiré, plus l'issue est incertaine",
    x = NULL, y = "Score possible (%)"
  ) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# On prépare les données
df_bulle <- predictions_2026 %>%
  left_join(dataset_master %>% select(SECTEUR, Population_Totale), by = "SECTEUR")

ggplot(df_bulle, aes(x = Pred_2026_Gauche, y = Pred_2026_RN)) +
  # La ligne d'égalité parfaite (50/50 entre Gauche et RN)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_point(aes(size = Population_Totale), color = "purple", alpha = 0.6) +
  geom_text(aes(label = paste("S.", SECTEUR)), vjust = -1.5, fontface = "bold") +
  # Zone de couleur pour guider la lecture
  annotate("text", x = 60, y = 15, label = "Bastions\nGAUCHE", color = "#E41A1C", fontface = "italic") +
  annotate("text", x = 15, y = 45, label = "Bastions\nRN", color = "#000080", fontface = "italic") +
  scale_size_continuous(range = c(5, 15), name = "Population") +
  coord_cartesian(xlim = c(0, 80), ylim = c(0, 60)) +
  labs(
    title = "Le Paysage Électoral : Gauche vs RN",
    subtitle = "Les secteurs proches de la ligne pointillée sont les zones clés (Swing Sectors)",
    x = "Score estimé Gauche (%)",
    y = "Score estimé RN (%)"
  ) +
  theme_minimal()
