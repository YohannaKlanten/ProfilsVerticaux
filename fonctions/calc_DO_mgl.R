# Le calcul est effectué selon la formule empirique de Benson & Krause (1984) pour l’eau douce :
# {Oxy_mgl = (Oxy_sat / 100) * exp(7.7117 - 1.31403 * log(Temperature + 45.93))}

calc_DO_mgl <- function(BD) {
  
  library(dplyr)
  
  # Fonction interne : concentration d'O2 à saturation (mg/L) selon la température
  oxy_sat_mgL <- function(temp) {
    exp(7.7117 - 1.31403 * log(temp + 45.93))
  }
  
  # Étape 1 : identifier les combinaisons uniques Date + NO_STATION + Profondeur
  BD_complet <- BD %>%
    group_by(Date, NO_STATION, Profondeur, no_profil) %>%
    mutate(
      has_temp = any(Param == "Temperature"),
      has_oxsat = any(Param == "Oxy_sat"),
      has_oxmgl = any(Param == "Oxy_mgl")
    ) %>%
    ungroup()
  
  # Étape 2 : créer les nouvelles lignes manquantes pour Oxy_mgl
  manquantes <- BD_complet %>%
    filter(has_temp & has_oxsat & !has_oxmgl) %>%
    group_by(Date, NO_STATION, Profondeur, no_profil) %>%
    summarise(
      Temperature = mean(Valeur[Param == "Temperature"], na.rm = TRUE),
      Oxy_sat = mean(Valeur[Param == "Oxy_sat"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Valeur = (Oxy_sat / 100) * oxy_sat_mgL(Temperature),
      Param = "Oxy_mgl"
    )
  
  # Étape 3 : récupérer les métadonnées associées
  metadonnees <- BD %>%
    group_by(Date, NO_STATION, Profondeur, no_profil) %>%
    slice(1) %>%
    ungroup() %>%
    select(-Param, -Valeur)
  
  # Étape 4 : fusionner avec les métadonnées et reconstituer les lignes complètes
  manquantes_final <- manquantes %>%
    left_join(metadonnees, by = c("Date", "NO_STATION", "Profondeur", "no_profil")) %>%
    relocate(Param, Valeur, .after = Profondeur)
  
  # Étape 5 : combiner avec la BD originale
  BD_complet <- bind_rows(BD, manquantes_final)
  
  message("✅ Paramètre Oxy_mgl ajouté pour ",
          nrow(manquantes_final), " combinaisons (Date, station, profondeur).")
  
  return(BD_complet)
}
