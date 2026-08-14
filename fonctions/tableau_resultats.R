tableau_resultats <- function(p, an){
  
  # --- Filtrage des données invalides
  p <- p %>%
    filter(Validation == 10)
  
  # --- Filtrage de l'année
  df <- p[p$an == an, ]
  
  df <- df %>%
    tidyr::pivot_wider(
      id_cols = c(NO_LAC, NO_STATION, ID, Date, Profondeur),
      names_from = Param,
      values_from = Valeur,
      values_fn = mean
    )
  
  station <- unique(df$NO_STATION)
  annee   <- an
  
  # Vérification Oxy_mgl
  if (!("Oxy_mgl" %in% colnames(df)) || all(is.na(df$Oxy_mgl))) {
    message(glue::glue("Impossible de générer le tableau pour {station}-{annee} (Oxy_mgl manquant)"))
    tab_vide <- data.frame(Message = "Pas d'information disponible")
    return(gridExtra::tableGrob(tab_vide))
  }
  
  
  # --- Calcul de la stratification par profil
  dspl <- split(df, df$ID)
  df_strat <- bind_rows(lapply(dspl, calc_strat))
  
  # --- Calcul des concentrations d'oxygène sous la thermocline
  donnees_strat_DO <- profils_DO_interp(df_strat, profil_col = "ID")
  
  # --- Interpolation des profils
  donnees_interp <- profils_interp(df, profil_col = "ID")
  
  
  # --- Calcul des seuils pour le tableau
  crit <- donnees_interp %>%
    dplyr::rowwise() %>%
    dplyr::mutate(crit = list(seuil_critere(Temperature))) %>%
    tidyr::unnest_wider(crit) %>%
    dplyr::mutate(
      respecte_froide = ifelse(
        is.na(Seuil_froide_mgL) | is.na(Oxy_mgl),
        NA,Oxy_mgl >= Seuil_froide_mgL),
      respecte_chaude = ifelse(
        is.na(Seuil_chaude_mgL) | is.na(Oxy_mgl),
        NA,Oxy_mgl >= Seuil_chaude_mgL))
  
  stress <- crit %>%
    group_by(Date) %>%
    summarise(
      pct_froide_false = {
        x <- respecte_froide
        if (all(is.na(x))) {
          NA_real_
        } else if (any(x == FALSE, na.rm = TRUE)) {
          100 * mean(x == FALSE, na.rm = TRUE)
        } else {
          0
        }
      },
      pct_chaude_false = {
        x <- respecte_chaude
        if (all(is.na(x))) {
          NA_real_
        } else if (any(x == FALSE, na.rm = TRUE)) {
          100 * mean(x == FALSE, na.rm = TRUE)
        } else {
          0
        }
      },
      .groups = "drop"
    )
  
  # --- Construction du tableau final
  tableau <- donnees_interp %>%
    dplyr::left_join(
      donnees_strat_DO %>% dplyr::select(Date, thermo, Oxy_sat_Hypo, Portion_hypo_anoxie),
      by = c("Date")
    ) %>%
    dplyr::distinct(Date, .keep_all = TRUE) %>%
    dplyr::left_join(stress, by = "Date") %>%
    dplyr::mutate(Date = format(as.Date(Date), "%Y-%m-%d")) %>%
    dplyr::transmute(
      Date,
      "Profondeur de la thermocline (m)" = thermo,
      "Saturation moyenne en oxygène sous la thermocline (%)" = Oxy_sat_Hypo,
      "Portion anoxique sous la thermocline (%)" = Portion_hypo_anoxie,
      "Portion de la colonne d'eau sous le critère - biote d'eau froide (%)" = pct_froide_false,
      "Portion de la colonne d'eau sous le critère - biote d'eau chaude (%)" = pct_chaude_false
    )
  
  
  #### Mise en forme du tableau
  
  # --- fonction pour couper un texte long en plusieurs lignes
  wrap_text <- function(x, width = 20) {
    stringr::str_wrap(x, width = width) %>%
      stringr::str_replace_all("\n", "\n")
  }
  
  # --- Ajuster la largeur des noms de colonnes
  colnames(tableau) <- vapply(colnames(tableau), wrap_text, character(1), width = 20)
  
  # On force 1 chiffre après la virgule dans les colonnes numériques
  tableau[] <- lapply(tableau, function(col) {
    if (is.numeric(col)) {
      formatC(col, format = "f", digits = 1)
    } else {
      as.character(col)
    }
  })
  
  # --- Création du grob avec thème épuré
  tableau_grob <- gridExtra::tableGrob(
    tableau,
    rows = NULL,
    theme = gridExtra::ttheme_default(
      core = list(
        fg_params = list(cex = 0.8, col = "black"),
        bg_params = list(fill = NA, col = NA)
      ),
      colhead = list(
        fg_params = list(fontface = "bold", cex = 0.9),
        bg_params = list(fill = "#f2f2f2", col = NA)
      )
    )
  )
  
  # --- titre
  titre <- grid::textGrob(
    paste("Caractéristiques des profils, année", annee),
    gp = grid::gpar(fontsize = 18, fontface = "bold")
  )
  
  # --- assemblage
  res <- gridExtra::arrangeGrob(
    titre,
    tableau_grob,
    ncol = 1,
    heights = c(0.1, 1)
  )
  
  # --- viewport largeur max 9 pouces
  res <- gridExtra::arrangeGrob(
    res,
    top = NULL,
    vp = grid::viewport(width = grid::unit(9, "in"), height = grid::unit(1, "npc"))
  )
  
  invisible(res)
  
}
