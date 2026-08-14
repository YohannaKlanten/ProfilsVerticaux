profils_DO_interp <- function(p, station_col = "NO_STATION", lac_col = "NO_LAC",
                              profondeur_col = "Profondeur",
                              oxy_sat_col = "Oxy_sat", oxy_mgl_col = "Oxy_mgl",
                              profil_col = "profil", date_col = "Date",
                              thermocline_col = "thermo") {
  
  # Fonction d'interpolation sécurisée : si moins de 2 valeurs non NA, renvoie NA
  safe_interp <- function(x, y, new_depth) {
    valid <- !is.na(x) & !is.na(y)
    if (sum(valid) < 2) {
      return(rep(NA, length(new_depth)))
    }
    approx(x[valid], y[valid], xout = new_depth, rule = 2)$y
  }
  
  stations_uniques <- unique(p[[station_col]])
  resultats_stations <- list()
  
  for (station in stations_uniques) {
    p_station <- p[p[[station_col]] == station, ]
    p_profiles <- split(p_station, f = as.factor(p_station[[profil_col]]))
    
    resultats_profil <- list()
    
    for (profil in names(p_profiles)) {
      L <- p_profiles[[profil]]
      
      resultats <- data.frame(Oxy_sat_Hypo = NA,
                              Oxy_mgl_Hypo = NA,
                              Portion_hypo_anoxie = NA)
      
      new_depth <- seq(min(L[[profondeur_col]], na.rm = TRUE),
                       max(L[[profondeur_col]], na.rm = TRUE),
                       by = 0.01)
      
      thermo <- L[[thermocline_col]][1]
      
      ## Oxygène saturé - Moyenne sous la thermocline
      if (sum(!is.na(L[[oxy_sat_col]])) >= 2) {
        interp_sat <- safe_interp(L[[profondeur_col]], L[[oxy_sat_col]], new_depth)
        df_sat <- data.frame(depth = new_depth, val = interp_sat)
        
        valeurs_hypo <- df_sat$val[df_sat$depth >= thermo]
        resultats$Oxy_sat_Hypo <- if (length(valeurs_hypo) == 0 || is.na(thermo)) NA else mean(valeurs_hypo, na.rm = TRUE)
      }
      
      ## Oxygène mg/L - Moyenne sous la thermocline + Portion anoxique dans l’hypolimnion
      if (oxy_mgl_col %in% colnames(L) && sum(!is.na(L[[oxy_mgl_col]])) >= 2) {
        interp_mgl <- safe_interp(L[[profondeur_col]], L[[oxy_mgl_col]], new_depth)
        df_mgl <- data.frame(depth = new_depth, val = interp_mgl)
        
        valeurs_mgl_hypo <- df_mgl$val[df_mgl$depth >= thermo]
        resultats$Oxy_mgl_Hypo <- if (length(valeurs_mgl_hypo) == 0 || is.na(thermo)) NA else mean(valeurs_mgl_hypo, na.rm = TRUE)
        
        hypo_df <- df_mgl[df_mgl$depth > thermo, ]
        resultats$Portion_hypo_anoxie <- if (nrow(hypo_df) == 0 || is.na(thermo)) NA else mean(hypo_df$val < 0.3, na.rm = TRUE) * 100
      } else if (!(oxy_mgl_col %in% colnames(L))) {
        message(paste("Colonne", oxy_mgl_col, "absente pour le profil", profil, "de la station", station))
      }
      
      # Ajouter les informations supplémentaires
      resultats[[date_col]]        <- L[[date_col]][1]
      resultats[[station_col]]     <- L[[station_col]][1]
      resultats[[lac_col]]         <- L[[lac_col]][1]
      resultats[[thermocline_col]] <- thermo
      
      resultats_profil[[profil]] <- resultats
    }
    
    resultats_stations[[station]] <- do.call(rbind, resultats_profil)
  }
  
  do.call(rbind, resultats_stations)
}

