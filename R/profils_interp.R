profils_interp <- function(p, station_col = "NO_STATION", lac_col = "NO_LAC",
                           profondeur_col = "Profondeur", oxy_sat_col = "Oxy_sat",
                           oxy_mgl_col = "Oxy_mgl", temp_col = "Temperature",
                           profil_col = "profil", date_col = "Date") {
  
  if (nrow(p) == 0) {
    return(data.frame(
      Profondeur = numeric(),
      Oxy_sat = numeric(),
      Oxy_mgl = numeric(),
      Temperature = numeric(),
      NO_STATION = character(),
      NO_LAC = character(),
      Date = as.Date(character()),
      profil = character()
    ))
  }
  
  stations_uniques <- unique(p[[station_col]])
  resultats <- list()
  
  for (station in stations_uniques) {
    p_station <- p %>% filter(.data[[station_col]] == station)
    profils <- unique(p_station[[profil_col]])
    
    for (profil in profils) {
      L <- p_station %>% filter(.data[[profil_col]] == profil)
      
      if (nrow(L) < 2 || all(is.na(L[[profondeur_col]]))) {
        next  # on saute ce profil s’il est inutilisable
      }
      
      new_depth <- seq(min(L[[profondeur_col]], na.rm = TRUE),
                       max(L[[profondeur_col]], na.rm = TRUE),
                       by = 0.01)
      
      safe_interp <- function(x, y) {
        if (sum(!is.na(x) & !is.na(y)) < 2) {
          return(rep(NA, length(new_depth)))
        }
        approx(x, y, xout = new_depth, rule = 2)$y
      }
      
      df_interp <- data.frame(
        Profondeur = new_depth,
        Oxy_sat = safe_interp(L[[profondeur_col]], L[[oxy_sat_col]]),
        Oxy_mgl = safe_interp(L[[profondeur_col]], L[[oxy_mgl_col]]),
        Temperature = safe_interp(L[[profondeur_col]], L[[temp_col]]),
        NO_STATION = station,
        NO_LAC = L[[lac_col]][1],
        Date = L[[date_col]][1],
        profil = profil
      )
      
      resultats[[paste(station, profil, sep = "_")]] <- df_interp
    }
  }
  
  if (length(resultats) == 0) {
    return(data.frame(
      Profondeur = numeric(),
      Oxy_sat = numeric(),
      Oxy_mgl = numeric(),
      Temperature = numeric(),
      NO_STATION = character(),
      NO_LAC = character(),
      Date = as.Date(character()),
      profil = character()
    ))
  }
  
  return(bind_rows(resultats))
}
