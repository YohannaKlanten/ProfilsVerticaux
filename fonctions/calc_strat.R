calc_strat <- function(p, prof_col = "Profondeur", value_col = "Temperature",
                       Smin_thermo = 1, Smin_meta = 1, interp_res = 0.1) {
  
  # Si le profil est vide, retourner un tibble vide avec NA
  if (nrow(p) == 0) {
    return(tibble(
      thermo = NA_real_,
      meta_top = NA_real_,
      meta_bot = NA_real_
    )[0, ])
  }
  
  # Vérifier les données valides
  df <- dplyr::distinct(p[, c(prof_col, value_col)])
  df <- df[complete.cases(df), ]
  depths <- df[[prof_col]]
  temps <- df[[value_col]]
  
  # Si moins de 3 points valides
  if(length(temps) < 3 || all(is.na(depths)) || all(is.na(temps))){
    return(tibble(
      thermo = NA_real_,
      meta_top = NA_real_,
      meta_bot = NA_real_
    ))
  }
  
  # Interpolation fine
  interp_depths <- seq(min(depths), max(depths), by = interp_res)
  interp_temps <- approx(depths, temps, xout = interp_depths)$y
  
  # Calcul du gradient
  grad <- diff(interp_temps) / interp_res
  grad_depths <- head(interp_depths, -1) + interp_res / 2
  
  # Éliminer les gradients à moins de 2 m de profondeur
  valid_idx <- grad_depths >= 2
  
  # Si aucun gradient est valide ou trop faible
  if (all(!valid_idx) || max(abs(grad[valid_idx]), na.rm = TRUE) < Smin_thermo) {
    thermo <- NA
    meta_top <- NA
    meta_bot <- NA
  } else {
    # Arrondir le gradient pour assurer que la position de thermo soit juste
    grad_round <- round(abs(grad), 3)
    
    # Trouver toutes les positions avec le gradient maximal arrondi
    max_grad_value <- max(grad_round[valid_idx], na.rm = TRUE)
    grad_indices <- which(valid_idx & grad_round == max_grad_value)
    
    # Profondeur moyenne (ou médiane) des indices max
    thermo <- median(grad_depths[grad_indices])
    
    # Trouver l'indice global le plus proche de thermo
    global_index <- which.min(abs(grad_depths - thermo))
    
    # Détection du haut et bas de la métalimnion
    above <- which(abs(grad[1:global_index]) < Smin_meta)
    meta_top <- if (length(above) > 0) grad_depths[max(above)] else min(interp_depths)
    
    below <- which(abs(grad[global_index:length(grad)]) < Smin_meta)
    meta_bot <- if (length(below) > 0) grad_depths[global_index + min(below) - 1] else max(interp_depths)
  }
  
  # Retourner les résultats avec thermo/meta_top/meta_bot répétés
  out <- data.frame(
    p,
    thermo = rep(thermo, nrow(p)),
    meta_top = rep(meta_top, nrow(p)),
    meta_bot = rep(meta_bot, nrow(p))
  )
  return(out)
}

