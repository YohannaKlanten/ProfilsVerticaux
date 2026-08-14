# Tableau des critères de qualité
criteres <- tibble::tibble(
  Temp = c(0, 5, 10, 15, 20, 25),
  Seuil_froide_mgL = c(8, 7, 6, 6, 5, 5),
  Seuil_chaude_mgL = c(7, 6, 5, 5, 4, 4)
)

# Températures à interpoler (0 à 25 °C par pas de 1)
temp_interp <- 0:25

# Interpolation linéaire pour les deux types de seuils
seuil_froide_interp <- approx(x = criteres$Temp, y = criteres$Seuil_froide_mgL, xout = temp_interp)$y
seuil_chaude_interp <- approx(x = criteres$Temp, y = criteres$Seuil_chaude_mgL, xout = temp_interp)$y

# Nouveau tableau interpolé
criteres_interp <- tibble(
  Temp = temp_interp,
  Seuil_froide_mgL = seuil_froide_interp,
  Seuil_chaude_mgL = seuil_chaude_interp
)


# Fonction pour trouver la classe de température la plus proche
seuil_critere <- function(temp) {
  if (length(temp) == 0 || is.na(temp)) {
    return(tibble::tibble(
      Temp = NA,
      Seuil_froide_mgL = NA,
      Seuil_chaude_mgL = NA
    ))
  }
  
  temp_ref <- criteres_interp$Temp
  idx <- which.min(abs(temp - temp_ref))
  return(criteres_interp[idx, ])
}
