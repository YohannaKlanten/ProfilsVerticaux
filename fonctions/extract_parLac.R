extract_parLac <- function(db, dossier_out = NULL,
                           gabarit = "X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/Extractions/Gabarits/Profils physicochimie_gabarit.xlsx"){
  
  
  # Pre-selection des colonnes
  cols <- c('NO_LAC','Nom du lac','NO_STATION','NO_STATION_RSVL',
            'Latitude','Longitude',
            'Date','Heure','Profondeur','Param','Valeur')
  
  # Selection des parametres
  params <- c("Temperature","Oxy_mgl","Oxy_sat","SpCond")
  
  # Selection des donnees valides
  valide <- c(NA, 10, 12)
  
  # Subset de la BD
  d2 <- db[which(db$Param %in% params & db$Validation %in% valide), cols]
  
  # Pivoter les donnees
  d3 <- tidyr::pivot_wider(d2, names_from='Param', values_from='Valeur')
  
  # lat/long remplacement par la valeur de la station si NA
  d3$lat <- ifelse(!is.na(d3$Latitude), d3$Latitude, d3$Latitude)
  d3$long <- ifelse(!is.na(d3$Longitude), d3$Longitude, d3$Longitude)
  
  # Separation par lac
  d_split <- split(d3, f=as.factor(d3$NO_LAC))
  
  cols_final <- c('Nom du lac', 'NO_STATION','NO_STATION_RSVL',
                  'lat','long','Date','Heure','Profondeur',
                  "Temperature","Oxy_mgl","Oxy_sat","SpCond")
  
  # Fonction d'exportation
  export_lac <- function(d_org){
    
    no_lce <- d_org$NO_LAC[1]
    nom_lac <- d_org$`Nom du lac`[1]
    
    dossier_lac <- file.path(dossier_out, paste(no_lce, nom_lac, sep='_'))
    if(!dir.exists(dossier_lac)) dir.create(dossier_lac, recursive = TRUE)
    
    d_final <- d_org[,cols_final]
    d_final <- dplyr::arrange(d_final, NO_STATION, desc(Date), Profondeur)
    
    # Chemin cible
    f_out <- file.path(dossier_lac, paste0(nom_lac,"_Profils physicochimie.xlsx"))
    
    # Sauvegarde dans un fichier temporaire local
    f_tmp <- tempfile(fileext = ".xlsx")
    file.copy(from=gabarit, to=f_tmp, overwrite = TRUE)
    wb <- openxlsx::loadWorkbook(f_tmp)
    openxlsx::writeData(wb, sheet='Données', d_final, startRow = 2, colNames = FALSE)
    
    # Essai avec retries si verrouillage
    ok <- FALSE; ntry <- 0
    while(!ok && ntry < 3){
      ntry <- ntry + 1
      try({
        openxlsx::saveWorkbook(wb, file=f_tmp, overwrite = TRUE)
        file.copy(f_tmp, f_out, overwrite = TRUE)
        ok <- TRUE
      }, silent=TRUE)
      if(!ok) Sys.sleep(1)  # attendre 1s avant nouvel essai
    }
    
    if(!ok) warning("Échec d'écriture pour ", nom_lac)
  }
  
  # Application a tous les lacs
  lapply(d_split, FUN=export_lac)
}




