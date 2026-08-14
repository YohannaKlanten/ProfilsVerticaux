importBD <- function(repo="X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/DB profils/Actuelle"){
  
  # Importer les fichiers excel
  physico <- openxlsx::read.xlsx(file.path(repo, 'Profils_physicochimie_L.xlsx'), detectDates = TRUE)
  station <- openxlsx::read.xlsx(file.path(repo, 'Liste stations.xlsx'), detectDates = TRUE, sep.names = " ")
  info <- openxlsx::read.xlsx(file.path(repo, 'Profils_info terrain.xlsx'), detectDates = TRUE) #pas encore utilisable, doit etre nettoyee avant de la lier a la BD
  
  # Lier les tables
  phy_st <- dplyr::left_join(physico, station, by=c("NO_STATION"="NO_STATION_BQMA"), relationship='many-to-one')
  BD <- phy_st
  
  # Exporter
  save(BD, file = file.path(repo, 'BD.rdata'))
  
  return(BD)
  
  
}
