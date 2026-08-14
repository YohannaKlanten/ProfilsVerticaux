graphFicheLot <- function(db, dossier_out){
  
  #split par station
  db_splt <- split(db, f=as.factor(db$nostation_nomlac))
  
  #nomenclature fichiers et creation des dossiers
  doss_lac <- unlist(lapply(1:length(db_splt), function(i){ paste(db_splt[[i]][1,'NO_LAC'], db_splt[[i]][1,'Nom du lac'], sep="_") } ))
  for(i in 1:length(doss_lac)){  
    if(!dir.exists(file.path(dossier_out, doss_lac[i]))){
      dir.create(file.path(dossier_out, doss_lac[i]))
    }
  }
  
  fichiers <- paste('Graph profils_',names(db_splt),'.pdf', sep = '')
  paths_final <- file.path(dossier_out, doss_lac, fichiers)
  
  
  #loop par station
  invisible(lapply(1:length(db_splt), function(i){
    message("Traitement de la station ", db_splt[[i]][1, "NO_STATION"])
    graphFiche(dst=db_splt[[i]], fichier=paths_final[i])
  }))
  
}
