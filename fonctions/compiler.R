compiler <- function(no_lac, no_station, dossier, export=T){
  
  
  #Importer les fichiers de donnees ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  #Donnees deja existantes
  doss_comp <- file.path(dossier, "Complet")
  
  file_complet_org <- paste(no_station, '_Complet.xlsx', sep='')
  
  if(file.exists(file_complet_org, recursive = F)){ #fichier complet existant
    file_complet_org <- list.files(path=doss_comp, pattern='*.xlsx', recursive = F)
    if(length(file_complet_org) > 1){stop('Plusieurs ou pas de fichiers _Complet.xlsx')}
    complet_org <- openxlsx::read.xlsx(file_complet_org, sheet=1,  detectDates=T)
    
  }else{ #fichier complet inexistant
    dossier_gab <- "X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/3_Données harmonisées/_Gabarit/Complet"
    file_gab <- file.path(dossier_gab,"Gabarit_Complet.xlsx")
    file.copy(from = file_gab, to = file.path(doss_comp, file_complet_org) )
    complet_org <- openxlsx::read.xlsx(file.path(doss_comp, file_complet_org), sheet=1,  detectDates=T)
    class(complet_org$Heure) <- 'character'
    
  }
  
  
  
  #Nouvelles donnees
  doss_acompiler <-file.path(dossier, 'A compiler')
  
  file_names <- list.files(path=doss_acompiler, pattern='*.xlsx', recursive = F)
  if(length(file_names) < 1){message('Aucun nouveau fichiers a compiler')}
  file_list <- lapply(file.path(doss_acompiler, file_names), openxlsx::read.xlsx, sheet=2, detectDates=T)
  
  
  
  #Compiler en un tableau ----
  #~~~~~~~~~~~~~~~~~~~~~~
  
  #compiler nouvelles donnees
  complet_new <- do.call(eval(parse(text="dplyr::bind_rows")), file_list)
  
  complet_new$NO_LAC <- no_lac
  complet_new$NO_STATION <- no_station
  complet_new$no_profil <- NA
  class(complet_new$Heure) <- 'character'
  complet_new <- dplyr::relocate(complet_new, NO_LAC,NO_STATION ,no_profil, .before=Date)
  
  #combiner ancienne et nouvelles donnees
  if(nrow(complet_org)==0){complet <- complet_new}else{complet <- dplyr::bind_rows(complet_org, complet_new)}
  complet <- complet[order(complet$Date),]
  
  #Detecter les doublons
  if(any(duplicated(complet[,-which(names(complet) %in% c('no_profil'))])) ){
    doublons <- TRUE
    warning('Présence de doublons, fichier excel non exporté')
  } else { doublons <- FALSE }
  
  
  #Fichier info ----
  #~~~~~~~~~~~~
  
  dates <- unique(complet$Date)
  info <- data.frame(no_profil=1:length(dates), Date=dates, NO_LAC=no_lac, NO_STATION = no_station)
  
  
  
  #No profils sequentiels ----
  #~~~~~~~~~~~~~~~~~~~~~~
  
  complet <- dplyr::left_join(complet[,-which(names(complet) == 'no_profil')], info[,c('Date','no_profil')], by='Date') %>% dplyr::relocate(no_profil, .before=Date)
  
  
  
  #Output ----
  #~~~~~~~
  
  if(export && !doublons){
    
    #fichier excel
    
    wb <- openxlsx::loadWorkbook(file.path(doss_comp ,file_complet_org))
    openxlsx::writeData(wb, sheet='Données', complet)
    openxlsx::writeData(wb, sheet='Info', info)
    openxlsx::saveWorkbook(wb, file=file.path(doss_comp, file_complet_org), overwrite = T)
    
    #nombre de profils
    message(paste('Nombre total de profils compilés : ', nrow(info)))
    
  }
  
  
  #list output R
  return(list(data=complet, info=info))
  
  
}
