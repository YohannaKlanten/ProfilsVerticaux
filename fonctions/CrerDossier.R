CreerDossier <- function(liste, no_bqma, no_lce, nom_lac){
  
  
  #Dossier de travail ----
  #~~~~~~~~~~~~~~~~~~
  
  path="X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/3_Données harmonisées"
  
  
  #Formatter la liste ---
  #~~~~~~~~~~~~~~~~~~
  
  #Enlever les lignes avec numeros manquants
  liste <- na.omit(liste[,c(no_bqma, no_lce, nom_lac)])
  
  #Verifier la liste (numeros BQMA et LCE)
  if(any(nchar(liste[,no_bqma]) != 8)){ stop('Attentation: Certains No BQMA ne contiennent pas 8 caractères') }
  if(any(nchar(liste[,no_lce]) != 5)){ stop('Attentation: Certains No LCE ne contiennent pas 5 caractères') }
  
  #Formater les noms de lac
  liste$nom_lac2 <- ifelse(grepl(',',liste[,nom_lac] ) ,stringr::str_extract(liste[,nom_lac], ".*(?=\\,)"), liste[,nom_lac] )
  
  
  
  #Creation dossier station ----
  #~~~~~~~~~~~~~~~~~~~~~~~~
  
  #Dossier existants et manquants
  dossier_existants <- list.dirs(path=path, recursive = F)
  liste$dossier_existant <- unlist(lapply(1:nrow(liste) ,function(i) ifelse(any(grepl(liste[i,no_bqma], dossier_existants )), 'oui', 'non' )))
  
  liste_miss <- liste[liste$dossier_existant == 'non',]
  
  if(nrow(liste_miss) == 0 ){ message('Tous les dossiers de la liste existent déjà')}else{
    
    
    #Creer les nouveaux dossier
    ###########################
    
    invisible(lapply(1:nrow(liste_miss), function(i){
      
      #infos station
      lce <- liste_miss[i,no_lce]
      bqma <- liste_miss[i,no_bqma]
      lac <- liste_miss[i,"nom_lac2"]
      info_station <- data.frame(no_lac=lce, no_station=bqma, nom_lac=lac)
      
      message(paste('Creation du dossier : ', paste(lce,'_', bqma, '_',lac, sep='')))
      
      
      #copier le dossier gabarit
      doss_name <- paste(path,'/', lce,'_', bqma, '_',lac, sep='')
      R.utils::copyDirectory(from=paste(path, '_Gabarit', sep='/'), to=doss_name, recursive = T)
      
      #renommer fichier
      setwd(paste(doss_name, 'Complet', sep = '/'))
      invisible(file.rename(from = 'Gabarit_Complet.xlsx', to = paste(bqma, '_Complet.xlsx', sep='')))
      
      setwd(doss_name)
      invisible(file.rename(from = 'Gabarit_script harmonisation.R', to = paste(lac, '_script harmonisation.R', sep='')))
      
      
      #fichier info_station
      write.table(info_station, 'info_station.txt', row.names = F, quote=F, sep="\t")
      
      setwd(path)
      
      
    }))
    
    
  }
  
  
}
