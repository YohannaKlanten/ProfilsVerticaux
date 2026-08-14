compiler_phy <- function(
    path_org="X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/3_Données harmonisées",
    path_final="X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/DB profils/Actuelle",
    BD_path = "X:/DOCUM/4300_Gest_donnees/Eau_Suivi/Lacs/Profils/DB profils/Actuelle/Profils_physicochimie_L.xlsx"){
  
  
  
  #~~~~~~~~~~~~~~~~~
  #Donnees existantes ----
  #~~~~~~~~~~~~~~~~~
  
  BD <- openxlsx::read.xlsx(BD_path, detectDates = T, sheet='Données')
  
  
  #~~~~~~~~~~~~~~~~~
  #Donnees completes ----
  #~~~~~~~~~~~~~~~~~
  
  
  #liste des dossiers
  dirs <- list.dirs(path_org)
  doss_comp <- dirs[which(grepl('/Complet',dirs) & !grepl('Gabarit', dirs))]
  
  cols_num <- c(
    "Temperature",
    "Oxy_mgl",
    "Oxy_sat",
    "SpCond",
    "pH",
    "Cond",
    "Chla",
    "Phycocyanine"
  )
  
  #importation des fichiers
  fich <- lapply(1:length(doss_comp), function(i) {
    if(length(list.files(doss_comp[i])) != 0){
      if(length(list.files(doss_comp[i])) == 1){
        f <- openxlsx::read.xlsx(list.files(doss_comp[i], full.names = TRUE), detectDates = TRUE)
        
        # --- Harmonisation des colonnes attendues ---
        for (col in cols_num) {
          if (!col %in% names(f)) {
            f[[col]] <- NA_real_
          } else {
            f[[col]] <- suppressWarnings(as.numeric(f[[col]]))
          }
        }
        
        if(nrow(f) > 0){
          #Definir colonnes
          if(!'Validation' %in% colnames(f)){f$Validation <- NA}
          if(!'Remarque' %in% colnames(f)){f$Remarque <- NA}
          f$Date <- as.Date(f$Date)
          cols_chr <- c("NO_LAC", "NO_STATION", "Heure", "Remarque" )
          for (col in cols_chr) {
            if (col %in% names(f)) {
              f[[col]] <- as.character(f[[col]])
            }
          }
          return(f)
          
        }
        
        
      }else{warning(paste('Plusieurs fichier Complet dans le dossier', doss_comp[i]))}
    }
    
  })
  
  #enlever les fichiers sans donnees
  fich <- fich[!unlist(lapply(fich, is.null))]
  fich <-  fich[unlist(lapply(fich, nrow)) !=0]
  
  
  #lisaison
  d <- do.call(rbind, fich)
  
  
  #ordre
  d <- d[order(d$NO_STATION, d$no_profil),]
  
  #ID
  d$ID <- paste(d$NO_STATION, d$no_profil, sep='-')
  
  #pivot
  cols_long <- colnames(d)[which(!colnames(d) %in% c("NO_LAC","NO_STATION","no_profil","Date","Heure","Profondeur", "ID"))]
  dl <- tidyr::pivot_longer(d, cols = any_of(cols_num), names_to = "Param",values_to = "Valeur")
  dl <- dplyr::relocate(dl, Profondeur, .before=Param)
  dl <- dl[-which(is.na(dl$Valeur)),]
  
  dl$Validation <- NA
  dl$Remarque <- NA
  
  
  
  #~~~~~~~~~~~~~~~~~~
  # Combiner nouvelles et anciennes donnees ----
  #~~~~~~~~~~~~~~~~~~
  
  
  # Distinguer anciennes et nouvelles lignes
  cols_join <- c("NO_LAC", "NO_STATION", "Date", "Heure", "Profondeur", "Param", "Valeur")
  
  old<- dplyr::semi_join(dl, BD, by=cols_join)
  new <- dplyr::anti_join(dl, BD, by=cols_join)
  
  message(paste('Nombre de lignes existantes : ', nrow(old)))
  message(paste('Nombre de nouvelles lignes : ', nrow(new)))
  
  if(nrow(old)+nrow(new) != nrow(dl)){warning('Nombre total de lignes incompatible')}
  
  # Mise a jour des ID et no profil
  BD1 <- dplyr::left_join(BD, dl, by=cols_join, suffix = c('','_new'))
  BD1$no_profil <- BD1$no_profil_new
  BD1$ID <- BD1$ID_new
  
  BD <- BD1[,colnames(BD)]
  
  # Jointure
  if(nrow(new) == 0){BDnew <- BD}else{
    BDnew <- dplyr::bind_rows(BD, new)
  }
  
  
  
  
  #~~~~~~~~~~~~~~~~~~
  #Nouvelles colonnes ----
  #~~~~~~~~~~~~~~~~~~
  
  
  
  #calculs statification
  
  calcStrat <- function(p, temp='Temperature', prof='Profondeur'){
    
    df1 <- data.frame(prof_arr = round(p$Profondeur, 1), temperature=p$Temperature)
    df2 <- aggregate(temperature ~ prof_arr,data=df1, FUN = mean)
    
    thermo <- rLakeAnalyzer::thermo.depth(wtr=df2$temperature, depths=df2$prof_arr)
    if(is.nan(thermo)){thermo <- NA}
    
    meta <- rLakeAnalyzer::meta.depths(df2$temperature, depths=df2$prof_arr)
    meta[is.nan(meta)] <- NA
    
    if(length(meta) > 1){
      meta_top <- meta[1]
      meta_bot <- meta[2]
    }else{
      meta_top <- NA
      meta_bot <- NA
    }
    
    out <- data.frame(p, thermo, meta_top, meta_bot)
    return(out)
    
  }
  
  
  #dspl <- split(d, f=as.factor(d$ID))
  
  #dspl1 <- lapply(dspl, FUN=calcStrat)
  
  #d <- do.call(rbind, dspl1)
  
  
  
  #~~~~~~~~~~~~~~
  #DB export
  #~~~~~~~~~~~~~~
  
  
  #formats
  BDw <- tidyr::pivot_wider(BDnew[,-which(colnames(BDnew) %in% c('Validation', 'Remarque'))], names_from=Param, values_from=Valeur)
  BDl <- BDnew[order(BDnew$NO_STATION, BDnew$no_profil, BDnew$Profondeur),]
  
  #backup
  nameBD_L <- paste('Profils_physicochimie_L_', Sys.Date(), '.xlsx', sep='')
  file.copy(from=BD_path, to=file.path(path_final, nameBD_L) ,overwrite = F)
  
  #exoprt xlsx
  wb <- openxlsx::loadWorkbook(BD_path)
  openxlsx::removeTable(wb, sheet='Données',table='table')
  openxlsx::writeDataTable(wb, sheet='Données', BDl, tableName = 'table')
  openxlsx::saveWorkbook(wb, file=BD_path, overwrite = T, returnValue = F)
  
  
  #output
  out <- list(old, new, BDw, BDl)
  names(out) <- c('old', 'new', 'BDw', 'BDl')
  return(out)
  
  
  
}
