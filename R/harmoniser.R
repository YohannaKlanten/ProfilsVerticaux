harmoniser <- function(file_name){
  
  
  #Importer le fichier ----
  #~~~~~~~~~~~~~~~~~~~~
  
  
  #Importation
  dt <- openxlsx::read.xlsx(file_name, sheet='Données triées', sep.names=' ', detectDates=T)
  info <- openxlsx::read.xlsx(file_name, sheet='Info')
  
  #format de dt
  for(i in 1:ncol(dt)){ #detect les colonnes numerique et ajuste le format
    if(!(class(dt[,i]) == 'Date' | colnames(dt)[i] == info$col_org[which(info$col_harm == 'Date')]) ){
      if(all(!grepl("[^[:digit:][:space:].-]", dt[,i])) | all(!grepl("[^[:digit:][:space:],-]", dt[,i]))){ dt[,i] <- as.numeric(dt[,i]) }
    }
  }
  
  #format de info
  info$Erreur <- NA
  
  info_sp <- split(info, factor(info$col_harm, levels=info$col_harm))
  
  range_format <- function(param){
    range <- t(param[,c('min','max')])
    if(param$class == 'numeric'){range <- as.numeric(range)}
    if(param$class == 'Date'){range <- as.Date(range)}
    if(param$class == 'times'){range  <- chron::chron(times=range)}
    return(range)
  }
  
  ranges <- lapply(info_sp, range_format) #min et max de chaque parametre dans le bon format
  
  #Creer table harmonise
  df=setNames(data.frame(matrix(ncol = length(info$col_harm), nrow = nrow(dt) )), nm=info$col_harm)
  
  
  
  #Match des colonnes ----
  #~~~~~~~~~~~~~~~~~~~~
  
  
  for(i in 1:nrow(info)){
    
    
    #colonne definie manuellement
    #############################
    
    if(!is.na(info$col_org[i])){
      if(info$col_org[i] %in% colnames(dt)){
        
        #copier colonne choisie
        df[,i] <- dt[,info$col_org[i]]
        info$Erreur[i] <- NA
        
        #Conversion unite
        if(is.numeric(df[,i]) & !is.na(info$unit_conv[i])){ df[,i] <- df[,i] * info$unit_conv[i] }
        
        #format date et heure
        if(info$col_harm[i] == 'Date'){
          if(!class(df[,i]) == 'Date'){df[,i] <- as.Date(lubridate::parse_date_time(df[,i], info$unit_org[i]))
          }
        }
        
        if(info$col_harm[i] == 'Heure'){
          if(is.numeric(df[,i])){df[,i] <- chron::times( df[,i])}else{
            df[,i] <- chron::chron(times=format(lubridate::parse_date_time(df[,i], info$unit_org[i]), format='%H:%M:%S'))
          }
        }
        
        #Test format
        if(all(!is.na(df[,i]))){
          if(class(df[,i]) != info$class[i]){
            info$Erreur[i] <- 'Le format de la colonne choisie est non conforme'
          }
          
          #Test range
          if(!is.na(ranges[[i]][1])){if( min(df[,i]) < ranges[[i]][1]){info$Erreur[i] <- 'Contient des valeurs hors des limites min max'}}
          if(!is.na(ranges[[i]][2])){if( max(df[,i]) > ranges[[i]][2]){info$Erreur[i] <- 'Contient des valeurs hors des limites min max'}}
          
        }
        
      }else{info$Erreur <- 'Nom de la colonne inexistant dans les données originales'}
      
      
    }else{
      
      #match automatique
      ##################
      
      #test des conditions
      c1=which(grepl(info$nom_contient[i], colnames(dt), ignore.case = T)) #selection par nom de colonne
      c2=which(lapply(dt,class) %in% c(info$class_org[i], info$class[i])) #match des class
      c3=if(info$class_org[i]=='numeric'){ which(unlist(lapply(dt,function(x) if(is.numeric(x)){all(dplyr::between(x, ranges[[i]][1], ranges[[i]][2]))}else{NA}  ))) }else{c2}
      
      choix=Reduce(intersect, list(c1,c2,c3)) #colonne(s) choisie(s)
      
      if(length(choix) == 1){
        df[,i] <- dt[,choix]
        info$col_org[i] <- colnames(dt)[choix]
        
        #Si date et heure
        if(info$col_harm[i] %in% c('Date', 'Heure')){
          
          if(info$col_harm[i] == 'Date'){
            if(!class(df[,i]) == 'Date'){df[,i] <- as.Date(lubridate::parse_date_time(df[,i], info$unit_org[i]))
            }
          }
          
          if(info$col_harm[i] == 'Heure'){ df[,i] <- chron::chron(times=format(lubridate::parse_date_time(df[,i], info$unit_org[i]), format='%H:%M:%S'))  }
          
          #Test date time format
          if(all(!is.na(df[,i]))){
            if(class(df[,i]) != info$class[i]){
              info$Erreur[i] <- 'Le format de la colonne choisie est non conforme'
            }
            
            #Test date time range
            if(!is.na(ranges[[i]][1])){if( min(df[,i]) < ranges[[i]][1]){info$Erreur[i] <- 'Contient des valeurs hors des limites min max'}}
            if(!is.na(ranges[[i]][2])){if( max(df[,i]) > ranges[[i]][2]){info$Erreur[i] <- 'Contient des valeurs hors des limites min max'}}
            
          }
          
        }
        
      }
      
      if(length(choix) < 1){info$Erreur[i] <- 'Echec: aucun match trouvé, à spécifier manuellement'}
      if(length(choix) > 1){info$Erreur[i] <- 'Echec: plusieurs colonnes possibles, à spécifier manuellement'}
      
      
    }
    
    
  }
  
  
  
  #Exportation ----
  #~~~~~~~~~~~~
  
  wb <- openxlsx::loadWorkbook(file_name)
  openxlsx::deleteData(wb, sheet='Données harmonisées', cols = 1:100, rows = 1:1000, gridExpand = T)
  openxlsx::writeData(wb, sheet='Données harmonisées', df)
  txt <- openxlsx::createStyle(numFmt = 'yyyy-mm-dd')
  openxlsx::addStyle(wb, sheet='Données harmonisées', cols=1, rows=c(2:(nrow(df)+1)), style=txt)
  openxlsx::writeData(wb, sheet='Info', info, startRow = 2, colNames = F)
  openxlsx::saveWorkbook(wb, file=file_name, overwrite = T, returnValue = F)
  
  
  
}
