graphFiche <- function(dst, fichier){
  
  #annees
  dst$an <- lubridate::year(dst$Date)
  annees <- sort(unique(dst$an), decreasing = TRUE)
  
  #page map et info
  
  cols_info <- c("Nom du lac", "NO_STATION",'NO_STATION_RSVL','NO_LAC',
                 'Zone ZGI','Région(s) administrative(s)','Municipalité(s)',
                 'Latitude','Longitude')
  
  info <- dst[1,cols_info]
  info$n_profil <- length(unique(dst$Date))
  info <- info[,c("Nom du lac",'NO_STATION_RSVL', "NO_STATION",'NO_LAC',
                  'Latitude','Longitude', 'n_profil',
                  'Zone ZGI','Région(s) administrative(s)','Municipalité(s)')]
  
  colnames(info)[2:8] <- c('Station RSVL','Station BQMA', 'No LCE','Latitude','Longitude', 'Nombre de profils réalisés', 'Zone de gestion intégrée')
  
  
  
  #production du PDF
  pdf(file=fichier,width=10, height=7)
  
  cex=1.5
  xlim=c(0,5)
  ylim=c(0,5)
  xpos=c(xlim[1],xlim[1]+1.5)
  
  par(mar=c(0,0,0,0))
  
  plot(NULL, xlim=xlim, ylim=xlim,xlab='',ylab='', xaxt='n', yaxt='n',bty="n")
  legend(x=xpos, y=ylim,legend=names(info[,1:8]), x.intersp=0,cex = cex, box.col='white', text.col = 'gray40', y.intersp = 2)
  legend(x=xpos+2, y=ylim,legend=info[1,1:8], cex = cex, x.intersp=0, box.col='white', y.intersp = 2)
  
  invisible(lapply(1:length(annees), function(i) {
    # Graphiques profils
    p <- graphProfil3(dst, an = annees[i])
    print(p)
    
    # Tableau résultats
    tab <- suppressMessages(tableau_resultats(dst, an = annees[i]))
    grid::grid.newpage()
    grid::grid.draw(tab)
    
    invisible(tab)
  }))
  
  dev.off()
  
}
