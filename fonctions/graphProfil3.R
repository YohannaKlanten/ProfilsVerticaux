graphProfil3 <- function(data, an){
  
  #Selection de l'an
  data <- data[which(data$an == an ),]
  
  #Profils 3 parametres
  g1=graphProfil_v2(data[which(data$Param == 'Temperature'),], lab_param = 'Temperature (°C)', leg=F)
  g2=graphProfil_v2(data[which(data$Param == 'Oxy_sat'),], lab_param = 'Oxygène (%)', leg=F, labs = c(date='Date', prof='  '))
  g3=graphProfil_v2(data[which(data$Param == 'SpCond'),], lab_param = expression(paste('Conductivité sp. (', mu, 'S/cm)')), leg=F, labs = c(date='Date', prof='  '))
  
  #page graphique
  g_leg <- ggpubr::get_legend(graphProfil_v2(data[which(data$Param == 'Temperature'),]))
  
  g_titre <- ggpubr::text_grob(paste('Profils verticaux, année ', an), size=18, face='bold', vjust=1)
  
  gridExtra::grid.arrange(gridExtra::arrangeGrob(g1, g2, g3, ncol = 3, top=g_titre),g_leg,
                          nrow = 2, heights = c(10, 2))
  
}
