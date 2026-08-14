graphProfil_v2 <- function(p=NULL,
                           prof_max=NULL,
                           valeur='Valeur',
                           prof='Profondeur',
                           date='Date',
                           valid_col='Validation',
                           valid_code=c(10,12,NA),
                           labs=c(date='Date', prof='Profondeur (m)'),
                           lab_param='',
                           leg=TRUE){
  
  
  
  #~~~~~~~~~~~~~~~~
  # Filtrer les données validées
  #~~~~~~~~~~~~~~~~
  p <- p[which(p[,valid_col] %in% valid_code),]
  
  # Garder une copie pour la température
  p_orig <- p
  
  #~~~~~~~~~~~~~~~~
  # Sélectionner et renommer les colonnes pour le graphique
  #~~~~~~~~~~~~~~~~
  p <- p[, which(names(p) %in% c(date, prof, valeur))]
  names(p) <- c('date', 'prof', 'var')
  p <- p[order(p$date, p$prof),]
  
  #~~~~~~~~~~~~~~~~
  # Calcul de la thermocline
  #~~~~~~~~~~~~~~~~
  p_temp <- p_orig[p_orig$Param == "Temperature", ]  # filtrage sur l'original
  p_temp <- p_temp[, c(date, prof, valeur)]
  names(p_temp) <- c("date", "prof", "var")
  
  
  p_strat <- p_temp %>%
    dplyr::group_by(date) %>%
    dplyr::group_modify(~ {
      res <- calc_strat(.x, prof_col='prof', value_col='var', Smin_thermo = 1, Smin_meta = 1)
      res <- dplyr::select(res, -any_of("date"))
      res
    }) %>%
    dplyr::ungroup()
  
  thermo_df <- p_strat %>%
    dplyr::select(date, thermo) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(thermo))
  
  #~~~~~~~~~~~~~~~~
  # Thème ggplot
  #~~~~~~~~~~~~~~~~
  th <- ggplot2::theme(
    panel.background=ggplot2::element_blank(),
    axis.line=ggplot2::element_line(colour="gray50"),
    plot.margin = ggplot2::unit(c(20,20,20,20), "pt"),
    title=ggplot2::element_text(size=12),
    axis.title.y=ggplot2::element_text(size=14, margin= ggplot2::margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x.top=ggplot2::element_text(size=14, margin= ggplot2::margin(t = 0, r = 0, b = 15, l = 0)),
    legend.key=ggplot2::element_rect(fill="white"),
    legend.position = ifelse(leg, 'top','none'),
    legend.margin=ggplot2::margin(t = 0, r = 0, b = 0, l = 0)
  )
  
  
  pt_size <- 2.5
  lwd <- 1
  pt_sh <- 19
  txt_size <- 5
  txt_col <- 'firebrick'
  
  #~~~~~~~~~~~~~~~~
  # Cas pas de données
  #~~~~~~~~~~~~~~~~
  if(nrow(p) == 0){
    graph_vide <- ggplot2::ggplot(data=p, ggplot2::aes(x=var, y=prof)) +
      ggplot2::scale_y_reverse(limits=c(10,1)) +
      ggplot2::scale_x_continuous(limits=c(0,10)) +
      ggplot2::geom_blank() +
      ggplot2::annotate("text",label="Pas d'information disponible", x=5, y=2, size=txt_size, col=txt_col) +
      th +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank())
    return(graph_vide)
  }
  
  #~~~~~~~~~~~~~~~~
  # Paramètres axes et couleurs
  #~~~~~~~~~~~~~~~~
  lim_prof <- if(is.null(prof_max)){c(ceiling(max(p$prof, na.rm = TRUE)), 0)} else {c(prof_max,0)}
  lim_var <- if(all(is.na(p$var))){c(0,1)} else {c(floor(min(p$var, na.rm = TRUE)), ceiling(max(p$var, na.rm = TRUE)))}
  if(diff(range(lim_var))< 5){lim_var <- c(lim_var[1]-3, lim_var[2]+3 )}
  
  col_mois <- c(jan='hotpink', fev='plum1', mar='mediumpurple1', avr='cyan1', mai='aquamarine1',
                juin='springgreen1', juil='green4', aout='yellowgreen', sep='goldenrod1', oct='darkorange1',
                nov='brown2', dec='violetred')
  colpal_fun <- colorRampPalette(col_mois)
  colpal <- colpal_fun(366)
  p$jour <- lubridate::yday(p$date)
  p$col_jour <- colpal[p$jour]
  col_nv <- tibble::deframe(p[!duplicated(p$date), c('date','col_jour')])
  
  #~~~~~~~~~~~~~~~~
  # Graphique
  #~~~~~~~~~~~~~~~~
  graph <- ggplot2::ggplot(data=p, ggplot2::aes(x=var, y=prof, group=date, color=as.character(date))) +
    ggplot2::scale_y_reverse(limits=lim_prof) +
    ggplot2::scale_x_continuous(limits=lim_var, position='top') +
    ggplot2::xlab(lab_param) +
    ggplot2::ylab("Profondeur (m)") +
    ggplot2::scale_color_manual(name='', values=col_nv) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = ifelse(length(unique(p$date)) < 8, 2, 3), byrow = TRUE),
      fill = ggplot2::guide_legend(title = labs)
    ) +
    ggplot2::theme(legend.key = ggplot2::element_rect(color = NA)) +
    ggplot2::geom_path(linewidth=lwd) +
    ggplot2::geom_point(size=pt_size, shape=pt_sh) +
    ggplot2::geom_hline(data = thermo_df, ggplot2::aes(yintercept = thermo, color = as.character(date)), linetype="dashed", linewidth=0.8) +
    th
  
  return(graph)
}
