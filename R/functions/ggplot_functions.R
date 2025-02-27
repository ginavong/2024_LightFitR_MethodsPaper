# Saving -----
save_fig = function(filename, fig, dimensions = c(20, 15)){
  
  # png
  fn = paste(filename, '.png', sep='')
  ggsave(fn, plot=fig, width=dimensions[1], height=dimensions[2], units='cm', dpi=600)
  
  #svg
  fn = paste(filename, '.svg', sep='')
  ggsave(fn, plot=fig, width=dimensions[1], height=dimensions[2], units='cm', dpi=600)
}

# Colours ----

led_colours = c('mediumslateblue', 'navy', 'royalblue', 'skyblue', 'forestgreen', 'darkorange', 'brown1', 'firebrick', 'black')

led_colours_dark = c('mediumslateblue', 'navy', 'royalblue', 'skyblue', 'forestgreen', 'darkorange', 'brown1', 'firebrick', 'white')

# Labels -----

irr_w_lab = expression('irradiance (W m'^-2 * nm^-1*')')
irr_umol_lab = expression('irradiance (μmol m'^-2*nm^-1*')')
irr_umol_peak_lab = expression('irradiance at LED peak (μmol m'^-2*nm^-1*')')

wl_lab = "wavelength (nm)"

# Transparent dark theme for presentations-----

theme_presentation = function() {
  ggplot2::theme(
    #Set black things to white
    line = element_line(color='#ffffff'),
    text = element_text(color="#ffffff", size=25),
    axis.text=element_text(color="#ffffff"),
    axis.line=element_line(color="#ffffff"),
    axis.ticks=element_line(color="#ffffff"),
    strip.text = element_text(colour = '#ffffff'),
    
    #Transparent background
    legend.background=element_rect(fill = "transparent",colour = NA),
    legend.key = element_rect(colour = NA, fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    strip.background = element_rect(fill='transparent', colour='grey')
  )
}

# Manuscript theme

theme_manuscript = function(LED.guide=TRUE, x.rotate=FALSE, y.rotate=FALSE){
  
  # Settings
  if(LED.guide==TRUE){
    LED.guide = c()
  }
  else{LED.guide = ggplot2::guides(colour='none')}
  
  if(x.rotate==TRUE){
    x.angle=90
  }
  else{ x.angle=0 }
  
  if(y.rotate==TRUE){
    y.angle=90
  }
  else{ y.angle=0 }
  
  bigText = 24
  smallText = 16

  # ggplot object
  
  list(ggplot2::theme_classic(),
    ggplot2::theme(text=element_text(size=bigText),
                   axis.text = element_text(size=smallText),
                   legend.text = element_text(size=smallText),
                   
                   axis.text.x=element_text(angle=x.angle),
                   axis.text.y = element_text(angle=y.angle)),
    LED.guide)
    
}

