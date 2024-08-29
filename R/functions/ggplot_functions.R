# Colours

led_colours = c('mediumslateblue', 'navy', 'royalblue', 'skyblue', 'forestgreen', 'darkorange', 'brown1', 'firebrick', 'black')

led_colours_dark = c('mediumslateblue', 'navy', 'royalblue', 'skyblue', 'forestgreen', 'darkorange', 'brown1', 'firebrick', 'white')


#---
# Transparent dark theme for presentations

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