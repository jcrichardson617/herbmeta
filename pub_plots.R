theme_Publication <- function(base_size=14, base_family="Helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(axis.line = element_line(color = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = "transparent",color = NA),
            axis.text.x=element_text(angle=0, hjust=0.40),
            axis.title.x = element_blank(),
            axis.title.y = element_text(color = "black"),
            axis.text = element_text(color = "black", size = 12),
            axis.ticks = element_line(color = "black"),
            plot.title = element_text(color = "black"),
            legend.position = ("none"),
            axis.ticks.x = element_blank()))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

library(ggplot2)
library(gridExtra)


bugsUNLUMPED + scale_colour_Publication()+ theme_Publication() #+ coord_fixed(ratio = 7)

