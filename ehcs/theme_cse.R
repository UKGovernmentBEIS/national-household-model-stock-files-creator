theme_cse <- function(base_size = 10, base_family = "sans") {
  theme(
    line =               element_line(colour = "grey70",
                                      size = 0.5,
                                      linetype = 1,
                                      lineend = "square"
    ),
    rect =               element_rect(fill = NA,
                                      colour = "grey70",
                                      size = 0.5,
                                      linetype = 1
    ),
    text =               element_text(family = base_family,
                                      face = "plain",
                                      colour = "black",
                                      size = base_size,
                                      hjust = 0.5,
                                      vjust = 0.5,
                                      angle = 0,
                                      lineheight = 1.1
    ),
    
    axis.line =          element_line(size=0.25),
    axis.line.y =        element_line(size=0.25),
    axis.text =          element_text(size=rel(0.9)),
    axis.text.x =        element_text(hjust = 1, angle=90),
    axis.text.y =        element_text(hjust = 1),
    axis.title =         element_text(face = "bold"),
    axis.title.x =       element_text(),
    axis.title.y =       element_text(angle = 90),
    axis.ticks =         element_line(size = 0.2),
    axis.ticks.length =  unit(0.3, "lines"),
    axis.ticks.margin =  unit(0.5, "lines"),
    axis.ticks.y =       element_blank(),
    
    legend.background =  element_rect(colour = NA),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    unit(1.5,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.key.width =   unit(1,"lines"),
    legend.margin =      unit(0.2, "cm"),
    legend.text =        element_text(size=rel(0.9)),
    legend.text.align =  NULL,
    legend.title =       element_text(size=rel(0.9)),
    legend.title.align = NULL,
    legend.position =    "bottom",
    legend.direction =   "horizontal",
    legend.justification="left",
    legend.box =         "vertical",
    
    panel.background =   element_rect(),
    panel.border =       element_rect(colour = NA),
    panel.grid =         element_line(colour = "grey70",
                                      size = 0.25,
                                      linetype = 3),
    panel.grid.major =   element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor =   element_blank(),
    panel.margin =       unit(0.5, "lines"),
    
    strip.background =   element_rect(fill = "white",
                                      colour = NA),
    strip.text.x =       element_text(size=rel(1.2)),
    strip.text.y =       element_text(angle = -90),
    
    plot.background =    element_rect(colour = "white",
                                      fill = "white"),
    plot.title =         element_text(size = rel(1.2)),
    plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    
    complete = TRUE
  )
}