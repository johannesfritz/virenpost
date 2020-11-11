library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(rmapshaper)
library(raster)
library(gtalibrary)
library(data.table)

## color multiple
map.3d.normal=rbind(data.frame(id=c(1:5),
                        label=c("Kaum positive Tests, stark steigend", "Kaum positive Tests, steigend", "Kaum positive Tests, stabil", "Kaum positive Tests, fallend", "Kaum positive Tests, stark fallend"),
                        color=c("#f1cc44", "#c3b03e", "#959438", "#687731", "#3a5b2b"),
                        abs.min=0,
                        abs.max=10,
                        growth.min=c(50,10,-10,-50,-100),
                        growth.max=c(10000,50,10,-10,-50)),
             data.frame(id=c(6:10),
                        label=c("Mehrere positive Tests, stark steigend", "Mehrere positive Tests, steigend", "Mehrere positive Tests, stabil", "Mehrere positive Tests, fallend", "Mehrere positive Tests, stark fallend"),
                        color=c("#efa035", "#cb943a", "#a8873f", "#857b44", "#636e49"),
                        abs.min=10,
                        abs.max=30,
                        growth.min=c(50,10,-10,-50,-100),
                        growth.max=c(10000,50,10,-10,-50)),
             data.frame(id=c(11:15),
                        label=c("RKI-Grenzwert positive Tests, stark steigend", "RKI-Grenzwert positive Tests, steigend", "RKI-Grenzwert positive Tests, stabil", "RKI-Grenzwert positive Tests, fallend", "RKI-Grenzwert positive Tests, stark fallend"),
                        color=c("#ed7327", "#d47637", "#bc7a47", "#a47d57", "#8b8067"),
                        abs.min=40,
                        abs.max=60,
                        growth.min=c(50,10,-10,-50,-100),
                        growth.max=c(10000,50,10,-10,-50)),
             data.frame(id=c(16:20),
                        label=c("Viele positive Tests, stark steigend", "Viele positive Tests, steigend", "Viele positive Tests, stabil", "Viele positive Tests, fallend", "Viele positive Tests, stark fallend"),
                        color=c("#cd4820", "#c75b39", "#c06e52", "#ba806b", "#b49285"),
                        abs.min=60,
                        abs.max=100,
                        growth.min=c(100,50,-10,-50,-100),
                        growth.max=c(10000,100,10,-10,-50)),
             data.frame(id=c(21:25),
                        label=c("Sehr viele positive Tests, stark steigend", "Sehr viele positive Tests, steigend", "Sehr viele positive Tests, stabil", "Sehr viele positive Tests, fallend", "Sehr viele positive Tests, stark fallend"),
                        color=c("#ac1e19", "#b8403b", "#c4615e", "#d18381", "#dda5a3"),
                        abs.min=100,
                        abs.max=10000,
                        growth.min=c(50,10,-10,-50,-100),
                        growth.max=c(10000,50,10,-10,-50)))


map.3d.elevated=rbind(data.frame(id=c(1:5),
                        label=c("Kaum positive Tests, stark steigend", "Kaum positive Tests, steigend", "Kaum positive Tests, stabil", "Kaum positive Tests, fallend", "Kaum positive Tests, stark fallend"),
                        color=c("#f1cc44", "#c3b03e", "#959438", "#687731", "#3a5b2b"),
                        abs.min=0,
                        abs.max=200,
                        growth.min=c(100,10,-10,-50,-100),
                        growth.max=c(10000,100,10,-10,-50)),
             data.frame(id=c(6:10),
                        label=c("Mehrere positive Tests, stark steigend", "Mehrere positive Tests, steigend", "Mehrere positive Tests, stabil", "Mehrere positive Tests, fallend", "Mehrere positive Tests, stark fallend"),
                        color=c("#efa035", "#cb943a", "#a8873f", "#857b44", "#636e49"),
                        abs.min=200,
                        abs.max=400,
                        growth.min=c(100,10,-10,-50,-100),
                        growth.max=c(10000,100,10,-10,-50)),
             data.frame(id=c(11:15),
                        label=c("RKI-Grenzwert positive Tests, stark steigend", "RKI-Grenzwert positive Tests, steigend", "RKI-Grenzwert positive Tests, stabil", "RKI-Grenzwert positive Tests, fallend", "RKI-Grenzwert positive Tests, stark fallend"),
                        color=c("#ed7327", "#d47637", "#bc7a47", "#a47d57", "#8b8067"),
                        abs.min=400,
                        abs.max=600,
                        growth.min=c(100,10,-10,-50,-100),
                        growth.max=c(10000,100,10,-10,-50)),
             data.frame(id=c(16:20),
                        label=c("Viele positive Tests, stark steigend", "Viele positive Tests, steigend", "Viele positive Tests, stabil", "Viele positive Tests, fallend", "Viele positive Tests, stark fallend"),
                        color=c("#cd4820", "#c75b39", "#c06e52", "#ba806b", "#b49285"),
                        abs.min=600,
                        abs.max=800,
                        growth.min=c(100,10,-10,-50,-100),
                        growth.max=c(10000,100,10,-10,-50)),
             data.frame(id=c(21:25),
                        label=c("Sehr viele positive Tests, stark steigend", "Sehr viele positive Tests, steigend", "Sehr viele positive Tests, stabil", "Sehr viele positive Tests, fallend", "Sehr viele positive Tests, stark fallend"),
                        color=c("#ac1e19", "#b8403b", "#c4615e", "#d18381", "#dda5a3"),
                        abs.min=800,
                        abs.max=10000,
                        growth.min=c(100,10,-10,-50,-100),
                        growth.max=c(10000,100,10,-10,-50)))

## multi map
multi_map <- function(geo.data){
  
  
  geo.data <- geo.data[with(geo.data, order(id, order)),]
  
  plot=  ggplot() +
    geom_polygon(data=geo.data, aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    # scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="") +
    # scale_fill_manual(values=map.3d$color, limits=c(0,25), guide=NULL)+
    scale_fill_gradientn(na.value="#dadada",
                        colours = c(as.character(map.3d$color)),
                        breaks=c(1:25),
                        labels=NULL,
                        guide=NULL,
                        limits=c(0,25))+
    # scale_fill_gradient(name="Positive Tests pro 100T Einwohner in der Vorwoche", 
    #                     na.value="#dadada",
    #                     low = gta_colour$green[3], 
    #                     high = gta_colour$red[1], 
    #                     breaks=seq(0,1500,250),
    #                     labels=c("0","250","500","750","1000","1250","1500"),
    #                     guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"),
    #                     limits=c(0,1500))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,-.15),
          legend.justification = c(0.5,0.3),
          legend.direction = "horizontal",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0,
          plot.background = element_rect(fill="#FFFFFF"),
          plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc"),
          
    ) 
  
  print(plot)
  return(plot)
}

# ggplot function to case maps
case_map <- function(geo.data){
  geo.data <- geo.data[with(geo.data, order(id, order)),]
  
  plot=  ggplot() +
    geom_polygon(data=geo.data, aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    # scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="") +
    scale_fill_gradient(name="Positive Tests pro 100T Einwohner in der Vorwoche", 
                        na.value="#dadada",
                        low = gta_colour$green[3], 
                        high = gta_colour$red[1], 
                        breaks=seq(0,1500,250),
                        labels=c("0","250","500","750","1000","1250","1500"),
                        guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"),
                        limits=c(0,1500))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,-.15),
          legend.justification = c(0.5,0.3),
          legend.direction = "horizontal",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0,
          plot.background = element_rect(fill="#FFFFFF"),
          plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc"),
          
    ) 
  
  print(plot)
  return(plot)
}

## growth maps
growth_map <- function(geo.data){
  geo.data <- geo.data[with(geo.data, order(id, order)),]
  
  plot =  ggplot() +
    geom_polygon(data=geo.data, aes(x = long, y = lat, group = group, fill = value), size = 0.2, color = "white") +
    coord_fixed() + # Important to fix world map proportions
    # scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="") +
    scale_fill_gradient2(name="Wachstumsrate der positiven Tests von Woche zu Woche", 
                         na.value="#dadada",
                         low = gta_colour$green[4], 
                         mid = gta_colour$green[1], 
                         high = gta_colour$red[1], 
                         midpoint=0,
                         breaks=seq(-100,220,50),
                         labels=seq(-100,220,50),
                         guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"),
                         limits=c(-100,220))+
    # scale_fill_gradientn(name="Wachstumsrate der positiven Tests von Woche zu Woche", 
    #                     na.value="#dadada",
    #                     colours = c(gta_colour$green[3],gta_colour$green[1],gta_colour$red[1]),
    #                     breaks=seq(-100,220,50),
    #                     labels=seq(-100,220,50),
    #                     guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"),
    #                     limits=c(-100,220))+
    # scale_fill_gradient(name="Wachstumsrate der positiven Tests von Woche zu Woche", 
    #                     na.value="#dadada",
    #                     low = gta_colour$green[3], 
    #                     high = gta_colour$red[1], 
  #                     breaks=seq(-100,220,50),
  #                     labels=seq(-100,220,50),
  #                     guide=guide_colorbar(barwidth=15, label.hjust = 0.5, title.position = "top"),
  #                     limits=c(-100,220))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.55,-.15),
        legend.justification = c(0.5,0.3),
        legend.direction = "horizontal",
        plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
        legend.text.align = 0,
        plot.background = element_rect(fill="#FFFFFF"),
        plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc"),
        
  ) 
  
  print(plot)
  return(plot)
}
