library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(rmapshaper)
library(raster)
library(gtalibrary)
library(data.table)

wd = ""
output.path = paste0(wd, "pics/")
shapefile.path = paste0(wd, "shapefiles/")
EPSG <- make_EPSG()
CRS("+init=epsg:4326")

# functions + GEO data
source("code/mapping functions.R")
load("output/geo.data.au.bez.Rdata")



## bzk multi
bzk.multi=merge(geo.data.au.bez, subset(cases.avg, end.date==max(cases.avg$end.date))[,c("gkz","cum.100k")],
              by.x="plz", by.y = "gkz", all.x=T)
bzk.multi$cum.100k[bzk.multi$plz>900]=unique(bzk.multi$cum.100k[bzk.multi$plz==900])

bzk.multi=merge(bzk.multi,subset(case.growth, date==max(case.growth$date))[,c("gkz","growth.rate")],
                by.x=c("plz"), by.y = "gkz", all.x=T)
bzk.multi$growth.rate[bzk.multi$plz>900]=unique(bzk.multi$growth.rate[bzk.multi$plz==900])
bzk.multi$value=0

for(i in 1:nrow(map.3d.elevated)){
  
  bzk.multi$value[bzk.multi$cum.100k>=map.3d.elevated$abs.min[i] &
                    bzk.multi$cum.100k<map.3d.elevated$abs.max[i] &
                    bzk.multi$growth.rate>=map.3d.elevated$growth.min[i] &
                    bzk.multi$growth.rate<map.3d.elevated$growth.max[i]]=map.3d.elevated$id[i]
}

bzk.multi <- bzk.multi[with(bzk.multi, order(id, order)),]
bzk.positiv.growth=multi_map(bzk.multi)

gta_plot_saver(plot=bzk.positiv.growth,
               path=paste0(save.path,"pics"),
               name=paste0("Bezirk - Multi-Map - ",Sys.Date()),
               png=F,
               jpg=T)


## GDE VBG
gde.multi=merge(geo.data.au.gem, subset(vbg.avg, end.date==max(vbg.avg$end.date))[,c("gkz","cum.100k")],
                by.x="plz", by.y = "gkz", all.y=T)
gde.multi=merge(gde.multi,subset(vbg.growth, date==max(vbg.growth$date))[,c("gkz","growth.rate")],
                by.x=c("plz"), by.y = "gkz", all.x=T)
gde.multi$value=0

for(i in 1:nrow(map.3d.elevated)){
  
  gde.multi$value[gde.multi$cum.100k>=map.3d.elevated$abs.min[i] &
                    gde.multi$cum.100k<map.3d.elevated$abs.max[i] &
                    gde.multi$growth.rate>=map.3d.elevated$growth.min[i] &
                    gde.multi$growth.rate<map.3d.elevated$growth.max[i]]=map.3d.elevated$id[i]
}

gde.multi <- gde.multi[with(gde.multi, order(id, order)),]
gde.positiv.growth=multi_map(gde.multi)

gta_plot_saver(plot=gde.positiv.growth,
               path=paste0(save.path,"pics"),
               name=paste0("Vorarlberg - Multi-Map - ",Sys.Date()),
               png=F,
               jpg=T)


vbg.risk=unique(gde.multi[,c("plz","value","cum.100k","growth.rate")])
vbg.risk=merge(vbg.risk, unique(vbg.gde[,c("GKZ","ï..gemeinde")]), by.x="plz", by.y="GKZ")
names(vbg.risk)[length(names(vbg.risk))]="gemeinde"


vbg.risk=vbg.risk[order(-vbg.risk$value),]
Encoding(vbg.risk$gemeinde)="UTF-8"
xlsx::write.xlsx(vbg.risk, file=paste0(save.path,"pics/Voralberg multi.xlsx"), row.names=F)


# Bauch abs
bzk.map=merge(geo.data.au.bez, subset(cases.avg, end.date==max(cases.avg$end.date))[,c("gkz","cum.100k")],
              by.x="plz", by.y = "gkz", all.x=T)
bzk.map$value=bzk.map$cum.100k
bzk.map$value[bzk.map$plz>900]=unique(bzk.map$value[bzk.map$plz==900])
bzk.map <- bzk.map[with(bzk.map, order(id, order)),]
bzk.positiv.vorwoche=case_map(bzk.map)

gta_plot_saver(plot=bzk.positiv.vorwoche,
               path=paste0(save.path,"pics"),
               name=paste0("Bezirk - Positive Tests Vorwoche - ",Sys.Date()),
               png=F,
               jpg=T)




#bauch growth
case.growth=merge(case.growth, unique(cases[,c("Bezirk","GKZ")]), by="Bezirk", all.x=T)
bzk.growth=merge(geo.data.au.bez, subset(case.growth, date==max(case.growth$date))[,c("gkz","growth.rate")],
                 by.x="plz", by.y = "gkz", all.x=T)
bzk.growth$value=bzk.growth$growth.rate
bzk.growth$value[bzk.growth$plz>900]=unique(bzk.growth$value[bzk.growth$plz==900])
bzk.growth <- bzk.growth[with(bzk.growth, order(id, order)),]
bzk.positiv.growth=growth_map(bzk.growth)

gta_plot_saver(plot=bzk.positiv.growth,
               path=paste0(save.path,"pics"),
               name=paste0("Bezirk - Positive Tests Vorwoche - Wachstum - ",Sys.Date()),
               png=F,
               jpg=T)