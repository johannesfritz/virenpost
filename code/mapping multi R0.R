library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(rmapshaper)
library(raster)
library(gtalibrary)
library(data.table)
library(stringr)
library(httr)
library(dplyr)
library(EpiEstim)

save.path=""
gta_colour_palette()

wd = ""
output.path = paste0(wd, "pics/")
shapefile.path = paste0(wd, "shapefiles/")
EPSG <- make_EPSG()
CRS("+init=epsg:4326")

# functions + GEO data
source("code/mapping functions.R")
load("output/geo.data.au.bez.Rdata")


## bzk multi R0


## Bezirk
r0.estimate=data_frame()
for(loc in unique(cases$Bezirk)){
  
  estimation.cases <- subset(dplyr::select(cases, geo=Bezirk, date=date, confirm=AnzahlFaelle), geo==loc)[,c("date","confirm")]
  
  res_parametric_si <- estimate_R(estimation.cases$confirm, 
                                  method="parametric_si",
                                  config = make_config(list(
                                    mean_si = 4, 
                                    std_si = 1)))
  
  r0.estimate=rbind(r0.estimate,
                    data.frame(geo=loc,
                               gkz=unique(cases$GKZ[cases$Bezirk==loc]),
                               date=estimation.cases$date[c(min(res_parametric_si$R$t_end):nrow(estimation.cases))],
                               r0=res_parametric_si$R$`Mean(R)`, 
                               stringsAsFactors = F))
  
}
Encoding(r0.estimate$geo)="UTF-8"

## bzk multi
bzk.multi=merge(geo.data.au.bez, subset(cases.avg, end.date==max(cases.avg$end.date))[,c("gkz","cum.100k")],
              by.x="plz", by.y = "gkz", all.x=T)
bzk.multi$cum.100k[bzk.multi$plz>900]=unique(bzk.multi$cum.100k[bzk.multi$plz==900])

bzk.multi=merge(bzk.multi,subset(r0.estimate, date==max(r0.estimate$date))[,c("gkz","r0")],
                by.x=c("plz"), by.y = "gkz", all.x=T)
bzk.multi$r0[bzk.multi$plz>900]=unique(bzk.multi$r0[bzk.multi$plz==900])

bzk.multi$value=0

for(i in 1:nrow(map.3d.r0)){
  
  bzk.multi$value[bzk.multi$cum.100k>=map.3d.r0$abs.min[i] &
                    bzk.multi$cum.100k<map.3d.r0$abs.max[i] &
                    bzk.multi$r0>=map.3d.r0$growth.min[i] &
                    bzk.multi$r0<map.3d.r0$growth.max[i]]=map.3d.r0$id[i]
}

bzk.multi <- bzk.multi[with(bzk.multi, order(id, order)),]
bzk.positiv.growth=multi_map(bzk.multi)

gta_plot_saver(plot=bzk.positiv.growth,
               path=paste0(save.path,"pics"),
               name=paste0("Bezirk - Multi-Map - R0 - prior week - ",Sys.Date()),
               png=F,
               jpg=T)
