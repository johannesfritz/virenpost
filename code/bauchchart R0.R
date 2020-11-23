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


## AGES data
cases=read.csv("https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv", sep=";", stringsAsFactors = F)
cases$date=as.Date(str_extract(cases$Time,"^\\d+\\.\\d+\\.\\d+"), "%d.%m.%Y")


## bzk multi R0
dates.of.interest=c(max(cases$date)-1, max(cases$date)-8,
                    max(cases$date)-15, as.Date("2020-04-01"))
my.colors=c("#954844","#226A7D","#717A2C","#B66C3B")

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

r0.estimate=merge(r0.estimate, 
                  unique(cases[,c("GKZ","AnzEinwohner")]),
                  by.x="gkz", by.y="GKZ", all.x=T)


## Bauchchart R0
bauch.r0=ggplot(subset(r0.estimate, date %in% dates.of.interest), aes(x=r0, colour=as.factor(date)))+
  stat_ecdf(size=1.2)+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,.1))+
  scale_x_continuous(breaks=seq(0,2.5,.25), limits = c(0,2.5))+
  scale_colour_manual(values=my.colors)+
  labs(colour=NULL,x="Schätzung des R0 auf Bezirksebene",y="Anteil aller Bezirke\n mit einer R0 kleiner X")+
  gta_theme()+
  geom_vline(aes(xintercept=0.5),
             color=gta_colour$green.shades(3)[2], linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1),
             color=gta_colour$amber.shades(3)[2], linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1.5),
             color=gta_colour$red.shades(3)[1], linetype="dashed", size=1)+
  ggtitle("Vor dem Lockdown fielen Neuansteckungen in 75% der politischen Bezirke", subtitle = "Basierend auf den Positivtests publiziert bis")


gta_plot_saver(plot=bauch.r0,
               path=paste0(save.path,"pics"),
               name=paste0("R0 - Bauchchart Bezirke - ", Sys.Date()),
               png=F,
               jpg=T)



## Bauchchart R0 POP
r0.pop=data.frame()
interval=.025
for(level in seq(interval,2.5,interval)){
  
  if(nrow(subset(r0.estimate, r0<=level))>0){
    
    rt=aggregate(AnzEinwohner ~ date, subset(r0.estimate, r0<=level) ,sum)
    rt$r0=level
    
    
  }else{
    rt=data.frame(date= unique(r0.estimate$date),
                  AnzEinwohner=0,
                  r0=level)

  }
    
  
  r0.pop=rbind(r0.pop,
               rt)
  
  rm(rt)
  
  
}


bauch.r0.pop=ggplot(subset(r0.pop, date %in% dates.of.interest), aes(x=r0, y=round(AnzEinwohner/1000,0), colour=as.factor(date)))+
  geom_line(size=1.2)+
  scale_y_continuous(breaks=seq(250, 9000,250), 
                     labels=function(x) format(x, big.mark = "'", scientific = FALSE))+
  scale_x_continuous(breaks=seq(0,2.5,.25), limits = c(0,2.5))+
  scale_colour_manual(values=my.colors)+
  labs(colour="Stichtag",x="Schätzung des R0 auf Bezirksebene",y="Anzahl Einwohner in Bezirken mit einem R0 kleiner X")+
  gta_theme()+
  geom_vline(aes(xintercept=0.5),
             color=gta_colour$green.shades(3)[2], linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1),
             color=gta_colour$amber.shades(3)[2], linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1.5),
             color=gta_colour$red.shades(3)[1], linetype="dashed", size=1)+
  ggtitle("Vor dem Lockdown lebten fast 7 Mio. Einwohner in Bezirken mit fallenden Neuansteckungen", subtitle = "Basierend auf den Positivtests publiziert bis")


gta_plot_saver(plot=bauch.r0.pop,
               path=paste0(save.path,"pics"),
               name=paste0("R0 - Bevölkerungszahl - ", Sys.Date()),
               png=F,
               jpg=T)



