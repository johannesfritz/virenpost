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
  scale_y_continuous(breaks=seq(500, 9000,500), 
                     labels=function(x) format(x, big.mark = "'", scientific = FALSE))+
  scale_x_continuous(breaks=seq(0,2.5,.25), limits = c(0,2.5))+
  scale_colour_manual(values=my.colors)+
  labs(colour=NULL,x="Schätzung des R0 auf Bezirksebene",y="Anzahl Einwohner in Bezirken mit einem R0 kleiner X\n(in Tausend)")+
  gta_theme()+
  geom_vline(aes(xintercept=0.5),
             color=gta_colour$green.shades(3)[2], linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1),
             color=gta_colour$amber.shades(3)[2], linetype="dashed", size=1)+
  geom_vline(aes(xintercept=1.5),
             color=gta_colour$red.shades(3)[1], linetype="dashed", size=1)+
  ggtitle("Vor dem Lockdown lebten fast 8.5 Mio. Einwohner in Bezirken\nmit fallenden Neuansteckungen", subtitle = "Basierend auf den Positivtests publiziert bis")


gta_plot_saver(plot=bauch.r0.pop,
               path=paste0(save.path,"pics"),
               name=paste0("R0 - Bevölkerungszahl - ", Sys.Date()),
               png=F,
               jpg=T)


## Ro vs. Pop tile
## Wocheninzidenz
cases.avg=data.frame()
avg.duration=7
p.id=1

for(dt in as.Date("2020-11-01"):max(cases$date)){
  
  for(gkz in unique(cases$GKZ)){
    
    cases.avg=rbind(cases.avg,
                    data.frame(gkz=gkz,
                               period.id=p.id,
                               end.date=as.Date(dt, origin="1970-01-01"),
                               cumulative=sum(subset(cases, GKZ==gkz & date>=(as.Date(dt, origin="1970-01-01")-avg.duration+1) & 
                                                       date<=as.Date(dt, origin="1970-01-01"))$AnzahlFaelle)))
    
  }
  print(as.Date(dt, origin="1970-01-01"))
  p.id=p.id+1
}
cases.avg$avg=cases.avg$cumulative/avg.duration
cases.avg=merge(cases.avg, aggregate(AnzEinwohner ~ GKZ + Bezirk, cases, max), by.x="gkz",by.y="GKZ")
cases.avg$cum.100k=round(cases.avg$cumulative/cases.avg$AnzEinwohner*100000,0)
Encoding(cases.avg$Bezirk)="UTF-8"



r0.tile=merge(unique(r0.estimate[, c("gkz","date","r0","AnzEinwohner")]), 
              unique(cases.avg[,c("gkz","end.date","cum.100k")]),
              by.x=c("gkz","date"), by.y=c("gkz", "end.date"))

r0.pop.tile=data.frame()
interval.r0=.25
interval.100t=50

for(level.r0 in seq(interval.r0, 2, interval.r0)){
  
  for(level.100t in seq(interval.100t, 800, interval.100t)){
    
    if(nrow(subset(r0.tile, r0<level.r0 & r0>=(level.r0-interval.r0) & cum.100k<level.100t & cum.100k >= (level.100t-interval.100t)))>0){
      
      rt=aggregate(AnzEinwohner ~ date, 
                   subset(r0.tile, r0<level.r0 & r0>=(level.r0-interval.r0) & cum.100k<level.100t & cum.100k >= (level.100t-interval.100t)), 
                   sum)
      rt$r0=level.r0
      rt$cum.100k=level.100t
      
      
    }else{
      rt=data.frame(date= unique(r0.estimate$date),
                    AnzEinwohner=0,
                    r0=level.r0,
                    cum.100k=level.100t)
      
    }
    
    r0.pop.tile=rbind(r0.pop.tile,
                      rt)
    rm(rt)
    
  }
  
}

r0.matrix=reshape(subset(r0.pop.tile, date=="2020-11-28")[,c("r0","cum.100k","AnzEinwohner")], idvar="r0", timevar = "cum.100k", direction = "wide")

xlsx::write.xlsx(r0.matrix, file="pics/R0-100k matrix- cloud.xlsx", showNA = F, row.names = F)

