library(httr)
library(stringr)
library(ggplot2)
library(gtalibrary)
gta_colour_palette()

save.path=""


## Pull Google data
mobility.google="https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
google.zip="temp/Google mobility by region.zip"

GET(mobility.google, write_disk(google.zip, overwrite=TRUE))
unzip(zipfile = google.zip, files = c("2020_AT_Region_Mobility_Report.csv", "2020_CH_Region_Mobility_Report.csv"), exdir = "temp")

mob.ggl.aut=read.csv("temp/2020_AT_Region_Mobility_Report.csv", stringsAsFactors=F)
mob.ggl.aut$sub_region_1[mob.ggl.aut$sub_region_1==""]="Österreich"
mob.ggl.ch=read.csv("temp/2020_CH_Region_Mobility_Report.csv", stringsAsFactors=F)
mob.ggl.ch$sub_region_1[mob.ggl.ch$sub_region_1==""]="Schweiz"
mob.ggl=rbind(mob.ggl.aut,
              mob.ggl.ch)
do.call(file.remove, list(list.files("temp", full.names = TRUE)[! grepl("zip$",list.files("temp", full.names = TRUE))]))


ggl.rec=subset(mob.ggl, (sub_region_2==""| is.na(sub_region_2)))[,c("sub_region_1","date",
                                                             "retail_and_recreation_percent_change_from_baseline",
                                                             "residential_percent_change_from_baseline",
                                                             "grocery_and_pharmacy_percent_change_from_baseline",
                                                             "parks_percent_change_from_baseline",
                                                             "transit_stations_percent_change_from_baseline",
                                                             "workplaces_percent_change_from_baseline")]


# ggl.rec=subset(mob.ggl.aut, grepl("AT",iso_3166_2_code) )[,c("sub_region_1","date",
#                                                                       "retail_and_recreation_percent_change_from_baseline",
#                                                                       "residential_percent_change_from_baseline",
#                                                                       "grocery_and_pharmacy_percent_change_from_baseline",
#                                                                       "parks_percent_change_from_baseline",
#                                                                       "transit_stations_percent_change_from_baseline")]


names(ggl.rec)=c("land","date","rec","res","groc","park","oeffi", "work")
ggl.rec$rec=ggl.rec$rec+100
ggl.rec$res=ggl.rec$res+100
ggl.rec$groc=ggl.rec$groc+100
ggl.rec$park=ggl.rec$park+100
ggl.rec$oeffi=ggl.rec$oeffi+100
ggl.rec$work=ggl.rec$work+100
ggl.rec$date=as.Date(ggl.rec$date)
ggl.rec.2=ggl.rec
ggl.rec.2$date=ggl.rec.2$date+7

ggl.rec=merge(ggl.rec, ggl.rec.2, by=c("land","date"))
ggl.rec$change.rec=ggl.rec$rec.x/ggl.rec$rec.y
ggl.rec$change.res=ggl.rec$res.x/ggl.rec$res.y
ggl.rec$change.groc=ggl.rec$groc.x/ggl.rec$groc.y
ggl.rec$change.park=ggl.rec$park.x/ggl.rec$park.y
ggl.rec$change.oeffi=ggl.rec$oeffi.x/ggl.rec$oeffi.y
ggl.rec$change.work=ggl.rec$work.x/ggl.rec$work.y

ggl.rec$wochentag=weekdays(ggl.rec$date)



plot.cutoff.aug="2020-09-15"
for(bl in unique(ggl.rec$land)){
  ggl.rec$rec.norm.aug[ggl.rec$land==bl]=ggl.rec$rec.x[ggl.rec$land==bl]/mean(ggl.rec$rec.x[ggl.rec$date>=plot.cutoff.aug & ggl.rec$date<=(as.Date(plot.cutoff.aug)+2) & ggl.rec$land==bl], na.rm=T)
  ggl.rec$res.norm.aug[ggl.rec$land==bl]=ggl.rec$res.x[ggl.rec$land==bl]/mean(ggl.rec$res.x[ggl.rec$date>=plot.cutoff.aug & ggl.rec$date<=(as.Date(plot.cutoff.aug)+2) & ggl.rec$land==bl], na.rm=T)
  ggl.rec$groc.norm.aug[ggl.rec$land==bl]=ggl.rec$groc.x[ggl.rec$land==bl]/mean(ggl.rec$groc.x[ggl.rec$date>=plot.cutoff.aug & ggl.rec$date<=(as.Date(plot.cutoff.aug)+2) & ggl.rec$land==bl], na.rm=T)
  ggl.rec$groc.oeffi.aug[ggl.rec$land==bl]=ggl.rec$oeffi.x[ggl.rec$land==bl]/mean(ggl.rec$oeffi.x[ggl.rec$date>=plot.cutoff.aug & ggl.rec$date<=(as.Date(plot.cutoff.aug)+2) & ggl.rec$land==bl], na.rm=T)
  ggl.rec$park.norm.aug[ggl.rec$land==bl]=ggl.rec$park.x[ggl.rec$land==bl]/mean(ggl.rec$park.x[ggl.rec$date>=plot.cutoff.aug & ggl.rec$date<=(as.Date(plot.cutoff.aug)+2) & ggl.rec$land==bl], na.rm=T)
  ggl.rec$work.norm.aug[ggl.rec$land==bl]=ggl.rec$work.x[ggl.rec$land==bl]/mean(ggl.rec$work.x[ggl.rec$date>=plot.cutoff.aug & ggl.rec$date<=(as.Date(plot.cutoff.aug)+2) & ggl.rec$land==bl], na.rm=T)
}


## 
gta_colour_palette()
library(reshape2)
ggl.long=melt(ggl.rec[,c("land","date","wochentag","rec.norm.aug","res.norm.aug","groc.norm.aug","groc.oeffi.aug","park.norm.aug","work.norm.aug")],
              id.vars = c("land","date","wochentag"))
ggl.long$variable=as.character(ggl.long$variable)
ggl.long$variable[ggl.long$variable=="rec.norm.aug"]="Gastronomie & Shopping"
ggl.long$variable[ggl.long$variable=="res.norm.aug"]="Wohngebäude"
ggl.long$variable[ggl.long$variable=="park.norm.aug"]="Parks"
ggl.long$variable[ggl.long$variable=="work.norm.aug"]="Arbeitsstätten"
ggl.long$variable[ggl.long$variable=="groc.norm.aug"]="Supermärkte & Apotheken"
ggl.long$variable[ggl.long$variable=="groc.oeffi.aug"]="Öffentlicher Verkehr"

mob.national=ggplot(subset(ggl.long, date>="2020-08-01" & wochentag=="Mittwoch" & land %in% c("Schweiz","Österreich")), 
                    aes(x=date, y=value, colour=land))+
  geom_line(size=1.1)+
  facet_wrap(~ variable)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d")), as.numeric(as.Date("2020-12-26","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$blue[2], gta_colour$desert[2]))+
  labs(colour="",x="Datum",y="Aktivitätsniveau\n(Basis: 15.9.2020 = 1)")+
  scale_y_continuous(sec.axis = dup_axis(), limits=c(0.25,1.5), breaks=seq(.25,1.5,.25))+
  ggtitle("Aktivitätsniveau national", subtitle = "(laut Google; mittwochs)")

gta_plot_saver(plot=mob.national,
               path=paste0(save.path,"pics"),
               name=paste0("Mobilität - AUTvCH - ",Sys.Date()),
               png=F,
               jpg=T)



mob.subnational=ggplot(subset(ggl.long, date>="2020-08-01" & wochentag=="Mittwoch" & land %in% c("Vorarlberg","St. Gallen")), 
                    aes(x=date, y=value, colour=land))+
  geom_line(size=1.1)+
  facet_wrap(~ variable)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d")), as.numeric(as.Date("2020-12-26","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2]))+
  labs(colour="",x="Datum",y="Aktivitätsniveau\n(Basis: 15.9.2020 = 1)")+
  scale_y_continuous(sec.axis = dup_axis(), limits=c(0.25,1.5), breaks=seq(.25,1.5,.25))+
  ggtitle("Aktivitätsniveau Transrhenubien", subtitle = "(laut Google; mittwochs)")

gta_plot_saver(plot=mob.subnational,
               path=paste0(save.path,"pics"),
               name=paste0("Mobilität - VBGvSG - ",Sys.Date()),
               png=F,
               jpg=T)



mob.subnational=ggplot(subset(ggl.long, date>="2020-08-01" & wochentag=="Mittwoch" & land %in% c("Vorarlberg","Tyrol")), 
                       aes(x=date, y=value, colour=land))+
  geom_line(size=1.1)+
  facet_wrap(~ variable)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d")),
                            as.numeric(as.Date("2020-12-28","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2]))+
  labs(colour="",x="Datum",y="Aktivitätsniveau\n(Basis: 15.9.2020 = 1)")+
  scale_y_continuous(sec.axis = dup_axis(), limits=c(0.25,1.5), breaks=seq(.25,1.5,.25))+
  ggtitle("Aktivitätsniveau Vor- und Hinterarlberg", subtitle = "(laut Google; mittwochs)")

gta_plot_saver(plot=mob.subnational,
               path=paste0(save.path,"pics"),
               name=paste0("Mobilität - VBGvTRL - ",Sys.Date()),
               png=F,
               jpg=T)


## 
mob.subnational=ggplot(subset(ggl.long, date>="2020-08-01" & wochentag=="Dienstag" & land %in% c("Vorarlberg","Tyrol", "St. Gallen")), 
                       aes(x=date, y=value, colour=land))+
  geom_line(size=1.1)+
  facet_wrap(~ variable)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2], gta_colour$turquoise[3]))+
  labs(colour="",x="Datum",y="Aktivitätsniveau\n(Basis: 15.9.2020 = 1)")+
  scale_y_continuous(sec.axis = dup_axis(), limits=c(0.25,1.5), breaks=seq(.25,1.5,.25))+
  ggtitle("Aktivitätsniveau in Ost und West von Rhein oder Arlberg", subtitle = "(laut Google; dienstags)")

gta_plot_saver(plot=mob.subnational,
               path=paste0(save.path,"pics"),
               name=paste0("Mobilität - VBGvTRLvsSG - ",Sys.Date()),
               png=F,
               jpg=T)



## cases VBGvSGvTRL
cases.vts=read.csv("data/VBG-TRL-SG.csv", sep=";", stringsAsFactors = F)
names(cases.vts)[1]="date"
cases.vts$date=as.Date(cases.vts$date, "%d.%m.%Y")

case.subnational=
ggplot(subset(cases.vts, date>="2020-08-01" & variable=="Positivanteil"), 
                       aes(x=date, y=value, colour=geo))+
  geom_line(size=1.1)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2], gta_colour$turquoise[3]))+
  scale_y_continuous(labels = scales::percent, sec.axis = dup_axis())+
  labs(colour="",x="Datum",y=NULL)+
  ggtitle("Anteil positiver Tests", subtitle = "in Ost und West von Rhein oder Arlberg")

gta_plot_saver(plot=case.subnational,
               path=paste0(save.path,"pics"),
               name=paste0("VBGvTRLvsSG - Positivanteil - ",Sys.Date()),
               png=F,
               jpg=T)
                 


case.subnational=
  ggplot(subset(cases.vts, date>="2020-08-01" & variable=="Wocheninzidenz pro 100T"), 
         aes(x=date, y=value, colour=geo))+
  geom_line(size=1.1)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2], gta_colour$turquoise[3]))+
  scale_y_continuous(sec.axis = dup_axis())+
  labs(colour="",x="Datum",y=NULL)+
  ggtitle("Positive Tests pro 100T Einwohner in der Vorwoche", subtitle = "in Ost und West von Rhein oder Arlberg")

gta_plot_saver(plot=case.subnational,
               path=paste0(save.path,"pics"),
               name=paste0("VBGvTRLvsSG - Positivtests - ",Sys.Date()),
               png=F,
               jpg=T)


                 
## search
searches=read.csv("data/google searches.csv", stringsAsFactors = F, sep=";")
Encoding(searches$country)="UTF-8"
names(searches)[1]="date"

searches$date=as.Date(searches$date, "%Y-%m-%d")
searches=searches[,c(1:4)]

norm.date="2020-09-13"
searches$value=searches$value+1
for(var in unique(searches$term)){
  
  for(cty in unique(searches$country)){
     
    searches$value[searches$country==cty & searches$term==var]=searches$value[searches$country==cty & searches$term==var]/searches$value[searches$country==cty & searches$term==var & searches$date==norm.date]
    
  }
}


search.dach=ggplot(subset(searches, date>="2020-08-01" ), 
       aes(x=date, y=value, colour=country))+
  geom_line(size=1.1)+
  facet_wrap(~ term)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2], gta_colour$brown[3]))+
  labs(colour="",x="Datum",y="Aktivitätsniveau\n(Basis: 13.9.2020 = 1)")+
  scale_y_continuous(sec.axis = dup_axis())+
  ggtitle("Google Suchbegriffhäufigkeit", subtitle = "Für D-A-CH.")


gta_plot_saver(plot=search.dach,
               path=paste0(save.path,"pics"),
               name=paste0("Suchbegriffe - ",Sys.Date()),
               png=F,
               jpg=T)



search.dach=ggplot(subset(searches), 
                   aes(x=date, y=value, colour=country))+
  geom_line(size=1.1)+
  facet_wrap(~ term)+
  geom_vline(xintercept = c(as.numeric(as.Date("2020-09-21","%Y-%m-%d")), 
                            as.numeric(as.Date("2020-11-03","%Y-%m-%d")),
                            as.numeric(as.Date("2020-11-17","%Y-%m-%d"))), colour=gta_colour$blue[1], linetype=3)+
  gta_theme()+
  scale_colour_manual(values=c(gta_colour$desert[2], gta_colour$blue[2], gta_colour$brown[3]))+
  labs(colour="",x="Datum",y="Aktivitätsniveau\n(Basis: 13.9.2020 = 1)")+
  scale_y_continuous(sec.axis = dup_axis())+
  ggtitle("Google Suchbegriffshäufigkeit", subtitle = "Für D-A-CH.")


gta_plot_saver(plot=search.dach,
               path=paste0(save.path,"pics"),
               name=paste0("Suchbegriffe - ganzes Jahr - ",Sys.Date()),
               png=F,
               jpg=T)











scale_fill_gradient(high =  gta_colour$blue[1], low= gta_colour$blue[4], na.value = 'grey82',breaks=c(0.5,1,1.5))+
  gta_theme()+
  labs(fill=NULL,x=NULL,y=NULL)+
  guides(fill=FALSE)+
  ggtitle("Aktivitätsniveau in Gastronomie & Geschäften", subtitle = "(ausgenommen Supermärkte und Apotheken; donnerstags)")



rec.all=ggplot(subset(ggl.rec, date>=plot.cutoff), aes(x=date, fill=rec.norm, y=land))+
  geom_tile()+
  scale_fill_gradient(high =  gta_colour$blue[1], low= gta_colour$blue[4], na.value = 'grey82',breaks=c(0.5,1,1.5))+
  gta_theme()+
  labs(fill=NULL,x=NULL,y=NULL)+
  guides(fill=FALSE)+
  ggtitle("Aktivitätsniveau in Gastronomie & Geschäften", subtitle = "(ausgenommen Supermärkte und Apotheken; alle Wochentage)")

gta_plot_saver(plot=rec.all,
               path="pics",
               name="Mobility - rec all 2020",
               png=F,
               jpg=T)



rec.aug=ggplot(subset(ggl.rec, date>="2020-08-01" & wochentag=="Donnerstag"), aes(x=date, fill=rec.norm.aug, y=land))+
  geom_tile()+
  scale_fill_gradient(high =  gta_colour$blue[1], low= gta_colour$blue[4], na.value = 'grey82',breaks=c(0.5,1,1.5))+
  gta_theme()+
  labs(fill=NULL,x=NULL,y=NULL)+
  guides(fill=FALSE)+
  ggtitle("Aktivitätsniveau in Gastronomie & Geschäften", subtitle = "(ausgenommen Supermärkte und Apotheken; donnerstags)")

gta_plot_saver(plot=rec.aug,
               path="pics",
               name="Mobility - rec aug 2020",
               png=F,
               jpg=T)



res.all=ggplot(subset(ggl.rec, date>=plot.cutoff), aes(x=date, fill=res.norm, y=land))+
  geom_tile()+
  scale_fill_gradient(high =  gta_colour$blue[1], low= gta_colour$blue[4], na.value = 'grey82',breaks=c(0.5,1,1.5))+
  gta_theme()+
  labs(fill=NULL,x=NULL,y=NULL)+
  guides(fill=FALSE)+
  ggtitle("Aktivitätsniveau in Wohnungen & Wohnhäusern")

gta_plot_saver(plot=res.all,
               path="pics",
               name="Mobility - res all 2020",
               png=F,
               jpg=T)

res.aug=ggplot(subset(ggl.rec, date>="2020-08-01" & wochentag=="Donnerstag"), aes(x=date, fill=res.norm, y=land))+
  geom_tile()+
  scale_fill_gradient(high =  gta_colour$blue[1], low= gta_colour$blue[4], na.value = 'grey82',breaks=c(0.5,1,1.5))+
  gta_theme()+
  labs(fill=NULL,x=NULL,y=NULL)+
  guides(fill=FALSE)+
  ggtitle("Aktivitätsniveau in Wohnungen & Wohnhäusern", subtitle = "donnerstags")

gta_plot_saver(plot=res.aug,
               path="pics",
               name="Mobility - res aug 2020",
               png=F,
               jpg=T)


ggplot(subset(ggl.rec, date>="2020-08-01" & wochentag=="Donnerstag"), aes(x=date, y=rec.norm, colour=land))+
  geom_line(size=1)
  scale_y_continuous(limits=c(50,150))






## Pull Apple data
mobility.apple=read.csv("data/apple mobility.csv", stringsAsFactors = F) # via https://covid19.apple.com/mobility
mob.app.aut=subset(mobility.apple, country=="Austria"|region=="Austria")
mob.app.aut$country="Austria"
mob.app.aut$alternative_name[nchar(mob.app.aut$alternative_name)<3]=mob.app.aut$region[nchar(mob.app.aut$alternative_name)<3]
Encoding(mob.app.aut$alternative_name)="UTF-8"


