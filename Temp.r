list.of.packages <- c("dplyr","plyr","lubridate","descr",
                      "pbapply","seas")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


zak.temp.df = read.csv("zak_temp.csv",header = T, 
                       stringsAsFactors = FALSE,na.strings ="")


zak.temp.df = filter(zak.temp.df, !is.na(Date))

library(seas)
library(lubridate)

zak.temp.df$Date = as.Date(zak.temp.df$Date, format = "%m/%d/%y")

zak.temp.df$Year = year(zak.temp.df$Date)

zak.temp.df$Month = month(zak.temp.df$Date)

zak.temp.df$Stream = "Zak"

zak.temp.df$Calc = "Meas"


#zak.temp.df$Month = mkseas(zak.temp.df, width = "mon")

#zak.temp.df$Season = mkseas(zak.temp.df, width = "JFM")

zak.temp.sum = aggregate(Temp ~ Year + Month, data = zak.temp.df, FUN = mean)


zak.temp.sum$Length = aggregate(Temp ~ Year + Month, data = zak.temp.df, FUN = length)$Temp

with(zak.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

######## uppidri

uppidri.temp.df = read.csv("uppidri_temp.csv",header = T, 
                           stringsAsFactors = FALSE,na.strings ="")


uppidri.temp.df = filter(uppidri.temp.df, !is.na(Date))

library(seas)
library(lubridate)

uppidri.temp.df$Date = as.Date(uppidri.temp.df$Date, format = "%m/%d/%y")

uppidri.temp.df$Year = year(uppidri.temp.df$Date)

uppidri.temp.df$Month = month(uppidri.temp.df$Date)

uppidri.temp.df$Stream = "UIdri"

uppidri.temp.df$Calc = "Meas"

#uppidri.temp.df$Month = mkseas(uppidri.temp.df, width = "mon")

#uppidri.temp.df$Season = mkseas(uppidri.temp.df, width = "JFM")

uppidri.temp.sum = aggregate(Temp ~ Year + Month, data = uppidri.temp.df, FUN = mean)


uppidri.temp.sum$Length = aggregate(Temp ~ Year + Month, data = uppidri.temp.df, FUN = length)$Temp

with(uppidri.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

par(mfrow = c(1,2))

####### Huda

huda.temp.df = read.csv("huda_temp.csv",header = T, 
                        stringsAsFactors = FALSE,na.strings ="")


huda.temp.df = filter(huda.temp.df, !is.na(Date))

library(seas)
library(lubridate)

huda.temp.df$Date = as.Date(huda.temp.df$Date, format = "%m/%d/%y")

huda.temp.df$Year = year(huda.temp.df$Date)

huda.temp.df$Month = month(huda.temp.df$Date)

huda.temp.df$Stream = "Huda"

huda.temp.df$Calc = "Meas"

#huda.temp.df$Month = mkseas(huda.temp.df, width = "mon")

#huda.temp.df$Season = mkseas(huda.temp.df, width = "JFM")

huda.temp.sum = aggregate(Temp ~ Year + Month, data = huda.temp.df, FUN = mean)


huda.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                 data = huda.temp.df, FUN = length)$Temp

with(huda.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))


##### Lower Idrijca

loidri.temp.df = read.csv("loidri_temp.csv",header = T, 
                          stringsAsFactors = FALSE,na.strings ="")


loidri.temp.df = filter(loidri.temp.df, !is.na(Date))

library(seas)
library(lubridate)

loidri.temp.df$Date = as.Date(loidri.temp.df$Date, format = "%m/%d/%y")

loidri.temp.df$Year = year(loidri.temp.df$Date)

loidri.temp.df$Month = month(loidri.temp.df$Date)

loidri.temp.df$Stream = "LIdri"

loidri.temp.df$Calc = "Meas"

#loidri.temp.df$Month = mkseas(loidri.temp.df, width = "mon")

#loidri.temp.df$Season = mkseas(loidri.temp.df, width = "JFM")

loidri.temp.sum = aggregate(Temp ~ Year + Month, data = loidri.temp.df, FUN = mean)


loidri.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                   data = loidri.temp.df, FUN = length)$Temp

with(loidri.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))


####### Studenc

stu.temp.df = read.csv("stu_temp.csv",header = T, 
                       stringsAsFactors = FALSE,na.strings ="")


stu.temp.df = filter(stu.temp.df, !is.na(Date))

library(seas)
library(lubridate)

stu.temp.df$Date = as.Date(stu.temp.df$Date, format = "%m/%d/%y")

stu.temp.df$Year = year(stu.temp.df$Date)

stu.temp.df$Month = month(stu.temp.df$Date)

stu.temp.df$Stream = "Stu"

stu.temp.df$Calc = "Meas"

#stu.temp.df$Month = mkseas(stu.temp.df, width = "mon")

#stu.temp.df$Season = mkseas(stu.temp.df, width = "JFM")

stu.temp.sum = aggregate(Temp ~ Year + Month, data = stu.temp.df, FUN = mean)


stu.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                data = stu.temp.df, FUN = length)$Temp

with(stu.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

#### Svenica

sve.temp.df = read.csv("sve_temp.csv",header = T, 
                       stringsAsFactors = FALSE,na.strings ="")


sve.temp.df = filter(sve.temp.df, !is.na(Date))

library(seas)
library(lubridate)

sve.temp.df$Date = as.Date(sve.temp.df$Date, format = "%m/%d/%y")

sve.temp.df$Year = year(sve.temp.df$Date)

sve.temp.df$Month = month(sve.temp.df$Date)

sve.temp.df$Stream = "Sve"

sve.temp.df$Calc = "Meas"

#sve.temp.df$Month = mkseas(sve.temp.df, width = "mon")

#sve.temp.df$Season = mkseas(sve.temp.df, width = "JFM")

sve.temp.sum = aggregate(Temp ~ Year + Month, data = sve.temp.df, FUN = mean)


sve.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                data = sve.temp.df, FUN = length)$Temp

with(sve.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

##### Gacnick

gat.temp.df = read.csv("gat_temp.csv",header = T, 
                       stringsAsFactors = FALSE,na.strings ="")


gat.temp.df = filter(gat.temp.df, !is.na(Date))

library(seas)
library(lubridate)

gat.temp.df$Date = as.Date(gat.temp.df$Date, format = "%m/%d/%y")

gat.temp.df$Year = year(gat.temp.df$Date)

gat.temp.df$Month = month(gat.temp.df$Date)

gat.temp.df$Stream = "Gac"

gat.temp.df$Calc = "Meas"

#gat.temp.df$Month = mkseas(gat.temp.df, width = "mon")

#gat.temp.df$Season = mkseas(gat.temp.df, width = "JFM")

gat.temp.sum = aggregate(Temp ~ Year + Month, data = gat.temp.df, FUN = mean)


gat.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                data = gat.temp.df, FUN = length)$Temp

with(gat.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

##### Zadlascica

zadla.temp.df = read.csv("zadla_temp.csv",header = T, 
                         stringsAsFactors = FALSE,na.strings ="")


zadla.temp.df = filter(zadla.temp.df, !is.na(Date))

library(seas)
library(lubridate)

zadla.temp.df$Date = as.Date(zadla.temp.df$Date, format = "%m/%d/%y")

zadla.temp.df$Year = year(zadla.temp.df$Date)

zadla.temp.df$Month = month(zadla.temp.df$Date)

zadla.temp.df$Stream = "Zadla"

zadla.temp.df$Calc = "Meas"

#zadla.temp.df$Month = mkseas(zadla.temp.df, width = "mon")

#zadla.temp.df$Season = mkseas(zadla.temp.df, width = "JFM")

zadla.temp.sum = aggregate(Temp ~ Year + Month, data = zadla.temp.df, FUN = mean)


zadla.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                    data = zadla.temp.df, FUN = length)$Temp

with(zadla.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))


###### Trebuscica   

trebu.temp.df = read.csv("trebu_temp.csv",header = T, 
                         stringsAsFactors = FALSE,na.strings ="")


trebu.temp.df = filter(trebu.temp.df, !is.na(Date))

library(seas)
library(lubridate)

trebu.temp.df$Date = as.Date(trebu.temp.df$Date, format = "%m/%d/%y")

trebu.temp.df$Year = year(trebu.temp.df$Date)

trebu.temp.df$Month = month(trebu.temp.df$Date)

trebu.temp.df$Stream = "Trebu"

trebu.temp.df$Calc = "Meas"

#trebu.temp.df$Month = mkseas(trebu.temp.df, width = "mon")

#trebu.temp.df$Season = mkseas(trebu.temp.df, width = "JFM")

trebu.temp.sum = aggregate(Temp ~ Year + Month, data = trebu.temp.df, FUN = mean)


trebu.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                    data = trebu.temp.df, FUN = length)$Temp

with(trebu.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

#with(trebu.temp.df, table(Year))
#Year
#1997 1998 1999 2000 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
#20  365  266  162  121  248  115  366  365  365  366  365  365  365  361  370  245 
# with(filter(trebu.temp.df, Year == 2013), table(Month)
#       + )
#Month
#1  2  3  4  5  6  7  8  9 10 11 12 
#31 28 31 30 31 30 31 31 30 31 30 36 

##### Lipovesck 

lipo.temp.df = read.csv("lipo_temp.csv",header = T, 
                        stringsAsFactors = FALSE,na.strings ="")


lipo.temp.df = filter(lipo.temp.df, !is.na(Date))

library(seas)
library(lubridate)

lipo.temp.df$Date = as.Date(lipo.temp.df$Date, format = "%m/%d/%y")

lipo.temp.df$Year = year(lipo.temp.df$Date)

lipo.temp.df$Month = month(lipo.temp.df$Date)

lipo.temp.df$Stream = "Lipo"

lipo.temp.df$Calc = "Meas"

#lipo.temp.df$Month = mkseas(lipo.temp.df, width = "mon")

#lipo.temp.df$Season = mkseas(lipo.temp.df, width = "JFM")

lipo.temp.sum = aggregate(Temp ~ Year + Month, data = lipo.temp.df, FUN = mean)


lipo.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                 data = lipo.temp.df, FUN = length)$Temp

with(lipo.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))


##### Upper Volaja

uppvol.temp.df = read.csv("uppvol_temp.csv",header = T, 
                          stringsAsFactors = FALSE,na.strings ="")


uppvol.temp.df = filter(uppvol.temp.df, !is.na(Date))

library(seas)
library(lubridate)

uppvol.temp.df$Date = as.Date(uppvol.temp.df$Date, format = "%m/%d/%y")

uppvol.temp.df$Year = year(uppvol.temp.df$Date)

uppvol.temp.df$Month = month(uppvol.temp.df$Date)

uppvol.temp.df$Stream = "UVol"

uppvol.temp.df$Calc = "Meas"

#uppvol.temp.df$Month = mkseas(uppvol.temp.df, width = "mon")

#uppvol.temp.df$Season = mkseas(uppvol.temp.df, width = "JFM")

uppvol.temp.sum = aggregate(Temp ~ Year + Month, data = uppvol.temp.df, FUN = mean)


uppvol.temp.sum$Length = aggregate(Temp ~ Year + Month, 
                                   data = uppvol.temp.df, FUN = length)$Temp

with(uppvol.temp.sum, boxplot(Temp ~ Month, ylim = c(0,16)))

#####

temp.all.df = 
  rbind(zak.temp.df, gat.temp.df, sve.temp.df, stu.temp.df, loidri.temp.df, uppidri.temp.df,
        trebu.temp.df, zadla.temp.df, lipo.temp.df,huda.temp.df)

#####


with(huda.temp.df, table(Year))
with(uppidri.temp.df, table(Year))
with(trebu.temp.df, table(Year))
with(zak.temp.df, table(Year))
with(gat.temp.df, table(Year))
with(sve.temp.df, table(Year))
with(stu.temp.df, table(Year))
with(zadla.temp.df, table(Year))
with(loidri.temp.df, table(Year))
with(lipo.temp.df, table(Year))


huda.comp.temp = filter(huda.temp.df, Year %in% (2009:2013))
huda.comp.temp$Stream = "Huda"
uppidri.comp.temp = filter(uppidri.temp.df, Year %in% (2009:2013))
uppidri.comp.temp$Stream = "UIdri" 
trebu.comp.temp = filter(trebu.temp.df, Year %in% (2009:2013))
trebu.comp.temp$Stream = "Trebu"
zak.comp.temp = filter(zak.temp.df, Year %in% c(2009,2012,2013))
zak.comp.temp$Stream = "Zak"
gat.comp.temp = filter(gat.temp.df, Year %in% (2009:2013))
gat.comp.temp$Stream = "Gac"
sve.comp.temp = filter(sve.temp.df, Year %in% (2009:2013))
sve.comp.temp$Stream = "Sve"
stu.comp.temp = filter(stu.temp.df, Year %in% (2009:2013))
stu.comp.temp$Stream = "Stu"
zadla.comp.temp = filter(stu.temp.df, Year %in% c(2009,2010,2011))
zadla.comp.temp$Stream = "Zadla" 
loidri.comp.temp = filter(loidri.temp.df, Year %in% (2009:2013))
loidri.comp.temp$Stream = "LIdri"
lipo.comp.temp = filter(lipo.temp.df, Year %in% (2009:2013))
lipo.comp.temp$Stream = "Lipo"



par(mfrow = c(2,5))
with(huda.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                             main = "Huda"))
with(uppidri.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                                main = "UppIdri"))
with(loidri.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                                main = "LoIdri"))
with(trebu.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                                main = "Trebu"))
with(zak.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                              main = "Zak"))
with(zadla.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                            main = "Zadla"))
with(gat.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                              main = "Gacnik"))
with(sve.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                            main = "Sve"))
with(stu.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                            main = "Stu"))
with(lipo.comp.temp, boxplot(Temp ~ Month, ylim = c(0,16), 
                            main = "Lipo"))

temp.df = rbind(huda.comp.temp,uppidri.comp.temp,loidri.comp.temp,
                trebu.comp.temp,zak.comp.temp,zadla.comp.temp,
                gat.comp.temp, sve.comp.temp,stu.comp.temp,
                lipo.comp.temp)
temp.mean.df = aggregate(Temp ~ Stream, data = temp.df, FUN = mean)

temp.su.mean.df = aggregate(Temp ~ Stream, data = filter(temp.df,
                                                      Month %in% c(6,7,8)),FUN = mean)

temp.wi.mean.df = aggregate(Temp ~ Stream, data = filter(temp.df,
                                                         Month %in% c(12,1,2)),FUN = mean)


prova = aggregate(Temp ~ Stream, 
                  data = filter(temp.df,
                                Stream == "Gac" & Month %in% c(6,7,8)),FUN = mean)




linf.k.stream = select(g.s.df1, Stream, k, linf)
linf.k.e.stream = linf.k.e.stream[match(temp.su.mean.df$Stream, linf.k.e.stream$Stream),]

par(mfrow = c(1,1))
plot(linf.k.e.stream$estimate ~  temp.mean.df$Temp)




####### Boxplot ggplot2 

library(ggplot2)
library(grid)
line.lwd = 0.9
size.label.x = 15
size.text.x =  15
size.point = 2
size.label.y = 15
size.text.y = 15
size.legend.text = 26
size.legend.title = 40
unit.legend.h = 1.8
unit.legend.w = 1.8
size.ann = 10
line.lty = 1
colour.axis = "gray20"
colour.theme = "black"
colour.axis.line = "gray20"
colour.line = "gray50"
temp.lab =  expression(paste("Temperature [",degree,"C]")) 
ylim.v = c(-1,20)


box.zak.gg  <- ggplot(zak.comp.temp, aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Zak") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.zak.gg 


######

box.huda.gg  <- ggplot(huda.comp.temp, 
                       aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Huda") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.huda.gg 

#########

box.sve.gg  <- ggplot(sve.comp.temp, 
                       aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Sve") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.sve.gg

######

box.stu.gg  <- ggplot(stu.comp.temp, 
                      aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Stu") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.stu.gg

#########

box.gat.gg  <- ggplot(gat.comp.temp, 
                      aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Gac") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-2,20)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.gat.gg

#######

box.loidri.gg  <- ggplot(loidri.comp.temp, 
                      aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("LIdri") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.loidri.gg

#######

box.uppidri.gg  <- ggplot(uppidri.comp.temp, 
                         aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("UIdri") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.uppidri.gg

#######

box.zadla.gg  <- ggplot(zadla.comp.temp, 
                          aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Zadla") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.zadla.gg

######

box.lipo.gg  <- ggplot(lipo.comp.temp, 
                        aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Lipo") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.lipo.gg

#######

box.trebu.gg  <- ggplot(trebu.comp.temp, 
                       aes(x = factor(Month), y = Temp)) + 
  geom_boxplot(stat = "boxplot", position = "dodge", outlier.colour = "black", 
               outlier.shape = 16, outlier.size = 2, notch = FALSE, notchwidth = 0.5) +
  ggtitle("Trebu") +
  theme(plot.title = element_text(lineheight=.8, face="bold", colour = colour.theme), 
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.background = element_blank(),
        axis.line = element_line(colour = colour.axis.line,
                                 size = 1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2, colour = colour.axis),
        axis.text.x  = element_text(size=size.text.x , colour = colour.axis),
        axis.title.y = element_text(size=size.label.y, vjust = 0.1, colour = colour.axis),
        axis.text.y  = element_text(size=size.text.y, colour = colour.axis),
        legend.text=element_text(size=size.legend.text,vjust = -30),
        legend.title=element_text(size=size.legend.title,vjust = -30),
        legend.key.height=unit(unit.legend.h ,"line"),
        legend.key.width=unit(unit.legend.w,"line")) +
  scale_y_continuous(limits = c(-1,18)) +
  scale_x_discrete() +
  labs(x = "Month") +
  labs(y = temp.lab)

box.trebu.gg

######

arrange_ggplot2(box.gat.gg, box.zak.gg, box.huda.gg, box.lipo.gg, box.loidri.gg, 
                box.uppidri.gg, box.zadla.gg, box.trebu.gg, box.stu.gg, box.sve.gg, ncol = 5)  