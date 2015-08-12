

Temp.tb = Temp.corr.f(temp.all.df)


####### ESTIMATE MISSING TEMPERATURE DATA  ###


library(dplyr)
library(lubridate)
library(mgcv)
#huda.temp.df
###### Huda complete 2002

## I use the temperature for Lipo

y.temp = filter(temp.all.df, Stream == "Huda" & 
         Year %in% c(2009,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2009,2010,2011,2013))$Temp

huda.lipo.lm = lm(y.temp ~ x.temp) ## Linear is ok

start = as.Date("01/01/2002", format = "%m/%d/%Y")
full.2002 <- seq(start, by='1 day', length=365)
partial = filter(huda.temp.df, Year == 2002)

#with(partial, Temp[match(full.2002, Date)])

compl.2002 = data.frame(Date=full.2002, 
                        Temp=with(partial, Temp[match(full.2002, Date)]))

fill.2002 = filter(compl.2002, is.na(Temp))

var.2002 = filter(lipo.temp.df, Date %in% fill.2002$Date)

fill.2002$Temp = predict(huda.lipo.lm, 
                         data.frame(x.temp = var.2002$Temp))

fill.2002$Year = year(fill.2002$Date)

fill.2002$Month = month(fill.2002$Date)

fill.2002$Stream = "Huda"

fill.2002$Calc = "Lipo"

###### 2004

start = as.Date("01/01/2004", format = "%m/%d/%Y")
full.2004 <- seq(start, by='1 day', length=366)
partial = filter(huda.temp.df, Year == 2004)
#with(partial, Temp[match(full.2002, Date)])

compl.2004 = data.frame(Date=full.2004, 
                        Temp=with(partial, Temp[match(full.2004, Date)]))

fill.2004 = filter(compl.2004, is.na(Temp))

var.2004 = filter(lipo.temp.df, Date %in% fill.2004$Date)

fill.2004$Temp = predict(huda.lipo.lm, 
                         data.frame(x.temp = var.2004$Temp))

fill.2004$Year = year(fill.2004$Date)

fill.2004$Month = month(fill.2004$Date)

fill.2004$Stream = "Huda"

fill.2004$Calc = "Lipo"

##### 2005

start = as.Date("01/01/2005", format = "%m/%d/%Y")
full.2005 <- seq(start, by='1 day', length=365)
partial = filter(huda.temp.df, Year == 2005)
#with(partial, Temp[match(full.2002, Date)])

compl.2005 = data.frame(Date=full.2005, 
                        Temp=with(partial, Temp[match(full.2005, Date)]))

fill.2005 = filter(compl.2005, is.na(Temp))

var.2005 = filter(lipo.temp.df, Date %in% fill.2005$Date)

fill.2005$Temp = predict(huda.lipo.lm, 
                         data.frame(x.temp = var.2005$Temp))

fill.2005$Year = year(fill.2005$Date)

fill.2005$Month = month(fill.2005$Date)

fill.2005$Stream = "Huda"

fill.2005$Calc = "Lipo"

##### 2006

start = as.Date("01/01/2006", format = "%m/%d/%Y")
full.2006 <- seq(start, by='1 day', length=365)
partial = filter(huda.temp.df, Year == 2006)
#with(partial, Temp[match(full.2002, Date)])

compl.2006 = data.frame(Date=full.2006, 
                        Temp=with(partial, Temp[match(full.2006, Date)]))

fill.2006 = filter(compl.2006, is.na(Temp))

var.2006 = filter(lipo.temp.df, Date %in% fill.2006$Date)

fill.2006$Temp = predict(huda.lipo.lm, 
                         data.frame(x.temp = var.2006$Temp))

fill.2006$Year = year(fill.2006$Date)

fill.2006$Month = month(fill.2006$Date)

fill.2006$Stream = "Huda"

fill.2006$Calc = "Lipo"

huda.temp.df = rbind(huda.temp.df,fill.2002,fill.2004,
                     fill.2005,fill.2006)
huda.temp.df = arrange(huda.temp.df,Date)


################ LIPO

##### 2007

y.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2009,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Huda" & 
                  Year %in% c(2009,2010,2011,2013))$Temp

lipo.huda.lm = lm(y.temp ~ x.temp)  # Linear is ok

start = as.Date("01/01/2007", format = "%m/%d/%Y")
full.2007 <- seq(start, by='1 day', length=365)
partial = filter(lipo.temp.df, Year == 2007)
#with(partial, Temp[match(full.2007, Date)])

compl.2007 = data.frame(Date=full.2007, 
                        Temp=with(partial, Temp[match(full.2007, Date)]))

fill.2007 = filter(compl.2007, is.na(Temp))

var.2007 = filter(huda.temp.df, Date %in% fill.2007$Date)

fill.2007$Temp = predict(lipo.huda.lm, 
                         data.frame(x.temp = var.2007$Temp))

fill.2007$Year = year(fill.2007$Date)

fill.2007$Month = month(fill.2007$Date)

fill.2007$Stream = "Lipo"

fill.2007$Calc = "Huda"

##### 2008

start = as.Date("01/01/2008", format = "%m/%d/%Y")
full.2008 <- seq(start, by='1 day', length=366)
partial = filter(lipo.temp.df, Year == 2008)
#with(partial, Temp[match(full.2008, Date)])

compl.2008 = data.frame(Date=full.2008, 
                        Temp=with(partial, Temp[match(full.2008, Date)]))

fill.2008 = filter(compl.2008, is.na(Temp))

var.2008 = filter(huda.temp.df, Date %in% fill.2008$Date)

fill.2008$Temp = predict(lipo.huda.lm, 
                         data.frame(x.temp = var.2008$Temp))

fill.2008$Year = year(fill.2008$Date)

fill.2008$Month = month(fill.2008$Date)

fill.2008$Stream = "Lipo"

fill.2008$Calc = "Huda"

lipo.temp.df = rbind(lipo.temp.df,fill.2007,fill.2008)
lipo.temp.df = arrange(lipo.temp.df,Date)

##### ZADLA from Lipo

#### 2000

y.temp = filter(temp.all.df, Stream == "Zadla" & 
                  Year %in% c(2003,2006,2009,2010,2011))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2003,2006,2009,2010,2011))$Temp



#zadla.lipo.lm = lm(y.temp ~ x.temp) ## Summer seems quite unpredictable, I use a non linear model as a better approximation

zadla.lipo.gam = gam(y.temp ~ s(x.temp))

start = as.Date("01/01/2000", format = "%m/%d/%Y")
full.2000 <- seq(start, by='1 day', length=366)
partial = filter(zadla.temp.df, Year == 2000)
#with(partial, Temp[match(full.2000, Date)])

compl.2000 = data.frame(Date=full.2000, 
                        Temp=with(partial, Temp[match(full.2000, Date)]))

fill.2000 = filter(compl.2000, is.na(Temp))

var.2000 = filter(lipo.temp.df, Date %in% fill.2000$Date)

fill.2000$Temp = predict(zadla.lipo.gam, 
                         data.frame(x.temp = var.2000$Temp))

fill.2000$Year = year(fill.2000$Date)

fill.2000$Month = month(fill.2000$Date)

fill.2000$Stream = "Zadla"

fill.2000$Calc = "Lipo"

#### 2002


start = as.Date("01/01/2002", format = "%m/%d/%Y")
full.2002 <- seq(start, by='1 day', length=365)
partial = filter(zadla.temp.df, Year == 2002)
#with(partial, Temp[match(full.2002, Date)])

compl.2002 = data.frame(Date=full.2002, 
                        Temp=with(partial, Temp[match(full.2002, Date)]))

fill.2002 = filter(compl.2002, is.na(Temp))

var.2002 = filter(lipo.temp.df, Date %in% fill.2002$Date)

fill.2002 = fill.2002[-which(!fill.2002$Date %in% var.2002$Date),] ### this because Lipo misses one day in 2002


fill.2002$Temp = predict(zadla.lipo.gam, 
                         data.frame(x.temp = var.2002$Temp))

fill.2002$Year = year(fill.2002$Date)

fill.2002$Month = month(fill.2002$Date)

fill.2002$Stream = "Zadla"

fill.2002$Calc = "Lipo"

####### 2004

start = as.Date("01/01/2004", format = "%m/%d/%Y")
full.2004 <- seq(start, by='1 day', length=366)
partial = filter(zadla.temp.df, Year == 2004)
#with(partial, Temp[match(full.2004, Date)])

compl.2004 = data.frame(Date=full.2004, 
                        Temp=with(partial, Temp[match(full.2004, Date)]))

fill.2004 = filter(compl.2004, is.na(Temp))

var.2004 = filter(lipo.temp.df, Date %in% fill.2004$Date)

fill.2004$Temp = predict(zadla.lipo.gam, 
                         data.frame(x.temp = var.2004$Temp))

fill.2004$Year = year(fill.2004$Date)

fill.2004$Month = month(fill.2004$Date)

fill.2004$Stream = "Zadla"

fill.2004$Calc = "Lipo"

######## 2005

start = as.Date("01/01/2005", format = "%m/%d/%Y")
full.2005 <- seq(start, by='1 day', length=365)
partial = filter(zadla.temp.df, Year == 2005)
#with(partial, Temp[match(full.2005, Date)])

compl.2005 = data.frame(Date=full.2005, 
                        Temp=with(partial, Temp[match(full.2005, Date)]))

fill.2005 = filter(compl.2005, is.na(Temp))

var.2005 = filter(lipo.temp.df, Date %in% fill.2005$Date)

fill.2005$Temp = predict(zadla.lipo.gam, 
                         data.frame(x.temp = var.2005$Temp))

fill.2005$Year = year(fill.2005$Date)

fill.2005$Month = month(fill.2005$Date)

fill.2005$Stream = "Zadla"

fill.2005$Calc = "Lipo"

##### 2012

start = as.Date("01/01/2012", format = "%m/%d/%Y")
full.2012 <- seq(start, by='1 day', length=366)
partial = filter(zadla.temp.df, Year == 2012)
#with(partial, Temp[match(full.2012, Date)])

compl.2012 = data.frame(Date=full.2012, 
                        Temp=with(partial, Temp[match(full.2012, Date)]))

fill.2012 = filter(compl.2012, is.na(Temp))

var.2012 = filter(lipo.temp.df, Date %in% fill.2012$Date)

fill.2012$Temp = predict(zadla.lipo.gam, 
                         data.frame(x.temp = var.2012$Temp))

fill.2012$Year = year(fill.2012$Date)

fill.2012$Month = month(fill.2012$Date)

fill.2012$Stream = "Zadla"

fill.2012$Calc = "Lipo"

##### 2013

start = as.Date("01/01/2013", format = "%m/%d/%Y")
full.2013 <- seq(start, by='1 day', length=365)
partial = filter(zadla.temp.df, Year == 2013)
#with(partial, Temp[match(full.2013, Date)])

compl.2013 = data.frame(Date=full.2013, 
                        Temp=with(partial, Temp[match(full.2013, Date)]))

fill.2013 = filter(compl.2013, is.na(Temp))

var.2013 = filter(lipo.temp.df, Date %in% fill.2013$Date)

fill.2013$Temp = predict(zadla.lipo.gam, 
                         data.frame(x.temp = var.2013$Temp))

fill.2013$Year = year(fill.2013$Date)

fill.2013$Month = month(fill.2013$Date)

fill.2013$Stream = "Zadla"

fill.2013$Calc = "Lipo"

zadla.temp.df = rbind(zadla.temp.df,fill.2000,fill.2002, fill.2004, fill.2005, fill.2012, fill.2013)
zadla.temp.df = arrange(zadla.temp.df,Date)


####### TREBU FROM LIPO

#### 2000

y.temp = filter(temp.all.df, Stream == "Trebu" & 
                  Year %in% c(2006,2009,2010,2011))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2006,2009,2010,2011))$Temp

trebu.lipo.lm = lm(y.temp ~ x.temp)

start = as.Date("01/01/2000", format = "%m/%d/%Y")
full.2000 <- seq(start, by='1 day', length=366)
partial = filter(trebu.temp.df, Year == 2000)
#with(partial, Temp[match(full.2000, Date)])

compl.2000 = data.frame(Date=full.2000, 
                        Temp=with(partial, Temp[match(full.2000, Date)]))

fill.2000 = filter(compl.2000, is.na(Temp))

var.2000 = filter(lipo.temp.df, Date %in% fill.2000$Date)

fill.2000$Temp = predict(trebu.lipo.lm, 
                         data.frame(x.temp = var.2000$Temp))

fill.2000$Year = year(fill.2000$Date)

fill.2000$Month = month(fill.2000$Date)

fill.2000$Stream = "Trebu"

fill.2000$Calc = "Lipo"

##### 2002

start = as.Date("01/01/2002", format = "%m/%d/%Y")
full.2002 <- seq(start, by='1 day', length=365)
partial = filter(trebu.temp.df, Year == 2002)
#with(partial, Temp[match(full.2002, Date)])

compl.2002 = data.frame(Date=full.2002, 
                        Temp=with(partial, Temp[match(full.2002, Date)]))

fill.2002 = filter(compl.2002, is.na(Temp))

fill.2002 = fill.2002[-which(!fill.2002$Date %in% var.2002$Date),] ### this because Lipo misses one day in 2002

var.2002 = filter(lipo.temp.df, Date %in% fill.2002$Date)

fill.2002$Temp = predict(trebu.lipo.lm, 
                         data.frame(x.temp = var.2002$Temp))

fill.2002$Year = year(fill.2002$Date)

fill.2002$Month = month(fill.2002$Date)

fill.2002$Stream = "Trebu"

fill.2002$Calc = "Lipo"

###### 2003
start = as.Date("01/01/2003", format = "%m/%d/%Y")
full.2003 <- seq(start, by='1 day', length=365)
partial = filter(trebu.temp.df, Year == 2003)
#with(partial, Temp[match(full.2003, Date)])

compl.2003 = data.frame(Date=full.2003, 
                        Temp=with(partial, Temp[match(full.2003, Date)]))

fill.2003 = filter(compl.2003, is.na(Temp))

var.2003 = filter(lipo.temp.df, Date %in% fill.2003$Date)

fill.2003$Temp = predict(trebu.lipo.lm, 
                         data.frame(x.temp = var.2003$Temp))

fill.2003$Year = year(fill.2003$Date)

fill.2003$Month = month(fill.2003$Date)

fill.2003$Stream = "Trebu"

fill.2003$Calc = "Lipo"

#### 2004

start = as.Date("01/01/2004", format = "%m/%d/%Y")
full.2004 <- seq(start, by='1 day', length=366)
partial = filter(trebu.temp.df, Year == 2004)
#with(partial, Temp[match(full.2004, Date)])

compl.2004 = data.frame(Date=full.2004, 
                        Temp=with(partial, Temp[match(full.2004, Date)]))

fill.2004 = filter(compl.2004, is.na(Temp))

var.2004 = filter(lipo.temp.df, Date %in% fill.2004$Date)

fill.2004$Temp = predict(trebu.lipo.lm, 
                         data.frame(x.temp = var.2004$Temp))

fill.2004$Year = year(fill.2004$Date)

fill.2004$Month = month(fill.2004$Date)

fill.2004$Stream = "Trebu"

fill.2004$Calc = "Lipo"

trebu.temp.df = rbind(trebu.temp.df,fill.2000,fill.2002, fill.2003, fill.2004)
trebu.temp.df = arrange(trebu.temp.df,Date)


#### UPPIDRI from LIPO (2000-2002)

#### 2000

y.temp = filter(temp.all.df, Stream == "UIdri" & 
                  Year %in% c(2003,2005,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2003,2005,2010,2011,2013))$Temp

uppidri.lipo.lm = lm(y.temp ~ x.temp)  ## Linear is ok

start = as.Date("01/01/2000", format = "%m/%d/%Y")
full.2000 <- seq(start, by='1 day', length=366)
partial = filter(uppidri.temp.df, Year == 2000)
#with(partial, Temp[match(full.2000, Date)])

compl.2000 = data.frame(Date=full.2000, 
                        Temp=with(partial, Temp[match(full.2000, Date)]))

fill.2000 = filter(compl.2000, is.na(Temp))

var.2000 = filter(lipo.temp.df, Date %in% fill.2000$Date)

fill.2000$Temp = predict(uppidri.lipo.lm, 
                         data.frame(x.temp = var.2000$Temp))

fill.2000$Year = year(fill.2000$Date)

fill.2000$Month = month(fill.2000$Date)

fill.2000$Stream = "UIdri"

fill.2000$Calc = "Lipo"

#### 2002

start = as.Date("01/01/2002", format = "%m/%d/%Y")
full.2002 <- seq(start, by='1 day', length=365)
partial = filter(uppidri.temp.df, Year == 2002)
#with(partial, Temp[match(full.2002, Date)])

compl.2002 = data.frame(Date=full.2002, 
                        Temp=with(partial, Temp[match(full.2002, Date)]))

fill.2002 = filter(compl.2002, is.na(Temp))

fill.2002 = fill.2002[-which(!fill.2002$Date %in% var.2002$Date),] ### this because Lipo misses one day in 2002

var.2002 = filter(lipo.temp.df, Date %in% fill.2002$Date)

fill.2002$Temp = predict(uppidri.lipo.lm, 
                         data.frame(x.temp = var.2002$Temp))

fill.2002$Year = year(fill.2002$Date)

fill.2002$Month = month(fill.2002$Date)

fill.2002$Stream = "UIdri"

fill.2002$Calc = "Lipo"

######## 1998 from ZADLA

y.temp = filter(temp.all.df, Stream == "UIdri" & 
                  Year %in% c(2003,2010,2011))$Temp
x.temp = filter(temp.all.df, Stream == "Zadla" & 
                  Year %in% c(2003,2010,2011))$Temp

uppidri.zadla.lm = lm(y.temp ~ x.temp) #Linear is ok

start = as.Date("01/01/1998", format = "%m/%d/%Y")
full.1998 <- seq(start, by='1 day', length=365)
partial = filter(uppidri.temp.df, Year == 1998)
#with(partial, Temp[match(full.1998, Date)])

compl.1998 = data.frame(Date=full.1998, 
                        Temp=with(partial, Temp[match(full.1998, Date)]))

fill.1998 = filter(compl.1998, is.na(Temp))


var.1998 = filter(zadla.temp.df, Date %in% fill.1998$Date)

fill.1998$Temp = predict(uppidri.zadla.lm, 
                         data.frame(x.temp = var.1998$Temp))

fill.1998$Year = year(fill.1998$Date)

fill.1998$Month = month(fill.1998$Date)

fill.1998$Stream = "UIdri"

fill.1998$Calc = "Zadla"

#### 2008 from Svenica

y.temp = filter(temp.all.df, Stream == "UIdri" & 
                  Year %in% c(2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Sve" & 
                  Year %in% c(2010,2011,2013))$Temp

## uppidri.sve.lm = lm(y.temp ~ x.temp)
uppidri.sve.gam = gam(y.temp ~ s(x.temp))
## Better not linear

start = as.Date("01/01/2008", format = "%m/%d/%Y")
full.2008 <- seq(start, by='1 day', length=366)
partial = filter(uppidri.temp.df, Year == 2008)
#with(partial, Temp[match(full.2008, Date)])

compl.2008 = data.frame(Date=full.2008, 
                        Temp=with(partial, Temp[match(full.2008, Date)]))

fill.2008 = filter(compl.2008, is.na(Temp))


var.2008 = filter(sve.temp.df, Date %in% fill.2008$Date)

fill.2008$Temp = predict(uppidri.sve.gam, 
                         data.frame(x.temp = var.2008$Temp))

fill.2008$Year = year(fill.2008$Date)

fill.2008$Month = month(fill.2008$Date)

fill.2008$Stream = "UIdri"

fill.2008$Calc = "Sve"

uppidri.temp.df = rbind(uppidri.temp.df,fill.1998,fill.2000, fill.2002, fill.2008)
uppidri.temp.df = arrange(uppidri.temp.df,Date)


### Lower Idrijca with Lipo

#### 2003

y.temp = filter(temp.all.df, Stream == "LIdri" & 
                  Year %in% c(2005,2006,2009,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2005,2006,2009,2010,2011,2013))$Temp

loidri.lipo.lm = lm(y.temp ~ x.temp) ##Linear is ok


start = as.Date("01/01/2003", format = "%m/%d/%Y")
full.2003 <- seq(start, by='1 day', length=365)
partial = filter(loidri.temp.df, Year == 2003)
#with(partial, Temp[match(full.2003, Date)])

compl.2003 = data.frame(Date=full.2003, 
                        Temp=with(partial, Temp[match(full.2003, Date)]))

fill.2003 = filter(compl.2003, is.na(Temp))


var.2003 = filter(lipo.temp.df, Date %in% fill.2003$Date)

fill.2003$Temp = predict(loidri.lipo.lm, 
                         data.frame(x.temp = var.2003$Temp))

fill.2003$Year = year(fill.2003$Date)

fill.2003$Month = month(fill.2003$Date)

fill.2003$Stream = "LIdri"

fill.2003$Calc = "Lipo"

loidri.temp.df = rbind(loidri.temp.df,fill.2003)
loidri.temp.df = arrange(loidri.temp.df,Date)



########## 2014 with LIPO

start = as.Date("01/01/2014", format = "%m/%d/%Y")
full.2014 <- seq(start, by='1 day', length=247)
partial = filter(loidri.temp.df, Year == 2014)
#with(partial, Temp[match(full.2003, Date)])

compl.2014 = data.frame(Date=full.2014, 
                        Temp=with(partial, Temp[match(full.2014, Date)]))

fill.2014 = filter(compl.2014, is.na(Temp))


var.2014 = filter(lipo.temp.df, Date %in% fill.2014$Date)

fill.2014$Temp = predict(loidri.lipo.lm, 
                         data.frame(x.temp = var.2014$Temp))

fill.2014$Year = year(fill.2014$Date)

fill.2014$Month = month(fill.2014$Date)

fill.2014$Stream = "LIdri"

fill.2014$Calc = "Lipo"

loidri.temp.df = rbind(loidri.temp.df,fill.2014)
loidri.temp.df = arrange(loidri.temp.df,Date)



########## 



###### Studenc with LIpo

### 2000

y.temp = filter(temp.all.df, Stream == "Stu" & 
                  Year %in% c(2005,2006,2009,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2005,2006,2009,2010,2011,2013))$Temp

stu.lipo.lm = lm(y.temp ~ x.temp)

start = as.Date("01/01/2000", format = "%m/%d/%Y")
full.2000 <- seq(start, by='1 day', length=366)
partial = filter(stu.temp.df, Year == 2000)
#with(partial, Temp[match(full.2000, Date)])

compl.2000 = data.frame(Date=full.2000, 
                        Temp=with(partial, Temp[match(full.2000, Date)]))

fill.2000 = filter(compl.2000, is.na(Temp))


var.2000 = filter(lipo.temp.df, Date %in% fill.2000$Date)

fill.2000$Temp = predict(stu.lipo.lm, 
                         data.frame(x.temp = var.2000$Temp))

fill.2000$Year = year(fill.2000$Date)

fill.2000$Month = month(fill.2000$Date)

fill.2000$Stream = "Stu"

fill.2000$Calc = "Lipo"

stu.temp.df = rbind(stu.temp.df,fill.2000)
stu.temp.df = arrange(stu.temp.df,Date)


#####  Svenica with Studenc

#### 2001

y.temp = filter(temp.all.df, Stream == "Sve" & 
                  Year %in% c(2006,2007,2009,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Stu" & 
                  Year %in% c(2006,2007,2009,2010,2011,2013))$Temp

# sve.stu.lm = lm(y.temp ~ x.temp)
sve.stu.gam = gam(y.temp ~ s(x.temp)) # better not linear

start = as.Date("01/01/2001", format = "%m/%d/%Y")
full.2001 <- seq(start, by='1 day', length=365)
partial = filter(sve.temp.df, Year == 2001)
#with(partial, Temp[match(full.2001, Date)])

compl.2001 = data.frame(Date=full.2001, 
                        Temp=with(partial, Temp[match(full.2001, Date)]))

fill.2001 = filter(compl.2001, is.na(Temp))


var.2001 = filter(stu.temp.df, Date %in% fill.2001$Date)

var.2001 = var.2001[-which(duplicated(var.2001$Date)),]
# 3 duplicated readings in var.2001 for Studenc

fill.2001$Temp = predict(sve.stu.gam, 
                         data.frame(x.temp = var.2001$Temp))

fill.2001$Year = year(fill.2001$Date)

fill.2001$Month = month(fill.2001$Date)

fill.2001$Stream = "Sve"

fill.2001$Calc = "Stu"

######### 2004

start = as.Date("01/01/2004", format = "%m/%d/%Y")
full.2004 <- seq(start, by='1 day', length=366)
partial = filter(sve.temp.df, Year == 2004)
#with(partial, Temp[match(full.2004, Date)])

compl.2004 = data.frame(Date=full.2004, 
                        Temp=with(partial, Temp[match(full.2004, Date)]))

fill.2004 = filter(compl.2004, is.na(Temp))


var.2004 = filter(stu.temp.df, Date %in% fill.2004$Date)


fill.2004$Temp = predict(sve.stu.gam, 
                         data.frame(x.temp = var.2004$Temp))

fill.2004$Year = year(fill.2004$Date)

fill.2004$Month = month(fill.2004$Date)

fill.2004$Stream = "Sve"

fill.2004$Calc = "Stu"

########  2005

start = as.Date("01/01/2005", format = "%m/%d/%Y")
full.2005 <- seq(start, by='1 day', length=365)
partial = filter(sve.temp.df, Year == 2005)
#with(partial, Temp[match(full.2005, Date)])

compl.2005 = data.frame(Date=full.2005, 
                        Temp=with(partial, Temp[match(full.2005, Date)]))

fill.2005 = filter(compl.2005, is.na(Temp))


var.2005 = filter(stu.temp.df, Date %in% fill.2005$Date)


fill.2005$Temp = predict(sve.stu.gam, 
                         data.frame(x.temp = var.2005$Temp))

fill.2005$Year = year(fill.2005$Date)

fill.2005$Month = month(fill.2005$Date)

fill.2005$Stream = "Sve"

fill.2005$Calc = "Stu"

sve.temp.df = rbind(sve.temp.df,fill.2001,fill.2004,fill.2005)
sve.temp.df = arrange(sve.temp.df,Date)

##### Gacnick from Lipo

### 2000

y.temp = filter(temp.all.df, Stream == "Gac" & 
                  Year %in% c(2005,2006,2009,2010,2011,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2005,2006,2009,2010,2011,2013))$Temp

#gat.lipo.lm = lm(y.temp ~ x.temp)
gat.lipo.gam = gam(y.temp ~ s(x.temp)) ### use a gam because the relationship is
#clearly not linear
start = as.Date("01/01/2000", format = "%m/%d/%Y")
full.2000 <- seq(start, by='1 day', length=366)
partial = filter(gat.temp.df, Year == 2000)
#with(partial, Temp[match(full.2000, Date)])

compl.2000 = data.frame(Date=full.2000, 
                        Temp=with(partial, Temp[match(full.2000, Date)]))

fill.2000 = filter(compl.2000, is.na(Temp))


var.2000 = filter(lipo.temp.df, Date %in% fill.2000$Date)

fill.2000$Temp = predict(gat.lipo.gam, 
                         data.frame(x.temp = var.2000$Temp))

fill.2000$Year = year(fill.2000$Date)

fill.2000$Month = month(fill.2000$Date)

fill.2000$Stream = "Gac"

fill.2000$Calc = "Lipo"

#### 1998 from Trebu

y.temp = filter(temp.all.df, Stream == "Gac" & 
                  Year %in% c(2006,2007,2009,2010,2011))$Temp
x.temp = filter(temp.all.df, Stream == "Trebu" & 
                  Year %in% c(2006,2007,2009,2010,2011))$Temp

#gat.trebu.lm = lm(y.temp ~ x.temp)
gat.trebu.gam = gam(y.temp ~ s(x.temp))
## I use gam because the relationship is clearly non-linear

start = as.Date("01/01/1998", format = "%m/%d/%Y")
full.1998 <- seq(start, by='1 day', length=365)
partial = filter(gat.temp.df, Year == 1998)
#with(partial, Temp[match(full.1998, Date)])

compl.1998 = data.frame(Date=full.1998, 
                        Temp=with(partial, Temp[match(full.1998, Date)]))

fill.1998 = filter(compl.1998, is.na(Temp))


var.1998 = filter(trebu.temp.df, Date %in% fill.1998$Date)

fill.1998$Temp = predict(gat.trebu.gam, 
                         data.frame(x.temp = var.1998$Temp))

fill.1998$Year = year(fill.1998$Date)

fill.1998$Month = month(fill.1998$Date)

fill.1998$Stream = "Gac"

fill.1998$Calc = "Trebu"

gat.temp.df = rbind(gat.temp.df,fill.1998,fill.2000)
gat.temp.df = arrange(gat.temp.df,Date)


##########  Zakojska from Lipo

#### 2000

y.temp = filter(temp.all.df, Stream == "Zak" & 
                  Year %in% c(2003,2005,2009,2013))$Temp
x.temp = filter(temp.all.df, Stream == "Lipo" & 
                  Year %in% c(2003,2005,2009,2013))$Temp

#zak.lipo.lm = lm(y.temp ~ x.temp)
zak.lipo.gam = gam(y.temp ~ s(x.temp))

start = as.Date("01/01/2000", format = "%m/%d/%Y")
full.2000 <- seq(start, by='1 day', length=366)
partial = filter(zak.temp.df, Year == 2000)
#with(partial, Temp[match(full.2000, Date)])

compl.2000 = data.frame(Date=full.2000, 
                        Temp=with(partial, Temp[match(full.2000, Date)]))

fill.2000 = filter(compl.2000, is.na(Temp))


var.2000 = filter(lipo.temp.df, Date %in% fill.2000$Date)

fill.2000$Temp = predict(zak.lipo.gam, 
                         data.frame(x.temp = var.2000$Temp))

fill.2000$Year = year(fill.2000$Date)

fill.2000$Month = month(fill.2000$Date)

fill.2000$Stream = "Zak"

fill.2000$Calc = "Lipo"


###### 2006

start = as.Date("01/01/2006", format = "%m/%d/%Y")
full.2006 <- seq(start, by='1 day', length=365)
partial = filter(zak.temp.df, Year == 2006)
#with(partial, Temp[match(full.2006, Date)])

compl.2006 = data.frame(Date=full.2006, 
                        Temp=with(partial, Temp[match(full.2006, Date)]))

fill.2006 = filter(compl.2006, is.na(Temp))


var.2006 = filter(lipo.temp.df, Date %in% fill.2006$Date)

fill.2006$Temp = predict(zak.lipo.gam, 
                         data.frame(x.temp = var.2006$Temp))

fill.2006$Year = year(fill.2006$Date)

fill.2006$Month = month(fill.2006$Date)

fill.2006$Stream = "Zak"

fill.2006$Calc = "Lipo"

##### 2010

start = as.Date("01/01/2010", format = "%m/%d/%Y")
full.2010 <- seq(start, by='1 day', length=365)
partial = filter(zak.temp.df, Year == 2010)
#with(partial, Temp[match(full.2010, Date)])

compl.2010 = data.frame(Date=full.2010, 
                        Temp=with(partial, Temp[match(full.2010, Date)]))

fill.2010 = filter(compl.2010, is.na(Temp))


var.2010 = filter(lipo.temp.df, Date %in% fill.2010$Date)

fill.2010$Temp = predict(zak.lipo.gam, 
                         data.frame(x.temp = var.2010$Temp))

fill.2010$Year = year(fill.2010$Date)

fill.2010$Month = month(fill.2010$Date)

fill.2010$Stream = "Zak"

fill.2010$Calc = "Lipo"

##### 2011

start = as.Date("01/01/2011", format = "%m/%d/%Y")
full.2011 <- seq(start, by='1 day', length=365)
partial = filter(zak.temp.df, Year == 2011)
#with(partial, Temp[match(full.2011, Date)])

compl.2011 = data.frame(Date=full.2011, 
                        Temp=with(partial, Temp[match(full.2011, Date)]))

fill.2011 = filter(compl.2011, is.na(Temp))


var.2011 = filter(lipo.temp.df, Date %in% fill.2011$Date)

fill.2011$Temp = predict(zak.lipo.gam, 
                         data.frame(x.temp = var.2011$Temp))

fill.2011$Year = year(fill.2011$Date)

fill.2011$Month = month(fill.2011$Date)

fill.2011$Stream = "Zak"

fill.2011$Calc = "Lipo"

##### 2008  from Lower Idrijca 

y.temp = filter(temp.all.df, Stream == "Zak" & 
                  Year %in% c(2005,2009,2013))$Temp
x.temp = filter(temp.all.df, Stream == "LIdri" & 
                  Year %in% c(2005,2009,2013))$Temp

#zak.loidri.lm = lm(y.temp ~ x.temp)
zak.loidri.gam = gam(y.temp ~ x.temp)

start = as.Date("01/01/2008", format = "%m/%d/%Y")
full.2008 <- seq(start, by='1 day', length=366)
partial = filter(zak.temp.df, Year == 2008)
#with(partial, Temp[match(full.2011, Date)])

compl.2008 = data.frame(Date=full.2008, 
                        Temp=with(partial, Temp[match(full.2008, Date)]))

fill.2008 = filter(compl.2008, is.na(Temp))


var.2008 = filter(loidri.temp.df, Date %in% fill.2008$Date)

fill.2008$Temp = predict(zak.loidri.gam, 
                         data.frame(x.temp = var.2008$Temp))

fill.2008$Year = year(fill.2008$Date)

fill.2008$Month = month(fill.2008$Date)

fill.2008$Stream = "Zak"

fill.2008$Calc = "LIdri"


##### 2007  from Lower Idrijca 



start = as.Date("01/01/2007", format = "%m/%d/%Y")
full.2007 <- seq(start, by='1 day', length=365)
#partial = filter(zak.temp.df, Year == 2007)
#with(partial, Temp[match(full.2011, Date)])

compl.2007 = data.frame(Date=full.2007, 
                        Temp=NA)

fill.2007 = filter(compl.2007, is.na(Temp))


var.2007 = filter(loidri.temp.df, Year == 2007)

fill.2007$Temp = predict(zak.loidri.gam, 
                         data.frame(x.temp = var.2007$Temp))

fill.2007$Year = year(fill.2007$Date)

fill.2007$Month = month(fill.2007$Date)

fill.2007$Stream = "Zak"

fill.2007$Calc = "LIdri"


####

zak.temp.df = rbind(zak.temp.df,fill.2000,fill.2006,fill.2008,fill.2010,fill.2011,fill.2007)
zak.temp.df = arrange(zak.temp.df,Date)

#### 1999 from Zakojska

years.comp = c(1998,2000:2006,2008:2013)
cor.zakt = rep(0,length(years.comp))
temp.tar = filter(zak.temp.df, Year == 1999)
for (i in 1:length(years.comp)) {
  
  temp.var = filter(zak.temp.df, 
                    Year == years.comp[i] & !(month(Date) == 2 & day(Date) == 29))
    
    temp.var = temp.var[1:202,] # I have the first 202 days in 1999
  
  cor.zakt[i] = as.numeric(cor.test(temp.tar$Temp, temp.var$Temp)$estimate)
  
  
}

best.cor.year = years.comp[which(cor.zakt == max(cor.zakt))]
#### it is 2012. I then use the 2010 to predict 1999

###

x.temp = filter(zak.temp.df, 
                Year == 2012 & 
                  !(month(Date) == 2 & day(Date) == 29))[1:202,]$Temp
y.temp = filter(zak.temp.df, Year == 1999)$Temp

zak99.zak12.lm = lm(y.temp ~ x.temp)

start = as.Date("01/01/1999", format = "%m/%d/%Y")
full.1999 <- seq(start, by='1 day', length=365)
partial = filter(zak.temp.df, Year == 1999)
#with(partial, Temp[match(full.2011, Date)])

compl.1999 = data.frame(Date=full.1999, 
                        Temp=with(partial, Temp[match(full.1999, Date)]))

fill.1999= filter(compl.1999, is.na(Temp))


var.1999 = filter(zak.temp.df, 
                  Year == 2012 & !(month(Date) == 2 & day(Date) == 29))[203:365,]
###### up to the 202nd day we have data from 1999


fill.1999$Temp = predict(zak99.zak12.lm, 
                         data.frame(x.temp = var.1999$Temp))

fill.1999$Year = year(fill.1999$Date)

fill.1999$Month = month(fill.1999$Date)

fill.1999$Stream = "Zak"

fill.1999$Calc = "Zak2012"


######

zak.temp.df = rbind(zak.temp.df,fill.1999)
zak.temp.df = arrange(zak.temp.df,Date)

########

#### 1997 from Zakojska

years.comp = c(1998,2000:2006,2008:2013)
cor.zakt = rep(0,length(years.comp))
temp.tar = filter(zak.temp.df, Year == 1997)
start = as.Date("01/01/1997", format = "%m/%d/%Y")
full.1997 <- seq(start, by='1 day', length=365)
incl.d = which(full.1997 %in% temp.tar$Date)
for (i in 1:length(years.comp)) {
  
  temp.var = filter(zak.temp.df, 
                    Year == years.comp[i] & !(month(Date) == 2 & day(Date) == 29))
  
  temp.var = temp.var[incl.d,] # I have the first 202 days in 1999
  
  cor.zakt[i] = as.numeric(cor.test(temp.tar$Temp, temp.var$Temp)$estimate)
  
  
}

best.cor.year = years.comp[which(cor.zakt == max(cor.zakt))]
#### it is 2012. I then use the 2012 to predict 1999

###

x.temp = filter(zak.temp.df, 
                Year == 2010 & 
                  !(month(Date) == 2 & day(Date) == 29))[incl.d,]$Temp
y.temp = filter(zak.temp.df, Year == 1997)$Temp

zak97.zak12.lm = lm(y.temp ~ x.temp)

start = as.Date("01/01/1997", format = "%m/%d/%Y")
full.1997 <- seq(start, by='1 day', length=365)
partial = filter(zak.temp.df, Year == 1997)
#with(partial, Temp[match(full.2011, Date)])

compl.1997 = data.frame(Date=full.1997, 
                        Temp=with(partial, Temp[match(full.1997, Date)]))

fill.1997 = filter(compl.1997, is.na(Temp))


var.1997 = filter(zak.temp.df, 
                  Year == 2012 & !(month(Date) == 2 & day(Date) == 29))[-incl.d,]
###### up to the 202nd day we have data from 1999


fill.1997$Temp = predict(zak97.zak12.lm, 
                         data.frame(x.temp = var.1997$Temp))

fill.1997$Year = year(fill.1997$Date)

fill.1997$Month = month(fill.1997$Date)

fill.1997$Stream = "Zak"

fill.1997$Calc = "Zak2012"


######

zak.temp.df = rbind(zak.temp.df,fill.1997)
zak.temp.df = arrange(zak.temp.df,Date)

#### we miss 3 days at the end of 2014

zak.last = matrix(0,3,6)
zak.last = as.data.frame(zak.last)
colnames(zak.last) = colnames(zak.temp.df)
start = as.Date("06/17/2014", format = "%m/%d/%Y")
zak.last$Date = seq(start, by='1 day', length=3)
zak.last$Temp = rep(zak.temp.df$Temp[nrow(zak.temp.df)],nrow(zak.last))
zak.last$Year = 2014
zak.last$Month = 6
zak.last$Stream = "Zak"
zak.last$Calc = "Same_as_b"
zak.temp.df = rbind(zak.temp.df,zak.last)

#### we miss x days at the ned of June/beginning of July 1996

zak.first = matrix(0,8,6)
zak.first = as.data.frame(zak.first)
colnames(zak.first) = colnames(zak.temp.df)
start = as.Date("06/26/1996", format = "%m/%d/%Y")
zak.first$Date = seq(start, by='1 day', length=8)
zak.first$Temp = rep(zak.temp.df$Temp[1],nrow(zak.first))
zak.first$Year = 1996
zak.first$Month = c(rep(6,5),rep(7,3))
zak.first$Stream = "Zak"
zak.first$Calc = "Same_as_a"
zak.temp.df = rbind(zak.first,zak.temp.df)
################

###### Gacnick with itself for 1999

years.comp = 2000:2013
cor.gatt = rep(0,length(years.comp))
temp.tar = filter(gat.temp.df, Year == 1999)
start = as.Date("01/01/1999", format = "%m/%d/%Y")
full.1999 <- seq(start, by='1 day', length=365)
incl.d = which(full.1999 %in% temp.tar$Date)
for (i in 1:length(years.comp)) {
  
  temp.var = filter(gat.temp.df, 
                    Year == years.comp[i] & !(month(Date) == 2 & day(Date) == 29))
  
  temp.var = temp.var[incl.d,] # I have the first 202 days in 1999
  
  cor.gatt[i] = as.numeric(cor.test(temp.tar$Temp, temp.var$Temp)$estimate)
  
  
}

best.cor.year = years.comp[which(cor.gatt == max(cor.gatt))]
#### it is 2005. I then use the 2005 to predict 1999

###

x.temp = filter(gat.temp.df, 
                Year == 2010 & 
                  !(month(Date) == 2 & day(Date) == 29))[incl.d,]$Temp
y.temp = filter(gat.temp.df, Year == 1999)$Temp

gat97.gat12.lm = lm(y.temp ~ x.temp)

start = as.Date("01/01/1999", format = "%m/%d/%Y")
full.1999 <- seq(start, by='1 day', length=365)
partial = filter(gat.temp.df, Year == 1999)
#with(partial, Temp[match(full.2011, Date)])

compl.1999 = data.frame(Date=full.1999, 
                        Temp=with(partial, Temp[match(full.1999, Date)]))

fill.1999 = filter(compl.1999, is.na(Temp))


var.1999 = filter(gat.temp.df, 
                  Year == 2005 & !(month(Date) == 2 & day(Date) == 29))[-incl.d,]



fill.1999$Temp = predict(gat97.gat12.lm, 
                         data.frame(x.temp = var.1999$Temp))

fill.1999$Year = year(fill.1999$Date)

fill.1999$Month = month(fill.1999$Date)

fill.1999$Stream = "Gac"

fill.1999$Calc = "Gac2005"

##########
gat.temp.df = rbind(gat.temp.df,fill.1999)
gat.temp.df = arrange(gat.temp.df,Date)

#########

#### we miss 3 days at the end of 2014

gat.last = matrix(0,3,6)
gat.last = as.data.frame(gat.last)
colnames(gat.last) = colnames(gat.temp.df)
start = as.Date("06/18/2014", format = "%m/%d/%Y")
gat.last$Date = seq(start, by='1 day', length=3)
gat.last$Temp = rep(gat.temp.df$Temp[nrow(gat.temp.df)],nrow(gat.last))
gat.last$Year = 2014
gat.last$Month = 6
gat.last$Stream = "Gac"
gat.last$Calc = "Same_as_b"
gat.temp.df = rbind(gat.temp.df,gat.last)

#########

temp.all.df = 
  rbind(zak.temp.df, gat.temp.df, sve.temp.df, stu.temp.df, loidri.temp.df, uppidri.temp.df,
        trebu.temp.df, zadla.temp.df, lipo.temp.df,huda.temp.df)

temp.all.aggr = aggregate(Temp ~ Stream + Year, data = temp.all.df, FUN = length)
temp.all.aggr = arrange(temp.all.aggr, Stream)

######## Degree-days

t_zero = 5
temp.all.df$degree_days = temp.all.df$Temp-t_zero
temp.all.df$degree_days = ifelse(temp.all.df$degree_days<0,
                                 0,temp.all.df$degree_days)

temp.all.df$Sampling_Season =
  ifelse(temp.all.df$Month %in% c(6,7,8), "Summer","Winter")

temp.all.aggr.dd = 
  aggregate(degree_days ~ Stream + Sampling_Season + Year,
            data = temp.all.df, FUN = sum)

temp.all.aggr.dd = dplyr::arrange(temp.all.aggr.dd, Stream, Year,
                                  Sampling_Season)

temp.all.aggr.dd.1 = aggregate(degree_days ~ Stream + Sampling_Season,
                               data = temp.all.aggr.dd, FUN = mean)

temp.all.mean.2006.aggr = aggregate(Temp ~ Stream, 
                                    data = aggregate(Temp ~ Stream + Year, data = filter(temp.all.df, Year %in% (2006:2013)), 
                                                     FUN = mean), FUN = mean)

temp.all.mean.2006.aggr$sd = aggregate(Temp ~ Stream, 
                                       data = aggregate(Temp ~ Stream + Year, data = filter(temp.all.df, Year %in% (2006:2013)),
                                                        FUN = mean), FUN = sd)$Temp

temp.all.mean.2006.aggr$Alt = c(916,583,544,461,509,601,538,719,356,705)

temp.all.mean.2006.aggr$Stream = factor(temp.all.mean.2006.aggr$Stream,levels = c("Huda","Stu","UIdri","LIdri","Lipo","Sve",
                                                                           "Trebu","Zadla","Gac","Zak"))
####################################################


temp.all.dd.2006.aggr = aggregate(degree_days ~ Stream, 
                                    data = aggregate(degree_days ~ Stream + Year, data = filter(temp.all.df, Year %in% (2006:2013)), 
                                                     FUN = sum), FUN = mean)

temp.all.dd.2006.aggr$sd = aggregate(degree_days ~ Stream, 
                                       data = aggregate(degree_days ~ Stream + Year, data = filter(temp.all.df, Year %in% (2006:2013)),
                                                        FUN = sum), FUN = sd)$degree_days


temp.all.dd.2006.aggr$Stream = factor(temp.all.mean.2006.aggr$Stream,levels = c("Huda","Stu","UIdri","LIdri","Lipo","Sve",
                                                                                  "Trebu","Zadla","Gac","Zak"))

temp.all.dd.2006.aggr$Alt = c(916,583,544,461,509,601,538,719,356,705)

with(temp.all.dd.2006.aggr, cor.test(degree_days,Alt))

cor.test(temp.all.dd.2006.aggr$degree_days,temp.all.mean.2006.aggr$Temp)
