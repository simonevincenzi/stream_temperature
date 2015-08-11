# table(huda.temp.df$Year)
# table(loidri.temp.df$Year)
# table(zak.temp.df$Year)
# 
# cor.test(filter(huda.temp.df, Year == 2010)$Temp,
#          filter(loidri.temp.df, Year == 2010)$Temp)
# 
# cor.test(filter(huda.temp.df, Year == 2011)$Temp,
#          filter(loidri.temp.df, Year == 2011)$Temp)
# 
# plot(filter(huda.temp.df, Year == 2011)$Temp ~ filter(loidri.temp.df, Year == 2011)$Temp)
# 
# summary(lm(filter(huda.temp.df, Year == 2011)$Temp ~ filter(loidri.temp.df, Year == 2011)$Temp))
# 
# summary(lm(filter(huda.temp.df, Year == 2013)$Temp ~ filter(zak.temp.df, Year == 2013)$Temp))

#### join together the temperature data

# temp.all.df = 
#   rbind(zak.temp.df, gat.temp.df, sve.temp.df, stu.temp.df, loidri.temp.df, uppidri.temp.df,
#         trebu.temp.df, zadla.temp.df, lipo.temp.df,huda.temp.df)
#temp.all.aggr = aggregate(Temp ~ Stream + Year, data = temp.all.df, FUN = length)

library(dplyr)
library(lubridate)

Temp.corr.f = function(temp.all.df) {
  
  df.col = rep(0,1000)
  temp.corr.df = data.frame(tar = df.col, var = df.col, 
                            cor = df.col, years.cor = df.col,
                            common.years = df.col,
                            miss.years = df.col, miss.in.var = df.col)
  
  aa = 1
  unique.stream = unique(temp.all.df$Stream)
  
  for (i in 1:length(unique.stream)) {
  
    day.tab.tar = as.data.frame(table(filter(temp.all.df, Stream == unique.stream[i])$Year))
  colnames(day.tab.tar)[1] = "Year"
  day.tab.tar$Year = as.numeric(as.character(day.tab.tar$Year))
  miss.years = filter(
    day.tab.tar, Freq < 300)$Year
  day.tab.tar.365 = filter(
    day.tab.tar, Freq == 365)

  for (j in 1:length(unique.stream)) {
    
    if (j!=i) {
      
    day.tab.var = as.data.frame(table(filter(temp.all.df, Stream == unique.stream[j])$Year))
     
     colnames(day.tab.var)[1] = "Year"
     day.tab.var$Year = as.numeric(as.character(day.tab.var$Year))
    day.tab.var.365 = filter(day.tab.var, Freq == 365)  
    day.tab.var.300 = filter(day.tab.var, Freq > 300)
    common.years = day.tab.tar.365$Year[which(day.tab.tar.365$Year %in% day.tab.var.365$Year)]
    miss.common.years = miss.years[miss.years %in% day.tab.var.300$Year] 
     temp.tar = filter(temp.all.df, Stream == unique.stream[i] & Year %in% common.years)
     temp.var = filter(temp.all.df, Stream == unique.stream[j] & Year %in% common.years)
     
    
     temp.corr.df$tar[aa] = unique.stream[i]
     temp.corr.df$var[aa] = unique.stream[j]
     temp.corr.df$cor[aa] = as.numeric(cor.test(temp.tar$Temp, temp.var$Temp)$estimate)
     temp.corr.df$years.cor[aa] = length(common.years)
    temp.corr.df$common.years[aa] = paste(common.years, collapse ="-")
    temp.corr.df$miss.years[aa] = paste(miss.years, collapse ="-")
    temp.corr.df$miss.in.var[aa] = paste(miss.common.years, collapse ="-")
      
    aa = aa + 1  
    }
    
  }
  
  }
  temp.corr.df =  temp.corr.df[1:(aa-1),]
  return(temp.corr.df)  
  }




