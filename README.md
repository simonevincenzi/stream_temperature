
#### stream_temperature

#### Correlation between stream temperatures in Slovenian streams in which marble trout live 


##### 1. Marble trout and Western Slovenian streams

Marble trout is a freshwater resident salmonid endemic in the Adriatic basin of Slovenia. Whether there are still pure marble trout populations living in the Po river system (Northern Italy) is subject of current research. Marble trout live in streams with mean summer temperature below 15°C and winter temperature ranging from 0 to 5 °C. Marble trout spawn in November-December and offspring emerge in May-June. 
The Marble Trout Conservation Program started in 1993 in the upper reaches of the Soca River basin and its tributaries - the Idrijca and Baca Rivers - in Western Slovenia. Eight pure marble trout populations, all isolated and separated from the downstream hybrid marble-brown trout zone by impassable waterfalls, live in headwater streams in the basins of Soca, Baca, and Idrijca Rivers: Huda [Huda], Lower Idrijca [LIdri], Upper Idrijca [UIdri], Lipovesck [Lipo], Studenc [Stu], Svenica [Sve], Zadlascica [Zadla], Trebuscica [Trebu].

##### 2. Analyses

For some of the analyses I intended to carry out (temperature-dependent survival and growth), it was necessary to have complete temperature records for all streams since the start of the sampling. Start by sourcing the file Temp.r.


```{r}
source("Temp.r")
```


The output temp.all.df (along with the production of a ten-panel plot with stream-specific monthly temparature boxplots) is a dataset with column Date, Temp (-- mean temp --), Year, Month, Stream, Calc (Meas = temperature has been recorded). Then, I tested the correlation between stream temperatures between pair of streams (one is the target - the one with missing data -- and the other is the tested) and I used the temperature data of the tested stream with highest correlation with the target stream to impute the missing data.

```{r}
Temp.tb = Temp.corr.f(temp.all.df)
```

The Temp.tb data.frame has target stream (tar), tested stream (var), correlation between stream temperature of the two streams (cor), 
years with common number of days with temperature recorded (common.years), years with missing data for the target stream (miss.years), years with missing data for the target stream, but with complete data for the tested stream (miss.in.var).
The correlation between water temperature of streams are typically very high (mean correlation[sd] = 0.97[0.01]).

Then, in each stream I impute the missing data using the data from the tested stream with the highest correlation with the target stream and best model (linear or non-linear - gam -, chosen according to best prediction).

```{r}
source("Temp.filling.r")
```

The output is the data frame `temp.all.df` with columns: Date, Temp (mean temperature of the day), Year, Month, Stream, 
Calc (Meas = temperature recorded in the stream, Gac2005 = in one year (1997) we had missing data for Gac and the only acceptable data for imputing was coming from Gac in 2005, Same_as_a = Same temperature as days after, Same_as_b = Same temperature as days before, Zak2012 = in one year (1997) we had missing data for Zak and the only acceptable data for imputing was coming from Zak in 2012, Stream_name = stream whose temperature data was used to impute missing data, degree_days = degree days for the day using 5C as base temperature, Sampling_Season = Summer for June, July, September - Winter for the rest of the year).   
