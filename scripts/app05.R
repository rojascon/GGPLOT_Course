#application of lesson 5

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv",
                        stringsAsFactors = FALSE );

#generate histogram of average humidity (relative humidity??)
hum = ggplot( data=weatherData ) + 
  geom_histogram(mapping=aes(x=relHum, y=..count..),  #proportions
                 bins=20,             #how many items in each bin
                 color="black",
                 fill="#c994c7") +
  geom_vline(mapping=aes(xintercept=mean(relHum)),
             color="blue",
             size=1.2) +
  theme_classic() +
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity (%)",
       y = "Counts");
plot(hum);

#Create a column in your data frame called biMonth that divides the year 
#into 6 categories: JanFeb, MarApr, MayJun, JulAug, SepOct, and NovDec
weatherData$month=as.numeric(format(as.Date(weatherData$dateYr),"%m"))
month=weatherData$month
biMonth = vector(mode="character", length=nrow(weatherData))

for(i in 1:length(weatherData$month)) # go through each date
{
  if(month[i]==01 | month[i]==02)
  {
    biMonth[i] = "JanFeb";
  }
  else if(month[i]==3 | month[i]==4)
  {
    biMonth[i] = "MarApr";
  }
  else if(month[i]==5 | month[i]==6)
  {
    biMonth[i] = "MayJun";
  }
  else if(month[i]==7 | month[i]==8)
  {
    biMonth[i] = "JulAug";
  }
  else if(month[i]==9 | month[i]==10)
  {
    biMonth[i] = "SepOct";
  }
  else if(month[i]==11 | month[i]==12)
  {
    biMonth[i] = "NovDec";
  }
 else # something went wrong... always good to check
  {
    biMonth[i] = "Error";
  }
}

#add the two month bins to data frame as a column
weatherData$biMonth = biMonth
weatherData$biMonth=factor(weatherData$biMonth, levels=c("JanFeb","MarApr",
                                                            "MayJun","JulAug",
                                                            "SepOct","NovDec"));
                                                        
#create a histogram for every bimonth category
hum = ggplot( data=weatherData ) + 
  geom_histogram(mapping=aes(x=relHum, y=..count..),  #proportions
                 bins=15,             #how many items in each bin
                 color="black",
                 fill="#c994c7") +
  facet_grid( facet=biMonth~.)+
  scale_x_continuous(breaks=ceiling(seq(from=38, to=96, length.out=10)))+
  theme_classic() +
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity (%)",
       y = "Counts");

plot(hum);

#### Create a stacked histogram for each biMonth category
hist_col=c("#1b9e77", "#d95f02", "darkorchid2", "#e7298a","#1f78b4","#fbb4ae") 
humsta= ggplot(data=weatherData) + 
  geom_histogram(mapping=aes(x=relHum, y=..count.., fill=biMonth),
                 bins=40,
                 color="grey20",
                 position="stack") +
  geom_vline(mapping=aes(xintercept=mean(relHum[biMonth=="JanFeb"])),
             color="blue",
             size=1.2) +
  geom_vline(mapping=aes(xintercept=mean(relHum[biMonth=="JulAug"])),
             color="black",
             size=1.2) +
  theme_classic() +
  theme(legend.position="bottom")+
  scale_fill_manual(values = hist_col)+
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity (%)",
       y = "Count",
       fill="Month Category")+
  guides(fill=guide_legend(nrow=1))

plot(humsta)