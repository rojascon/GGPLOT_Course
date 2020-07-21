#application of lesson 7

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv",
                        stringsAsFactors = FALSE );

#Create a vector that holds the quantile values for the 30th and 70th 
#percentile of relHumidity
hum_quant = quantile(weatherData$relHum, probs=c(0.30, 0.70));

#Using the quantile values, create a new column called humidityLevel that creates 
#three levels for relHumidity
###  <63 vs. 63-75 vs. >75

###METHOD 1: for loop
for(i in 1:nrow(weatherData))  
{
  # if the value in windSpeed is less than or equal to 30th quantile
  if(weatherData$relHum[i] <= hum_quant[1])
  {
    weatherData$humLevel[i] = "Low";
  }
  # if the value in windSpeed is greater than or equal to 70th quantile
  else if(weatherData$windSpeed[i] >= hum_quant[2]) 
  {
    weatherData$humLevel[i] = "High";
  }
  else # the value in windSpeed is in between those 2 quantiles
  {
    weatherData$humLevel[i] = "Medium";
  }
};

###Method 2: using empty vector to start filling out
###[values that result do not exactly match method 1's]
weatherData$humlevel=c();

weatherData$humlevel[
  weatherData$relHum<=hum_quant[1]
  ]="Low";

weatherData$humlevel[
  weatherData$relHum>=hum_quant[2]
  ]="High";

weatherData$humlevel[
  is.na(weatherData$humlevel)
  ]="Medium";

#label for facet grid labels
windLabels = c(Low = "Light Winds",
               Medium = "Medium Winds",
               High = "Strong Winds");

#vector of my chosen colors
mycol=c("#66c2a5","#ffd92f","#8da0cb","#e78ac3");

###Create a boxplot of stnPressure vs humidityLevel
#humidity factor in order
humplot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=humlevel, y=stnPressure, 
                           fill=factor(windDir, levels=c("North", "East", "South","West"))),
               na.rm=TRUE) +
  facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High")),
                             labeller=as_labeller(windLabels)) +
  theme_bw() +
  scale_x_discrete(limits = c("Low", "Medium", "High")) +
  scale_fill_manual(values=mycol)+
  labs(title = "Change in Pressure vs. Relative Humidity",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity Levels",
       y = "Pressure",
       fill="Wind Direction");

plot(humplot)
