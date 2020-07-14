#application of lesson 6

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv",
                        stringsAsFactors = FALSE )

#Create a vector that holds the quantile values for the 
#20, 40, 60, and 80th percentile of stnPressure
press_quant = quantile(weatherData$stnPressure, probs=c(0.20, 0.40, 0.60, 0.80 ))

#Create a new column called pressureLevel that creates 
# 5 evenly spaced levels for stnPressure
for(day in 1:nrow(weatherData))  
{
  if(weatherData$stnPressure[day] <= press_quant[1])
  {
    weatherData$press_lev[day] = "Low";
  }
  else if(weatherData$stnPressure[day] <= press_quant[2])
  {
    weatherData$press_lev[day] = "Medium";
  }
  else if(weatherData$stnPressure[day] <= press_quant[3])
  {
    weatherData$press_lev[day] = "Medium High";
  }
  else if(weatherData$stnPressure[day] <= press_quant[4])
  {
    weatherData$press_lev[day] = "High";
  }
  else # something went wrong... always good to check
  {
    weatherData$press_lev[day] = "Very High";
  }
}

#organize the levels of this new pressure factor
weatherData$press_lev=factor(weatherData$press_lev,
                           levels=c("Low", "Medium", "Medium High", "High","Very High"));

#Make a boxplot of windSusSpeed vs pressureLevel
pressplot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=press_lev, y=windSusSpeed)) +
  theme_bw() +
  labs(title = "Wind Speed by Pressure",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure",
       y = "Wind Speed (mph)")+
  theme(axis.title.x=element_text(size=14, color="black",face="bold"),
        axis.title.y=element_text(size=14, color="black",face="bold"), 
        plot.title=element_text(size=16),
        plot.subtitle=element_text(size=12,  
                                   color ="darkblue"),
        axis.text = element_text(family="sans", size=12));

plot(pressplot);

#Find the dates for the three outliers at the lowest pressure level and
#label them with their dates on the plot
outval=weatherData[weatherData$windSusSpeed>40,];

pressplotB = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=press_lev, y=windSusSpeed)) +
  theme_bw() +
  labs(title = "Wind Speed by Pressure",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure",
       y = "Wind Speed (mph)")+
  geom_text(data = outval, aes(x = 1, y = windSusSpeed, 
                               label = dateYr), nudge_x = 0.5)+
  theme(axis.title.x=element_text(size=14, color="black",face="bold"),
        axis.title.y=element_text(size=14, color="black",face="bold"), 
        plot.title=element_text(size=16),
        plot.subtitle=element_text(size=12,  
                                   color ="darkblue"),
        axis.text = element_text(family="sans", size=12));

plot(pressplotB)

###for annotating, could have also used annotate, like in the lesson
#annotate(geom="text", 
         #x=1,
         #y=20, 
         #color="blue",
         #label=paste("median:", northMed) ) 




