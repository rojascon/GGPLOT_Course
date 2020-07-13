source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

#calculate 30th and 70th quantile of wind speed
#this is a 2-part vector
windSpeedQuant = quantile(weatherData$windSpeed, probs=c(.30, .70))

#categorize wind speed values into low medium high
#comparing windspeed column to the 2 values from above and creating a new colum called 
#windSpeedLevel

for(day in 1:nrow(weatherData))  
{
  # if the value in windSpeed is less than or equal to 30th quantile
  if(weatherData$windSpeed[day] <= windSpeedQuant[1])
  {
    weatherData$windSpeedLevel[day] = "Low";
  }
  # if the value in windSpeed is greater than or equal to 70th quantile
  else if(weatherData$windSpeed[day] >= windSpeedQuant[2]) 
  {
    weatherData$windSpeedLevel[day] = "High";
  }
  else # the value in windSpeed is in between those 2 quantiles
  {
    weatherData$windSpeedLevel[day] = "Medium";
  }
}
  
#convert direction (0-360 degrees) to cardinal direction (N, S, W,E)
#creatinge new column called windDir
for(day in 1:nrow(weatherData))
{
  # if the direction is greater than 315 OR less than 45 degrees
  if(weatherData$windSusDir[day] >= 315 || 
     weatherData$windSusDir[day] < 45)
  {
    weatherData$windDir[day] = "North";
  }
  # if the direction is greater than 45 AND less than 135 degrees
  else if(weatherData$windSusDir[day] >= 45 && 
          weatherData$windSusDir[day] < 135)
  {
    weatherData$windDir[day] = "East";
  }
  # if the direction is greater than 135 AND less than 225 degrees
  else if(weatherData$windSusDir[day] >= 135 && 
          weatherData$windSusDir[day] < 225)
  {
    weatherData$windDir[day] = "South";
  }
  else # the direction is between 225 and 315 degrees
  {
    weatherData$windDir[day] = "West";
  }
}

#calculate current days high T- previous day high T
#first value should be NA
#createing column called changeMaxTemp
for(day in 1:nrow(weatherData))
{
  if( day == 1)  # no day before the first day
  {
    weatherData$changeMaxTemp[day] = NA; 
  }
  else  # subtract previous day's maxTemp from current day's maxTemp
  {
    weatherData$changeMaxTemp[day] = weatherData$maxTemp[day] -
      weatherData$maxTemp[day-1];
  }
}

#save modified data frame
write.csv(weatherData, file="data/LansingNOAA2016-3.csv");

### Re-order the directions on the x-axis using factor(s)
weatherData$windDir=factor(weatherData$windDir,
                     levels=c("North", "East", "South", "West"));

#boxplot of changeMaxTemp vs windDir
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp)) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

#violin plot ofchangeMaxTemp vs windDir
#violin plots show distributions instead of quantiles
#na.rm=TRUE to ignaore NA values
thePlot = ggplot(data=weatherData) +
  geom_violin(mapping=aes(x=windDir, y=changeMaxTemp),
              na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

#add lines to whiskers of boxplot
#have to add both geom boxplot and stat boxplot
#stat boxplot asks for "geom" and "width" parameters
thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), 
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

#modify how outlier values look on boxplot
#red, 60% opaque (40% transparent), 4mm tall, and shaped like an "@".
thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               outlier.shape = "@", 
               outlier.color = "red",
               outlier.alpha = 0.6, 
               outlier.size = 4 ) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot)

#annotate plot with median values for North and South directions only
northVals=which(weatherData$windDir == "North");
southVals=which(weatherData$windDir == "South");

weatherData[northVals, "changeMaxTemp"]
weatherData[southVals, "changeMaxTemp"]

northMed = median(weatherData[northVals,"changeMaxTemp"], na.rm=TRUE);
southMed = median(weatherData[southVals,"changeMaxTemp"], na.rm=TRUE)

#alternative calculate 2 median values and save to a vector
A=median(weatherData$changeMaxTemp[weatherData$windDir=="North"])
B=median(weatherData$changeMaxTemp[weatherData$windDir=="South"])

#modify the boxplot to add the ANNOTATE parameter
#TEXT=we are adding text
#x= over which boxplot to add text to
#y= correlates with y-axis values
#label= actual value that will be pasted
thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), 
               na.rm=TRUE,
               outlier.shape = "@",
               outlier.color = "red",
               outlier.alpha = 0.6,
               outlier.size = 4 ) +
  annotate(geom="text", # North median
           x=1,
           y=20, 
           color="blue",
           label=paste("median:", northMed) ) +
  annotate(geom="text",  # South median
           x=3, 
           y=-10, 
           color="red",
           label=paste("median:", southMed) ) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot)


#condensing multiple for loops
for(day in 1:nrow(weatherData))
{
  ## Adding a column that gives relative wind speed for the day
  # Winds less than 6.4 miles/hour -- label as "Low"
  if(weatherData[day,"windSpeed"] <= windSpeedQuant[1])
  {
    weatherData[day,"windSpeedLevel"] = "Low";
  }
  # Winds greater than 10.2 miles/hour -- label as "High"
  else if(weatherData[day,"windSpeed"] >= windSpeedQuant[2])
  {
    weatherData[day,"windSpeedLevel"] = "High";
  }
  else # wind speeds between 6.4 and 10.2 miles/hour -- label as "Medium"
  {
    weatherData[day,"windSpeedLevel"] = "Medium";
  }
  
  ## Adding a column that gives the cardinal wind direction
  # if the direction is greater than 315 OR less than 45 degrees
  if(weatherData[day,"windSusDir"] >= 315 ||
     weatherData[day,"windSusDir"] < 45)
  {
    weatherData[day,"windDir"] = "North";
  }
  # if the direction is greater than 45 AND less than 135 degrees
  else if(weatherData[day,"windSusDir"] >= 45 &&
          weatherData[day,"windSusDir"] < 135)
  {
    weatherData[day,"windDir"] = "East";
  }
  # if the direction is greater than 135 AND less than 225 degrees
  else if(weatherData[day,"windSusDir"] >= 135 &&
          weatherData[day,"windSusDir"] < 225)
  {
    weatherData[day,"windDir"] = "South";
  }
  else # the directions is between 225 and 315 degrees
  {
    weatherData[day,"windDir"] = "West";
  }
  ### Adding a changeMaxTemp column
  if(day == 1)
  {
    weatherData[day,"changeMaxTemp"] = NA;
  }
  else
  {
    weatherData[day,"changeMaxTemp"] = weatherData[day,"maxTemp"] -
      weatherData[day-1,"maxTemp"];
  }
}



