source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: Convert trace rain amounts 'T' to the numeric value 0.005
# Copy precip values to a new column, precipNum
weatherData$precipNum = weatherData$precip;

# Go through all rows in weatherData
for(i in 1:nrow(weatherData))
{
  # check precipNum value -- if the value is T, change to 0.05
  if(weatherData$precipNum[i] == "T")
  {
    weatherData$precipNum[i] = 0.005;
  }
}

##ALTERNATIVE METHOD: vector operations
weatherData$precipNum[weatherData$precip == "T"] = 0.005;

# convert precipNum column to numeric
weatherData$precipNum = as.numeric(weatherData$precipNum);

#### Part 2: Plot the precipitation for each season using geom_col
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=season, y=precipNum), 
           width=0.7) +
  theme_bw() +
  labs(title = "Seasonal precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Seasons",
       y = "Precipitation (inches)");
plot(thePlot);

#### Part 3: extract month from date
weatherData$month = format.Date(weatherData$dateYr, format="%b");

#### Part 4: Plot precipitation for each month using geom_col
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=precipNum),
           width=0.7) +
  theme_bw() +
  labs(title = "Monthly precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Precipitation (inches)");
plot(thePlot);

#### Part 5: Reorder the months on the x-axis using scale x discrete
##R already has a vector of months in order titled month.abb

thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=precipNum),
           width=0.4) +
  scale_x_discrete(limits = month.abb) +
  theme_bw() +
  labs(title = "Monthly precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Precipitation (inches)");
plot(thePlot);

#### Part 6: Add the monthly average as a horizontal line
##parameters are under geom_hline
monthlyAvg = sum(weatherData$precipNum)/12;

thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=precipNum),
           width=0.5) +
  scale_x_discrete(limits = month.abb) +
  geom_hline(mapping = aes( yintercept = monthlyAvg ), 
             color="red",  
             size=1.5, 
             linetype=2) +
  theme_bw() +
  labs(title = "Monthly precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Precipitation (inches)");
plot(thePlot);

#### Part 7: Color each bar by pressure amount using fill. Color is a gradient of blue.
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=precipNum, fill=stnPressure),
           width=0.5) +
  scale_x_discrete(limits = month.abb) +
  geom_hline(mapping = aes( yintercept= sum(precipNum)/12 ),
             color="red",
             size=2,
             linetype=2) +
  theme_bw() +
  labs(title = "Monthly precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Precipitation (inches)");
plot(thePlot);

#### Part 8: Reorder the data so that the fill values are in an increasing fashion not scattered
##changed legend title with labs fill=
reorderedData = weatherData[order(weatherData$stnPressure),];

thePlot = ggplot(data=reorderedData) +
  geom_col(mapping=aes(x=month, y=precipNum, fill=stnPressure),
           width=0.5) +
  scale_x_discrete(limits = month.abb) +
  geom_hline(mapping = aes( yintercept= sum(precipNum)/12 ),
             color="red",
             size=1.5,
             linetype=2) +
  theme_bw() +
  labs(title = "Monthly precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Precipitation (inches)",
       fill= "Pressure");
plot(thePlot);

#### Part 9: Change fill color using scale fill gradient and legend title using labs fill
reorderedData = weatherData[order(weatherData$stnPressure),];
thePlot = ggplot(data=reorderedData) +
  geom_col(mapping=aes(x=month, y=precipNum, fill=stnPressure),
           width=0.4) +
  scale_x_discrete(limits = month.abb) +
  geom_hline(mapping = aes( yintercept= sum(precipNum)/12 ),
             color= rgb(red=0, green=0.5, blue=0),
             size=2,
             linetype=2) +
  scale_fill_gradient(low = rgb(red=1, green=0.5, blue=0),   # orange
                      
                      high = rgb(red=0, green=0, blue=1)) +  # blue
  theme_bw() +
  labs(title = "Monthly precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Precipitation (inches)",
       fill = "Pressure");
plot(thePlot);

##extension: move boxplots to BEFORE X-AXIS TICK MARK
# Note: after_stat() does not have its own parameters -- the parameters are for the 
#caller (in this case, position_nudge()).  
#after_stat() is delaying the call and passing the parameter into position_nudge() 
#after all statistics have been completed
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=heatDays),
           fill = "red",
           width=0.6,
           position=position_nudge(after_stat(x=-0.3))) +
  scale_x_discrete(limits = month.abb) +
  theme_bw() +
  labs(title = "Heating and Cooling Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Cumulative Degrees (F)");
plot(thePlot);