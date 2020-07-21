source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: A different way to arrange x-axis values than using
## factor(levels=)
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  scale_x_discrete(limits=c("North", "East", "South", "West")) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 2: using the Fill function to add windspeed level
##can set the order of the factor levels there and then (fill=factor)
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)",
       fill = "Wind Speeds");
plot(thePlot);

### Part 3: Adding color using rgb()
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  scale_fill_manual(values = c(rgb(red=1, green=1, blue=0),        # low: yellow
                               rgb(red=1, green=0.2, blue=0),      # medium: red
                               rgb(red=0.5, green=0, blue=0.8))) + # high: purple
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)",
       fill = "Wind Speeds"); # changes the legend (fill) title
plot(thePlot);

### Part 4: Using facets along the y-axis
##so wind direction for every wind speed (high low medium facets)
##setting up order of wind direction using scale x discrete
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=windSpeedLevel ~ .) + # facet in vertical direction
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 5: same thing but facets along yaxis (stacked one next to the other)
### NOTE HOW they used factor in facet grid to order the facets
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High"))) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 6: Filling and coloring boxplots
##using color= fill= in geom boxplot
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               color="blue",   # outline color
               fill="red") +   # fill color
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High"))) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 7: Filling and coloring facets
##TREATING EVERY Individual boxplot as independent of the rest
##have to create a 12-item vector of color and fill colors
##boxplots are colored in order; NA=blank/clear
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               color=c("blue", rep("black", 3), 
                       "green", rep("black", 3), 
                       "orange", rep("black", 3)),
               fill=c(rep(NA, 8), rep("red", 3), NA)) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High"))) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

##### Part 8: Giving the facets a different name using labeller function
##under the facet grid parameter
windLabels = c(Low = "Light Winds",
               Medium = "Medium Winds",
               High = "Strong Winds");

thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               color=c("blue", rep("black", 3),
                       "green", rep("black", 3),
                       "orange", rep("black", 3)),
               fill=c(rep(NA, 8), rep("red", 3), NA)) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High")),
             labeller=as_labeller(windLabels)) +  ### HERE
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot)