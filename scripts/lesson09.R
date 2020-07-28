source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: Create a Humidity vs. Temperature scatterplot
##no background gridlines
thePlot = ggplot(data=weatherData) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 2: Use dates instead of points
##using geom_text and the label subcomponent
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, label=date)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 3: Reformat the dates
##color the dates and change their font size
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, label=date),
            color="darkgreen", 
            size=2.5) +   # change size
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(thePlot);

#### Part 4: grep to find date indexes to inform color gradient legend
springIndex = grep(weatherData$date, pattern="3-21");
summerIndex = grep(weatherData$date, pattern="6-21");
fallIndex = grep(weatherData$date, pattern="9-21");
winterIndex = grep(weatherData$date, pattern="12-21");

#### Part 6:Plotting dates and color coding them using a gradient
## geom text (color=)
##scale_color_gradientn (colors, breaks, and labels for those breaks
##values refers to where the colors start and end (how much slice for each color)
##height of legend (legend.key.height)
#3 labs (color=) name of color gradient legend
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","brown","red","green","blue"),
                        values=c(0, 0.2, 0.55, 0.85, 1),
                        breaks=c(winterIndex, springIndex,
                                 summerIndex, fallIndex),
                        labels=c("winter","spring","summer","fall")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(25, units="pt")) +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity",
       color = "Dates");  
plot(thePlot);

#### Part 10: Legend modifications
#legend.key.height
#legend.key.width
#legend.direction
#legend.position (x,y coords)
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=avgTemp, y=relHum, color=1:nrow(weatherData),
                        label=date),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","brown","red","green","blue"),
                        values=c(0, 0.2, 0.55, 0.85, 1),
                        breaks=c(winterIndex, springIndex,
                                 summerIndex, fallIndex),
                        labels=c("winter","spring","summer","fall")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(15, units="pt"),  # height
        legend.key.width = unit(40, units="pt"),   # width
        legend.direction = "horizontal",           # alignment
        legend.position = c(0.25, 0.08)) +         # position
  
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity",
       color = "");  
plot(thePlot);