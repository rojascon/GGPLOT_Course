#application of lesson 09
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );
weatherData$precipNum=weatherData$precip
weatherData$precipNum[weatherData$precipNum == "T"] = 0.005;


#### Part 4: grep to find date indexes to inform color gradient legend
springIndex = grep(weatherData$date, pattern="3-21");
summerIndex = grep(weatherData$date, pattern="6-21");
fallIndex = grep(weatherData$date, pattern="9-21");
winterIndex = grep(weatherData$date, pattern="12-21");

#Create a text plot of Precipitation vs Humidity
# Label the points with avgTemp
# Color the text points using a gradient that goes from blue (coldest temp) to darkgreen (middle), to red (hottest)
# Change the y-axis to a logarithmic scale: scale_y_continuous(trans="log10")
# Move the legend into the plot area making sure not to cover any points

preciphum = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=relHum, y=as.numeric(precipNum), color=1:nrow(weatherData),
                        label=avgTemp), size=4) +
  scale_y_continuous(trans="log10")+
  scale_color_gradientn(colors=c("red","darkgreen","blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(25, units="pt"),  # height
        legend.key.width = unit(20, units="pt"),   # width
        legend.direction = "vertical",           # alignment
        legend.position = c(0.15,0.75),         # position
        axis.title.x=element_text(size=14, color="black",face="bold"),
        axis.title.y=element_text(size=14, color="black",face="bold"), 
        plot.title=element_text(size=16),
        plot.subtitle=element_text(size=12,  
                                   color ="darkblue"),
        axis.text = element_text(family="sans", size=12),
        legend.text =element_text(family="sans", size=10),
        legend.title=element_text(family="sans", size=12, face="bold"))+
  
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity",
       y = "Precipitation (log)",
       color = "Average Temperature");
plot(preciphum);