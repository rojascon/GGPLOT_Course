source(file="scripts/reference.R") # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE)

#### Part 1: Plot humidity vs temperature ####
plotData = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=avgTemp, y=relHum), 
              color="darkgreen",
              size=2.5,
              shape=17,
              alpha = 0.4 ) +
  theme_bw() +
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature (F)",
       y = "Humidity (%)") +
  theme(axis.title.x=element_text(size=14, color="orangered2"),
        axis.title.y=element_text(size=14, color="orangered4"), 
        plot.title=element_text(size=18, face="bold", 
                                color ="darkblue"),
        plot.subtitle=element_text(size=10, face="bold.italic", 
                                   color ="brown", family="serif"));
plot(plotData)