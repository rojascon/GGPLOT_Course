#July 6 assignment

#application of lesson 3

source(file="scripts/reference.R") # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE)

#### Part 1: Plot Average Wind Speed (windSpeed) vs. Daily Temperature Departure (tempDept)
#temperature departure= how much average temperature changed between two consecutive days

plotData = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=abs(tempDept), y=windSpeed), 
              color="#41b6c4", 
              size=2, 
              shape=5,
              alpha=0.7) +
  theme_bw()+
  labs(title = "Wind Speed vs. Temperature Difference",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature (F)",
       y = "Wind Speed (mph)")+
  scale_y_continuous(limits=c(0,20), breaks=seq(from=5,to=20,by=5))+
  scale_x_continuous(breaks=c(-1,2,5,8,11,14,17,20,23,26,29))+
  theme(axis.title.x=element_text(size=14, color="#d94801",
                                  face="italic", hjust=0.2),
        axis.text.x=element_text(angle=45,hjust=1),
        axis.title.y=element_text(size=14, color="black",
                                  angle=0, vjust=0.3), 
        axis.text.y=element_text(angle=45),
        plot.title=element_text(size=16, 
                                color ="black", 
                                hjust = 0.8),
        plot.subtitle=element_text(size=12, 
                                   face="bold.italic", 
                                   color ="#b10026", 
                                   family="sans",
                                   hjust=0.7))+
  geom_smooth(mapping=aes(x=abs(tempDept), y=windSpeed),
              method="lm",
              color="purple", 
              size=0.8, 
              linetype=5, 
              fill="yellow")
plot(plotData)

