#application of lesson 3

source(file="scripts/reference.R") # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE)

#### Part 1: Plot Average Wind Speed (windSpeed) vs. Daily Temperature Departure (tempDept)
#temperature departure= how much average temperature changed between two consecutive days

plotData = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=abs(tempDept), y=windSpeed), 
              color="#737373", 
              size=2, 
              shape=18,
              alpha=0.7) +
  theme_bw()+
  labs(title = "Wind Speed vs. Daily Temperature Departure",
       subtitle = "From Lansing NOAA 2016 Data",
       x = "Temperature Departure (F)",
       y = "Wind Speed (mph)")+
  theme(axis.title.x=element_text(size=14, color="black",face="bold"),
        axis.title.y=element_text(size=14, color="black",face="bold"), 
        plot.title=element_text(size=16, face="bold", 
                                color ="purple"),
        plot.subtitle=element_text(size=12,  
                                   color ="darkblue", family="serif"))+
  geom_smooth(mapping=aes(x=abs(tempDept), y=windSpeed),
              method="lm",
              color="black", 
              size=0.8, 
              linetype=3, 
              fill="goldenrod2");
plot(plotData)
ggsave(filename="images/app3_plot2.png", 
       device="png",
       plot=plotData, 
       width=6, 
       height=8, 
       units="cm", 
       dpi=400)
         
         
         
         
         
         