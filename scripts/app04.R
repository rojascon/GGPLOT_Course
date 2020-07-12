#application of lesson 4

source(file="scripts/reference.R") # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE)

#### adding a date column that includes year
theDate = weatherData$date
theDate = paste(theDate, "-2016", sep="")
theDate = as.Date(theDate, format="%m-%d-%Y")
weatherData$dateYr = theDate

###changing 3 temperature columns from F to C
weatherData$cmax=5/9*(weatherData$maxTemp-32)
weatherData$cmin=5/9*(weatherData$minTemp-32)
weatherData$cavg=5/9*(weatherData$avgTemp-32)

#subset weather data to only include dates of interest
wd=weatherData[(weatherData$dateYr>="2016-03-21") & (weatherData$dateYr<="2016-09-21"),]

#alternative way to subset the data
### get the earliest and latest dates that we want to plot
#firstDateIndex = which(weatherData$dateYr == as.Date("2016-03-21")); 
#lastDateIndex = which(weatherData$dateYr == as.Date("2016-09-21")); 
               
#### plot maximum and minimum temperatures vs Date
#Ctemp_plot = ggplot(data=weatherData[firstDateIndex:lastDateIndex,]) +
Ctemp_plot = ggplot(data=wd) + 
  geom_line(mapping=aes(x=dateYr, y=cmax),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=cmin),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=cavg),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (°C)") +
  theme_bw()+
  theme(panel.background = element_rect(size=2, color="grey0"),
    plot.title = element_text(hjust = 0.90, face="bold"),
        plot.subtitle = element_text(hjust = 0.90),
        axis.text = element_text(family="sans", size=11),
        axis.text.y=element_text(color="red"),
        axis.text.x=element_text(color="blue"),
        axis.title.x=element_text(size=14, color="black",face="bold"),
        axis.title.y=element_text(size=14, color="black",face="bold"))+
  scale_y_continuous(breaks=ceiling(seq(from=-7, to=34, length.out=4)))+
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-09-21")),
               date_breaks = "9 weeks", 
               date_labels = format("%b-%d-%Y"))
plot(Ctemp_plot)
