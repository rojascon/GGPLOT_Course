#Replicate this image 
#Connie Rojas

{
  source(file="scripts/reference.R");  # this line will be in all your scripts
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE);
  
  # This scatterplot show that...
  plotData = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=abs(tempDept), y=windSpeed),
               color=rgb(red=0, green=.6, blue=.6), 
               size=2, 
               shape=23,
               alpha = 0.7 ) +
    scale_y_continuous(limits=c(0,20), 
                       breaks=seq(from=5,to=20,by=5), expand = c(0, 0))+
    scale_x_continuous(breaks=c(-1,2,5,8,11,14,17,20,23,26,29))+
    theme_bw() +
    labs(title = "Wind Speed vs. Temperature Difference",
         subtitle = "Lansing, Michigan: 2016",
         x = "Temperature (F)",
         y = "Humidity \
         (%)") +
    theme(axis.text.y=element_text(angle=30),
          axis.text.x=element_text(angle=-45),
          axis.title.x=element_text(size=15, face="italic", 
                                    color=rgb(red=.8, green=.3, blue=0),
                                    hjust=0.2),
          axis.title.y=element_text(size=12, angle=0, vjust=0.3),
          plot.title=element_text(size=16, 
                                  color ="black", 
                                  hjust = 0.8),
          plot.subtitle=element_text(face="bold.italic",
                                     color ="brown",hjust=0.7),
          aspect.ratio=3/5,
          panel.background = element_rect(fill="grey90",
                                          size=2, color="grey90"),
          panel.grid.minor = element_line(color="grey50", linetype=4),
          panel.grid.major = element_line(color="grey90"),
          plot.background = element_rect(fill = "bisque1")) +
    # default confidence level is 95%
    geom_smooth(mapping=aes(x=abs(tempDept), y=windSpeed), 
                method="lm",
                color="purple", 
                size=1.2, 
                linetype=5, 
                fill="yellow");   
  plot(plotData)
}
