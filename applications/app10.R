#application of lesson 10
{
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

##Find how different weather conditions in the weatherType column 
##correlate with tempDept. Note: tempDept is how far the temperature for the day was 
#from the historic average temperature for that day.

#subset data with indices that meet a weather condition
snowyDays = grep(weatherData$weatherType, pattern="SN");
breezyDays = grep(weatherData$weatherType, pattern="BR");
foggyDays=grep(weatherData$weatherType, pattern="FG");

#Create a histogram of tempDept restricted to days that meet 
#a condition in the weatherType column:
#Place a vertical line at the average tempDept for the condition and 
#label the line with the average value

snowy = ggplot( data=weatherData[snowyDays,]) + 
  geom_histogram(mapping=aes(x=tempDept, y=..count..),  
                 bins=10,             
                 color="black",
                 fill="#c994c7") +
  scale_x_continuous(breaks=ceiling(seq(from=-30, to=20, length.out=10)))+
  theme_classic() +
  geom_vline(mapping=aes(xintercept=mean(weatherData$tempDept[snowyDays])),
             color="darkblue",
             size=1.2)+
  annotate(geom="text",
           x=2.5+mean(weatherData$tempDept[snowyDays]), 
           y=11, 
           color="red",    
           label=paste("mean:", -4.11) ) +
  labs(title = "Temperature Departure Histogram",
       subtitle = "For Snowy Days in Lansing, Michigan (2016)",
       x = "Temperature Departure",
       y = "Counts");
plot(snowy);

#Repeat step 2 for two more conditions in the weatherType column 
breezy = ggplot( data=weatherData[breezyDays,]) + 
  geom_histogram(mapping=aes(x=tempDept, y=..count..),  
                 bins=10,             
                 color="black",
                 fill="aquamarine") +
  scale_x_continuous(breaks=ceiling(seq(from=-20, to=30, length.out=10)))+
  theme_classic() +
  geom_vline(mapping=aes(xintercept=mean(weatherData$tempDept[breezyDays])),
             color="darkblue",
             size=1.2)+
  annotate(geom="text",
           x=2.5+mean(weatherData$tempDept[snowyDays]), 
           y=30, 
           color="red",    
           label=paste("mean:", 2.82) ) +
  labs(title = "Temperature Departure Histogram",
       subtitle = "For Breezy Days in Lansing, Michigan (2016)",
       x = "Temperature Departure",
       y = "Counts");
plot(breezy);

foggy = ggplot( data=weatherData[foggyDays,]) + 
  geom_histogram(mapping=aes(x=tempDept, y=..count..),  
                 bins=10,             
                 color="black",
                 fill="goldenrod") +
  scale_x_continuous(breaks=ceiling(seq(from=-20, to=20, length.out=10)))+
  theme_classic() +
  geom_vline(mapping=aes(xintercept=mean(weatherData$tempDept[foggyDays])),
             color="darkblue",
             size=1.2)+
  annotate(geom="text",
           x=3.5+mean(weatherData$tempDept[snowyDays]), 
           y=7, 
           color="red",    
           label=paste("mean:", 2.58) ) +
  labs(title = "Temperature Departure Histogram",
       subtitle = "For Foggy Days in Lansing, Michigan (2016)",
       x = "Temperature Departure",
       y = "Counts");
plot(foggy);

#Create a histogram of tempDept restricted to days where two conditions 
#occur in the weatherType column
#breezy and foggy
snowyAndfoggy = intersect(snowyDays, foggyDays); # days with rain AND wind

sfoggy = ggplot( data=weatherData[snowyAndfoggy,]) + 
  geom_histogram(mapping=aes(x=tempDept, y=..count..),  
                 bins=10,             
                 color="black",
                 fill="grey57") +
  scale_x_continuous(breaks=ceiling(seq(from=-20, to=20, length.out=10)))+
  theme_classic() +
  geom_vline(mapping=aes(xintercept=mean(weatherData$tempDept[snowyAndfoggy])),
             color="darkblue",
             size=1.2)+
  annotate(geom="text",
           x=2.5+mean(weatherData$tempDept[snowyAndfoggy]), 
           y=6, 
           color="red",    
           label=paste("mean:", -5.05) ) +
  labs(title = "Temperature Departure Histogram",
       subtitle = "For Foggy and Breezy Days in Lansing, Michigan (2016)",
       x = "Temperature Departure",
       y = "Counts");
plot(sfoggy);

#Create a histogram of tempDept restricted to days where one of 
#two conditions occur in the weatherType column (e.g., rainy OR breezy)
snowyOrfoggy = union(snowyDays, foggyDays);

sorf = ggplot( data=weatherData[snowyOrfoggy,]) + 
  geom_histogram(mapping=aes(x=tempDept, y=..count..),  
                 bins=10,             
                 color="black",
                 fill="firebrick") +
  scale_x_continuous(breaks=ceiling(seq(from=-20, to=20, length.out=10)))+
  theme_classic() +
  geom_vline(mapping=aes(xintercept=mean(weatherData$tempDept[snowyOrfoggy])),
             color="darkblue",
             size=1.2)+
  annotate(geom="text",
           x=2.5+mean(weatherData$tempDept[snowyOrfoggy]), 
           y=15, 
           color="red",    
           label=paste("mean:", -1.15) ) +
  labs(title = "Temperature Departure Histogram",
       subtitle = "For Foggy and Breezy Days in Lansing, Michigan (2016)",
       x = "Temperature Departure",
       y = "Counts");
plot(sorf);

#Using grid.arrange(), place the 5 histograms you created in the 
#previous steps on one canvas
grid.arrange(snowy,breezy,foggy,sfoggy,sorf,
             nrow=2);

#Using grid.arrange(), pick three histograms from steps 2-4 and place them on a canvas
#Resize at least 2 of the histogram so they take up more than 1 cell
grid.arrange(snowy, sfoggy, breezy, 
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,NA),
                                   c(3,NA,NA)));
}