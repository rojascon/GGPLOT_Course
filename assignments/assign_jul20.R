##### In-class application #### 
#Connnie Rojas

#  Part 1: Practice with the breakpoints:  
#    What is the breakpoint doing inside the for loops?
#      note: you can add/remove breakpoints while debugging (especially useful when in for loops)
#    Put it two (or more) breakpoints and use Continue

#The breakpoints inside a for loop pause the loop at that breakpoint, if click 
#continue, it will stop again at that statement

#  Part 2: Humidity vs Precipitation boxplots
#    Humidity goes on the y-axis
#    Two boxes on the x-axis: (1) Days that had precip (2) Days that had no precip
#    Use "weatherType" column to determine precipitation
#      days with precip have either "RA" or "SN" in the "weatherType" column
#    Break down the problem -- try to individually get "RA" and "SN" before trying to combine them

##LOAD DATA
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv",
                        stringsAsFactors = FALSE )

#DETERMINE WHETHER DAYS HAD PRECIPITATION OR DIDNT HAVE PRECIPITATION
##METHOD 1: use %in% [NOT ENCOURAGED; buggy]
typ=gsub(pattern=",", replacement="", x=weatherData$weatherType)

for(i in 1:length(typ))  
{
  if("SN" %in% typ[i])
  {
    weatherData$precip_cat[i] = "yes";
  }
  else if("RA" %in% typ[i])
  {
    weatherData$precip_cat[i] = "yes";
  }
  else
   {
     weatherData$precip_cat[i] = "no";
   }
}

##METHOD 2: use grep
yes_precip=grep(pattern="SN|RA", x=weatherData$weatherType)
weatherData$precip_cat_2="no"
weatherData$precip_cat_2[yes_precip]="yes"

#MAKE YOUR PLOT
hmpre = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=precip_cat_2,y=relHum)) +
  theme_bw() +
  labs(title = "Relative Humidity vs. Precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Precipitation",
       y = "Relative Humidity")+
  theme(axis.title.x=element_text(size=14, color="black",face="bold"),
        axis.title.y=element_text(size=14, color="black",face="bold"), 
        plot.title=element_text(size=16),
        plot.subtitle=element_text(size=12,  
                                   color ="darkblue"))

plot(hmpre)


