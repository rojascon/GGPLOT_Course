##### In-class application #### 
#Connnie Rojas

{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE);
  
  #### Create a column the give whether there was precipitation
  
  #### grepl produces a Boolean vector the same length as the data: 
  #      values are TRUE if they contain RA or SN, FALSE otherwise
  daysWithPrecip = grepl(x=weatherData$weatherType, pattern="RA|SN");
  
  # days will take values from 1 to 366 (the number of rows)
  for(day in 1:nrow(weatherData))  
  {
    if(daysWithPrecip[day] == TRUE)  # day had either RA or SN
    {
      weatherData$precipitation[day] = 1;   # set precip to 1
    }
    else   # day had neither RA nor SN
    {
      weatherData$precipitation[day] = 0;   # set precip to 0
    }
  }
  
  # GGPlot cannot factor a numeric column -- need to convert column to string (characters)
  weatherData$precipitation = as.character(weatherData$precipitation);
  
  # Original boxplot to replicate
  plot1 = ggplot(data=weatherData) +   
    geom_boxplot(mapping=aes(x=precipitation, y=relHum)) +
    theme_bw() +
    scale_x_discrete(labels=c("No Precip", "Precip")) +  
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Precipitation",
         y = "Humidity");
  plot(plot1);
  
  ##### Assignment:
  # - Make a copy of the above plot using manually created boxplots
  # - add a line to represent the mean value for each box
  # - add text to represent the standard deviations for each box
  # - get *one box* working fully before trying the second
  
  ##### To manually get data for boxplots (don't read if you want a challenge)
  # 1) get all relHum values for days where there is no precipitation
  #    one way: weatherData$relHum[weatherData$precipitation==0]
  noP=weatherData$relHum[weatherData$precipitation==0];
  
  # 2) get the mean, median, and standard deviation for the data from #1
  meanP=mean(noP);
  medianP=ceiling(median(noP));
  standevP=sd(noP);
  
  # 3) find the 1st and 3rd quartile (0.25 and 0.75 quantile) 
  #    of the data using quantile() 
  quants = quantile(noP, probs=c(0.25, 0.75 ))
  # 4) solve for interquartile range (IQR -- the box height): 
  #    IQR = 3rd quartile - 1st quartile
  IQR=as.numeric(quants[2]-quants[1])
  # 5) solve for extreme whisker ends: 
  #    - high: (3rd quartile) + 1.5*IQR
  #    - low: (1st quartile) - 1.5*IQR
  high=as.numeric(quants[2])+(1.5*IQR)
  low=as.numeric(quants[1])-(1.5*IQR)
    # 6) Solve for actual whisker end:
    #    - high: the highest value in the data less than the high whisker value
    #    - low: the lowest value in the data greater than the low whisker value
  highend=max(noP)
  lowdata=noP[noP>low]
  lowend=min(lowdata)
  
    # 7) Repeat 1-6 for relHum values for days where there is precipitation
  yesP=weatherData$relHum[weatherData$precipitation>0];
  meanyP=mean(yesP);
  medianyP=ceiling(median(yesP));
  standevyP=sd(yesP);
  
  quantsyp = quantile(yesP, probs=c(0.25, 0.75 ))
  IQRyp=as.numeric(quants[2]-quants[1])
  highyp=as.numeric(quants[2])+(1.5*IQR)
  lowyp=as.numeric(quants[1])-(1.5*IQR)
  
  highend_yp=max(yesP)
  lowdata_yp=yesP[yesP>low]
  lowend_yp=min(lowdata)
  
##### Manually plot humidity vs precipitation ######
  plot2 = ggplot() +     # no dataframe because the data is being manually entered
    
    # first boxplot
    geom_boxplot(stat="identity",     # manually enter data
                 # scroll far down on geom_boxplot() Help page to find these aesthetics
                 mapping=aes(x = 1,            # x-position of box
                             ymin = lowend,        # actual low whisker end
                             lower = quants[1],       # low end of box (1st quartile/0.25 quantile)
                             middle = medianP,      # median of data
                             upper = quants[2],       # high end of box (3rd quartile/0.75 quantile)
                             ymax = high   )) +  # actual high whisker end
    geom_boxplot(stat="identity",     # manually enter data
                 # scroll far down on geom_boxplot() Help page to find these aesthetics
                 mapping=aes(x = 2,            # x-position of box
                             ymin = lowend_yp,        # actual low whisker end
                             lower = quantsyp[1],       # low end of box (1st quartile/0.25 quantile)
                             middle = medianyP,      # median of data
                             upper = quantsyp[2],       # high end of box (3rd quartile/0.75 quantile)
                             ymax = highyp   )) +  # actual high whisker end
    theme_classic() + 
    # Can use annotate(), geom="text" to add text 
    annotate(geom="text",
             x = 1, y = 35,         # text is at (x,y)
             label = "No Precipitation",  # https://en.wikipedia.org/wiki/List_of_Unicode_characters
             color = "black") +
    annotate(geom="text",
             x = 2, y = 35,         # text is at (x,y)
             label = "Yes Precipitation",  # https://en.wikipedia.org/wiki/List_of_Unicode_characters
             color = "black") +
    
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, MI 2016",
         x = "",
         y = "Relative Humidity")
    
    #scale_x_continuous(breaks = c(1, 2), #ended here, trying to use this to say "yes precip" "no precip"
                       #labels = c("this", "that")) 
    
  
plot(plot2)
}
