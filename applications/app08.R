#application of lesson 8
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

##Create a bar plot of Cooling Days (coolDays) for each month
weatherData$month = format.Date(weatherData$dateYr, format="%b");

cdays = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  theme_bw() +
  labs(title = "Cool days to achieve most comfortable T",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Degree of Cooling Needed");
plot(cdays);

#Create a vector that consists of the first two values in the weatherType column 
#(i.e., the new vector will have at most one weather condition for the day)
typ=gsub(pattern=",", replacement="", x=weatherData$weatherType)

for(i in 1:length(typ))
{
  typ[i] = strtrim(typ[i], 2);
};

length(unique(typ))==8;
weatherData$wtype=typ;

####Create a bar plot of Cooling Days (coolDays) for each month; use FILL
cdays = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays, fill=wtype),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_manual(values=c('#b3e2cd','#fdcdac','#cbd5e8','#f4cae4','#e6f5c9',
  '#fff2ae','#f1e2cc','#cccccc'))+
  theme_bw() +
  labs(title = "Cool days needed to achieve a comfortable T",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Degree of Cooling Needed",
       fill="Weather Type");
plot(cdays);

###Create a bar plot of Heating Days (heatDays) for each month; use FILL
#Add a horizontal line to the plot that represent the sum of all 
#coolDays for the year (it should be in the 800s)

hdays = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=heatDays, fill=wtype),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_manual(values=c('#b3e2cd','#fdcdac','#cbd5e8','#f4cae4','#e6f5c9',
                             '#fff2ae','#f1e2cc','#cccccc'))+
  geom_hline(mapping = aes(yintercept = sum(coolDays)), 
             color="blue",  
             size=1.5, 
             linetype=1)+
  annotate(geom="text",  
           x=5, 
           y=sum(weatherData$coolDays)+50, 
           color="red",
           label="Sum of all Cool Days" ) +
  theme_bw() +
  labs(title = "Hot days Needed to achieve a comfortable T",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Degree of Heating Needed",
       fill="Weather Type");
plot(hdays);

##side by side barplots of Cooling Days and Heating Days
all_days=ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays),
           width=0.4,
           position=after_stat(position_nudge(x=-0.2)),
           fill="blue",
           color="black") +
  geom_col(mapping=aes(x=month, y=heatDays),
           width=0.4,
           position=after_stat(position_nudge(x=0.2)),
           fill="red") +
  scale_x_discrete(limits = month.abb) +
  theme_bw() +
  labs(title = "Hot and Cool days Needed to achieve a comfortable T",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Degree of Heating or Cooling Needed",
       fill="Weather Type");
plot(all_days);
