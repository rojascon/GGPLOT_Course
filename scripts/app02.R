#application of lesson 2

# execute the lines of code from reference.r
source(file="scripts/reference.r")

# read in CSV file and save the content to packageData
deaths = read.csv(file="data/accdeaths.csv")
colnames(deaths)[2]="accdeaths"

#plot accdeaths vs time
plotData = ggplot( data=deaths ) + 
  geom_point(mapping=aes(x=time, y=accdeaths)) +
  ggtitle( label="US Accidental Deaths (1973-1978)" ) +
  scale_x_continuous(breaks = seq(from=1973, to=1979, by=0.5)) +
  scale_y_continuous(breaks=seq(from=7000, to=11000, by=2000))+
  ylab("No. Accidental Deaths")+
  theme( axis.text.x=element_text(angle=45, hjust=1) )
plot(plotData)

