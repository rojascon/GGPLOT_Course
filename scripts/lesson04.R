source(file="scripts/reference.R") # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE)

#formatting date column so it includes the year "2016"

#### Part 1: save the dataframe date column to a vector
theDate = weatherData$date
# theDate = weatherData[["date"]]; # equivalent to previous line
# theDate = weatherData[ , "date"]; # equivalent to previous 2 lines in base R

# append (paste) "-2016" to all values in theDate vector
theDate = paste(theDate, "-2016", sep="")
# theDate = paste(theDate, "2016", sep="-"); # functionally equivalent to the previous line

# c) Save the values in Date format instead of as characters
theDate = as.Date(theDate, format="%m-%d-%Y")

# d) Save theDate back to the data frame as a new column
weatherData$dateYr = theDate
# weatherData[["dateYr"]] = theDate; # equivalent to previous line
# weatherData[, "dateYr"] = theDate; # equivalent to previous 2 lines in base R

#### plot maximum and minimum temperatures vs Date
temp_plot = ggplot(data=weatherData) + 
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  # size and color relate to the border, fill is the inside color 
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4), #grid lines vertical
        panel.grid.major = element_line(color="grey100"), #grid lines horizontal
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        axis.text = element_text(color="blue", family="mono", size=9))+
  scale_y_continuous(limits = c(-15,90),
                     breaks = seq(from=-15, to=90, by=20)) +
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-12-21")),
               date_breaks = "6 weeks", 
               date_labels = format("%m/%d"))
plot(temp_plot)