#in-class Application
#Connie Rojas

# #First assignment:
#   -	Create 3 other animations:
#   o	1 scatterplot (using different variables)
# o	2 of the following: histogram, boxplot, bar plot 
# -	Modify the transitions in each plot
# -	Use closest_state somewhere in each of the plots
# -	Challenges:
#   o	Use closest_state in somewhere other than title or subtitle
# o	Save plot as a gif
# o	Use one other function on the cheat sheet

{
  source(file="scripts/reference.R");  # this line will be in all your scripts
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE);
  
  library(package=gganimate);  # package used to create the animation mappings
  
  # scatterplot mapping
  plot1 = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=snow, y=windSpeed)) +
    labs(title = 'Wind Speed (y) vs. Amount of Snow (x) by {closest_state} Wind Direction (animation)',  
         subtitle = 'Lansing, MI - 2016',
         x = 'Amount of Snow', 
         y = 'Wind Speed (mph)') +
    theme_bw() +
    transition_states(states = windDir, 
                      transition_length = 2, # relative animation time (default: 1)  
                      state_length = 1,      # relative pause time (default: 1)
                      wrap = TRUE);          # gif always wraps so this is useless
  print(plot1)
  
  #boxplot mapping
  hum_quant = quantile(weatherData$relHum, probs=c(0.30, 0.70));
  weatherData$humlevel=c();
  
  weatherData$humlevel[
    weatherData$relHum<=hum_quant[1]
    ]="Low";
  
  weatherData$humlevel[
    weatherData$relHum>=hum_quant[2]
    ]="High";
  
  weatherData$humlevel[
    is.na(weatherData$humlevel)
    ]="Medium";
  
  ###stnPressure vs humidityLevel
  humplot = ggplot(data=weatherData) +
    geom_boxplot(mapping=aes(x=humlevel, y=stnPressure),
                 na.rm=TRUE) +
    theme_bw() +
    scale_x_discrete(limits = c("Low", "Medium", "High")) +
    labs(title = "Change in Pressure vs. Relative Humidity by {closest_state} Wind Direction (animation)",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity Levels",
         y = "Pressure")+
    transition_states(states = windDir, 
                      transition_length = 1, # relative animation time (default: 1)  
                      state_length = 1,      # relative pause time (default: 1)
                      wrap = TRUE);          # gif always wraps so this is useless;
  
  print(humplot);
  
  #histogram mapping
  temp = ggplot(data=weatherData ) + 
    geom_histogram(mapping=aes(x=maxTemp, y=..count..),  #proportions
                   color="black",
                   fill="#c994c7") +
    theme_classic() +
    labs(title = "Maximum Temperature Histogram by {closest_state} season (animation)",
         subtitle = "Lansing, Michigan: 2016",
         x = "Maximum Temperature",
         y = "Counts")+
    transition_states(states = season, 
                      transition_length = 1, # relative animation time (default: 1)  
                      state_length = 2,      # relative pause time (default: 1)
                      wrap = TRUE)+
    view_zoom();          # gi
  
  print(temp);
 
  ##SAVE histogram to .GIF
  anim_save(filename = "media/hist_temp_season.gif",
            animation = temp,
            nframes = 60,       # number of frames in animation
            fps = 5,            # frames per second
            start_pause = 10,   # first frame lasts for 10 frames
            end_pause = 5,     # last frame lasts for 5 frames
            rewind = TRUE);     # like wrap, FALSE does not do much for gif

  ##SAVE scatter to video
  # anim_save() -- saving as an mp4 video
  #the file cannot be viewed (i.e. it is not a true .mp4 file)
  library(package=av)
  anim_save(filename = "media/boxplot_humidity_pressure.mp4",
            animation = humplot,
            renderer = av_renderer(),
            nframes = 60,       # number of frames in animation
            fps = 3);           # frames per second
  
#Second assignment:
#     -	Save a plot as both a gif and a video with modified timing
#   o	don't forget to install the av package
# o	save the files to a folder called media in your class project
# -	Use, and explain, at least one Label variable other than closest_state or frame_time in your plot
# -	Use, and explain, at least two functions other than transition_states() and transition_time() from the cheat sheet in any plot
# -	Challenge: Use, and explain, at least one function from each of these categories on the cheat sheet
# o	view_*()
# o	enter/exit_*()
# o	shadow_*()
# o	ease_aes()

   
}
