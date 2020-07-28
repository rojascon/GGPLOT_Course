##### In-class application #### 
#Connnie Rojas

{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE);
  
  ### Probably want the column to be numeric...
  weatherData$precip3 = as.numeric(gsub(x=weatherData$precip, 
                                        pattern="T", 
                                        replacement="0.005"));

  # 1) Using one for loop:
  #    Find the total rainfall in April, July, and November
  #    - you will need three state variables
 
  aprilP = 0;
  julyP=0;
  novP=0;
  
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="04-"))
    {
      aprilP = aprilP + weatherData$precip3[i]; 
    }
    if(grepl(x=weatherData$date[i], pattern="07-"))
    {
      julyP = julyP + weatherData$precip3[i]; 
    }
    if(grepl(x=weatherData$date[i], pattern="11-"))
    {
      novP = novP + weatherData$precip3[i]; 
    }
  }

# 2) Using one for loop:
#    Find the total rainfall for the first three months and the last three months
#    - you can extend the grep pattern (i.e., pattern="RA|SN|FG"  
#                                             looks for rain, snow, or fog)

first3m=0;
last3m=0;
for(i in 1:length(weatherData$precip3))
{
  if(grepl(x=weatherData$date[i], pattern="01-|02-|03-"))
  {
    first3m = first3m + weatherData$precip3[i]; 
  }
  if(grepl(x=weatherData$date[i], pattern="10-|11-|12-"))
  {
    last3m = last3m + weatherData$precip3[i]; 
  }
  }
# 3) Using one for loop:
#    Find the month with the most amount of rain
jan=0;
feb=0;
mar=0;
apr=0;
may=0;
june=0;
july=0;
aug=0;
sept=0;
oct=0;
nov=0;
dec=0;

for(i in 1:length(weatherData$precip3))
{
  if(grepl(x=weatherData$date[i], pattern="01-"))
  {
    jan = jan + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="02-"))
  {
    feb = feb + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="03-"))
  {
    mar = mar + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="04-"))
  {
    apr = apr + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="05-"))
  {
    may = may + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="06-"))
  {
    june = june + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="07-"))
  {
    july = july + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="08-"))
  {
    aug = aug + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="09-"))
  {
    sept = sept + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="10-"))
  {
    oct = oct + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="11-"))
  {
    nov = nov + weatherData$precip3[i]; 
  }
  else if(grepl(x=weatherData$date[i], pattern="12-"))
  {
    dec = dec + weatherData$precip3[i]; 
  }
}
total=c(jan,feb,mar,apr,may,june,july,aug,sept,oct,nov,dec)
a=max(total)
print(a)


##Ciara's double for loop
months = rep(0,12)
pattern = c("01-", "02-", "03-", "04-", "05-", "06-", 
            "07-", "08-", "09-", "10-", "11-", "12-")

for(m in 1:length(pattern))
{ 
  for (i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern=pattern[m]))
    {
      months[m] = months[m] + weatherData$precip3[i];
    }
  }
}
}

