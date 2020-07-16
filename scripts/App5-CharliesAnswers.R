source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016.csv", #-2
                        stringsAsFactors = FALSE );

#### & vs && (note: | vs || is similar)
# When comparing a single value, & and && function exactly the same

# When comparing multiple values (i.e., a vector of values):
a = c(2,3,4,1,2);
b = c(4,3,2,1,4);

# & will compare each value resulting in a Boolean (T/F) vector the same length as 
#   the vectors being compared
c = (a>2 & b>2);    # compares each set of values -- gives a TRUE/FALSE vector

# && takes the results of & and &s them 
d = (a>2 && b>2);   # TRUE only if EVERY value of a and b meet conditions, FALSE otherwise

# this is functionally equivalent to the above line 
e = (a[1]>2 & b[1]>2) & (a[2]>2 & b[2]>2) & (a[3]>2 & b[3]>2) &
    (a[4]>2 & b[4]>2) & (a[5]>2 & b[5]>2);

# I have not seen any use for && in R


#### For loops -- creates 3 new columns with converted temperatures
for(i in c("minTemp", "maxTemp", "avgTemp"))  # for the columns with these names
{    
  colName = paste(i, "Celsius", sep="");      # add a new column with "Celsius" appended to name
  for(j in 1:nrow(weatherData))
  {
    # the $ format (weatherData$colName) does not work here
    weatherData[j, colName] = (5/9) * (weatherData[j, i] -32);  # conversion
  }
}

# Honestly, for 3 columns, I would probably just do each individually in a for loop

