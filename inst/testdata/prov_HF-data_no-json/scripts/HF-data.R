#############################
### HF METEOROLOGICAL DATA###
#############################

# collect provenance
# library(rdtLite)
# prov.init(prov.dir="prov", snapshot.size = 1)

raw.file <- readLines("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/metsta.dat", warn = FALSE)
hourly.vector <- vector()
daily.vector <- vector()

for(i in 1:length(raw.file)){
  if(substring(raw.file[i], 1, 3) == "101") 
    hourly.vector <- append(hourly.vector, raw.file[i])
  else
    daily.vector <- append(daily.vector, raw.file[i])
}

hourly.file <- file("hourly.csv")
daily.file <- file("daily.csv")

writeLines(hourly.vector, con = hourly.file)
writeLines(daily.vector, con = daily.file)

close(hourly.file)
close(daily.file)

hourly.df <- read.csv("hourly.csv", header = FALSE)
daily.df <- read.csv("daily.csv", header = FALSE)

####################################################################

# name columns
colnames(hourly.df) <- c("type", "year", "JulDate", "time", "airTemp", 
                             "humidity", "dewpoint", "precipitation", 
                             "solRad", "phoRad", "netRad", "barPressure",
                             "windSpeed", "vectorWindSpeed", "windDirection",
                             "windDeviation", "gustSpeed", "soilTemp")

# get obs with rain
rain.df <- hourly.df[hourly.df$precipitation > 0, ]

# get obs where air temp was below soil temp
temp.df <- hourly.df[hourly.df$airTemp < hourly.df$soilTemp, ]

# get obs with rain and where air temp was below soil temp
rain.temp.df <- rain.df[rain.df$airTemp < rain.df$soilTemp, ]

# list 5 selected columns for all days
selected.columns.df <- hourly.df[ , c("JulDate", "time", "airTemp", "soilTemp", "precipitation")]

# list 5 selected columns for days with rain and where air temp < soil temp
filtered.selected.columns.df <- rain.temp.df[ , c("JulDate", "time", "airTemp", "soilTemp", "precipitation")]

# find 5 highest recorded winds 
five.highest.wind <- hourly.df[order(hourly.df$windSpeed, decreasing = TRUE), ]
five.highest.wind <- five.highest.wind[1:5, ]

# find some features for 5 days with highest wind
feature.highest.wind.df <- five.highest.wind[ , c("JulDate", "time", "airTemp", "precipitation", "windSpeed")]

# add a column to track days with rain 
hourly.df$hasRain <- hourly.df$precipitation > 0 

# get the highest temp from a function for a given Julian date
highest.temp.Jul.date <- function(jul.date){
  specific.jul.date.df <- hourly.df[hourly.df$JulDate == jul.date, ]
  return(max(specific.jul.date.df$airTemp))
}

print(highest.temp.Jul.date(121))

# get min and max of Julian dates
min.Jul.date <- min(hourly.df$JulDate)
max.Jul.date <- max(hourly.df$JulDate)

# vector for a range of min to max Julian date
Jul.range.vector <- c (min.Jul.date:max.Jul.date)

# get the highest temp for each Julian date and put them in a data frame
high.temp.each.day.vector <- sapply(Jul.range.vector, highest.temp.Jul.date)
high.temp.days.df <- data.frame(Jul.range.vector, high.temp.each.day.vector)
colnames(high.temp.days.df) <- c("Day", "Temp")

# function to get the total precipitaition for a given Julian date
total.rain.specific.day <- function(jul.date){
  temp.date.df <- hourly.df[hourly.df$JulDate == jul.date, ]
  return(sum(temp.date.df$precipitation))
}

total.rain.each.day.vector <- sapply(Jul.range.vector, total.rain.specific.day)
total.rain.days.df <- data.frame(Jul.range.vector, total.rain.each.day.vector)
colnames(total.rain.days.df) <- c("Day", "TotalRain")

# get number of days there was rain
summary(total.rain.days.df$TotalRain > 0)

# plot high air temp against Julian date 
plot(high.temp.days.df$Day, high.temp.days.df$Temp)

# add best fit line for the high temperature
abline(lsfit(high.temp.days.df$Day, high.temp.days.df$Temp), col = "red")



# get quick look at data
# summary(HF.data.frame) 
# head(HF.data.frame) #default: 6 first lines
# head(HF.data.frame, n = 1L) #get the first line
# head(HF.data.frame, n = -14700L) #get all but 14700 last lines
# tail(HF.data.frame)
# str(HF.data.frame)

# # end provenance
# prov.quit()

