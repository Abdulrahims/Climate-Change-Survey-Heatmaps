library(ggplot2)
library(ggmap)

climate_change <- read.csv("Climate_change.csv", stringsAsFactors = FALSE)

#for loop to remove columns that aren't US. Or use dplyr, one of those R/SQL packages

#13866 is the magic number

#try subsetting by = "US"
for(i in 1:nrow(climate_change)){
  if(climate_change$countryCode[i] != "US"){
    climate_change <- climate_change[-c(i), ]
  }
}

#Just US data
climate_change_us <- data.frame(climate_change)
climate_change_us_problevel <- climate_change_us

#Just one question for now, seriousness of climate change. Remove other variables
climate_change_us_problevel$harm.level <- NULL
climate_change_us_problevel$personal <- NULL
climate_change_us_problevel$Date <- NULL

#Recode responses to numeric values
#1 - Not a problem
#2 - A not too serious problem
#3 - A somewhat serious problem
#4 - A very serious problem

for(i in 1:nrow(climate_change_us_problevel)){
  if(climate_change_us_problevel$Prob.level[i] == "Not a problem"){
    climate_change_us_problevel$Prob.level[i] = 1
  }
  else if (climate_change_us_problevel$Prob.level[i] == "A not too serious problem"){
    climate_change_us_problevel$Prob.level[i] = 2
  }
  else if (climate_change_us_problevel$Prob.level[i] == "A somewhat serious problem"){
    climate_change_us_problevel$Prob.level[i] = 3
  }
  else if (climate_change_us_problevel$Prob.level[i] == "A very serious problem"){
    climate_change_us_problevel$Prob.level[i] = 4
  }
  else{
    print("Irregular value")
  }
}

#Need numeric variable to map. Convert from character to numeric
climate_change_us_problevel$Prob.level <- as.numeric(climate_change_us_problevel$Prob.level)

#Only need latitude and longitude for mapping, and the data
climate_change_us_problevel <- subset(climate_change_us_problevel, , -c(X, ID, cityName, countryCode, countryName, ipAddress,timeZone,zipCode))

#Begin creating heatmaps

#National prob level

nat_problevel <- get_map(location = c(lon = -96, lat = 38), zoom = 4,
                             maptype = "roadmap", scale = 2)

ggmap(nat_problevel) +
  geom_point(data = climate_change_us_problevel, aes(x = longitude, y = latitude, colour = Prob.level, alpha = 0.9), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  scale_colour_continuous(low = "blue", high = "red", space = "Lab", guide = "colorbar")


#Florida prob level map
florida_problevel <- get_map(location = c(lon = -83, lat = 28), zoom = 7,
                 maptype = "roadmap", scale = 2)

ggmap(florida_problevel) +
  geom_point(data = climate_change_us_problevel, aes(x = longitude, y = latitude, colour = Prob.level, alpha = 0.3), size = 5, shape = 20) +
  guides(fill=TRUE, alpha=FALSE, size=FALSE) + 
  scale_colour_continuous(low = "blue", high = "red", space = "Lab", guide = "colorbar")

#Harm level

climate_change_us_harmlevel <- climate_change_us

climate_change_us_harmlevel <- subset(climate_change_us, ,-c(X, Prob.level, personal, ID, Date, cityName, countryCode, countryName, ipAddress,timeZone,zipCode))

#Recoding
#1 - Never
#2 - Not for many years
#3 - In the next few years
#4 - Now

for(i in 1:nrow(climate_change_us_harmlevel)){
  if(climate_change_us_harmlevel$harm.level[i] == "Never"){
    climate_change_us_harmlevel$harm.level[i] = 1
  }
  else if (climate_change_us_harmlevel$harm.level[i] == "Not for many years"){
    climate_change_us_harmlevel$harm.level[i] = 2
  }
  else if (climate_change_us_harmlevel$harm.level[i] == "In the next few years"){
    climate_change_us_harmlevel$harm.level[i] = 3
  }
  else if (climate_change_us_harmlevel$harm.level[i] == "Now"){
    climate_change_us_harmlevel$harm.level[i] = 4
  }
  else{
    print("Irregular value")
  }
}

climate_change_us_harmlevel$harm.level <- as.numeric(climate_change_us_harmlevel$harm.level)

#Florida harm level
harmmap <- get_map(location = c(lon = -83, lat = 28), zoom = 7,
                   maptype = "roadmap", scale = 2)
ggmap(harmmap) +
  geom_point(data = climate_change_us_harmlevel, aes(x = longitude, y = latitude, colour = harm.level, alpha = 0.3), size = 5, shape = 20) +
  guides(fill=TRUE, alpha=FALSE, size=FALSE) + 
  scale_colour_continuous(low = "blue", high = "red", space = "Lab", guide = "colorbar")

