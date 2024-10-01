install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
colnames(datCO2)[4] <- "CO2"

datCC <- read.csv("/cloud/project/activity03/climate-change.csv")

#in-class prompt 1
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere 
#in base R and in ggplot2.
datCC$Day <- as.Date(datCC$Day)
norHem <- datCC[datCC$Entity == "Northern Hemisphere",]
southHem <- datCC[datCC$Entity == "Southern Hemisphere",]

plot(norHem$Day, norHem$temperature_anomaly,
     type = "b",
     xlab = "Date",
     ylab = "Temperature Anomaly (C) in Northern Hemisphere")
plot(southHem$Day, southHem$temperature_anomaly,
     type = "b",
     xlab = "Date",
     ylab = "Temperature Anomaly (C) in Southern Hemisphere")
ggplot(data = datCC, aes(x = Day, y = temperature_anomaly, colour = Entity)) +
  geom_point(alpha = .3) +
  labs(x = "Day", y = "Temperature Anomaly (C)")

#in-class prompt 2
#Plot the total all time emissions for the United States, Mexico, and Canada.
UMC <- filter(datCO2, Entity %in% c("United States","Mexico", "Canada")) %>%
  group_by(Entity) 

ggplot(data = UMC, aes(x = Year, y = CO2, colour = Entity)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "CO2 Emissions") +
  theme_classic()

#bonus ??????
(paste ("CO"[2])

  
View(datCO2$Entity)
#Question 1
#Make a graph that communicates about emissions from any countries of your choice.
#Explain how you considered principles of visualization in making your graph.
us_china <- datCO2 %>%
  filter(Entity == "United States" | Entity == "China")

ggplot(data = us_china, aes(x = Year, y = CO2, colour = Entity)) +
  geom_density(aes(fill = Entity)) +
  geom_line() +
  labs(x = "Year", y = "CO2 Emissions")
  theme_classic()
#change axis units, colors, axis labels, aesthetically pretty
  
#Question 2
#You are tasked with communicating the change in world air temperatures and 
#CO2 emissions to a broad audience in visually appealing graphs. 
#Make two graphs to present in your word document side by side. 
#Plot world CO2 emissions on one graph and world air temperature anomalies on the other graph.
  
#Question 3
#Look up any type of environmental data of your interest in our world in data (link in tutorial). 
#Download the csv and upload it to RStudio Cloud. Remake the graph. 
#You may make the graph exactly as it is or alter it to present the data in a different format. 
#Explain how you considered principles of visualization in making your graph. 
#Explain the main conclusion of the graph.
  
