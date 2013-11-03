# Created by @ajantriks for India Water Portal
# Shared under Attribution-ShareAlike Unported license
# Details: <http://ajantriks.github.io/iwp/ground_water.html>

# load the dataset
gw <- read.csv("GW_level_for_whole_country.csv")

# remove the unwanted variables
gw$Id <- NULL
gw$Site.Name <- NULL
gw$Avg <- NULL

# loading library *reshape2* and melting dataset 
# to get unique combinations of state, district, time and ground water
library(reshape2)
gw.m <- melt(gw, id = c("State", "District", "Year"))

# finding average ground water across test sites for each district
gw.m$Ground_Water_Level <- with(gw.m, ave(value, State, District, Year, variable))

# casting and melting to removed unwanted rows
gw.c <- dcast(gw.m, State + District + Year ~ variable, mean)
gw.m <- melt(gw.c, id = c("State", "District", "Year"))

# creating day column
gw.m$day <- 1

# converting alphabetic month column to numeric column
gw.m$month[gw.m$variable == "Jan"] <- 1
gw.m$month[gw.m$variable == "Feb"] <- 2
gw.m$month[gw.m$variable == "Mar"] <- 3
gw.m$month[gw.m$variable == "Apr"] <- 4
gw.m$month[gw.m$variable == "May"] <- 5
gw.m$month[gw.m$variable == "Jun"] <- 6
gw.m$month[gw.m$variable == "Jul"] <- 7
gw.m$month[gw.m$variable == "Aug"] <- 8
gw.m$month[gw.m$variable == "Sep"] <- 9
gw.m$month[gw.m$variable == "Oct"] <- 10
gw.m$month[gw.m$variable == "Nov"] <- 11
gw.m$month[gw.m$variable == "Dec"] <- 12

# combining day, month and year to a single column
gw.m$d <- paste(gw.m$day, gw.m$month, gw.m$Year, sep = "-")
gw.m$Date <- as.Date(gw.m$d, format = "%d-%m-%Y")
gw.m$Date <- as.Date(gw.m$Date)

# remove columns not required anymore
gw.m$variable <- NULL
gw.m$day <- NULL
gw.m$month <- NULL
gw.m$year <- NULL
gw.m$d <- NULL

# renaming the ground water level column
colnames(gw.m)[4] <- "Ground_Water_Level"

# remove rows where ground water level is equal to zero
d <- subset(gw.m, gw.m$Ground_Water_Level != 0)

# take a state-specific subset
d2 <- subset(d, d$State == "ANDHRA PRADESH")

# load libraries *scales* and *ggplot2*
library(scales)
library(ggplot2)

# generating the line chart
ggplot(d2, aes(x = date, y = value, group = District)) + geom_line(colour = "#02A4D3") + ylab("Ground Water Level") + xlab("Year") + scale_x_date(breaks = date_breaks("4 months"), labels = date_format("%b-%y")) + coord_cartesian(xlim = c(as.Date("2005-01-01"), as.Date("2009-12-01")))