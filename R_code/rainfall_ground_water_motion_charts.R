# Created by @ajantriks for India Water Portal
# Shared under Attribution-ShareAlike Unported license
# Details: <http://ajantriks.github.io/iwp/rainfall_ground_water_motion_charts.html>

# Reading the IMD rainfall data file

r <- read.csv("imd_rainfall_2004-2010_01.csv")
View(r)

# Reorganising all the month columns into a two-columns format with each month and corresponding rainfall value
library(reshape2)

rm <- melt(r, id = c('State', 'District', 'Year'))
View(rm)

# Creating a 'Month' variable with numeric value for each month

rm$Month[rm$variable == "January"] <- "01"
rm$Month[rm$variable == "February"] <- "02"
rm$Month[rm$variable == "March"] <- "03"
rm$Month[rm$variable == "April"] <- "04"
rm$Month[rm$variable == "May"] <- "05"
rm$Month[rm$variable == "June"] <- "06"
rm$Month[rm$variable == "July"] <- "07"
rm$Month[rm$variable == "August"] <- "08"
rm$Month[rm$variable == "September"] <- "09"
rm$Month[rm$variable == "October"] <- "10"
rm$Month[rm$variable == "November"] <- "11"
rm$Month[rm$variable == "December"] <- "12"
View(rm)

# Creating a 'Day' column, generating a 'Date' column and formatting it

rm$Day <- 01
View(rm)
rm$Date <- paste(rm$Day, rm$Month, rm$Year, sep = "-")
rm$Date <- as.Date(rm$Date, format = "%d-%m-%Y")
View(rm)

# Saving the reorganised IMD rainfall file

write.csv(rm, file = "imd_rainfall_2004-2010_02.csv")

# Both re-organised IMD rainfall data file and the ground-water level data file are opened on notepad
# and the district names across both the files are standardised

# After re-organisation and homogenisation of district names, the rainfall and groundwater data files are opened

r <- read.csv("imd_rainfall_2004-2010_02.csv")
g <- read.csv("ground_water_india_2005-09_02.csv")

# The date columns are standardised, and a new column 'ID' is created by pasting together the district name and the date for each row

r$Date <- as.Date(r$Date, format = "%Y-%m-%d")
g$Date <- as.Date(g$Date, format = "%Y/%m/%d")
r$ID <- paste(r$District, r$Date, sep = "-")
g$ID <- paste(g$District, g$Date, sep = "-")
View(r)
View(g)

# Merge

m <- merge(r, g, by.x = "ID", all.x = "TRUE", by.y = "ID")
View(m)

# Dropping rows for years 2004 and 2010

m$Drop <- 0
m$Drop[m$Year.x == 2004] <- 1
m$Drop[m$Year.x == 2010] <- 1
ms <- subset(m, m$Drop == 0)
View(ms)

# Creating new data table with required columns

m2 <- data.frame(ms$State.x, ms$District.x, ms$Date.x, ms$value, ms$Ground_Water_Level, ms$Year.x, ms$variable, ms$Month, ms$Day)
View(m2)

# Renaming columns and saving data table

colnames(m2)[1] <- "State"
colnames(m2)[2] <- "District"
colnames(m2)[3] <- "Date"
colnames(m2)[4] <- "Monthly.Rainfall"
colnames(m2)[5] <- "Groundwater.Level"
colnames(m2)[6] <- "Year"
colnames(m2)[7] <- "Month.Name"
colnames(m2)[8] <- "Month"
colnames(m2)[9] <- "Day"
View(m2)

write.csv(m2, file = "rainfall_ground_water_motion_charts_data.csv")

# Generating the motion chart (including zero values for rainfall and groundwater level)

m <- read.csv("rainfall_ground-water_motion-chart_data.csv")
m$X <- NULL
m$Day <- NULL
m$Month <- NULL
m$Month.Name <- NULL
m$Year <- NULL
View(m)

library(googleVis)
c <- gvisMotionChart(m, idvar="District", timevar="Date")
print(c, file = "rainfall_ground-water_motion-chart.html")

# Generating the motion chart (excluding zero values for rainfall and groundwater level)

m$Drop <- 0
m$Drop[m$Monthly.Rainfall == 0] <- 1
m$Drop[m$Groundwater.Level == 0] <- 1
View(m)
m2 <- subset(m, m$Drop == 0)
View(m2)
m2$Drop <- NULL
c2 <- gvisMotionChart(m2, idvar="District", timevar="Date")
print(c2, file = "rainfall_ground-water_motion-chart_02.html")
