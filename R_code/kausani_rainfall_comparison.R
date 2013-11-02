# Created by @ajantriks for India Water Portal
# Shared under Attribution-ShareAlike Unported license
# Details: <http://ajantriks.github.io/iwp/kausani_rainfall.html>

# Reorganising rainfall data for the district of Almora

# Opening the first IMD rainfall data spreadsheet file (pre-converted to .csv format)
d <- read.csv ("imd_states_1-10.csv")
View(d)

# Replace the values of *District.ID* by "Almora" (for easy reference) if the present value is "9" 
# and the corresponding *State.ID* value is "5" (that is the *State.ID* for Uttaranchal)
d$District.ID [d$State.ID == "5" & d$District.ID == "9"] <- "Almora"

# Taking a subset for the district of Almora
a <- subset(d, d$District.ID == "Almora")
View(a)

# Re-organising the IMD rainfall data format to convert "months" from multiple columns to different values taken by the same column
library(reshape2)
am <- melt(a, id = c('State.ID', 'District.ID', 'Year'))
View(am)

# Renaming the months from string to numeric values
am$Month[am$variable == "January"] <- "01"
am$Month[am$variable == "February"] <- "02"
am$Month[am$variable == "March"] <- "03"
am$Month[am$variable == "April"] <- "04"
am$Month[am$variable == "May"] <- "05"
am$Month[am$variable == "June"] <- "06"
am$Month[am$variable == "July"] <- "07"
am$Month[am$variable == "August"] <- "08"
am$Month[am$variable == "September"] <- "09"
am$Month[am$variable == "October"] <- "10"
am$Month[am$variable == "November"] <- "11"
am$Month[am$variable == "December"] <- "12"
View(am)

# Generating a "Day" column, needed to generate dates
am$Day <- "01"

# Generating a "Date' column and converting it into standard *Date* format
am$Date <- paste(am$Day, am$Month, am$Year, sep = "-")
am$Date <- as.Date(am$Date, format = "%d-%m-%Y")
View(am)

# Selecting only the required columns, and renaming them
a2 <- data.frame(am$Date, am$Monthly.Rainfall.Almora, am$Year, am$Month)
View(a2)
colnames(a2)[1] <- "Date"
colnames(a2)[2] <- "Monthly.Rainfall.Almora"
colnames(a2)[3] <- "Year"
colnames(a2)[4] <- "Month"

# Taking a subset of years 1987 to 2002, and saving the final re-organised rainfall data for the district of Almora
a3 <- subset(a2, a2$Year > 1986)
write.csv(a3, file = "almora_rainfall_1987-2002.csv")

# Reorganising rainfall data for Kausani

# Reading the Kausani rainfall data file for 1987 
# (pre-converted to CSV format from the original XLS format and inital correction of wrongly spelt and entered data is done manually)
k <- read.csv("kausani_rainfall_1987_01.csv")
View(k)

# Creating a "Year" column with the uniform value "1987"
k$Year <- "1987"
View(k)

# Reorganising the data to convert months from being multiple columns to different values taken by the same column
library(reshape2)
k2 <- melt(k, id = c('Year', 'X'))
View(k2)

# Replacing absent or incorrect values for the rainfall column
k2$value[k2$value == "-"] <- 0
k2$value[k2$value == "Trace"] <- 0
k2$value[k2$value == ""] <- 0
k2$value[k2$value == "."] <- 0
k2$value[k2$value == "_"] <- 0
View(k2)

# Renaming the months from string to numeric values
k2$Month[k2$variable == "Jan"] <- "01"
k2$Month[k2$variable == "Feb"] <- "02"
k2$Month[k2$variable == "Mar"] <- "03"
k2$Month[k2$variable == "Apr"] <- "04"
k2$Month[k2$variable == "May"] <- "05"
k2$Month[k2$variable == "June"] <- "06"
k2$Month[k2$variable == "July"] <- "07"
k2$Month[k2$variable == "Aug"] <- "08"
k2$Month[k2$variable == "Sept"] <- "09"
k2$Month[k2$variable == "Oct"] <- "10"
k2$Month[k2$variable == "Nov"] <- "11"
k2$Month[k2$variable == "Dec"] <- "12"
View(k2)

# Generating a "Date' column and converting it into standard *Date* format
k2$Date <- paste(k2$X, k2$Month, k2$Year, sep = "-")
k2$Date <- as.Date(k2$Date, format = "%d-%m-%Y")
View(k2)

# Renaming columns, removing the column named "variable", and saving the final reorganised rainfall data file for Kausani (for 1987)
colnames(k2)[2] <- "Day"
colnames(k2)[4] <- "Daily.Rainfall.Kausani"
k2$variable <- NULL
View(k2)
write.csv(k2, file = "kausani_rainfall_1987_02.csv")

# The "kausani_rainfall_1987_02.csv" is opened in Leafpad (or Notepad) and the rows with "NA" values in their "Date" column are removed. 
# These are dates that do not exist (such as 29th February 1987) but were created due to the organisation of the original Kausani rainfall data file.

# Generating the rainfall comparison charts

# Opening the IMD rainfall data file for Almora and taking the subset for "1987"
a <- read.csv("almora_rainfall_1987-2002.csv")
a2 <- subset(a, a$Year == "1987")
View(a2)

# Generating daily average rainfall for each month (divided by 29 for the month of February during leap years)
a2$Daily.Rainfall.Almora[a2$Month == "1"] <- with(a2, Monthly.Rainfall.Almora[Month == "1"]/31)
a2$Daily.Rainfall.Almora[a2$Month == "2"] <- with(a2, Monthly.Rainfall.Almora[Month == "2"]/28)
a2$Daily.Rainfall.Almora[a2$Month == "3"] <- with(a2, Monthly.Rainfall.Almora[Month == "3"]/31)
a2$Daily.Rainfall.Almora[a2$Month == "4"] <- with(a2, Monthly.Rainfall.Almora[Month == "4"]/30)
a2$Daily.Rainfall.Almora[a2$Month == "5"] <- with(a2, Monthly.Rainfall.Almora[Month == "5"]/31)
a2$Daily.Rainfall.Almora[a2$Month == "6"] <- with(a2, Monthly.Rainfall.Almora[Month == "6"]/30)
a2$Daily.Rainfall.Almora[a2$Month == "7"] <- with(a2, Monthly.Rainfall.Almora[Month == "7"]/31)
a2$Daily.Rainfall.Almora[a2$Month == "8"] <- with(a2, Monthly.Rainfall.Almora[Month == "8"]/31)
a2$Daily.Rainfall.Almora[a2$Month == "9"] <- with(a2, Monthly.Rainfall.Almora[Month == "9"]/30)
a2$Daily.Rainfall.Almora[a2$Month == "10"] <- with(a2, Monthly.Rainfall.Almora[Month == "10"]/31)
a2$Daily.Rainfall.Almora[a2$Month == "11"] <- with(a2, Monthly.Rainfall.Almora[Month == "11"]/30)
a2$Daily.Rainfall.Almora[a2$Month == "12"] <- with(a2, Monthly.Rainfall.Almora[Month == "12"]/31)
View(a2)

# Reformatting the "Date" column as standard R date column
a2$Date <- as.Date(a2$Date, format = "%Y-%m-%d")
View(a2)

# Opening the (reorganised) Kausani rainfall data file for 1987
k <- read.csv("kausani rainfall data/kausani_rainfall_1987_02.csv")
View(k)

# Reformatting the "Date" column as standard R date column
k$Date <- as.Date(k$Date, format = "%Y-%m-%d")
View(k)

# Merging Almora and Kausani rainfall data files using the *Date* variable to match the rows 
# The *all.x = "TRUE"* commands ensures that all values of *Date* in the Kausani rainfall data file is kept in the merged file
m <- merge(k, a2, by.x = "Date", all.x = "TRUE", by.y = "Date")
View(m)

# Removing columns not needed anymore, and renaming some other columns
m$X.x <- NULL
m$X.y <- NULL
m$X.1 <- NULL
m$Year.y <- NULL
m$Month.y <- NULL
colnames(m)[2] <- "Year"
colnames(m)[5] <- "Month"
View(m)

# Replacing the "Daily.Rainfall.Almora" values for all days of each month by the average "Daily.Rainfall.Almora" (for that month) value
# calculated earlier and stored for the first day of that month
m$Daily.Rainfall.Almora[m$Month == "1"] <- with(m, Daily.Rainfall.Almora[Month == "1" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "2"] <- with(m, Daily.Rainfall.Almora[Month == "2" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "3"] <- with(m, Daily.Rainfall.Almora[Month == "3" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "4"] <- with(m, Daily.Rainfall.Almora[Month == "4" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "5"] <- with(m, Daily.Rainfall.Almora[Month == "5" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "6"] <- with(m, Daily.Rainfall.Almora[Month == "6" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "7"] <- with(m, Daily.Rainfall.Almora[Month == "7" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "8"] <- with(m, Daily.Rainfall.Almora[Month == "8" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "9"] <- with(m, Daily.Rainfall.Almora[Month == "9" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "10"] <- with(m, Daily.Rainfall.Almora[Month == "10" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "11"] <- with(m, Daily.Rainfall.Almora[Month == "11" & Day == "1"])
m$Daily.Rainfall.Almora[m$Month == "12"] <- with(m, Daily.Rainfall.Almora[Month == "12" & Day == "1"])
View(m)

# Plotting Almora and Kausani rainfall data together
library(ggplot2)
library(scales)
ggplot(m, aes(x = Date)) + geom_bar(stat = "identity", aes(y = Daily.Rainfall.Almora), fill = "#636363", colour = "#636363") + geom_bar(stat = "identity", aes(y = Daily.Rainfall.Kausani), fill = "#6195ED", colour = "#6195ED") + scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b-%Y")) + scale_y_continuous(limits = c(0, 175), breaks = c(0, 10, 20, 30, 40, 50, 75, 100, 125, 150, 175)) + theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "#eeeeee"), axis.line = element_line(color = "#666666"), axis.title = element_text(size = "18", color = "#666666"), axis.ticks = element_line(color = "#666666"), axis.text = element_text(color = "#666666", size = "16")) + xlab("") + ylab("Rainfall")

