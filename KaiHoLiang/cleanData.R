install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")

library(readr)
library(dplyr)
library(tidyr)

data <- read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data_20240721.csv")

head(data)
str(data)
summary(data)

data$Age[is.na(data$Age)] <- 'Unknown'

data <- data %>% distinct()

data$Date <- as.Date(data$Date, format = "%m%d%Y")

data <- data %>% rename(
  Latitude = X,
  Longitude = Y,
  SquirrelID = `Unique Squirrel ID`
)


write_csv(data, "cleaned_squirrel_data.csv")
