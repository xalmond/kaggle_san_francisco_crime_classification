library(lubridate)
library(timeDate)
library(RCurl)
library(RJSONIO)

# Address lng and lat from Google Geocode

address_gps <- function(address) {
  
  root <- "http://maps.google.com/maps/api/geocode/json?address="
  address_url <- URLencode(paste(root, address,", San Francisco, California, USA &sensor=false", sep = ""))
  gps <- fromJSON(address_url,simplify = FALSE)
  if(gps$status=="OK") {
    lng <- gps$results[[1]]$geometry$location$lng
    lat <- gps$results[[1]]$geometry$location$lat
    return(c(lng, lat))
  } else {
    return(c(NA,NA))
  }
}

modify_dataset <- function(df){
  
  # Modyfying outliers modifies X and Y of dataset if X=-120.50000 using column "Address"
  
  df$OldX <- df$X
  df$OldY <- df$Y
  df <- transform(df, X = ifelse(OldX == -120.5000, address_gps(Address)[1], OldX))
  df <- transform(df, Y = ifelse(OldX == -120.5000, address_gps(Address)[2], OldY))
  df$OldX <- NULL
  df$OldY <- NULL
  
  # Modifying position features
  
  df$District <- as.numeric(as.factor(df$PdDistrict))
  
  # Adding feature block(0) vs corner(1)
  
  df$Corner <- as.factor(as.numeric(rowSums(as.data.frame(grepl("/", df$Address)))))
  
  # Adding distances to Police Stations 
  
  df$DistCentral <- sqrt((df$X+122.409895)^2+(df$Y-37.798706)^2) # Distance to Central Police Station
  df$Dist02 <- sqrt((df$X+122.389289)^2+(df$Y-37.772515)^2) # Distance to Southern Police Station
  df$Dist03 <- sqrt((df$X+122.398031)^2+(df$Y-37.729729)^2) # Distance to BayView Police Station
  df$Dist04 <- sqrt((df$X+122.421979)^2+(df$Y-37.762829)^2) # Distance to Mission Police Station
  df$Dist05 <- sqrt((df$X+122.432417)^2+(df$Y-37.780179)^2) # Distance to Northern Police Station
  df$Dist06 <- sqrt((df$X+122.455195)^2+(df$Y-37.767784)^2) # Distance to ParkPolice Station
  df$Dist07 <- sqrt((df$X+122.464477)^2+(df$Y-37.780021)^2) # Distance to Richmond Poli ce Station
  df$Dist08 <- sqrt((df$X+122.446305)^2+(df$Y-37.724773)^2) # Distance to Ingleside Police Station
  df$Dist09 <- sqrt((df$X+122.481471)^2+(df$Y-37.743708)^2) # Distance to Taraval Police Station
  df$Dist10 <- sqrt((df$X+122.412927)^2+(df$Y-37.783663)^2) # Distance to Tenderloin Police Station
  df$DistMin <- apply(df[,10:19],1,min)
  df <- df[,-c(11:19)]
  
  # Modifying date features
  
  df$Year <- as.numeric(year(df$Dates))
  df$Month <- as.numeric(month(df$Dates))
  df$Day <- as.numeric(day(df$Dates))
  df$Hour <- as.numeric(hour(df$Dates))
  df$WeekDay <- as.numeric(wday(df$Dates))
  
  # Adding feature workday(0) vs holiday(1)
  
  df$Holiday <- as.Date(paste(df$Year, "-", df$Month, "-", df$Day, sep = ""))
  df$Holiday <- as.numeric(isHoliday(as.timeDate(df$Holiday)))

  # Adding feature day(0) vs night(1). See http://www.gaisma.com/en/location/san-francisco-california.html
  
  df$Night<- 0
  df$Night[df$Month ==  1 & (df$Hour > 18 | df$Hour < 6)] <- 1
  df$Night[df$Month ==  2 & (df$Hour > 19 | df$Hour < 6)] <- 1
  df$Night[df$Month ==  3 & (df$Hour > 20 | df$Hour < 6)] <- 1
  df$Night[df$Month ==  4 & (df$Hour > 21 | df$Hour < 5)] <- 1
  df$Night[df$Month ==  5 & (df$Hour > 22 | df$Hour < 5)] <- 1
  df$Night[df$Month ==  6 & (df$Hour > 22 | df$Hour < 5)] <- 1
  df$Night[df$Month ==  7 & (df$Hour > 22 | df$Hour < 5)] <- 1
  df$Night[df$Month ==  8 & (df$Hour > 21 | df$Hour < 5)] <- 1
  df$Night[df$Month ==  9 & (df$Hour > 20 | df$Hour < 5)] <- 1
  df$Night[df$Month == 10 & (df$Hour > 19 | df$Hour < 5)] <- 1
  df$Night[df$Month == 11 & (df$Hour > 18 | df$Hour < 5)] <- 1
  df$Night[df$Month == 12 & (df$Hour > 18 | df$Hour < 6)] <- 1
  
  # Adding daily weather conditions
  
  df$DATE <- df$Year*10000+df$Month*100+df$Day
  
  temperature <- read.csv("./data/daily_temperature_san_francisco.csv", header = FALSE)
  temperature$DATE <- temperature$V3*10000+temperature$V1*100+temperature$V2
  df <- merge(x = df, y = temperature, by = "DATE", all.x = TRUE)
  df$Temperature <- df$V4
  
  precipitation <- read.csv("data/daily_precipitation_san_francisco.csv")
  precipitation <- precipitation[precipitation$STATION=="GHCND:USW00023272",]
  df <-  merge(x = df, y = precipitation, by = "DATE", all.x = TRUE)
  
  return(df)
  
}

# Preparation of train dataset

train <- read.csv("data/train.csv", stringsAsFactors = FALSE)

train$Descript <- NULL
train$Resolution <- NULL
train_eng <- modify_dataset(train)
train_eng <- subset(train_eng, , -c(DATE, Dates, DayOfWeek, PdDistrict, Address,
                                    V1, V2, V3, V4, STATION, STATION_NAME, AWND))
write.csv(train_eng,"./data/train_eng.csv", row.names = FALSE)

# Preparation of test dataset

test <- read.csv("data/test.csv", stringsAsFactors = FALSE)

test_eng <- modify_dataset(test)
test_eng <- subset(test_eng, , -c(DATE, Dates, DayOfWeek, PdDistrict, Address,
                                  V1, V2, V3, V4, STATION, STATION_NAME, AWND))
write.csv(test_eng,"./data/test_eng.csv", row.names = FALSE)


