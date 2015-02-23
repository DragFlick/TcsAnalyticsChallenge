
## Setting the Pre-requsites 
setwd("C:/CHITRESH - DUMPS/TCS DATA ANALYTICS CHALLENGE") 
install.packages("maptools")
library(maptools)


TCSBUSDATA <- read.csv("./sample-Siruseri-Bus-data.csv")

TCSBUSDATA$time_stamp <- as.POSIXlt(TCSBUSDATA$time_stamp)
levels(TCSBUSDATA$procedure_id) <- "Bus:00409DFF-FF581762"
levels(TCSBUSDATA$feature_of_interest_id) <- "00409DFF-FF581762"
levels(TCSBUSDATA$phenomenon_id) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
TCSBUSDATA <- TCSBUSDATA[ ,-5]


gps_data <- (TCSBUSDATA$st_astext)
levels(gps_data)[1] <- NA
gps_data <- as.character(gps_data)
indexNA <- is.na(gps_data)
gps_data[!indexNA] <- substring(as.character(gps_data[!indexNA]) , first = 7 , last = nchar(as.character(gps_data[!indexNA])  , type = "chars" , allowNA = FALSE)-1) 
        

longitude <- character(length = 0)
latitude <- character(length = 0)

for(count in 1:length(gps_data))
{
        if ( !is.na(gps_data[count]))
        {
                longitude[count] <-  strsplit(gps_data[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(gps_data[count], split = " " , fixed = TRUE)[[1]][2]
        }
                
        else
        {
                longitude[count] <- NA
                latitude[count]  <- NA
                
        }
                
                
}

TCSBUSDATA <- transform( TCSBUSDATA , longitude = longitude , latitude = latitude )
TCSBUSDATA <- TCSBUSDATA[ ,-7]



long_data <- longitude[!is.na(longitude)]
lat_data <- latitude[!is.na(latitude)]


##########################################################


Jan21LongData <- longitude[TCSBUSDATA$TimeStamp < "2014-01-22" ]
Jan21LatData <- latitude[TCSBUSDATA$TimeStamp < "2014-01-22" ]
Jan22LongData <- longitude[TCSBUSDATA$TimeStamp > "2014-01-22" & TCSBUSDATA$TimeStamp < "2014-01-23" ]
Jan22LatData <- latitude[TCSBUSDATA$TimeStamp > "2014-01-22" & TCSBUSDATA$TimeStamp < "2014-01-23" ]
Jan21LongData <- Jan21LongData[!is.na(Jan21LongData)]
Jan21LatData <- Jan21LatData[!is.na(Jan21LatData)]
Jan22LongData <- Jan22LongData[!is.na(Jan22LongData)]
Jan22LatData <- Jan22LatData[!is.na(Jan22LatData)]

## Numeric Conversions 

Jan21LongData <- as.numeric(Jan21LongData)
Jan21LatData <- as.numeric(Jan21LatData)
Jan22LongData <- as.numeric(Jan22LongData)
Jan22LatData <- as.numeric(Jan22LatData)

##############################################################



names(TCSBUSDATA) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntres" , "PhenomenonID" , "TextValue" , "NumericValue" ,"Longitude" ,"Latitude" )

workdata <- TCSBUSDATA[, -c(2,3)]
Jan21 <- workdata[ indexJan21 <- workdata$TimeStamp < "2014-01-22" , ]
Jan22 <- workdata[ indexJan22 <- workdata$TimeStamp > "2014-01-22" & workdata$TimeStamp < "2014-01-23",]



## As  Character 
long_data_Jan21 <- as.character(Jan21$Longitude[!is.na(Jan21$Longitude)])
lat_data_Jan21 <- as.character(Jan21$Latitude[!is.na(Jan21$Latitude)])
long_data_Jan22 <- as.character(Jan22$Longitude[!is.na(Jan22$Longitude)])
lat_data_Jan22 <- as.character(Jan22$Latitude[!is.na(Jan22$Latitude)])


par(mfrow = c(1,2))

plot(lat_data_Jan21 ,long_data_Jan21 , pch = 20 , col = "red" , xlab = "Latitude" , ylab = "Longitude" )
lines(lat_data_Jan21 ,long_data_Jan21 ,  lwd = 1 , col = "red")

points( lat_data_Jan22 ,long_data_Jan22 , pch = 20 , col = "blue")
lines(lat_data_Jan22 , long_data_Jan22 , lwd = 1 , col = "blue")



## plot(long_data_Jan22 , lat_data_Jan22 , pch = 20 )

plot(lat_data_Jan21 ,long_data_Jan21 , pch = 20 , col = "red" , xlab = "Latitude" , ylab = "Longitude" , type = "n")
points( lat_data_Jan22 ,long_data_Jan22 , pch = 20 , col = "blue")
lines(lat_data_Jan22 , long_data_Jan22 , lwd = 1 , col = "blue")

## Plotting data on Time Scale
par(mfrow = c(2,2))

with(data = Jan21 , plot(x = TimeStamp , y = as.numeric(as.character(Longitude)) , pch = 20 , col = "red" , ylab = "Longitude" ))
with(data = Jan21 , plot(x = TimeStamp , y = as.numeric(as.character(Latitude)) , pch = 20 , col = "blue" , ylab = "Latitude" ))

with(data = Jan22 , plot(x = TimeStamp , y = as.numeric(as.character(Longitude)) , pch = 20 , col = "red" , ylab = "Longitude" ))
with(data = Jan22, plot(x = TimeStamp , y = as.numeric(as.character(Latitude)) , pch = 20 , col = "blue" , ylab = "Latitude" ))

