
# setwd("C:/Users/179029/Desktop/TCS TACTICS - DATA ANALYTICS CHALLENGE")
#setwd("C:/DUMPS - CHITRESH - NDLS/TCS TACTICS - DATA ANALYTICS CHALLENGE")
#setwd("C:/CHITRESH - DUMPS/TCS DATA ANALYTICS CHALLENGE") 
options(digits = 20)

## Radius of Earth in Meters 
RADIUSOFEARTH <- 6378100
PI <- 3.14159265359
RADIANCONV <- PI/180


#####################################################################
# Returns Cumulative Distance Values if a dataframe of Longitudes and Latitudes are Passed Into 
# Sum the return value to Obtain the overall Point to Point Distance 


## Distance Function uses the HaverShine Formula to Evaluate The curved Distance 

Distance <- function(gpsdata)
{
        RADIUSOFEARTH <- 6378100
        RADIANCONV <- 3.14159265359/180
        
        long <- gpsdata$Longitude * RADIANCONV
        lat <- gpsdata$Latitude *  RADIANCONV
        distance <- 0
        
        for(idx in 2: length(long))
        {
                dx <- lat[idx] - lat[idx-1]
                dy <- long[idx] - long[idx-1]
                # "semichordlenght" is square of half the chord length between the points.                
                semichordlenght <- sin(dx/2)^2 + cos(lat[idx])*cos(lat[idx-1]) * sin(dy)^2  # 
                # "omega" is angular Distance in Radians        
                omega <- 2 * atan2(sqrt(semichordlenght) , sqrt(1-semichordlenght)) 
                distance <- c(distance , omega*RADIUSOFEARTH  )               
        }
        
        distance
        
}



##########################################################################




library(sqldf)


Inpfilenames <- c( "./data/Siruseri-BusData-4Tactics.csv",
                   "./data/Siruseri-BusData-4Tactics-E1.csv",
                   "./data/Siruseri-BusData-4Tactics-K1.csv",
                   "./data/Siruseri-BusData-4Tactics-K4.csv",
                   "./data/Siruseri-BusData-4Tactics-K9.csv",
                   "./data/Siruseri-BusData-4Tactics-V1.csv"
)

## DATA IMPORT 




DATA01 <- read.csv("./data/Siruseri-BusData-4Tactics.csv")
DATA01 <- transform(DATA01 , MimeType = NA , ObserveationID = NA )


DATA02 <- read.csv("./data/Siruseri-BusData-4Tactics-E1.csv")
DATA03 <- read.csv("./data/Siruseri-BusData-4Tactics-K1.csv")
DATA04 <- read.csv("./data/Siruseri-BusData-4Tactics-K4.csv")
DATA05 <- read.csv("./data/Siruseri-BusData-4Tactics-K9.csv")
DATA06 <- read.csv("./data/Siruseri-BusData-4Tactics-V1.csv")




## RENAMING THE COLUMNS 

names(DATA01) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NumericValue" , "Location" , "MimeType" , "ObserveationID")
names(DATA02) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NumericValue" , "Location" , "MimeType" , "ObserveationID")
names(DATA03) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NumericValue" , "Location" , "MimeType" , "ObserveationID")
names(DATA04) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NumericValue" , "Location" , "MimeType" , "ObserveationID")
names(DATA05) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NumericValue" , "Location" , "MimeType" , "ObserveationID")
names(DATA06) <- c("TimeStamp" , "ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NumericValue" , "Location" , "MimeType" , "ObserveationID")

## Removing Lines with No TimeStamp Entries 


index <- complete.cases(DATA01[,c("TimeStamp")])
DATA01 <- DATA01[index ,]
DATA01 <- DATA01[ !(DATA01$TimeStamp == "null") , ]
index <- grepl(" "  ,DATA01$TimeStamp)
DATA01 <- DATA01[index ,]
rm("index")


index <- complete.cases(DATA02[,c("TimeStamp")])
DATA02 <- DATA02[index ,]
rm("index")


index <- complete.cases(DATA03[,c("TimeStamp")])
DATA03 <- DATA03[index ,]
rm("index")


index <- complete.cases(DATA04[,c("TimeStamp")])
DATA04 <- DATA04[index ,]
rm("index")


index <- complete.cases(DATA05[,c("TimeStamp")])
DATA05 <- DATA05[index ,]
rm("index")


index <- complete.cases(DATA06[,c("TimeStamp")])
DATA06 <- DATA06[index ,]
rm("index")


## Converting TimeStamp values to POSIXlt Class

DATA01$TimeStamp <- as.POSIXlt(DATA01$TimeStamp)
DATA02$TimeStamp <- as.POSIXlt(DATA02$TimeStamp)
DATA03$TimeStamp <- as.POSIXlt(DATA03$TimeStamp)
DATA04$TimeStamp <- as.POSIXlt(DATA04$TimeStamp)
DATA05$TimeStamp <- as.POSIXlt(DATA05$TimeStamp)
DATA06$TimeStamp <- as.POSIXlt(DATA06$TimeStamp)

## Changing Values for the Levels for PhenomenonID and FeatureOfIntrest


levels(DATA01$PhenomenonID) <- c("JUNK" ,"JourneyDirection" , "Location" , "RouteID" , "Speed")
levels(DATA01$FeatureOfIntrest) <- c("JUNK" , "00409DFF-FF581762")
DATA01$ProcedureID <- as.character(DATA01$ProcedureID)
DATA01$ProcedureID <- substr(DATA01$ProcedureID  ,56 , 72 )


levels(DATA02$PhenomenonID) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
levels(DATA02$FeatureOfIntrest) <- c("00409DFF-FF581776")
levels(DATA02$ProcedureID) <- "00409DFF-FF581776"


levels(DATA03$PhenomenonID) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
levels(DATA03$FeatureOfIntrest) <- "00409DFF-FF5817C8"
levels(DATA03$ProcedureID) <- "00409DFF-FF5817C8"

levels(DATA04$PhenomenonID) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
levels(DATA04$FeatureOfIntrest) <- c("00409DFF-FF58175F")
levels(DATA04$ProcedureID) <- "00409DFF-FF58175F"


levels(DATA05$PhenomenonID) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
levels(DATA05$FeatureOfIntrest) <- "00409DFF-FF5818B7"
levels(DATA05$ProcedureID) <- "00409DFF-FF5818B7"


levels(DATA06$PhenomenonID) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
levels(DATA06$FeatureOfIntrest) <- "00409DFF-FF581794"
levels(DATA06$ProcedureID) <- "00409DFF-FF581794"




############################################################################################################################

###  Cleaning Up the Latitude and Longitude Data and Merging it With Speed Data for Data Frame DATA01  ###

## First subsetting the location data and thereafter Identifying the GPS data 
## and extracting Longitude and Latitude data from it 

LocationDATA01 <- DATA01[DATA01$PhenomenonID == "Location" ,] 
coordinates <- as.character(LocationDATA01$Location)
Index <- grepl("^POINT" , as.character(coordinates)) 
LocationDATA01 <- LocationDATA01[Index, ] 
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]
LocationDATA01$GPSXY <- coordinates
LocationDATA01 <- LocationDATA01[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ") 
Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})
Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
LocationDATA01$Longitude  <- Longitude
LocationDATA01$Latitude  <- Latitude
LocationDATA01$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA01$TimeStamp))))

## Shortlisting Speed Data

SpeedDATA01 <- DATA01[DATA01$PhenomenonID == "Speed" ,]
SpeedDATA01 <- SpeedDATA01[,c("TimeStamp" , "NumericValue")]
SpeedDATA01$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA01$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA01 . Also adding Direction and Route ID to the data

FinalDATA01 <- merge(LocationDATA01 , SpeedDATA01 , by = "TS" )
FinalDATA01 <- transform(FinalDATA01 , RouteID = "K3" , Direction = "PickUp")
rm("SpeedDATA01","LocationDATA01","Index","Latitude" , "Longitude" ,"coordinates" ,"gpsdata")


#####################################################################################################################


###  Cleaning Up the Latitude and Longitude Data and Merging it With Speed Data for Data Frame DATA02  ###


## First subsetting the location data and thereafter Identifying the GPS data 
## and extracting Longitude and Latitude data from it 


LocationDATA02 <- DATA02[DATA02$PhenomenonID == "Location" ,] 
coordinates <- as.character(LocationDATA02$Location)


Index <- grepl("^POINT" , as.character(coordinates)) 

LocationDATA02 <- LocationDATA02[Index, ] 
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]
LocationDATA02$GPSXY <- coordinates
LocationDATA02 <- LocationDATA02[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ") 
Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})
Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
LocationDATA02$Longitude  <- Longitude
LocationDATA02$Latitude  <- Latitude
LocationDATA02$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA02$TimeStamp))))

## Shortlisting Speed Data

SpeedDATA02 <- DATA02[DATA02$PhenomenonID == "Speed" ,]
SpeedDATA02 <- SpeedDATA02[,c("TimeStamp" , "NumericValue")]
SpeedDATA02$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA02$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA02 . Also adding Direction and Route ID to the data

FinalDATA02 <- merge(LocationDATA02 , SpeedDATA02 , by = "TS" )
FinalDATA02 <- transform(FinalDATA02 , RouteID = "E1" , Direction = "PickUp")
rm("SpeedDATA02","LocationDATA02","Index","Latitude" , "Longitude" ,"coordinates" ,"gpsdata")



###################################################################################################################

###  Cleaning Up the Latitude and Longitude Data and Merging it With Speed Data for Data Frame DATA03  ###


## First subsetting the location data and thereafter Identifying the GPS data 
## and extracting Longitude and Latitude data from it 


LocationDATA03 <- DATA03[DATA03$PhenomenonID == "Location" ,] 
coordinates <- as.character(LocationDATA03$Location)
Index <- grepl("^POINT" , as.character(coordinates)) 
LocationDATA03 <- LocationDATA03[Index, ] 
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]
LocationDATA03$GPSXY <- coordinates
LocationDATA03 <- LocationDATA03[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ") 
Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})
Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
LocationDATA03$Longitude  <- Longitude
LocationDATA03$Latitude  <- Latitude
LocationDATA03$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA03$TimeStamp))))

## Shortlisting Speed Data

SpeedDATA03 <- DATA03[DATA03$PhenomenonID == "Speed" ,]
SpeedDATA03 <- SpeedDATA03[,c("TimeStamp" , "NumericValue")]
SpeedDATA03$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA03$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA03 . Also adding Direction and Route ID to the data

FinalDATA03 <- merge(LocationDATA03 , SpeedDATA03 , by = "TS" )
FinalDATA03 <- transform(FinalDATA03 , RouteID = "K1" , Direction = "PickUp")
rm("SpeedDATA03","LocationDATA03","Index","Latitude" , "Longitude" ,"coordinates" ,"gpsdata")






######################################################################################################################



###  Cleaning Up the Latitude and Longitude Data and Merging it With Speed Data for Data Frame DATA04  ###


## First subsetting the location data and thereafter Identifying the GPS data 
## and extracting Longitude and Latitude data from it 


LocationDATA04 <- DATA04[DATA04$PhenomenonID == "Location" ,] 
coordinates <- as.character(LocationDATA04$Location)
Index <- grepl("^POINT" , as.character(coordinates)) 
LocationDATA04 <- LocationDATA04[Index, ] 
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]
LocationDATA04$GPSXY <- coordinates
LocationDATA04 <- LocationDATA04[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ") 
Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})
Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
LocationDATA04$Longitude  <- Longitude
LocationDATA04$Latitude  <- Latitude
LocationDATA04$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA04$TimeStamp))))

## Shortlisting Speed Data

SpeedDATA04 <- DATA04[DATA04$PhenomenonID == "Speed" ,]
SpeedDATA04 <- SpeedDATA04[,c("TimeStamp" , "NumericValue")]
SpeedDATA04$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA04$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA04 . Also adding Direction and Route ID to the data

FinalDATA04 <- merge(LocationDATA04 , SpeedDATA04 , by = "TS" )
FinalDATA04 <- transform(FinalDATA04 , RouteID = "K4" , Direction = "PickUp")

rm("SpeedDATA04","LocationDATA04","Index","Latitude" , "Longitude" ,"coordinates" ,"gpsdata")


######################################################################################################################


###  Cleaning Up the Latitude and Longitude Data and Merging it With Speed Data for Data Frame DATA05  ###


## First subsetting the location data and thereafter Identifying the GPS data 
## and extracting Longitude and Latitude data from it 


LocationDATA05 <- DATA05[DATA05$PhenomenonID == "Location" ,] 
coordinates <- as.character(LocationDATA05$Location)
Index <- grepl("^POINT" , as.character(coordinates)) 
LocationDATA05 <- LocationDATA05[Index, ] 
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]
LocationDATA05$GPSXY <- coordinates
LocationDATA05 <- LocationDATA05[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ") 
Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})
Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
LocationDATA05$Longitude  <- Longitude
LocationDATA05$Latitude  <- Latitude
LocationDATA05$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA05$TimeStamp))))

## Shortlisting Speed Data

SpeedDATA05 <- DATA05[DATA05$PhenomenonID == "Speed" ,]
SpeedDATA05 <- SpeedDATA05[,c("TimeStamp" , "NumericValue")]
SpeedDATA05$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA05$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA05 . Also adding Direction and Route ID to the data

FinalDATA05 <- merge(LocationDATA05 , SpeedDATA05 , by = "TS" )
FinalDATA05 <- transform(FinalDATA05 , RouteID = "K9" , Direction = "PickUp")
rm("SpeedDATA05","LocationDATA05","Index","Latitude" , "Longitude" ,"coordinates" ,"gpsdata")


#######################################################################################################################


###  Cleaning Up the Latitude and Longitude Data and Merging it With Speed Data for Data Frame DATA06  ###


## First subsetting the location data and thereafter Identifying the GPS data 
## and extracting Longitude and Latitude data from it 


LocationDATA06 <- DATA06[DATA06$PhenomenonID == "Location" ,] 
coordinates <- as.character(LocationDATA06$Location)
Index <- grepl("^POINT" , as.character(coordinates)) 
LocationDATA06 <- LocationDATA06[Index, ] 
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]
LocationDATA06$GPSXY <- coordinates
LocationDATA06 <- LocationDATA06[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ") 
Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})
Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
LocationDATA06$Longitude  <- Longitude
LocationDATA06$Latitude  <- Latitude
LocationDATA06$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA06$TimeStamp))))

## Shortlisting Speed Data

SpeedDATA06 <- DATA06[DATA06$PhenomenonID == "Speed" ,]
SpeedDATA06 <- SpeedDATA06[,c("TimeStamp" , "NumericValue")]
SpeedDATA06$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA06$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA06 . Also adding Direction and Route ID to the data

FinalDATA06 <- merge(LocationDATA06 , SpeedDATA06 , by = "TS" )
FinalDATA06 <- transform(FinalDATA06 , RouteID = "V1" , Direction = "PickUp")
rm("SpeedDATA06","LocationDATA06","Index","Latitude" , "Longitude" ,"coordinates" ,"gpsdata")









##############################


## TCS BUS DATA - Consolidating data for all files in a single Variable


TCSBUSMASTERDATA <- rbind(DATA01 ,DATA02 ,DATA03 , DATA04 ,DATA05 ,DATA06 )
data <- TCSBUSMASTERDATA



######################################################################


## EXPLORATORY ANALYSIS 

#####################################################################
## Speed Data
SpeedData <- subset(data , PhenomenonID == "Speed" )

## Location Data
LocationData <- subset(data , PhenomenonID == "Location")
LocationData <- LocationData[complete.cases(LocationData[,c("Longitude" , "Latitude")]) , ]


## Joining Speed and Location Data to Create a Single Data Set

locdata <- LocationData[,c("TimeStamp" , "Longitude" , "Latitude" , "TS")]
spddata <- SpeedData[,c("TimeStamp" ,"NumericValue" , "TS")]
locdata$TS <- as.numeric(locdata$TS)
spddata$TS <- as.numeric(spddata$TS)
SpeedLocationData <- merge(spddata , locdata , by = "TS")
SpeedLocationData <- SpeedLocationData[,-c(4)]
names(SpeedLocationData) <- c("TS" , "TimeStamp" , "Speed(MPH)"  ,"Longitude" , "Latitude")

###
SPDLOCATIONNDATA <- SpeedLocationData
rm("LocationData" ,"locdata" ,"spddata" ,"SpeedLocationData")


####################################### Feb 12th , 2015#################################3


## Following Line can Identify the Junk data in the Location Field 

Index <- grepl("POINT" , as.character(data$Location))
JunkLocationData <- data[Index ,]

## 

testdata <- DATA01[DATA01$TimeStamp$year == (2014-1900) , ]
testdata <- testdata[DATA01$TimeStamp$mon == 0 , ]
testdata <- testdata[DATA01$TimeStamp$mday == 1 , ]
Jan012014 <- testdata
testdata <- testdata[,c(1,4,6:7,11:13)]
options(digits = 20)
long <- as.numeric(as.character(testdata$Longitude))
lat <- as.numeric(as.character(testdata$Latitude))
TS <- testdata$TS
gpsdata <- data.frame(TimeStamp = TS , Longitude = long , Latitude = lat)
gpsdata <- gpsdata[complete.cases(gpsdata) , ]






g <-  ggplot(gpsdata, aes(Latitude, Longitude))
g <- g +  geom_point()
g        


dist <- Distance(gpsdata)



###########################Another Attempt with Another Date###############################



testdata <- DATA01[DATA01$TimeStamp$year == (2014-1900) , ]
testdata <- testdata[DATA01$TimeStamp$mon == 0 , ]
testdata <- testdata[DATA01$TimeStamp$mday == 9 , ]
Jan092014 <- testdata
testdata <- testdata[,c(1,4,6:7,11:13)]
long <- testdata$Longitude
lat <- testdata$Latitude
TS <- testdata$TS
plotdata <- data.frame(TimeStamp = TS , Longitude = long , Latitude = lat)
plotdata <- plotdata[complete.cases(plotdata) , ]

plotdata$Longitude <- as.numeric(as.character(plotdata$Longitude ))
plotdata$Latitude <- as.numeric(as.character(plotdata$Latitude ))










