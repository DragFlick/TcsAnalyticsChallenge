
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



## Cleaning Up the Latitude and Longitude Data and ## 


coordinates <- as.character(DATA01$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(length(coordinates))
latitude <- character(length(coordinates))

for( count in 1: length(coordinates))
{
        if( Index[count])
        {
                longitude[count] <-  strsplit(coordinates[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(coordinates[count], split = " " , fixed = TRUE )[[1]][2]        
        }
        
        else
        {
                
                longitude[count] <- NA
                latitude[count]  <- NA
        }
}
     
        
DATA01 <- transform(DATA01 , Longitude = as.character(longitude) , Latitude = as.character(latitude) , TS = as.POSIXct(TimeStamp))
rm("Index" , "longitude" ,"latitude" , "coordinates")

### 

Index <- complete.cases(DATA02[,c("Location")])
coordinates <- as.character(DATA02$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(length(coordinates))
latitude <- character(length(coordinates))

for( count in 1: length(coordinates))
{
        if( Index[count])
        {
                longitude[count] <-  strsplit(coordinates[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(coordinates[count], split = " " , fixed = TRUE )[[1]][2]        
        }
        
        else
        {
                
                longitude[count] <- NA
                latitude[count]  <- NA
        }
}

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA02 <- transform(DATA02 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))
rm("Index" , "longitude" ,"latitude" , "coordinates")

###########

Index <- complete.cases(DATA03[,c("Location")])
coordinates <- as.character(DATA03$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(length(coordinates))
latitude <- character(length(coordinates))

for( count in 1: length(coordinates))
{
        if( Index[count])
        {
                longitude[count] <-  strsplit(coordinates[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(coordinates[count], split = " " , fixed = TRUE )[[1]][2]        
        }
        
        else
        {
                
                longitude[count] <- NA
                latitude[count]  <- NA
        }
}

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA03 <- transform(DATA03 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))
rm("Index" , "longitude" ,"latitude" , "coordinates")

#############


Index <- complete.cases(DATA04[,c("Location")])
coordinates <- as.character(DATA04$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(length(coordinates))
latitude <- character(length(coordinates))

for( count in 1: length(coordinates))
{
        if( Index[count])
        {
                longitude[count] <-  strsplit(coordinates[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(coordinates[count], split = " " , fixed = TRUE )[[1]][2]        
        }
        
        else
        {
                
                longitude[count] <- NA
                latitude[count]  <- NA
        }
}

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA04 <- transform(DATA04 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))
rm("Index" , "longitude" ,"latitude" , "coordinates")


#######

Index <- complete.cases(DATA05[,c("Location")])
coordinates <- as.character(DATA05$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(length(coordinates))
latitude <- character(length(coordinates))

for( count in 1: length(coordinates))
{
        if( Index[count])
        {
                longitude[count] <-  strsplit(coordinates[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(coordinates[count], split = " " , fixed = TRUE )[[1]][2]        
        }
        
        else
        {
                
                longitude[count] <- NA
                latitude[count]  <- NA
        }
}




## Assimilating Longitude and Latitude Data to Original DataFrame

DATA05 <- transform(DATA05 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))
rm("Index" , "longitude" ,"latitude" , "coordinates")



##########




Index <- complete.cases(DATA06[,c("Location")])
coordinates <- as.character(DATA06$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(length(coordinates))
latitude <- character(length(coordinates))

for( count in 1: length(coordinates))
{
        if( Index[count])
        {
                longitude[count] <-  strsplit(coordinates[count], split = " " , fixed = TRUE)[[1]][1]
                latitude[count]  <-  strsplit(coordinates[count], split = " " , fixed = TRUE )[[1]][2]        
        }
        
        else
        {
                
                longitude[count] <- NA
                latitude[count]  <- NA
        }
}

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA06 <- transform(DATA06 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))
rm("Index" , "longitude" ,"latitude" , "coordinates")


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










