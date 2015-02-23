
setwd("C:/CHITRESH - DUMPS/TCS DATA ANALYTICS CHALLENGE") 

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


index <- complete.cases(DATA02[,c("TimeStamp")])
DATA02 <- DATA02[index ,]


index <- complete.cases(DATA03[,c("TimeStamp")])
DATA03 <- DATA03[index ,]


index <- complete.cases(DATA04[,c("TimeStamp")])
DATA04 <- DATA04[index ,]


index <- complete.cases(DATA05[,c("TimeStamp")])
DATA05 <- DATA05[index ,]


index <- complete.cases(DATA06[,c("TimeStamp")])
DATA06 <- DATA06[index ,]



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



## Cleaning Up the Latitude and Longitude Data 



Index <- complete.cases(DATA01[,c("Location")])
coordinates <- as.character(DATA01$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(0)
latitude <- character(0)
longitude[Index] <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][1]
latitude[Index]  <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][2]
longitude[!Index] <- NA
latitude[!Index]  <- NA

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA01 <- transform(DATA01 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))

### 

Index <- complete.cases(DATA02[,c("Location")])
coordinates <- as.character(DATA02$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(0)
latitude <- character(0)
longitude[Index] <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][1]
latitude[Index]  <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][2]
longitude[!Index] <- NA
latitude[!Index]  <- NA

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA02 <- transform(DATA02 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))


###########

Index <- complete.cases(DATA03[,c("Location")])
coordinates <- as.character(DATA03$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(0)
latitude <- character(0)
longitude[Index] <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][1]
latitude[Index]  <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][2]
longitude[!Index] <- NA
latitude[!Index]  <- NA

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA03 <- transform(DATA03 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))


#############


Index <- complete.cases(DATA04[,c("Location")])
coordinates <- as.character(DATA04$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(0)
latitude <- character(0)
longitude[Index] <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][1]
latitude[Index]  <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][2]
longitude[!Index] <- NA
latitude[!Index]  <- NA

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA04 <- transform(DATA04 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))



#######

Index <- complete.cases(DATA05[,c("Location")])
coordinates <- as.character(DATA05$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(0)
latitude <- character(0)
longitude[Index] <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][1]
latitude[Index]  <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][2]
longitude[!Index] <- NA
latitude[!Index]  <- NA

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA05 <- transform(DATA05 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))




##########




Index <- complete.cases(DATA06[,c("Location")])
coordinates <- as.character(DATA06$Location)
Index <- grepl("POINT" , as.character(coordinates))
coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
longitude <- character(0)
latitude <- character(0)
longitude[Index] <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][1]
latitude[Index]  <-  strsplit(coordinates[Index], split = " " , fixed = TRUE)[[1]][2]
longitude[!Index] <- NA
latitude[!Index]  <- NA

## Assimilating Longitude and Latitude Data to Original DataFrame

DATA06 <- transform(DATA06 , Longitude = longitude , Latitude = latitude , TS = as.POSIXct(TimeStamp))

##############################


## TCS BUS DATA - Consolidating data for all files in a single Variable


TCSBUSMASTERDATA <- rbind(DATA01 ,DATA02 ,DATA03 , DATA04 ,DATA05 ,DATA06 )
data <- TCSBUSMASTERDATA



######################################################################


EXPLORATORY ANALYSIS 

#####################################################################

 SpeedData <- subset(data , PhenomenonID == "Speed" )
LocationData <- subset(data , PhenomenonID == "Location")

LocationData <- LocationData[complete.cases(LocationData[,c("Longitude" , "Latitude")]) , ]


locdata <- LocationData[,c("TimeStamp" , "Longitude" , "Latitude" , "TS")]
spddata <- SpeedData[,c("TimeStamp" ,"NumericValue" , "TS")]
locdata$TS <- as.numeric(locdata$TS)
spddata$TS <- as.numeric(spddata$TS)


SpeedLocationData <- merge(spddata , locdata , by = "TS")
SpeedLocationData <- SpeedLocationData[,-c(4)]
names(SpeedLocationData) <- c("TS" , "TimeStamp" , "Speed(MPH)"  ,"Longitude" , "Latitude")

SPDLCNDATA <- SpeedLocationData











    





        
        
        
        
        
        
        


























levels(DATA03$PhenomenonID) <- c("JourneyDirection" , "Location" , "RouteID" , "Speed")
CoOrdinateData <- subset(DATA03 , PhenomenonID == "Location")

index <- grepl( "POINT",CoOrdinateData$StText) 

Coordinates <- CoOrdinateData[index ,]

Coordinates$TimeStamp <- as.POSIXlt(Coordinates$TimeStamp)

Coordinates$StText <- gsub("POINT" ,"" , Coordinates$StText )

StText <- character(length = 0)
longitude <- character(0)
latitude <- character(0)

for(count in 1:nrow(Coordinates))
{
        StText[count] <- substr(Coordinates$StText[count] ,2 , nchar(Coordinates$StText[count])-1)
        longitude[count]  <- strsplit( StText[count] , " ")[[1]][1]
        latitude[count] <- strsplit(StText[count] , " ")[[1]][2]
}




Coordinates$StText <- StText



plot(latitude , longitude , pch = 20 , col = "red")








TCSBUSDATA <- rbind(DATA02 , DATA03 , DATA04 , DATA05 , DATA06)
TCSBUSDATA <- TCSBUSDATA[,-c(9:10)]
TCSBUSDATA <- rbind(TCSBUSDATA , DATA01)
names(TCSBUSDATA) <- c("TimeStamp" ," ProcedureID" , "FeatureOfIntrest" ,"PhenomenonID" , "OfferingID" , "TextValue" , "NUmericValue" , "StText")


WorkData <- TCSBUSDATA
WorkData$TimeStamp <- as.POSIXlt(WorkData$TimeStamp)
levels(WorkData$PhenomenonID) <- c("JourneyDirection" , "Location" ,"RouteID" , "Speed" , "NA")
levels(WorkData$FeatureOfIntrest) <- c("00409DFF-FF581776" , "00409DFF-FF5817C8" , "00409DFF-FF58175F"
                                       ,"00409DFF-FF5818B7" , "00409DFF-FF581794" ,"NA" , "00409DFF-FF581762" )










