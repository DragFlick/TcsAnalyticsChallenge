
# setwd("C:/Users/179029/Desktop/TCS TACTICS - DATA ANALYTICS CHALLENGE")
#setwd("C:/DUMPS - CHITRESH - NDLS/TCS TACTICS - DATA ANALYTICS CHALLENGE")
#setwd("C:/CHITRESH - DUMPS/TCS DATA ANALYTICS CHALLENGE") 
# paths <- c( "C:/Program Files/R/R-3.1.2/library" , "C:/Program Files/R/R-3.1.2/ImportedLibrary")
#.libPaths(paths)
# library(sqldf)


## INITIAL DECLARATIONS 

## File Path for the Input Files - You may either place the files at the R Working Directory or you may set the path to 
## files here 

#setwd("C:/DUMPS - CHITRESH - NDLS/TCS TACTICS - DATA ANALYTICS CHALLENGE")

RouteK3 <- "./data/Siruseri-BusData-4Tactics.csv"
RouteE1 <- "./data/Siruseri-BusData-4Tactics-E1.csv"
RouteK1 <- "./data/Siruseri-BusData-4Tactics-K1.csv"
RouteK4 <- "./data/Siruseri-BusData-4Tactics-K4.csv"
RouteK9 <- "./data/Siruseri-BusData-4Tactics-K9.csv"
RouteV1 <- "./data/Siruseri-BusData-4Tactics-V1.csv"
 
options(digits = 20)

## Radius of Earth in Meters . You may set the parameter in any Unit and the distance value will be in the same unit 
RADIUSOFEARTH <- 6378100
## Value of PI
PI <- 3.14159265359
## Conversion factor for Converting from Degree to Radian 
RADIANCONV <- PI/180


#####################################################################
# Returns Cumulative Distance Values if a dataframe of Longitudes and Latitudes are Passed Into 
# Sum the return value to Obtain the overall Point to Point Distance 


## Distance Function uses the HaverShine Formula to Evaluate The curved Distance 

DistanceXY <- function(gpsdata)
        
{
        RADIUSOFEARTH <- 6378100
        RADIANCONV <- 3.14159265359/180
        
        if(nrow(gpsdata) > 1)
                
        {
                long <- as.numeric(gpsdata$Longitude) * RADIANCONV
                lat <- as.numeric(gpsdata$Latitude) *  RADIANCONV
                distance <- as.numeric(0)
                
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
        
        else
                distance <- 0
}



## Function to Evaluate Cumulative Distance Covered for that particular Route



CumDist <- function(DaysData)
{
        
        dist <- DaysData$Distance
        TotDist <- numeric(length(dist))
        TotDist[1] <- 0
        
        for ( idx in 2 : length(dist))
        {
                TotDist[idx] <- sum(dist[1:idx])
        }
        
        TotDist
        
}


##########################################################################






## DATA IMPORT - Importing data from CSV Files 




DATA01 <- read.csv(RouteK3)
DATA01 <- transform(DATA01 , MimeType = NA , ObserveationID = NA )


DATA02 <- read.csv(RouteE1)
DATA03 <- read.csv(RouteK1)
DATA04 <- read.csv(RouteK4)
DATA05 <- read.csv(RouteK9)
DATA06 <- read.csv(RouteV1)




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




###################################################################################################

## Adding Date Column to Dataset

DATA01$Date <- as.POSIXlt(substr(as.character(DATA01$TimeStamp) , 1 ,10))
DATA02$Date <- as.POSIXlt(substr(as.character(DATA02$TimeStamp) , 1 ,10))
DATA03$Date <- as.POSIXlt(substr(as.character(DATA03$TimeStamp) , 1 ,10))
DATA04$Date <- as.POSIXlt(substr(as.character(DATA04$TimeStamp) , 1 ,10))
DATA05$Date <- as.POSIXlt(substr(as.character(DATA05$TimeStamp) , 1 ,10))

ts <- as.character(DATA06$TimeStamp)
ts <- substr(ts , 1, 10)
ts <- as.POSIXlt(ts)
DATA06$Date <- ts 

rm("ts")

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

## Creating Input Data Frame for Calculating Distance and then Evaluating Distances between adjoining Points 

gpsdata <- data.frame(Longitude = Longitude , Latitude = Latitude)
# Distance <- DistanceXY(gpsdata)



LocationDATA01$Longitude  <- Longitude
LocationDATA01$Latitude  <- Latitude
LocationDATA01$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA01$TimeStamp))))
# LocationDATA01$Distance <- Distance


## Shortlisting Speed Data

SpeedDATA01 <- DATA01[DATA01$PhenomenonID == "Speed" ,]
SpeedDATA01 <- SpeedDATA01[,c("TimeStamp" , "NumericValue")]
SpeedDATA01$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA01$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA01 . Also adding Direction and Route ID to the data

FinalDATA01 <- merge(LocationDATA01 , SpeedDATA01 , by = "TS" )
FinalDATA01 <- transform(FinalDATA01 , RouteID = "K3" , Direction = "PickUp")




## Categorizing data on the basis of Days of the week ## 



## Categorizing data based on Days of Week ## 


Sun <- NULL;Mon <- NULL ; Tue <- NULL ; Wed <- NULL  
Thu <- NULL ; Fri <- NULL ; Sat <- NULL 
wklydata <- list()
dow <- as.numeric(unique(FinalDATA01$TimeStamp.x$wday))
dow <- dow +1
for( wkday in dow)
        wklydata[[wkday]] <- subset(FinalDATA01 , TimeStamp.x$wday == wkday)

for(idx in dow)
{
        if(idx==1)
                Sun <- wklydata[[idx]]    
        if(idx==2)
                Mon <- wklydata[[idx]] 
        if(idx==3)
                Tue <- wklydata[[idx]] 
        if(idx==4)
                Wed <- wklydata[[idx]] 
        if(idx==5)
                Thu <- wklydata[[idx]] 
        if(idx==6)
                Fri <- wklydata[[idx]]
        if(idx==7)
                Sat <- wklydata[[idx]] 
        
}


if (!is.null(nrow(Sun)))
        if(nrow(Sun) != 0) Sun <- transform(Sun , WkDay = "Sunday")
if (!is.null(nrow(Mon))) 
        if(nrow(Mon) != 0) Mon <- transform(Mon , WkDay = "Monday")
if (!is.null(nrow(Tue)))
        if(nrow(Tue) != 0) Tue <- transform(Tue , WkDay = "Tuesday")
if (!is.null(nrow(Wed))) 
        if(nrow(Wed) != 0) Wed <- transform(Wed , WkDay = "Wednesday")
if (!is.null(nrow(Thu)))
        if(nrow(Thu) != 0) Thu <- transform(Thu , WkDay = "Thursday")
if (!is.null(nrow(Fri)))
        if(nrow(Fri) != 0) Fri <- transform(Fri , WkDay = "Friday")
if (!is.null(nrow(Sat)))
        if(nrow(Sat) != 0) Sat <- transform(Fri , WkDay = "Saturday")





RouteK3 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)
RouteK3 <- transform(RouteK3 , WkDay = as.factor(WkDay))







RouteK3Data <- data.frame()
for (TripDate in as.character(unique(RouteK3$Date)))
{
        data <- subset(RouteK3 , Date == TripDate )
        data <- data[order(data$TimeStamp.x) , ]
        distxy <-DistanceXY(data)
        data$Distance <- distxy
        RouteLength <- CumDist(data)
        data$RouteLength <- RouteLength
        RouteK3Data <- rbind(RouteK3Data , data)
}





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

## Creating Input Data Frame for Calculating Distance and then Evaluating Distances between adjoining Points 

gpsdata <- data.frame(Longitude = Longitude , Latitude = Latitude)
# Distance <- DistanceXY(gpsdata)



LocationDATA02$Longitude  <- Longitude
LocationDATA02$Latitude  <- Latitude
LocationDATA02$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA02$TimeStamp))))
# LocationDATA02$Distance <- Distance

## Shortlisting Speed Data

SpeedDATA02 <- DATA02[DATA02$PhenomenonID == "Speed" ,]
SpeedDATA02 <- SpeedDATA02[,c("TimeStamp" , "NumericValue")]
SpeedDATA02$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA02$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA02 . Also adding Direction and Route ID to the data

FinalDATA02 <- merge(LocationDATA02 , SpeedDATA02 , by = "TS" )
FinalDATA02 <- transform(FinalDATA02 , RouteID = "E1" , Direction = "PickUp")




#### Categorizing data based on Day Of Week 

## Categorizing data based on Days of Week ## 


Sun <- NULL;Mon <- NULL ; Tue <- NULL ; Wed <- NULL  
Thu <- NULL ; Fri <- NULL ; Sat <- NULL 
wklydata <- list()
dow <- as.numeric(unique(FinalDATA02$TimeStamp.x$wday))
dow <- dow +1
for( wkday in dow)
        wklydata[[wkday]] <- subset(FinalDATA02 , TimeStamp.x$wday == wkday)

for(idx in dow)
{
        if(idx==1)
                Sun <- wklydata[[idx]]    
        if(idx==2)
                Mon <- wklydata[[idx]] 
        if(idx==3)
                Tue <- wklydata[[idx]] 
        if(idx==4)
                Wed <- wklydata[[idx]] 
        if(idx==5)
                Thu <- wklydata[[idx]] 
        if(idx==6)
                Fri <- wklydata[[idx]]
        if(idx==7)
                Sat <- wklydata[[idx]] 
        
}


if (!is.null(nrow(Sun)))
        if(nrow(Sun) != 0) Sun <- transform(Sun , WkDay = "Sunday")
if (!is.null(nrow(Mon))) 
        if(nrow(Mon) != 0) Mon <- transform(Mon , WkDay = "Monday")
if (!is.null(nrow(Tue)))
        if(nrow(Tue) != 0) Tue <- transform(Tue , WkDay = "Tuesday")
if (!is.null(nrow(Wed))) 
        if(nrow(Wed) != 0) Wed <- transform(Wed , WkDay = "Wednesday")
if (!is.null(nrow(Thu)))
        if(nrow(Thu) != 0) Thu <- transform(Thu , WkDay = "Thursday")
if (!is.null(nrow(Fri)))
        if(nrow(Fri) != 0) Fri <- transform(Fri , WkDay = "Friday")
if (!is.null(nrow(Sat)))
        if(nrow(Sat) != 0) Sat <- transform(Fri , WkDay = "Saturday")



RouteE1 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)
RouteE1 <- transform(RouteE1 , WkDay = as.factor(WkDay))





RouteE1Data <- data.frame()
for (TripDate in as.character(unique(RouteE1$Date)))
{
        data <- subset(RouteE1 , Date == TripDate )
        data <- data[order(data$TimeStamp.x) , ]
        distxy <-DistanceXY(data)
        data$Distance <- distxy
        RouteLength <- CumDist(data)
        data$RouteLength <- RouteLength
        RouteE1Data <- rbind(RouteE1Data , data)
}






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


gpsdata <- data.frame(Longitude = Longitude , Latitude = Latitude)
# Distance <- DistanceXY(gpsdata)



LocationDATA03$Longitude  <- Longitude
LocationDATA03$Latitude  <- Latitude
LocationDATA03$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA03$TimeStamp))))
# LocationDATA03$Distance <- Distance

## Shortlisting Speed Data

SpeedDATA03 <- DATA03[DATA03$PhenomenonID == "Speed" ,]
SpeedDATA03 <- SpeedDATA03[,c("TimeStamp" , "NumericValue")]
SpeedDATA03$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA03$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA03 . Also adding Direction and Route ID to the data

FinalDATA03 <- merge(LocationDATA03 , SpeedDATA03 , by = "TS" )
FinalDATA03 <- transform(FinalDATA03 , RouteID = "K1" , Direction = "PickUp")



## Categorizing data based on Days of Week ## 


Sun <- NULL;Mon <- NULL ; Tue <- NULL ; Wed <- NULL  
Thu <- NULL ; Fri <- NULL ; Sat <- NULL 
wklydata <- list()
dow <- as.numeric(unique(FinalDATA03$TimeStamp.x$wday))
dow <- dow +1
for( wkday in dow)
        wklydata[[wkday]] <- subset(FinalDATA03 , TimeStamp.x$wday == wkday)

for(idx in dow)
{
        if(idx==1)
                Sun <- wklydata[[idx]]    
        if(idx==2)
                Mon <- wklydata[[idx]] 
        if(idx==3)
                Tue <- wklydata[[idx]] 
        if(idx==4)
                Wed <- wklydata[[idx]] 
        if(idx==5)
                Thu <- wklydata[[idx]] 
        if(idx==6)
                Fri <- wklydata[[idx]]
        if(idx==7)
                Sat <- wklydata[[idx]] 
        
}


if (!is.null(nrow(Sun)))
        if(nrow(Sun) != 0) Sun <- transform(Sun , WkDay = "Sunday")
if (!is.null(nrow(Mon))) 
        if(nrow(Mon) != 0) Mon <- transform(Mon , WkDay = "Monday")
if (!is.null(nrow(Tue)))
        if(nrow(Tue) != 0) Tue <- transform(Tue , WkDay = "Tuesday")
if (!is.null(nrow(Wed))) 
        if(nrow(Wed) != 0) Wed <- transform(Wed , WkDay = "Wednesday")
if (!is.null(nrow(Thu)))
        if(nrow(Thu) != 0) Thu <- transform(Thu , WkDay = "Thursday")
if (!is.null(nrow(Fri)))
        if(nrow(Fri) != 0) Fri <- transform(Fri , WkDay = "Friday")
if (!is.null(nrow(Sat)))
        if(nrow(Sat) != 0) Sat <- transform(Fri , WkDay = "Saturday")



RouteK1 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)
RouteK1 <- transform(RouteK1 , WkDay = as.factor(WkDay))


# rm("wklydata" , "Sun" , "Mon" , "Tue" , "Wed" , "Thu" , "Fri" , "Sat" , "wkday" , "FinalDATA03")


RouteK1Data <- data.frame()
for (TripDate in as.character(unique(RouteK1$Date)))
{
        data <- subset(RouteK1 , Date == TripDate )
        data <- data[order(data$TimeStamp.x) , ]
        distxy <-DistanceXY(data)
        data$Distance <- distxy
        RouteLength <- CumDist(data)
        data$RouteLength <- RouteLength
        RouteK1Data <- rbind(RouteK1Data , data)
}




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


gpsdata <- data.frame(Longitude = Longitude , Latitude = Latitude)
# Distance <- DistanceXY(gpsdata)



LocationDATA04$Longitude  <- Longitude
LocationDATA04$Latitude  <- Latitude
LocationDATA04$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA04$TimeStamp))))
# LocationDATA04$Distance <- Distance




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




### Categorizing data based on Days of the Week ## 


## Categorizing data based on Days of Week ## 


Sun <- NULL;Mon <- NULL ; Tue <- NULL ; Wed <- NULL  
Thu <- NULL ; Fri <- NULL ; Sat <- NULL 
wklydata <- list()
dow <- as.numeric(unique(FinalDATA04$TimeStamp.x$wday))
dow <- dow +1
for( wkday in dow)
        wklydata[[wkday]] <- subset(FinalDATA04 , TimeStamp.x$wday == wkday)

for(idx in dow)
{
        if(idx==1)
                Sun <- wklydata[[idx]]    
        if(idx==2)
                Mon <- wklydata[[idx]] 
        if(idx==3)
                Tue <- wklydata[[idx]] 
        if(idx==4)
                Wed <- wklydata[[idx]] 
        if(idx==5)
                Thu <- wklydata[[idx]] 
        if(idx==6)
                Fri <- wklydata[[idx]]
        if(idx==7)
                Sat <- wklydata[[idx]] 
        
}


if (!is.null(nrow(Sun)))
        if(nrow(Sun) != 0) Sun <- transform(Sun , WkDay = "Sunday")
if (!is.null(nrow(Mon))) 
        if(nrow(Mon) != 0) Mon <- transform(Mon , WkDay = "Monday")
if (!is.null(nrow(Tue)))
        if(nrow(Tue) != 0) Tue <- transform(Tue , WkDay = "Tuesday")
if (!is.null(nrow(Wed))) 
        if(nrow(Wed) != 0) Wed <- transform(Wed , WkDay = "Wednesday")
if (!is.null(nrow(Thu)))
        if(nrow(Thu) != 0) Thu <- transform(Thu , WkDay = "Thursday")
if (!is.null(nrow(Fri)))
        if(nrow(Fri) != 0) Fri <- transform(Fri , WkDay = "Friday")
if (!is.null(nrow(Sat)))
        if(nrow(Sat) != 0) Sat <- transform(Fri , WkDay = "Saturday")




RouteK4 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)
RouteK4 <- transform(RouteK4 , WkDay = as.factor(WkDay))




RouteK4Data <- data.frame()
for (TripDate in as.character(unique(RouteK4$Date)))
{
        data <- subset(RouteK4 , Date == TripDate )
        distxy <-DistanceXY(data)
        data <- data[order(data$TimeStamp.x) , ]
        data$Distance <- distxy
        RouteLength <- CumDist(data)
        data$RouteLength <- RouteLength
        RouteK4Data <- rbind(RouteK4Data , data)
}



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



gpsdata <- data.frame(Longitude = Longitude , Latitude = Latitude)
# Distance <- DistanceXY(gpsdata)



LocationDATA05$Longitude  <- Longitude
LocationDATA05$Latitude  <- Latitude
LocationDATA05$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA05$TimeStamp))))
# LocationDATA05$Distance <- Distance



## Shortlisting Speed Data

SpeedDATA05 <- DATA05[DATA05$PhenomenonID == "Speed" ,]
SpeedDATA05 <- SpeedDATA05[,c("TimeStamp" , "NumericValue")]
SpeedDATA05$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA05$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA05 . Also adding Direction and Route ID to the data

FinalDATA05 <- merge(LocationDATA05 , SpeedDATA05 , by = "TS" )
FinalDATA05 <- transform(FinalDATA05 , RouteID = "K9" , Direction = "PickUp")




## Categorizing Data in Terms of Days of Week 


## Categorizing data based on Days of Week ## 


Sun <- NULL;Mon <- NULL ; Tue <- NULL ; Wed <- NULL  
Thu <- NULL ; Fri <- NULL ; Sat <- NULL 
wklydata <- list()
dow <- as.numeric(unique(FinalDATA05$TimeStamp.x$wday))
dow <- dow +1
for( wkday in dow)
        wklydata[[wkday]] <- subset(FinalDATA05 , TimeStamp.x$wday == wkday)

for(idx in dow)
{
        if(idx==1)
                Sun <- wklydata[[idx]]    
        if(idx==2)
                Mon <- wklydata[[idx]] 
        if(idx==3)
                Tue <- wklydata[[idx]] 
        if(idx==4)
                Wed <- wklydata[[idx]] 
        if(idx==5)
                Thu <- wklydata[[idx]] 
        if(idx==6)
                Fri <- wklydata[[idx]]
        if(idx==7)
                Sat <- wklydata[[idx]] 
        
}


if (!is.null(nrow(Sun)))
        if(nrow(Sun) != 0) Sun <- transform(Sun , WkDay = "Sunday")
if (!is.null(nrow(Mon))) 
        if(nrow(Mon) != 0) Mon <- transform(Mon , WkDay = "Monday")
if (!is.null(nrow(Tue)))
        if(nrow(Tue) != 0) Tue <- transform(Tue , WkDay = "Tuesday")
if (!is.null(nrow(Wed))) 
        if(nrow(Wed) != 0) Wed <- transform(Wed , WkDay = "Wednesday")
if (!is.null(nrow(Thu)))
        if(nrow(Thu) != 0) Thu <- transform(Thu , WkDay = "Thursday")
if (!is.null(nrow(Fri)))
        if(nrow(Fri) != 0) Fri <- transform(Fri , WkDay = "Friday")
if (!is.null(nrow(Sat)))
        if(nrow(Sat) != 0) Sat <- transform(Fri , WkDay = "Saturday")



RouteK9 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)
RouteK9 <- transform(RouteK9 , WkDay = as.factor(WkDay))




RouteK9Data <- data.frame()
for (TripDate in as.character(unique(RouteK9$Date)))
{
        data <- subset(RouteK9 , Date == TripDate )
        data <- data[order(data$TimeStamp.x) , ]
        distxy <-DistanceXY(data)
        data$Distance <- distxy
        RouteLength <- CumDist(data)
        data$RouteLength <- RouteLength
        RouteK9Data <- rbind(RouteK9Data , data)
}


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

gpsdata <- data.frame(Longitude = Longitude , Latitude = Latitude)
# Distance <- DistanceXY(gpsdata)



LocationDATA06$Longitude  <- Longitude
LocationDATA06$Latitude  <- Latitude
LocationDATA06$TS <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA06$TimeStamp))))
# LocationDATA06$Distance <- Distance


## Shortlisting Speed Data

SpeedDATA06 <- DATA06[DATA06$PhenomenonID == "Speed" ,]
SpeedDATA06 <- SpeedDATA06[,c("TimeStamp" , "NumericValue")]
SpeedDATA06$TS <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA06$TimeStamp))))

## Final Speed and Location Data Corresponding to DATA06 . Also adding Direction and Route ID to the data

FinalDATA06 <- merge(LocationDATA06 , SpeedDATA06 , by = "TS" )
FinalDATA06 <- transform(FinalDATA06 , RouteID = "V1" , Direction = "PickUp")



## Categorizing data in days of week 



## Categorizing data based on Days of Week ## 


Sun <- NULL;Mon <- NULL ; Tue <- NULL ; Wed <- NULL  
Thu <- NULL ; Fri <- NULL ; Sat <- NULL 
wklydata <- list()

dow <- as.numeric(unique(FinalDATA06$TimeStamp.x$wday))
dow <- dow +1
for( wkday in dow)
        wklydata[[wkday]] <- subset(FinalDATA06 , TimeStamp.x$wday == wkday)

for(idx in dow)
{
        if(idx==1)
                Sun <- wklydata[[idx]]    
        if(idx==2)
                Mon <- wklydata[[idx]] 
        if(idx==3)
                Tue <- wklydata[[idx]] 
        if(idx==4)
                Wed <- wklydata[[idx]] 
        if(idx==5)
                Thu <- wklydata[[idx]] 
        if(idx==6)
                Fri <- wklydata[[idx]]
        if(idx==7)
                Sat <- wklydata[[idx]] 
        
}


if (!is.null(nrow(Sun)))
        if(nrow(Sun) != 0) Sun <- transform(Sun , WkDay = "Sunday")
if (!is.null(nrow(Mon))) 
        if(nrow(Mon) != 0) Mon <- transform(Mon , WkDay = "Monday")
if (!is.null(nrow(Tue)))
        if(nrow(Tue) != 0) Tue <- transform(Tue , WkDay = "Tuesday")
if (!is.null(nrow(Wed))) 
        if(nrow(Wed) != 0) Wed <- transform(Wed , WkDay = "Wednesday")
if (!is.null(nrow(Thu)))
        if(nrow(Thu) != 0) Thu <- transform(Thu , WkDay = "Thursday")
if (!is.null(nrow(Fri)))
        if(nrow(Fri) != 0) Fri <- transform(Fri , WkDay = "Friday")
if (!is.null(nrow(Sat)))
        if(nrow(Sat) != 0) Sat <- transform(Fri , WkDay = "Saturday")




RouteV1 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)
RouteV1 <- transform(RouteV1 , WkDay = as.factor(WkDay))





RouteV1Data <- data.frame()
for (TripDate in as.character(unique(RouteV1$Date)))
{
        data <- subset(RouteV1 , Date == TripDate )
        data <- data[order(data$TimeStamp.x) , ]
        distxy <-DistanceXY(data)
        data$Distance <- distxy
        RouteLength <- CumDist(data)
        data$RouteLength <- RouteLength
        RouteV1Data <- rbind(RouteV1Data , data)
}




#####################################################################################################################

### Consolidating Route data for all Buses 

TCSBusRouteData <- rbind(RouteK3Data , RouteE1Data , RouteK1Data , RouteK4Data , RouteK9Data , RouteV1Data)
TCSBusRouteData <- TCSBusRouteData[ , -c(5:9 ,11,14)]
names(TCSBusRouteData)[2] <- "TimeStamp"
names(TCSBusRouteData)[8] <- "NumericValue"



######################################################################

### Removing the Staging Variables and Redundant Data 

rm("RouteK3" , "RouteE1" , "RouteK1" , "RouteK4" , "RouteK9" , "RouteV1" ,"DATA01" ,  "DATA02" , "DATA03",
   "DATA04" , "DATA05" , "DATA06","gpsdata","idx" ,"coordinates" ,
   "dow","RouteK3Data" , "RouteE1Data" , "RouteK1Data" ,"RouteK4Data" , "RouteK9Data" ,
   "RouteV1Data","data" , "distxy", "TripDate" , "RouteLength" , "Sun" , "Mon" , "Tue" ,"LocationDATA01",
   "Wed" , "Thu" , "Fri" , "Sat","wkday","wklydata" , "FinalDATA06" ,"FinalDATA05" ,"FinalDATA03" , 
   "FinalDATA04" , "FinalDATA01" , "FinalDATA02" , "Longitude" , "Latitude","Index","SpeedDATA05" , 
   "SpeedDATA04" , "SpeedDATA03" , "SpeedDATA02" , "SpeedDATA01" , "SpeedDATA06","LocationDATA03" , 
   "LocationDATA04" , "LocationDATA05" , "LocationDATA06","index" , "LocationDATA02")

########################################################################################################
