#setwd("C:/DUMPS - CHITRESH - NDLS/TCS TACTICS - DATA ANALYTICS CHALLENGE")
#setwd("C:/CHITRESH - DUMPS/TCS DATA ANALYTICS CHALLENGE") 

paths <- c( "C:/Program Files/R/R-3.1.2/library" , "C:/Program Files/R/R-3.1.2/ImportedLibrary")
.libPaths(paths)

# 
# coordinates <- LocationDATA01$Location
# Index01 <- grepl("POINT" , as.character(coordinates))

# Index02 <- grepl("^[0-9]" , as.character(coordinates))

LocationDATA01 <- DATA01[DATA01$PhenomenonID == "Location" ,]
coordinates <- as.character(LocationDATA01$Location)
Index <- grepl("POINT" , as.character(coordinates))
LocationDATA01 <- LocationDATA01[Index, ]

# LocationDATA01 <- LocationDATA01[!Index02, ]
# coordinates <- LocationDATA01$Location

coordinates[Index] <- substr(coordinates[Index] , start = 7 , stop = nchar(coordinates[Index]) - 1 )
coordinates <- coordinates[Index]


LocationDATA01$GPSXY <- coordinates
LocationDATA01 <- LocationDATA01[,-c(9:10)]
gpsdata <- strsplit(coordinates , " ")

## Longitude - Vertical Lines Running from NORTH pole to SOUTH Pole. (E/W) depending upon
##which side of Prime Meridian you are lying

## Latitude - Horizontal Lines running parallel to Equator. (N/S) depending upon which
## hemisphere you are 

Longitude <- sapply(gpsdata , function(gpsdata) { gpsdata[1]} )
Latitude <-  sapply(gpsdata , function(gpsdata) {gpsdata[2]})

options(digits=4)

Longitude <- as.numeric(Longitude)
Latitude <- as.numeric(Latitude)
qplot(Longitude , Latitude )



LocationDATA01$Latitude <- Latitude
LocationDATA01$Longitude <- Longitude
LocationDATA01$TSinSeconds <- as.integer(floor(as.numeric(as.POSIXct(LocationDATA01$TimeStamp))))

SpeedDATA01 <- DATA01[DATA01$PhenomenonID == "Speed" ,]
SpeedDATA01 <- SpeedDATA01[,c("TimeStamp" , "NumericValue")]
SpeedDATA01$TSinSeconds <- as.integer(floor(as.numeric(as.POSIXct(SpeedDATA01$TimeStamp))))
options(digits=20)

Index <- order(Jan22data$TimeStamp , decreasing = FALSE)
Jan22data <- Jan22data[Index , ]
View(Jan22data)
qplot(TimeStamp , NumericValue , data = Jan22data)
qplot(Longitude ,Latitude )



FinalData01 <- merge(LocationDATA01 , SpeedDATA01, by = c("TSinSecs"))

with(data = LocationDATA01 , plot(Latitude , Longitude , pch = 20 , col = "red"))



## To remove leading and trailing spaces 
gsub("^\\s+|\\s+$", "", X )

####################################################Feb 24th , 2015 ############################################


datawgps <- LocationDATA02[Index, ] 
datawogps <- LocationDATA02[!Index, ] 

gps <- datawogps$Location


Index <- grepl("^POINT" , as.character(gps)) 

gpsdata <- FinalDATA02[ ,c(11,12)]
dist <- Distance(gpsdata)

gpsdata$Distance <- dist


k3lenght <- sum(FinalDATA01$Distance)

data <- subset(FinalDATA01 , TimeStamp.x > '2013-08-01' & TimeStamp.x < '2013-08-02')





wklydata <- list()

for( wkday in 1:7)
        wklydata[[wkday]] <- subset(FinalDATA01 , TimeStamp.x$wday == (wkday-1 ))

wklydata <- apply(wklydata , )

Sun <- wklydata[[1]]
Mon <- wklydata[[2]]
Tue <- wklydata[[3]]
Wed <- wklydata[[4]]
Thu <- wklydata[[5]]
Fri <- wklydata[[6]]
Sat <- wklydata[[7]]

Sun <- transform(Sun , WkDay = "Sunday")
Mon <- transform(Mon , WkDay = "Monday")
Tue <- transform(Tue , WkDay = "Tuesday")
Wed <- transform(Wed , WkDay = "Wednesday")
Thu <- transform(Thu , WkDay = "Thursday")
Fri <- transform(Fri , WkDay = "Friday")
Sat <- transform(Sat , WkDay = "Saturday")

Routek3 <- rbind(Sun , Mon , Tue , Wed , Thu , Fri ,Sat)













