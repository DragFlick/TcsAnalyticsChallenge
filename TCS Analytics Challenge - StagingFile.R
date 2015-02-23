
DATA01YR2013 <- DATA01[DATA01$TimeStamp$year == 2013-1900 , ]
DATA01YR2014 <- DATA01[DATA01$TimeStamp$year == 2014-1900 , ]

Jan22014 <- DATA01YR2014[DATA01YR2014$TimeStamp$mday == 2 , ]

## Shortlisting Loction Data for Jan 2 , 2014

Jan22014loc <- Jan22014[Jan22014$PhenomenonID == "Location",]
gpsdata <- as.character(Jan22014loc$Location)
Index <- grepl("POINT" , gpsdata)
Jan22014loc <- Jan22014loc[Index,]

gpsdata2Jan2014 <- as.character(Jan22014loc$Location)

gpsdata2Jan2014[Index] <- substr(gpsdata2Jan2014[Index] , start = 7 , 
                                 stop = nchar(gpsdata2Jan2014[Index]) - 1 )


long2Jan <- character(0)
lat2Jan <- character(0)
for(count in 1:length(gpsdata))
{
        long2Jan <- c(long2Jan ,  strsplit(gpsdata2Jan2014[count], split = " " , fixed = TRUE)[[1]][1])
        lat2Jan <-  c(lat2Jan ,   strsplit(gpsdata2Jan2014[count], split = " " , fixed = TRUE)[[1]][2])
}




################################ For Jan 1 ####### 



DATA01YR2013 <- DATA01[DATA01$TimeStamp$year == 2013-1900 , ]
DATA01YR2014 <- DATA01[DATA01$TimeStamp$year == 2014-1900 , ]

Jan12014 <- DATA01YR2014[DATA01YR2014$TimeStamp$mday == 2 , ]

## Shortlisting Loction Data for Jan 1 , 2014

Jan12014loc <- Jan22014[Jan22014$PhenomenonID == "Location",]
gpsdata <- as.character(Jan22014loc$Location)
Index <- grepl("POINT" , gpsdata)
Jan12014loc <- Jan12014loc[Index,]

gpsdata1Jan2014 <- as.character(Jan12014loc$Location)

gpsdata1Jan2014[Index] <- substr(gpsdata1Jan2014[Index] , start = 7 , 
                                 stop = nchar(gpsdata1Jan2014[Index]) - 1 )


long1Jan <- character(0)
lat1Jan <- character(0)
for(count in 1:length(gpsdata))
{
        long1Jan <- c(long1Jan ,  strsplit(gpsdata1Jan2014[count], split = " " , fixed = TRUE)[[1]][1])
        lat1Jan <-  c(lat1Jan ,   strsplit(gpsdata1Jan2014[count], split = " " , fixed = TRUE)[[1]][2])
}

## Comparing Plots # #

par(mfrow = c(1,2))
plot(lat1Jan , long1Jan , pch = 20 , col = "red")
plot(lat2Jan , long2Jan , pch = 20 , col = "green" )

timelag <-difftime( max(Jan12014$TimeStamp) , "2014-01-02 07:35:41 IST" , units = "hours" )  

options(digits = 20)
long <- as.numeric(long1Jan)
lat <- as.numeric(lat1Jan)

Distance <- 0

RADUSOFEARTH <- 6378100
options(digits = 20)
for(count in 1:length(long1Jan))
{
        dist <- sqrt((long[count+1] - long[count])^2 + (lat[count+1] - lat[count])^2)*RADUSOFEARTH
        Distance <- Distance + dist
}

