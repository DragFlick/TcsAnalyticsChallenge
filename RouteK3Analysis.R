View(RouteK3Data)
RouteK3WData <- RouteK3Data[ , -c(3:9,14:15)]
names(RouteK3WData)[2] <- "TimeStamp"


WkList <- list()
wkdays <- unique(RouteK3WData$WkDay)
for(day in wkdays)
{
        WkList[[day]] <- subset(RouteK3WData , WkDay == day)
}

Sun <- WkList[["Sunday"]]
Sun <- Sun[order(Sun$TimeStamp) , ]

Sun <- subset( Sun , Distance > 100 )
SunMorn <- subset(Sun , TimeStamp > "2014-01-19 04:00:00" & TimeStamp < "2014-01-19 08:00:00") 
SunMorn <- SunMorn[order(SunMorn$TimeStamp) , ]

dist <- SunMorn$Distance
TimeStamp <- SunMorn$TimeStamp
TS <- SunMorn$TS

TimeStamp <- as.POSIXct(T)



modFit <- loess(dist ~ TS )
TStmp <- sample(TS , size  = 10 , replace = FALSE)

inpdf <- data.frame( TS = TStmp ) 

res <- predict( modFit , newdata = inpdf)




hours <- TimeStamp$hour
min <- TimeStamp$min
secs <- TimeStamp$sec



Mon <- WkList[["Monday"]]
Mon <- Mon[order(Mon$TimeStamp), ]



DyList <- list()
Dates <- unique(Mon$Date)
for(day in as.character(Dates))
{
        DyList[[day]] <- subset(Mon , WkDay == day)
}


DyList <- subset(Mon , WkDay == as.POSIXlt( "2013-11-04 IST"))

