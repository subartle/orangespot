
# https://support.google.com/cloud/answer/6158862?hl=en&ref_topic=6262490

# http://stackoverflow.com/questions/34402979/increase-the-api-limit-in-ggmaps-geocode-function-in-r


my.google.dev.api.key <- # your api key here


library(RJSONIO)
library(RCurl)

getGeoData <- function(location)
{

  results.list <- NULL
  
  for( i in 1:length(location) )
  {
    location.i <- gsub(' ','+',location[i] )
  
    geo_data <- getURL( paste( "https://maps.googleapis.com/maps/api/geocode/json?address=",
                        location.i, "&key=", my.google.dev.api.key, sep=""))
                      
    geo_data_list <- fromJSON( geo_data )
  
    results.list[[i]] <- geo_data_list
    
    print(i)
  
  }
 
  return( results.list )
  
}



res3 <- getGeoData(c("San Francisco","Los Angeles","Atlanta, GA", "not a real address"))





getGeocodeStatus <- function( g.list )
{
  geocode.status <- NULL

  for( i in 1:length(g.list) )
  {

    geocode.status <- c( geocode.status, unlist( g.list[[i]] )["status"] )  

  }
  
 # names( geocode.status ) <- NULL
 
 return( geocode.status )

}

getGeocodeStatus( res3 )





getLatLon <- function( g.list )
{
  dat.coords <- NULL

  for( i in 1:length(g.list) )
  {

    if( unlist( g.list[[i]] )["status"] == "OK" )
    {
       lat.lon.temp <- data.frame( lat=unlist( g.list[[i]] )["results.geometry.location.lat"],
                                   lon=unlist( g.list[[i]] )["results.geometry.location.lng"] )
  
       dat.coords <- rbind( dat.coords, lat.lon.temp )  
    }
    
    if( unlist( g.list[[i]] )["status"] != "OK" )
    {
       lat.lon.temp <- data.frame( lat=NA, lon=NA )
  
       dat.coords <- rbind( dat.coords, lat.lon.temp )  
    }    
    
  }
  
 rownames( dat.coords ) <- NULL
 
 return( dat.coords )

}


getLatLon( res3 )





# check solutions

library( ggmap )

geocode( res3 )




# GEOCODE SAMPLE

setwd( "C:/Users/jdlecy/Dropbox/02 - CLASSES/02 - MASTERS/09 - DDM II/Code Violations" )

dat <- read.csv( "Violation Report with lot ID.csv" )

names( dat )

dat2 <- dat[ 1:5, ]

address.for.geocode <- paste( dat2$Address, ", Syracuse, NY", sep="" )

geo.list <- getGeoData( address.for.geocode )

lat.lon <- t( sapply( geo.list, "[[", c(1,1,3,1) ) )








# GEOCODE ALL

setwd( "C:/Users/jdlecy/Dropbox/02 - CLASSES/02 - MASTERS/09 - DDM II/Code Violations" )

dat <- read.csv( "Violation Report with lot ID.csv" )



dat1 <- dat[ 1:10000, ]

address.for.geocode1 <- paste( dat1$Address, ", Syracuse, NY", sep="" )

geo.list1 <- getGeoData( address.for.geocode1 )

geo.coords1 <- getLatLon( geo.list1 )




dat2 <- dat[ 10001:20000, ]

address.for.geocode2 <- paste( dat2$Address, ", Syracuse, NY", sep="" )

geo.list2 <- getGeoData( address.for.geocode2 )

geo.coords2 <- getLatLon( geo.list2 )

write.csv( dat2, "10k to 20k w Lat Lon.csv", row.names=F )




dat3 <- dat[ 20001:30000, ]

address.for.geocode3 <- paste( dat3$Address, ", Syracuse, NY", sep="" )

geo.list3 <- getGeoData( address.for.geocode3 )

geo.coords3 <- getLatLon( geo.list3 )

write.csv( dat3, "20k to 30k w Lat Lon.csv", row.names=F )





dat4 <- dat[ 30001:40000, ]

address.for.geocode4 <- paste( dat4$Address, ", Syracuse, NY", sep="" )

geo.list4 <- getGeoData( address.for.geocode4 )

geo.coords4 <- getLatLon( geo.list4 )

write.csv( dat4, "30k to 40k w Lat Lon.csv", row.names=F )




dat5 <- dat[ 40001:51427, ]

address.for.geocode5 <- paste( dat5$Address, ", Syracuse, NY", sep="" )

geo.list5 <- getGeoData( address.for.geocode5 )

geo.coords5 <- getLatLon( geo.list5 )

write.csv( dat5, "40k to 51k w Lat Lon.csv", row.names=F )






dat1 <- cbind( dat1, geo.coords1 )

dat2 <- cbind( dat2, geo.coords2 )

dat3 <- cbind( dat3, geo.coords3 )

dat4 <- cbind( dat4, geo.coords4 )

dat5 <- cbind( dat5, geo.coords5 )

dat6 <- rbind( dat1, dat2, dat3, dat4, dat5 )


write.csv( dat6, "code violations.csv", row.names=F )
