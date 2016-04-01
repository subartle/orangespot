library("dplyr")
library("DT")
library("ggplot2")
library("htmltools")
library("leaflet")
#library("rcharts")
library("shiny")

# DATA STEPS

code.violations <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")

lat.lon <- code.violations[ , c("lat","lon") ]

lat.lon <- na.omit( lat.lon )

# SHINY APP SERVER

shinyServer(function(input, output) {   # , session
  

  
  output$mymap <- renderLeaflet({
    
    # build base map on load
        
    syr.map <- leaflet(data=lat.lon ) %>% 
                addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
                setView(lng=-76.13, lat=43.03, zoom=13) %>%
                setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)

    #  syr.map <- addCircles( syr.map, lng = lat.lon$lon, lat = lat.lon$lat )
    
    syr.map
    
  })
    
})







#    l <- leaflet( data=lat.lon ) %>% 
#      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
#      setView(lng=-76.13, lat=43.03, zoom=13) %>%
#      setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)
#    
#      l <- leafletProxy("mymap", session, data=lat.lon) %>%
#           addCircleMarkers(~lon, ~lat, 
#           radius=~2, 
#           fillOpacity=0.5,
#           color="steel blue", popup="your mom", fillColor = "gray",
#           layerId=paste0("p", 1:nrow(lat.lon))) %>%
#           
#    l
#   })
  
  
  


  
 #  output$table <- DT::renderDataTable({
 #    action <- dataTableAjax(session, clean)
 #    DT::datatable(clean, filter = 'top', server=T, options = list(
 #      pageLength = 10, autoWidth = TRUE, ajax=list(url=action)))
 #  })
  
# }) 