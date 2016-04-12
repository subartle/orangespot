
library("dplyr")
library("leaflet")
library("shiny")



# DATA STEPS

code.violations <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")
code.severity <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/Severity.csv")

code.violations <- merge(code.violations, code.severity, by.x = "Code", by.y = "Row.Labels", all.x=T )

code.violations$Severity[is.na(code.violations$Severity)] <- "FALSE"  #this makes the NA in severity 0

#Convert to time class
code.violations$Violation.Date <- as.Date(code.violations$Violation.Date,format = "%m/%d/%y")

code.violations$Complaint.Close.Date <- as.Date(code.violations$Complaint.Close.Date, format = "%m/%d/%y")

code.violations$Complaint.Date <- as.Date(code.violations$Complaint.Date, "%m/%d/%y")

code.violations$Comply.By.Date <- as.Date(code.violations$Comply.By.Date, format = "%m/%d/%y")

#new variables representing time between dates and getting rid of negative amounts (due to incorrect data entry)
code.violations <- mutate(code.violations, TimeBetweenOCB = code.violations$Comply.By.Date - code.violations$Violation.Date)
code.violations$TimeBetweenOCB[code.violations$TimeBetweenOCB < 0 ] <- NA

code.violations <- mutate(code.violations, TimeBetweenCV = (code.violations$Complaint.Date - code.violations$Violation.Date) )
code.violations$TimeBetweenCV[code.violations$TimeBetweenCV < 0 ] <- NA

code.violations <- mutate(code.violations, TimeBetweenOC = (code.violations$Complaint.Close.Date - code.violations$Violation.Date))
code.violations$TimeBetweenOC[code.violations$TimeBetweenOC < 0 ] <- NA

code.violations$TimeBetweenOC[is.na(code.violations$TimeBetweenOC)] <- 9999  #this makes the NA in severity 0


#lat.lon
lat.lon <- code.violations[ 5000:10000 , c("lat","lon") ] # sample for dev purposes

lat.lon <- na.omit( lat.lon )

#static color vectors

#color vector open closed
col.vec.open.closed <- NULL
col.vec.open.closed <- ifelse( code.violations$Violation.Status == "Open", "orange", NA)
col.vec.open.closed <- ifelse( code.violations$Violation.Status == "Closed", "gray25", col.vec.open.closed  )

col.vec.severity <- NULL
col.vec.severity <- ifelse( code.violations$Severity == "1", "#FFFF80FF", NA )
col.vec.severity <- ifelse( code.violations$Severity == "2", "#FFFF00FF", col.vec.severity)
col.vec.severity <- ifelse( code.violations$Severity == "3", "#FFAA00FF", col.vec.severity)
col.vec.severity <- ifelse( code.violations$Severity == "4", "#FF5500FF", col.vec.severity)
col.vec.severity <- ifelse( code.violations$Severity == "5", "#FF0000FF", col.vec.severity)
col.vec.severity <- ifelse( code.violations$Severity == "FALSE", "gray10", col.vec.severity)

#color vector time between open closed - TOC

col.vec.TOC <- NULL
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 0 & code.violations$TimeBetweenOC <= 60, "#FF0000FF", NA )
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 60 & code.violations$TimeBetweenOC <= 123, "#FF4000FF", col.vec.TOC)
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 123 & code.violations$TimeBetweenOC <= 186, "#FF8000FF", col.vec.TOC)
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 186 & code.violations$TimeBetweenOC <=249, "#FFBF00FF", col.vec.TOC)
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 249 & code.violations$TimeBetweenOC <= 312, "#FFFF00FF", col.vec.TOC)
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 312 & code.violations$TimeBetweenOC <= 400, "FFFF80FF", col.vec.TOC)
col.vec.TOC <- ifelse( code.violations$TimeBetweenOC == 9999, "gray10", col.vec.TOC)

#pop up 
violation.description <- code.violations$Code 


# SERVER

my.server <- function(input, output) 
{  
  
  col.vec <- reactive({
    
    if( input$color == "Severity" ) 
    {
      col.vec.severity
    }  
    if(input$color == "Open/Closed")
    {
      col.vec.open.closed
    }
    if( input$color == "Time to Close")
    {
      col.vec.TOC
    }
  })
  output$mymap <- renderLeaflet({
    
    # build base map on load
    
    syr.map <- leaflet(data=lat.lon ) %>% 
      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
      setView(lng=-76.13, lat=43.03, zoom=13) %>%
      setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)
 
       
   
    syr.map <- addCircleMarkers( syr.map, lng = lat.lon$lon, lat = lat.lon$lat, col=col.vec, popup = violation.description )
    
  })
}

## UI

my.ui <- navbarPage("Orangespot", id="nav", collapsible=T,
                    
                    tabPanel( "Map",  
                              
                              
                              tags$head(
                                includeScript("./www/analytics.js"),
                                tags$link(rel = "stylesheet", type = "text/css",
                                          href = "ion.rangeSlider.skinFlat.css"),
                                includeScript("./www/spin.min.js"),
                                includeCSS("./www/styles.css")
                              ),
                              
                              
                              
                              # leafletOutput( "mymap" ) #, width="100%", height="100%" )  # not sure why height not working here - check CSS
                              
                              leafletOutput("mymap", width="100%", height="800" ), 
                              
                              
                              
                              
                              
                              absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                                             draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                                             width = 360, height = "auto",
                                             
                                             h2(),
                                             p(class="intro",
                                               strong("Orangespot"), " shows code violations in",
                                               "Syracuse, NY. Data from",
                                               a("SYR Neighborhood and Business Development.",
                                                 href="http://www.syrgov.net/Neighborhood_and_Business_Development.aspx",
                                                 target="_blank")),
                                             
                                             tabsetPanel(
                                               
                                               tabPanel("Controls",
                                                        
                                                        dateRangeInput('dates',
                                                                       label = 'Occurred between:',
                                                                       start = as.Date("2010-01-01"), end = as.Date("2013-07-01")),
                                                        
                                                        selectInput("color", "Colour by:",
                                                                    choices=list("Open/Closed", "Severity", "Time to Close")), #"Days to Comply")),
                                                
                                                        
                                                        sliderInput("slider", label="Severity:",
                                                                    min=1, max=5, value=c(1,5), step=1, ticks=T),
                                                        
                                                        
                                                        checkboxGroupInput("checkGroup", label = "Open/Closed:",
                                                        choices = list("Open" = 1, "Closed" = 2)),
                                                        #
                                                        # fluidRow(
                                                        #   column(6,
                                                        #   sliderInput("base", label="Point size:",
                                                        #      min=1, max=5, value=1)
                                                        #  ),
                                                        #
                                                        #  column(6,
                                                        #    selectInput("scale", label="Scale by:", width=120,
                                                        #      selected="Vehicles",
                                                        #     choices=c("Casualties", "Vehicles"))#)
                                                        #  )
                                                        #),
                                                        
                                                        hr(class="thin"),
                                                        p("Under development by",
                                                          a("@benjaminlmoore", href="http://twitter.com/benjaminlmoore",
                                                            target="_blank"),
                                                          HTML("&bull;"), "See the code on ",
                                                          a("github", href="http://github.com/blmoore/blackspot",
                                                            target="_blank"),
                                                          class="foot")
                                               ),
                                               
                                               # tabPanel("Graphs",
                                               #   #p("Static plots"),
                                               #   plotOutput("monthTotals", height = "110px"),
                                               #   plotOutput("month_waffle", height = "120px"),
                                               #   #hr(),
                                               #   plotOutput("involving", height = "120px", width="100%"),
                                               #   hr(class="thin")
                                               # ),
                                               
                                               tabPanel("About",
                                                        p(class="topp", "Visualize code violations in the city of Syracuse",
                                                          "between 2012 and 2015 in this interactive map."
                                                        ),
                                                        p("Orangespot is written in ",
                                                          a("Shiny,", href="http://shiny.rstudio.com/", target="_blank"),
                                                          "a web application framework for the R language.",
                                                          "Maps are built with ",
                                                          a("leaflet.js", href="http://leafletjs.com/", target="_blank"),
                                                          "via the",
                                                          a("R language bindings,", href="https://rstudio.github.io/leaflet/",
                                                            target="_blank"),
                                                          "and using map data from",
                                                          a("Google Maps.", href="http://www.google.com/maps",
                                                            target="_blank")
                                                        ),
                                                        p("Project under development by ",
                                                          p("Susannah Bartlett & Rory Tikalsky under the guidance of Professor Jesse Lecy, Maxwell School of Citizenship and Public Affairs.",
                                                          HTML("&mdash;"),
                                                          "see the full code on ",
                                                          a("github", href="http://github.com/blmoore/blackspot",
                                                            target="_blank"),
                                                          "or run locally with:"
                                                        ),
                                                        pre("shiny::runGitHub('blmoore/blackspot')"),
                                                        hr(class="thin")
                                               )
                                               # end about panel
                                             ) )
                              
                              
                              
                              
                              
                              
                              
                              
                              
                    ) # end of tabPanel "Map"
                    ))  




shinyApp( ui=my.ui, server=my.server )





# addCircles(map, lng = NULL, lat = NULL, radius = 10, layerId = NULL, group = NULL, 
#     stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = TRUE, 
#     fillColor = color, fillOpacity = 0.2, dashArray = NULL, popup = NULL, 
#     options = pathOptions(), data = getMapData(map))





