
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

#pop up 
violation.description <- code.violations$Code 

# SERVER

my.server <- function(input, output) 
{  
  #static color vectors
  
  #color vector open closed
  col.vec.open.closed <- NULL
  col.vec.open.closed <- ifelse( code.violations$Violation.Status == "Open", "orange", NA)
  col.vec.open.closed <- ifelse( code.violations$Violation.Status == "Closed", "blanchedalmond", col.vec.open.closed  )
  
  #color vector severity
  col.vec.severity <- NULL
  col.vec.severity <- ifelse( code.violations$Severity == "1", "thistle", NA )
  col.vec.severity <- ifelse( code.violations$Severity == "2", "plum", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "3", "orchid", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "4", "mediumorchid", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "5", "darkorchid", col.vec.severity)
  col.vec.severity <- ifelse( code.violations$Severity == "FALSE", "whitesmoke", col.vec.severity)
  
  #color vector time between open closed - TOC
  col.vec.TOC <- NULL
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 0 & code.violations$TimeBetweenOC <= 60, "skyblue", NA )
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 60 & code.violations$TimeBetweenOC <= 123, "deepskyblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 123 & code.violations$TimeBetweenOC <= 186, "dodgerblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 186 & code.violations$TimeBetweenOC <=249, "royalblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 249 & code.violations$TimeBetweenOC <= 312, "navy", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC >= 312 & code.violations$TimeBetweenOC <= 400, "midnightblue", col.vec.TOC)
  col.vec.TOC <- ifelse( code.violations$TimeBetweenOC == 9999, "whitesmoke", col.vec.TOC)
   
 colvec <- reactive({
    
    if( input$color == "Severity" ) 
    {
      return(col.vec.severity)
    }  
    if(input$color == "Open/Closed")
    {
      return(col.vec.open.closed)
    }
    if( input$color == "Time to Close")
    {
      return(col.vec.TOC)
    }
  })
  
 #datlat <- reactive({
   
  # if(code.violations$Violation.Date >= input$dates[1] & code.violations$Violation.Date <= input$dates[2])
  # {
  #   return(code.violations$lat)
  # }
  #})
 
 #datlon <- reactive({
   
  # if(code.violations$Violation.Date >= input$dates[1] & code.violations$Violation.Date <= input$dates[2])
  # {
  #   return(code.violations$lon)
  # }
  # })
 
  output$mymap <- renderLeaflet({
    
    # build base map on load
    
    syr.map <- leaflet(data=lat.lon ) %>% 
      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
      setView(lng=-76.13, lat=43.03, zoom=13) %>%
      setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)
  
 
    syr.map <- addCircleMarkers( syr.map, lng = lat.lon$lon, lat = lat.lon$lat, col=colvec(), popup = violation.description )
    
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
                                                                       start = as.Date("2011-01-01"), end = as.Date("2015-12-31")),
                                                        
                                                        selectInput("color", "Colour by:",
                                                                    choices=list("Open/Closed", "Severity", "Time to Close")), #"Days to Comply")),
                                                
                                                        hr(class="thin"),
                                                        p("See About Tab",
                                                          a("", href="",
                                                            target="_blank"),
                                                          HTML("&bull;"), "See the code on ",
                                                          a("github", href="http://github.com/subartle/orangespot",
                                                            target="_blank"),
                                                          class="foot")
                                               ),
                                               
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
                                                          a("github", href="http://github.com/subartle/orangespot",
                                                            target="_blank"),
                                                          "or run locally with:"
                                                        ),
                                                        pre("shiny::runGitHub('subartle/orangespot')"),
                                                        hr(class="thin")
                                               )
                                               # end about panel
                                             ) )
        
                    ) # end of tabPanel "Map"
                    ))  
shinyApp( ui=my.ui, server=my.server )
