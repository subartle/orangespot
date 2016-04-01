
library("dplyr")
library("leaflet")
library("shiny")



# DATA STEPS

code.violations <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")

lat.lon <- code.violations[ 5000:10000 , c("lat","lon") ] # sample for dev purposes

lat.lon <- na.omit( lat.lon )



# SERVER

my.server <- function(input, output) 
{  
  

  
  output$mymap <- renderLeaflet({
    
    # build base map on load
        
    syr.map <- leaflet(data=lat.lon ) %>% 
                addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
                setView(lng=-76.13, lat=43.03, zoom=13) %>%
                setMaxBounds(lng1=-75, lat1=41, lng2=-77,  lat2=45)

    # make up colors for testing purposes
    
    fake.violations <- sample( c("Bed Bugs","Demolition","Trash","Safety"), size=nrow(lat.lon), replace=T )
    col.vec <- ifelse( fake.violations == "Bed Bugs", "orange", NA )

    
    # syr.map <- addCircles( syr.map, lng = lat.lon$lon, lat = lat.lon$lat, col=col.vec )
    
    syr.map <- addCircleMarkers( syr.map, lng = lat.lon$lon, lat = lat.lon$lat, col=col.vec, popup=fake.violations )
    
    syr.map
    
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
              choices=c("None", "Severity")),

            # sliderInput("alpha", label="Opacity:",
            #  min=0, max=1, value=0.4, step=.025, ticks=T),
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
            p(class="topp", "Explore vehicle collisions recorded in Edinburgh",
              "between 2010 and 2013 in this interactive data visualisation."
              ),
            p("Blackspot is written in ",
              a("Shiny,", href="http://shiny.rstudio.com/", target="_blank"),
              "a web application framework for the R language.",
              "Maps are built with ",
              a("leaflet.js", href="http://leafletjs.com/", target="_blank"),
              "via the",
              a("R language bindings,", href="https://rstudio.github.io/leaflet/",
                target="_blank"),
              "and using map data from",
              a("Open Street Map.", href="http://www.openstreetmap.org/copyright",
                target="_blank")
              ),
            p("Project under development by ",
              a("@benjaminlmoore", href="http://twitter.com/benjaminlmoore",
                target="_blank"),
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

)



shinyApp( ui=my.ui, server=my.server )





# addCircles(map, lng = NULL, lat = NULL, radius = 10, layerId = NULL, group = NULL, 
#     stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5, fill = TRUE, 
#     fillColor = color, fillOpacity = 0.2, dashArray = NULL, popup = NULL, 
#     options = pathOptions(), data = getMapData(map))





