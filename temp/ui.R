library("dplyr")
library("DT")
library("shiny")
library("leaflet")


shinyUI( navbarPage("Orangespot", id="nav", collapsible=T,
  
  tabPanel("Map",  
  
      leafletOutput("mymap", width="100%", height="100%")  ) 
      # div(class="outer",

  ))




 #     tags$head(
 #       includeScript("analytics.js"),
 #       tags$link(rel = "stylesheet", type = "text/css",
 #         href = "ion.rangeSlider.skinFlat.css"),
 #       includeCSS("styles.css")
 #     ),

     


#       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#         draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
#         width = 360, height = "auto",
# 
#         h2(),
#        p(class="intro",
#           strong("Blackspot"), " shows vechicle collisions in",
#           "the city of Edinburgh, UK. Data from",
#           a("Edinburgh Open Data.",
#             href="http://www.edinburghopendata.info/dataset/vehicle-collisions",
#             target="_blank"))
#       ))


  
  #, tabPanel("Table", DT::dataTableOutput("table"))