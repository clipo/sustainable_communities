#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(here)
library("raster")
library("rgdal")
library("sp")
library("RCurl")
library(RCurl)

#URL <- "https://www2.census.gov/geo/tiger/TIGER2018/CBSA/tl_2018_us_cbsa.zip"
#download.file(URL,destfile=here("data/MSA/MSA.zip"),method="libcurl")
#unzip(here("data/MSA/MSA.zip"))
msa_Boundary <-readOGR(".","tl_2018_us_cbsa") 
merged <- merge(msa_Boundary,data,by.x="NAME",by.y="X")
spplot(merged,"x")
pal <- colorFactor(rainbow(8), merged$x,
                   na.color = "transparent")
p_popup <- paste0("<strong>MSA: </strong>", merged$NAME)

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "This button doesn't do anything.")
)
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(merged) %>%
      addPolygons(
        stroke = FALSE, # remove polygon borders
        fillColor = ~pal(x), # set fill color with function from above and value
        fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
        popup = p_popup) %>%
      addTiles() %>%
      addLegend("bottomright",  # location
                pal=pal,    # palette function
                values=~x,  # value to be passed to palette function
                title = 'Cluster') # legend title
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
