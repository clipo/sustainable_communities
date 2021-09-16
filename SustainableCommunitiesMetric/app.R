#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options("rgdal_show_exportToProj4_warnings"="none")
library(shiny)
library(leaflet)
library(here)
library("raster")
library("rgdal")
library("sp")

msa_Boundary <-readOGR(".","simplified_MSA") 
data<-read.csv("cluster_assignment.csv")
merged <- merge(msa_Boundary,data,by.x="NAME",by.y="X")
pal <- colorFactor(rainbow(10), merged$x,
                   na.color = "transparent")
p_popup <- paste0("<strong>Community: </strong>", merged$NAME)

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("mymap"),
)
server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet(merged ) %>%
      setView(lat=37.0902, lng=-95.7129,zoom=2)%>%
      addPolygons(
        stroke = FALSE, # remove polygon borders
        fillColor = ~pal(x), # set fill color with function from above and value
        fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
        popup = p_popup) %>%
      addTiles() %>%
      addLegend("bottomleft",pal = pal, values = c(1,2,3,4,5,6,7,8,9,10), 
                layerId = "colorLegend", title="Cluster")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
