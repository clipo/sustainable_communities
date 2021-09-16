## Extra code for various purposes
#

## download the TIGER files

library(RCurl)
URL <- "https://www2.census.gov/geo/tiger/TIGER2015/CBSA/tl_2015_us_cbsa.zip"
download.file(URL,destfile="data/MSA/MSA.zip",method="libcurl")
unzip(here("data/MSA/MSA.zip"))
msa_Boundary <-readOGR(here("data/MSA"),"tl_2018_us_cbsa") 

data<-read.csv("results/cluster_assignment.csv")
merged <- merge(msa_Boundary,data,by.x="NAME",by.y="X")

### to create a simplified shapefile from the Tiger Files
simplified <- rmapshaper::ms_simplify(msa_Boundary)
writeOGR(simplified, "data/MSA", "simplified_MSA", 
         driver = "ESRI Shapefile") #also you were missing the driver argument

## display results via leaflet
library(leaflet) 
leaflet(merged) %>%
  addPolygons()

library(leaflet) 
pal <- colorFactor(rainbow(10), merged$x,
                   na.color = "transparent")
p_popup <- paste0("<strong>MSA: </strong>", merged$NAME)

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