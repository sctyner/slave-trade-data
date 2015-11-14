# get port locations - source http://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_pageLabel=msi_portal_page_62&pubCode=0015
library(ggplot2)
library(maptools)
test_ports <- readShapePoints(fn = "WPI_Shapefile/WPI")
summary(test_ports)
ports <- data.frame(test_ports)
test_ports$LATITUDE
test_ports$
  
qplot(data= ports, x = LONGITUDE, y = LATITUDE)  

unique(ports$PORT_NAME)

ports$PORT_NAME <- tolower(as.character(ports$PORT_NAME))

library(ggmap)
# attempt to geocode port arrival names:
geo_ports <- geocode(unique(data$portdep))
