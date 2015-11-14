# just focus on 10 trips for right now.

first_trips <- data[1:10,]

port_vars <- grep('por',names(first_trips))
slave_vars <- grep('slav', names(first_trips))
first_trips[,port_vars]
first_trips[,slave_vars] # all nas :(
names(data)[port_vars]

test <- geocode(na.omit(first_trips$embport))

grep("mozam", ports$PORT_NAME)
coord1 <- geocode("mozambique")
ports$PORT_NAME[grep("bahia", ports$PORT_NAME)]

ports$PORT_NAME[grep('janeiro', ports$PORT_NAME)]
ports$PORT_NAME[grep('cabinda', ports$PORT_NAME)]
ports$PORT_NAME[grep('helen', ports$PORT_NAME)]
ggplot(data = ports, aes(x = LONGITUDE, y=LATITUDE)) + geom_point() +
  geom_point(data = ports[grep("bahia", ports$PORT_NAME),], color = 'red') + 
  geom_point(data = ports[grep('janeiro', ports$PORT_NAME),], color = 'purple') + 
  geom_point(data = ports[grep('cabinda', ports$PORT_NAME),], color = 'green') + 
  geom_point(data = coord1, aes(x = lon, y = lat), color = 'green') + 
  geom_point(data = coord2, aes(x = lon, y = lat), color = 'blue') + 
  geom_point(data = coord3, aes(x = lon, y = lat), color = 'pink') +
  geom_point(data = coord4, aes(x = lon, y = lat), color = 'yellow') 
  
coord2 <- geocode("bahia")
coord3 <- geocode("pernambuco")
coord4 <- geocode("west central africa and st. helena")

# OK.  first things first, need to clean the port names for everything.
data$embport <- sub(", port unspecified", "", data$embport) 
data$embport <- sub(" \\(location undetermined\\)", "", data$embport) 
embport_coords <- geocode(na.omit(unique(data$embport)), source = 'google') 
embport_coords$name <- na.omit(unique(data$embport))
embport_coords$var <- 'embport'
# so here's a plot of all the ports that the geocode() function was able to get coords for

ggplot(data = ports, aes(x = LONGITUDE, y=LATITUDE)) + geom_point(alpha = .5) +
  geom_point(data = embport_coords, aes(x = lon, y = lat), color = 'red') + 
  geom_text(data= embport_coords, aes(x = lon, y = lat, label = name),color = 'blue')


# OK.  first things first, need to clean the port names for everything.
data$embport2 <- sub(", port unspecified", "", as.character(data$embport2)) 
#data$embport2 <- sub(" \\(location undetermined\\)", "", data$embport) 
embport2_coords <- geocode(na.omit(unique(data$embport2)), source = 'google') 
embport2_coords$name <- na.omit(unique(data$embport2))
embport2_coords$var <- 'embport2'

ggplot(data = ports, aes(x = LONGITUDE, y=LATITUDE)) + geom_point(alpha = .3) +
  geom_point(data = embport2_coords, aes(x = lon, y = lat), color = 'red', alpha = .5) + 
  geom_point(data = embport_coords, aes(x = lon, y = lat), color = 'blue', alpha = .5) +
  geom_point(data = arrport_coords, aes(x= lon, y = lat), color = 'green', alpha = .5)

# OK.  first things first, need to clean the port names for everything.
unique(as.character(data$arrport))
data$arrport <- sub(", port unspecified", "", as.character(data$arrport)) 
data$arrport <- sub(" \\(location undetermined\\)", "", data$arrport) 
arrport_coords <- geocode(na.omit(unique(data$arrport)), source = 'google') 
arrport_coords$name <- na.omit(unique(data$arrport))
arrport_coords$var <- 'arrport'
# OK.  first things first, need to clean the port names for everything.
unique(as.character(data$arrport2))
data$arrport2 <- sub(", port unspecified", "", as.character(data$arrport2)) 
data$arrport2 <- sub(", colony unspecified", "", as.character(data$arrport2)) 
data$arrport2 <- sub(" \\(colony unspecified\\)", "", data$arrport2) 
arrport2_coords <- geocode(na.omit(unique(data$arrport2)), source = 'google') 
arrport2_coords$name <- na.omit(unique(data$arrport2))
arrport2_coords$var <- 'arrport2'


library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points){  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

embport_coords <- na.omit(embport_coords)
embport_coords$country <- coords2country(embport_coords[,c('lon', 'lat')])

embport_coords[which(embport_coords$country == "United States of America"),]
# capital of angola, cape of good hope, some large part of africa, nun river in
# nigeria, brass river is offshoot of rio nun, (Aného,togo), las salinas chile,
# cameroon, ???, ???, ???

embport2_coords <- na.omit(embport2_coords)
embport2_coords$country <- coords2country(embport2_coords[,c('lon', 'lat')])

embport2_coords[which(embport2_coords$country == "United States of America"),]
#capital of angola, cameroon,  see site for windward coast, (Aného,togo)

arrport_coords <- na.omit(arrport_coords)
arrport_coords$country <- coords2country(arrport_coords[,c('lon', 'lat')])
arrport_coords

arrport2_coords <- na.omit(arrport2_coords)
arrport2_coords$country <- coords2country(arrport2_coords[,c('lon', 'lat')])
arrport2_coords

# OK.  first things first, need to clean the port names for everything.
unique(as.character(data$portdep))
data$portdep <- sub(", port unspecified", "", as.character(data$portdep)) 
data$portdep <- sub(", colony unspecified", "", as.character(data$portdep)) 
data$portdep <- sub(" \\(colony unspecified\\)", "", data$portdep) 
portdep_coords <- geocode(na.omit(unique(data$portdep)), source = 'google') 
portdep_coords$name <- na.omit(unique(data$portdep))
portdep_coords$var <- 'portdep'
portdep_coords <- na.omit(portdep_coords)
portdep_coords$country <- coords2country(portdep_coords[,c('lon', 'lat')])
portdep_coords

unique(as.character(data$sla1port))
data$sla1port <- sub(", port unspecified", "", as.character(data$sla1port)) 
data$sla1port <- sub(", region unspecified", "", as.character(data$sla1port)) 
data$sla1port <- sub(", location unspecified", "", as.character(data$sla1port)) 
data$sla1port <- sub(", colony unspecified", "", as.character(data$sla1port)) 
data$sla1port <- sub(" \\(colony unspecified\\)", "", data$sla1port) 
sla1port_coords <- geocode(na.omit(unique(data$sla1port)), source = 'google') 
sla1port_coords$name <- na.omit(unique(data$sla1port))
sla1port_coords$var <- 'sla1port'
sla1port_coords <- na.omit(sla1port_coords)
sla1port_coords$country <- coords2country(sla1port_coords[,c('lon', 'lat')])
sla1port_coords

unique(as.character(data$portret))
data$portret <- sub(", port unspecified", "", as.character(data$portret)) 
portret_coords <- geocode(na.omit(unique(data$portret)), source = 'google') 
portret_coords$name <- na.omit(unique(data$portret))
portret_coords$var <- 'portret'
portret_coords <- na.omit(portret_coords)
portret_coords$country <- coords2country(portret_coords[,c('lon', 'lat')])
portret_coords
