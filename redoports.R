completes <- which(data$fate2 %in% c("Slaves disembarked Americas", "Slaves disembarked in Africa/Europe"))

from_to <- na.omit(data[completes,c('voyageid','ptdepimp', 'mjbyptimp', 'mjslptimp', 'slaximp', 'slamimp')])


library(magrittr)
from_to$ptdepimp %<>% as.character
from_to$mjbyptimp %<>% as.character
from_to$mjslptimp %<>% as.character


port_info_clean <- read.csv("port_info.csv", stringsAsFactors = FALSE)

# fugheddaboutit

# from https://github.com/emory-libraries/voyages/blob/develop/initialdata/geographical.json

library(rjson)
json_file <- "locations_json.txt"
json_data <- fromJSON(file=json_file)

library(plyr)

loc.type <- NULL
for (i in 1:931){
  type1 <- ldply(json_data[[i]][[2]])
  loc.type <- rbind(loc.type, type1)
}

summary(loc.type[,1])

broadregion.idx <- which(loc.type[,1] == 'voyage.broadregion')
place.idx <- which(loc.type[,1] == 'voyage.place')
region.idx <- which(loc.type[,1] == 'voyage.region')



broadregion.info <- NULL
for (i in broadregion.idx){
  info <- ldply(json_data[[i]][[3]])
  broadregion.info <- rbind(broadregion.info, info)
}

summary(broadregion.info$.id)
broadregion.info %<>% arrange(.id)
broadregion.info2 <- data.frame(value = broadregion.info[29:35,2], location.type = 'broad_region', location.name = broadregion.info[1:7,2],
                                latitude = broadregion.info[8:14,2], longitude = broadregion.info[15:21,2])

place.info <- NULL
for (i in place.idx){
  info <- ldply(json_data[[i]][[3]])
  place.info <- rbind(place.info, info)
}
summary(place.info$.id)
place.info %<>% arrange(.id)
place.info2 <- data.frame(value = place.info[4993:5824,2], location.type = 'place', location.name = place.info[1665:2496,2],
           latitude = place.info[1:832,2], longitude = place.info[833:1664,2])

region.info <- NULL
for (i in region.idx){
  info <- ldply(json_data[[i]][[3]])
  region.info <- rbind(region.info, info)
}
summary(region.info$.id)
region.info %<>% arrange(.id)

region.info2 <- data.frame(value = region.info[553:644,2], location.type = 'region', location.name = region.info[277:368,2],
           latitude = region.info[93:184,2], longitude = region.info[185:276,2])
tail(subset(region.info, .id == 'longitude'))

locations.info <- rbind(broadregion.info2, region.info2, place.info2)
locations.info$value %<>% as.character %<>% as.numeric
locations.info$location.name %<>% as.character
locations.info$longitude %<>% as.character %<>% as.numeric
locations.info$latitude %<>% as.character %<>% as.numeric

summary(match(ports, locations.info$location.name))
ports[which(is.na(match(ports, locations.info$location.name)))]

#locations.info$location.name[grep("guad", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Guadaloupe, port unspecified")] <- "Guadeloupe, port unspecified"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Guadaloupe, port unspecified")] <- "Guadeloupe, port unspecified"
from_to$mjslptimp[which(from_to$mjslptimp == "Guadaloupe, port unspecified")] <- "Guadeloupe, port unspecified"

#locations.info$location.name[grep("st. paul", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "St. Paul de Loanda")] <- "St. Paul"
from_to$mjbyptimp[which(from_to$mjbyptimp == "St. Paul de Loanda")] <- "St. Paul"
from_to$mjslptimp[which(from_to$mjslptimp == "St. Paul de Loanda")] <- "St. Paul"

#locations.info$location.name[grep("new york", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "New York, port unspecified")] <- "New York"
from_to$mjbyptimp[which(from_to$mjbyptimp == "New York, port unspecified")] <- "New York"
from_to$mjslptimp[which(from_to$mjslptimp == "New York, port unspecified")] <- "New York"

#locations.info$location.name[grep("mars", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Marseille")] <- "Marseilles"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Marseille")] <- "Marseilles"
from_to$mjslptimp[which(from_to$mjslptimp == "Marseille")] <- "Marseilles"

#locations.info$location.name[grep("saint", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Saint John")] <- "Saint John (Antigua)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Saint John")] <- "Saint John (Antigua)"
from_to$mjslptimp[which(from_to$mjslptimp == "Saint John")] <- "Saint John (Antigua)"

#locations.info$location.name[grep("sene", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Senegal")] <- "French Africa (Goree or Senegal)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Senegal")] <- "French Africa (Goree or Senegal)"
from_to$mjslptimp[which(from_to$mjslptimp == "Senegal")] <- "French Africa (Goree or Senegal)"

#locations.info$location.name[grep("ban", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Bance Island")] <- "Bance Island (Ben's Island)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Bance Island")] <- "Bance Island (Ben's Island)"
from_to$mjslptimp[which(from_to$mjslptimp == "Bance Island")] <- "Bance Island (Ben's Island)"

#locations.info$location.name[grep("cap", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Cape Mount")] <- "Cape Mount (Cape Grand Mount)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Cape Mount")] <- "Cape Mount (Cape Grand Mount)"
from_to$mjslptimp[which(from_to$mjslptimp == "Cape Mount")] <- "Cape Mount (Cape Grand Mount)"

#locations.info$location.name[grep("ano", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Anomabu, Adja, Agga")] <- "Anomabu"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Anomabu, Adja, Agga")] <- "Anomabu"
from_to$mjslptimp[which(from_to$mjslptimp == "Anomabu, Adja, Agga")] <- "Anomabu"

#locations.info$location.name[grep("windward", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Windward + Ivory + Gold +  Benin")] <- "Windward + Ivory + Gold + Benin"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Windward + Ivory + Gold +  Benin")] <- "Windward + Ivory + Gold + Benin"
from_to$mjslptimp[which(from_to$mjslptimp == "Windward + Ivory + Gold +  Benin")] <- "Windward + Ivory + Gold + Benin"

#locations.info$location.name[grep("ses", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Sestos")] <- "Young Sestos"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Sestos")] <- "Young Sestos"
from_to$mjslptimp[which(from_to$mjslptimp == "Sestos")] <- "Young Sestos"

#locations.info$location.name[grep("mala", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Côte de Malaguette")] <- "Côte de Malaguette (runs through to Cape Palmas on Windward Coast)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Côte de Malaguette")] <- "Côte de Malaguette (runs through to Cape Palmas on Windward Coast)"
from_to$mjslptimp[which(from_to$mjslptimp == "Côte de Malaguette")] <- "Côte de Malaguette (runs through to Cape Palmas on Windward Coast)"

#locations.info$location.name[grep("sug", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Sugary")] <- "Sugary (Siekere)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Sugary")] <- "Sugary (Siekere)"
from_to$mjslptimp[which(from_to$mjslptimp == "Sugary")] <- "Sugary (Siekere)"

#locations.info$location.name[grep("sur", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Surinam")] <- "Suriname"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Surinam")] <- "Suriname"
from_to$mjslptimp[which(from_to$mjslptimp == "Surinam")] <- "Suriname"

#locations.info$location.name[grep("cast", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Newcastle")] <- "Newcastle (Nevis)"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Newcastle")] <- "Newcastle (Nevis)"
from_to$mjslptimp[which(from_to$mjslptimp == "Newcastle")] <- "Newcastle (Nevis)"


#locations.info$location.name[grep("vera", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Vera Cruz")] <- "Veracruz"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Vera Cruz")] <- "Veracruz"
from_to$mjslptimp[which(from_to$mjslptimp == "Vera Cruz")] <- "Veracruz"

#locations.info$location.name[grep("spanish", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Spanish Central America, port unspecified")] <- "Spanish Americas, port unspecified"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Spanish Central America, port unspecified")] <- "Spanish Americas, port unspecified"
from_to$mjslptimp[which(from_to$mjslptimp == "Spanish Central America, port unspecified")] <- "Spanish Americas, port unspecified"

#locations.info$location.name[grep("dom", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Santo Domingo, port unspecified")] <- "Saint-Domingue, port unspecified"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Santo Domingo, port unspecified")] <- "Saint-Domingue, port unspecified"
from_to$mjslptimp[which(from_to$mjslptimp == "Santo Domingo, port unspecified")] <- "Saint-Domingue, port unspecified"

#locations.info$location.name[grep("la ", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "La Guiara")] <- "La Guaira"
from_to$mjbyptimp[which(from_to$mjbyptimp == "La Guiara")] <- "La Guaira"
from_to$mjslptimp[which(from_to$mjslptimp == "La Guiara")] <- "La Guaira"

#locations.info$location.name[grep("spanish", ignore.case = T, as.character(locations.info$location.name))]
from_to$ptdepimp[which(from_to$ptdepimp == "Spanish West Indies, colony unspecified")] <- "Spanish Caribbean, unspecified"
from_to$mjbyptimp[which(from_to$mjbyptimp == "Spanish West Indies, colony unspecified")] <- "Spanish Caribbean, unspecified"
from_to$mjslptimp[which(from_to$mjslptimp == "Spanish West Indies, colony unspecified")] <- "Spanish Caribbean, unspecified"

head(from_to)

from_to$yeardep <- data$yeardep[which(data$voyageid %in% from_to$voyageid)]
from_to$yearaf <- data$yearaf[which(data$voyageid %in% from_to$voyageid)]
from_to$sladeath <- from_to$slaximp - from_to$slamimp

from_to <- from_to[-which(from_to$mjslptimp == "???"),]

# merge lat/long data with port names

ports <- data.frame(name = unique(c(unique(from_to$ptdepimp), unique(from_to$mjbyptimp), unique(from_to$mjslptimp))))

head(locations.info)

ports <- merge(ports, locations.info, by.x = 'name', by.y = 'location.name')


library(sp)
library(rworldmap)
# below fn from: http://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r
coords2country = function(points){  
  countriesSP <- getMap(resolution='low')
  # convert our list of points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  # return the ADMIN names of each country
  indices$ADMIN  
}

ports$country <- coords2country(ports[,c('longitude','latitude')])

head(ports)

from_to <- from_to[-grep('074', as.character(from_to$mjbyptimp)),]

# plot network: departure point to point where slaves are taken

slavenet1 <- merge(from_to, ports, by.x = 'ptdepimp', by.y = 'name', all = TRUE)

library(ggplot2)
library(geomnet)
library(dplyr)
ggplot(data = slavenet1, aes(from_id = ptdepimp, to_id = mjbyptimp)) +
  geom_net(directed = TRUE)

ggplot(data = slavenet1) + 
  geom_net(directed = TRUE, layout = NULL, aes(from_id = ptdepimp, to_id = mjbyptimp, x = longitude, y = latitude))
