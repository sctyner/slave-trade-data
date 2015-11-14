# Workflow for correcting geocoded points: 

# 1. Correct places in US that make no sense 
# 2. Check distances to coast that are over a certain small distance


# ocean borders shape files 
library(ggplot2)
library(maptools)
oceans <- readShapePoly(fn = "ne_50m_ocean//ne_50m_ocean")
oceans2 <- fortify(oceans)
str(oceans)
summary(oceans2)
ggplot(data = oceans2, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = NA, color = 'black')

rivers <- readShapeLines(fn = "ne_10m_rivers_lake_centerlines//ne_10m_rivers_lake_centerlines")
rivers2 <- fortify(rivers)
summary(rivers2)
ggplot(data = rivers2, aes(x = long, y = lat)) +
  geom_line(aes(group = group, order = order), color = 'black') 
  

library(geosphere)

check_port_dists <- distm(x = as.matrix(port_info[, c('lon','lat')]), y = as.matrix(oceans2[, c('long','lat')]))

min_port_dists <- apply(check_port_dists, 1, min)
port_info$dist_to_sea <- min_port_dists

ggplot(data = oceans2, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = NA, color = 'black') +
  geom_point(data = port_info, aes(x = lon, y = lat, color = dist_to_sea))
