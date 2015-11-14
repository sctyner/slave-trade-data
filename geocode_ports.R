port_info <- unique(c(as.character(slavenet$from_port), as.character(slavenet$to_port)))
                    
library(ggmap)

coord1 <- data.frame(lon = NA, lat = NA)

for (i in 1:length(port_info)){
  coord1[i,] <- geocode(port_info[i], source = 'google')
}
coord1

port_info <- data.frame(port=port_info[-length(port_info)], coord1)

library(dplyr)
port_info$country <- NA
for (i in 1:nrow(port_info)){
  if (!is.na(port_info$lat[i])){
    port_info$country[i] <- as.character(coords2country(port_info[i,2:3]))
  } else port_info$country[i] <- NA
}

# write out to csv for easier editing
write.csv(port_info, 'port_info.csv')

mapWorld$aes_params$fill <- 'white'
mp <- ggplot() + mapWorld
mp + geom_point(data = port_info, aes(x = lon, y = lat), alpha = .75) +
  geom_text(data = subset(port_info, country == "United States of America"), aes(x = lon, y = lat, label = country)) + 
  theme_bw() 

mp + geom_point(data = subset(port_info, country == "United States of America"), 
                aes(x = lon, y = lat), alpha = .75) + 
  geom_text(data =subset(port_info, country == "United States of America"), aes(x = lon, y = lat, label= port))

usa <- map_data("state")
subset(port_info, country == "United States of America")

ggplot(usa, aes(long,lat, group = group)) + geom_polygon(fill = 'white', color = 'black') +
  geom_text(inherit.aes = FALSE, data =subset(port_info, country == "United States of America"), aes(x = lon, y = lat, label= port))


read