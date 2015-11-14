# whole map, coloring port arrivals. 

all_ports <- rbind(embport_coords, embport2_coords, arrport_coords, arrport2_coords,
      portdep_coords, sla1port_coords, portret_coords)

mapWorld$aes_params$fill <- 'white'
mp <- ggplot() + mapWorld
mp + geom_point(data = all_ports, aes(x = lon, y = lat, color = var), alpha = .75) +
  scale_color_brewer(palette = 'Paired') + theme_bw()

# next steps: 

# 1. draw network from portdep, or embport, embport2 (if no portdep, embport)
  # to arrport, arrport2, sla1port (one of these) to portret if applicable
# 2. make the edges directed
# 3. make the lines as wide as the number of slaves on board. 
  # 3a. Taper the lines to reflect deaths along the way?

