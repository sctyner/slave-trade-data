# combining ports with other data. 

# Variables to get # 
# slaves on voyage: tslavesp
# slaves lost on voyage: sladafri, sladvoy, sladamer
# country of ship: natinimp
# ship id: shipname
# day month year: datarr32, datarr33, datarr34
# datedepa, datedepb, datedepc

  ###(check laws against slave trade in UK vs US),
# merge with latitude/longitude info from earlier

trips_interest <- data[which(data$voyageid %in% slavenet$voyageid),]

slavenet <- data.frame(na.omit(slavenet))
slavenet$from_port <- as.character(slavenet$from_port)
undefined_dep <- c(subset(slavenet, from_port == "undefined 074")$voyageid, 
  subset(slavenet, from_port == "undefined 405")$voyageid)
slavenet$from_port[which(slavenet$voyageid %in% undefined_dep)] <- as.character(data[which(data$voyageid %in% undefined_dep), c("portdep")])


vars_interest <- c("voyageid", "tslavesp", "sladafri", "sladvoy", "sladamer", "natinimp", 
                   "shipname", "datedepa", "datedepb", "datedepc", "datarr32", "datarr33", "datarr34")

ship_data <- trips_interest[, vars_interest]

# data: port_info_clean, slavenet, ship_data

head(port_info_clean)

summary(ship_data)
summary(slavenet)

slavenet_info <- merge(slavenet, ship_data, by = "voyageid")
str(port_info_clean)
library(dplyr)
#slavenet_info_from <- left_join(slavenet_info, port_info_clean[,-1], by = c("from_port" = "port"))
slavenet_info_all <- merge(slavenet_info, port_info_clean, by.x = "from_port", by.y = "port", all = TRUE)

library(lubridate)
dates_ships_leave <- slavenet_info_all[,10:12]
dates_ships_arrive <- slavenet_info_all[,13:15]

date_departs <- dmy(paste(dates_ships_leave[,1], dates_ships_leave[,2], dates_ships_leave[,3], sep = '-'))
date_arrives <- dmy(paste(dates_ships_arrive[,1], dates_ships_arrive[,2], dates_ships_arrive[,3], sep = '-'))

slavenet_info_all$date_depart <- date_departs
slavenet_info_all$date_arrive <- date_arrives
library(ggplot2)
library(geomnet)
ggplot() + mapWorld + 
  geom_net(data = slavenet_info_all, directed = TRUE, ealpha = 0.1, layout = NULL, 
           aes(from_id = from_port, to_id = to_port, x = lon, y = lat, colour = datarr34)) 


 
  