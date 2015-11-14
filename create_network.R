# combining from - to data for network plotting

# maybe only deal with the voyages that were completed as intended
completes <- which(data$fate == 'Voyage completed as intended')

from_to <- data[completes,c('embport', 'embport2', 'portdep', 'arrport', 'arrport2', 'sla1port', 'portret')]

from <- from_to[,1:3]
from
to <- from_to[, c(4:6)]
library(plyr)
from_nas <- ldply(apply(from, 1, function(x) sum(is.na(x))),  .fun = rbind)
from_to2[which(from_nas[,2] == 3),1])

to_nas <- ldply(apply(to, 1, function(x) as.numeric(sum(is.na(x)))), .fun = rbind)
to_nas[which(to_nas[,2] == 3),1]

from_to2 <- data[completes,c('voyageid','embport', 'embport2', 'portdep', 'arrport', 'arrport2', 'sla1port', 'portret')]
from_to3 <- from_to2[-as.numeric(unique(c(from_to2[which(from_nas[,2] == 3),1], from_to2[which(to_nas[,2] == 3),1]))),]

slave_net <- data.frame(matrix(NA, nrow=nrow(from_to), ncol = 2))

for(i in 1:nrow(from_to)){
  if (!is.na(from_to$embport[i])){
    slave_net[i,1] <- as.character(from_to$embport[i])
  } else if(!is.na(from_to$embport2)[i]){
    slave_net[i,1] <- as.character(from_to$embport2[i])
  } else {
    slave_net[i,1] <- from_to$portdep[i]
  }
  if (!is.na(from_to$arrport[i])){
    slave_net[i,2] <- from_to$arrport[i]
  } else if(!is.na(from_to$arrport2)[i]){
    slave_net[i,2] <- from_to$arrport2[i]
  } else {
    slave_net[i,2] <- from_to$sla1port[i]
  }
}
  
head(slave_net)    

slave_net$voyageid <- from_to2$voyageid

slave_net2 <- na.omit(slave_net)
names(slave_net2)[1:2] <- c("from_port", "to_port")
all_ports <- data.frame(port = unique(c(slave_net2$from_port, slave_net2$to_port)))

library(ggplot2)
library(geomnet)

slavenet <- merge(slave_net2, all_ports, by.x = "from_port", by.y = "port", all = TRUE)
slavenet$from_port <- as.factor(slavenet$from_port)
slavenet$to_port <- as.factor(slavenet$to_port)

head(slavenet)

ggplot(data = slavenet, aes(from_id = from_port , to_id = to_port)) + 
  geom_net(label = TRUE, layout = 'fruchtermanreingold') + 
  theme_net()

# # slaves on voyage, # slaves lost on voyage, country of ship, ship id, year, (check laws against slave trade in UK vs US),
# merge with latitude/longitude info from earlier