from_to_2 <- from_to 
from_to_2$mjbyptimp <- from_to$mjslptimp
from_to_2$mjslptimp <- from_to$mjbyptimp

from_to_cheat <- rbind(from_to, from_to_2)
slavenet_all_2 <- create_net_data2(edge_data = from_to, node_data = ports, 
                                  variable = NULL, value = NULL, from_id = "mjbyptimp" , 
                                  to_id = "mjslptimp", node_id = "name")


fix_arrows <- arrow(angle = rep(30, nrow(slavenet_all_2)),
                    length = rep(unit(c(10,0),'points'),nrow(slavenet_all_2)/2),
                    ends = rep("last",each = nrow(slavenet_all_2)),
                    type = rep("closed", nrow(slavenet_all_2)))

fix_arrows2 <- arrow(angle = rep(30, nrow(slavenet_all_2)),
                    length = rep(unit(c(10,0),'points'),each = nrow(slavenet_all_2)/2),
                    ends = rep("last",each = nrow(slavenet_all_2)),
                    type = rep("closed", nrow(slavenet_all_2)))

mp + geom_net(data= slavenet_all_2, layout = NULL, 
              ealpha = .15, arrowgap = 1, directed = TRUE, 
              arrow = fix_arrows,
              aes(x=longitude, y = latitude, from_id = mjbyptimp, to_id=mjslptimp,
                  color = natinimp)) +
  theme(legend.position = 'bottom') + lims(x = c(-105,75), y = c(-50,50)) + theme_net()

slavenet_all_3 <- slavenet_all_2
slavenet_all_3$directed <- rep(c(TRUE, FALSE), each = nrow(slavenet_all_2)/2)

# create another function to merge the data

# function to create a data set appropirate for network plotting. 
# variable, from_id, to_id, node_id need to be characters. value needs to be character or numeric. 
create_net_data2 <- function(edge_data, node_data, variable = NULL, value = NULL, from_id, to_id, node_id){
  if (!is.null(variable) && !is.null(value)){
    idx <- which(edge_data[, variable] %in% value)
    edge_data <- edge_data[idx,]
  } else{edge_data <- edge_data}
  edge_data2 <- edge_data
  edge_data2[,from_id] <- edge_data[,to_id]
  edge_data2[,to_id] <- edge_data[,from_id]
  edge_data <- rbind(edge_data, edge_data2)
  nodes1 <- unique(as.character(edge_data[,from_id]))
  nodes2 <- unique(as.character(edge_data[,to_id]))
  nodes <- unique(c(nodes1, nodes2))
  node_idx <- which(node_data[, node_id] %in% nodes)
  node_data <- node_data[node_idx,]
  net_data <- merge(edge_data, node_data, by.x = from_id, by.y = node_id, all = T)
}



slavenet_all <- create_net_data2(edge_data = from_to, node_data = ports, variable = NULL, value = NULL, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")

slavenet_all_arr <- slavenet_all %>% dplyr::arrange(mjbyptimp,mjslptimp)
slavenet_all_arr2 <- slavenet_all %>% dplyr::arrange(voyageid)
head(slavenet_all_arr2)

head(match(slavenet_all$voyageid, slavenet_all_arr2$voyageid))

rep(unit(c(10,0),'points'), nrow(slavenet_all))[match(slavenet_all_arr[,c('voyageid','mjbyptimp')], slavenet_all_arr2[,c('voyageid','mjbyptimp')])]

library(prodlim)
row.match(slavenet_all_arr[,c('voyageid','mjbyptimp')], slavenet_all_arr2[,c('voyageid','mjbyptimp')]))


fix_arrows3 <- arrow(angle = rep(30, nrow(slavenet_all)),
                     length = unit(c(10*as.numeric(slavenet_all_arr$mjbyptimp %in% from_to$mjbyptimp)),'points'),
                    ends = rep("last", nrow(slavenet_all)),
                    type = rep("closed", nrow(slavenet_all)))

  head(which(slavenet_all_arr$mjbyptimp %in% from_to$mjbyptimp))


head(slavenet_all_arr)

mp + geom_net(data= slavenet_all_2, layout = NULL, 
              ealpha = .15, arrowgap = 1, directed = TRUE, 
              arrow = fix_arrows3,
              aes(x=longitude, y = latitude, from_id = mjbyptimp, to_id=mjslptimp,
                  color = natinimp)) +
  theme(legend.position = 'bottom') + lims(x = c(-105,75), y = c(-50,50)) + theme_net()

