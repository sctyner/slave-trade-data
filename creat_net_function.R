# function to create a data set appropirate for network plotting. 
# variable, from_id, to_id, node_id need to be characters. value needs to be character or numeric. 
create_net_data <- function(edge_data, node_data, variable = NULL, value = NULL, from_id, to_id, node_id){
  if (!is.null(variable) && !is.null(value)){
    idx <- which(edge_data[, variable] %in% value)
    edge_data <- edge_data[idx,]
  } else{edge_data <- edge_data}
  nodes1 <- unique(as.character(edge_data[,from_id]))
  nodes2 <- unique(as.character(edge_data[,to_id]))
  nodes <- unique(c(nodes1, nodes2))
  node_idx <- which(node_data[, node_id] %in% nodes)
  node_data <- node_data[node_idx,]
  net_data <- merge(edge_data, node_data, by.x = from_id, by.y = node_id, all = T)
}

test_net_fn <- create_net_data(edge_data = from_to, node_data = ports, variable = NULL, value = NULL, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")

ggplot(data = test_net_fn) + 
  geom_net(directed = TRUE, layout = NULL, aes(from_id = mjbyptimp, 
                                               to_id = mjslptimp, x = longitude, y = latitude)) + facet_wrap(~century)


from_to$century <- as.numeric(substr(as.character(from_to$yeardep), 1,2)) + 1

st16 <- create_net_data(edge_data = from_to, node_data = ports, variable = "century", value = 16, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")
st17 <- create_net_data(edge_data = from_to, node_data = ports, variable = "century", value = 17, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")
st18 <- create_net_data(edge_data = from_to, node_data = ports, variable = "century", value = 18, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")
st19 <- create_net_data(edge_data = from_to, node_data = ports, variable = "century", value = 19, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")

stbycent <- rbind(st16, st17, st18, st19)

stbycent2 <- create_net_data(edge_data = from_to, node_data = ports, variable = "century", value = 16:19, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")

ggplot(data = st19) + 
  geom_net(directed = TRUE, aes(from_id = mjbyptimp, 
                                               to_id = mjslptimp))

ggplot(stbycent) + geom_net(aes(from_id = mjbyptimp, 
                                to_id = mjslptimp, x = longitude, y = latitude, color = century), directed = TRUE, layout = NULL) 

ggplot(stbycent2) + geom_net(aes(from_id = mjbyptimp, 
                                to_id = mjslptimp, x = longitude, y = latitude, color = century), directed = TRUE, layout = NULL) 

### Try to facet by century. ## 

# doesn't work. :( 
ggplot(stbycent2) + geom_net(aes(from_id = mjbyptimp, 
                                to_id = mjslptimp, x = longitude, y = latitude, color = century), directed = TRUE, layout = NULL) +
  facet_wrap(~century)



st1790 <- create_net_data(edge_data = from_to, node_data = ports, variable = "yeardep", value = 1790, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")
st1791 <- create_net_data(edge_data = from_to, node_data = ports, variable = "yeardep", value = 1791, from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")

st17901 <- create_net_data(edge_data = from_to, node_data = ports, variable = "yeardep", value = c(1790,1791), from_id = "mjbyptimp" , to_id = "mjslptimp", node_id = "name")

#looks fine
ggplot(st1790) + geom_net(aes(from_id = mjbyptimp, 
                                to_id = mjslptimp, x = longitude, y = latitude), directed = TRUE, layout = NULL) 
#looks fine
ggplot(st1791) + geom_net(aes(from_id = mjbyptimp, 
                              to_id = mjslptimp, x = longitude, y = latitude), directed = TRUE, layout = NULL) 
#doesn't work
ggplot(st17901) + geom_net(aes(from_id = mjbyptimp, 
                              to_id = mjslptimp, x = longitude, y = latitude), directed = TRUE, layout = NULL) + facet_wrap(~yeardep)
#also doesn't work
ggplot(rbind(st1790,st1791)) + geom_net(aes(from_id = mjbyptimp, 
                               to_id = mjslptimp, x = longitude, y = latitude), directed = TRUE, layout = NULL) + facet_wrap(~yeardep)
