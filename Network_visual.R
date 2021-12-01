library(tidyverse)
data <- read_csv("C:/Users/xwang004/Downloads/R/Network_Data.csv")

data<-data %>% 
  rename(
    source=DepCity,
    destination=ArrCity,
    weight=CarbonEmissioninKg
  )

sources <- data %>%
  distinct(source)%>%
  rename(label=source)

destinations <- data %>%
  distinct(destination)%>%
  rename(label=destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>% rowid_to_column("id")
nodes

per_route <- data %>%  
  group_by(source, destination) %>%
  summarise(weight=sum(weight)/1000) %>% 
  ungroup()
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges_bkp<-edges
edges <- select(edges, from, to, weight)
edges


##METHOD 1 network
install.packages("network")
library("network")

routes_network <- network(edges, vertex.attr = nodes,
  matrix.type = "edgelist",loops = TRUE, ignore.eval = FALSE)

#check type
class(routes_network)

plot(routes_network, vertex.cex = 3, mode = "circle")

##METHOD 2 igraph
rm(routes_network)
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, 
                          vertices = nodes, directed = FALSE)
routes_igraph

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.05)

##METHOD 3 tidygraph and ggraph
##tidygraph and ggraph represent an attempt to bring network analysis into the tidyverse workflow.
library(tidygraph)
library(ggraph)

##method1: create tbl_graph directly
routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
##method2: convert igraph or network object to tbl_graph
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy

##Rearrange the rows in the edges by weights
routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "CO2e") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "CO2e") +
  theme_graph()

##METHOD 4 dynamic network
library(visNetwork)
library(networkD3)

###VISNETWORK
visNetwork(nodes, edges)

edges_new <- dplyr::mutate(edges, width = weight/5000 + 1)

visNetwork(nodes, edges_new) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

##netwoekD3
nodes_d3 <- dplyr::mutate(nodes, id = id - 1)
edges_d3 <- dplyr::mutate(edges, from = from - 1, to = to - 1)
forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight",  
             opacity = 10, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "t CO2e")

install.packages(c("igraph","graphlayouts","ggraph","ggplot2","visNetwork"))
library(igraph)
library(ggraph)
library(graphlayouts)

