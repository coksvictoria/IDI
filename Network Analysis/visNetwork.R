library(tidyverse)

##Supplier Data
data <- read_csv("C:/Users/xwang004/Downloads/R/Data/Supplier.csv")
names(data)
data<-data %>% 
  rename(
    source_id=COST_CENTRE,
    source=DESCR_30,
    destination_id=SUPP,
    destination=NAME,
    weight=TOTL_NETT
  )

data<-filter(data,weight>100000)

sources <-select(data,source_id, source,weight)
sources$name<-"C"
sources$colorid<-1
sources<-sources%>%
  rename(label=source,id=source_id)


destinations <- select(data,destination_id, destination,weight)
destinations$name<-"S"
destinations$colorid<-2
destinations<-destinations%>%
  rename(label=destination,id=destination_id)

nodes <- full_join(sources, destinations)

nodes<-nodes%>%
  group_by(id,label,name,colorid)%>%
  summarise(weight=sum(weight)/1000)

# nodes<-nodes[!duplicated(nodes), ]

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


##METHOD 4 dynamic network
library(visNetwork)
library(networkD3)

###VISNETWORK

##basic plot
visNetwork(nodes, edges)

##Plot attributes
nodes$shape <- "dot" 
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$weight # Text on click
nodes$label <- nodes$label # Node label
nodes$size <- nodes$weight/100# Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- c("gold", "orange")[nodes$colorid]
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

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



