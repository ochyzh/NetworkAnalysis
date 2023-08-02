library(networkdata)
library(statnet)
data("duqueData")

for (i in 1:8) {
  set.network.attribute(dipl_ties[[i]], 'vertex.pid','vertex.names')
}
#Takes 5 min to run:
diplDyn = networkDynamic(network.list=dipl_ties, vertex.pid='vertex.names')
par(mfrow = c(1,2))
plot(
  network.extract(diplDyn, at = 1),
  main = "1970", displaylabels = T)
plot(
  network.extract(diplDyn, at = 6),
  main = "2005", displaylabels = T)

class(dipl_ties[[1]])


# Load required libraries
library(networkdata)
library(statnet)
library(sna)

# Load the dataset
data("duqueData")

# Filter nodes based on their degree (top N nodes with highest degree)
top_nodes <- 50  # Adjust the number as needed to reduce the network size

# Calculate node degrees

# Create a new network with only the top nodes and their associated edges
filtered_dipl_ties <- lapply(dipl_ties, function(net) {
  net1<-apply(net, 2 ,as.numeric)
  node_degrees <- apply(net1,1, sum, na.rm=TRUE)
  # Get the names of top N nodes based on degree
  top_node_names <- names(head(sort(node_degrees, decreasing = FALSE), top_nodes))
  # Select the relevant nodes and their associated edges
  filtered_net <- net[top_node_names, top_node_names]
  return(filtered_net)
})

# Set network attributes
for (i in 1:length(filtered_dipl_ties)){
  filtered_dipl_ties[[i]]<-as.network(as.matrix(filtered_dipl_ties[[i]]),directed = TRUE, loops=FALSE,matrix.type="adjacency")
  set.vertex.attribute(filtered_dipl_ties[[i]], 'vertex.pid', 'vertex.names')
  }



# Create the dynamic network
diplDyn <- networkDynamic(network.list = filtered_dipl_ties, vertex.pid = 'vertex.names')

# Plot the filtered dynamic networks
par(mfrow = c(1, 2))
plot(
  network.extract(diplDyn, at = 1),
  main = "1970 (Filtered)",
  displaylabels = TRUE
)
plot(
  network.extract(diplDyn, at = 6),
  main = "2005 (Filtered)",
  displaylabels = TRUE
)
