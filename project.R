#PART 0
# load data
library(lda)
library(igraph)
library(networkD3)

data("sampson")

# take matrix SAMPLK2 and store it inside a variable
monk_mat <- sampson$SAMPLK2

#PART 1
#STATIC graph
#stores igraph into variable g
g <- graph_from_adjacency_matrix(
  monk_mat,
  mode = "directed",
  weighted = TRUE,
  diag = FALSE
)

#plot the igraph
plot(g)

# adding monks to the graph
n <- nrow(monk_mat)

nodes <- data.frame(
  id    = 0:(n - 1),
  name  = colnames(monk_mat),
  group = 1L
)

# makes a table of all of the monk connections
edge_idx <- which(monk_mat > 0, arr.ind = TRUE)
links <- data.frame(
  source = edge_idx[, 1] - 1,        # 0-indexed
  target = edge_idx[, 2] - 1,        # 0-indexed
  value  = monk_mat[edge_idx]        # tie strength
)

# save the interactive graph to fn
fn <- forceNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target",
  NodeID = "name", Group = "group", 
  Value = "value",
  fontSize = 14, opacity = 0.85, zoom = TRUE, charge = -180
)

#show the interactive graph
fn



