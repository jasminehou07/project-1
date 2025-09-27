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


#PART 2
# out & in degree
out_degree <- degree(g, mode = "out") #outgoing ties
in_degree  <- degree(g, mode = "in") #incoming ties

# mean tie strength
monks_with_ties <- monk_mat[out_degree > 0, ] #takes only monks that have ties
mean_tie_strength <- mean(monks_with_ties[monks_with_ties > 0]) #finds mean tie strength between monks that have ties with each other


# store results in a list
stats_observed <- list(
  out_degree = out_degree,
  in_degree = in_degree,
  mean_tie_strength = mean_tie_strength
)

