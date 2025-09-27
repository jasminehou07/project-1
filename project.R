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
  source = edge_idx[, 1] - 1,
  target = edge_idx[, 2] - 1, 
  value  = monk_mat[edge_idx]    
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


# barplot of in-degree
barplot(in_degree, names.arg = names(in_degree), main = "In-degree of Sampson's Monks", xlab = "Monks", ylab = "In-degree")


# most liked and least liked
max_in <- max(in_degree) #highest in-degree = most liked
min_in <- min(in_degree) #lowest in-degree = most liked
most_liked  <- names(in_degree)[in_degree == max_in] #finds the name of the monk with highest in-degree
least_liked <- names(in_degree)[in_degree == min_in] #finds the monk with the lowest in-degree

#display the most and least liked
paste("The most liked monk is", most_liked)
paste("The least liked monk is", least_liked)
