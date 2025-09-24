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

