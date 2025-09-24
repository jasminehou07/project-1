# load data
library(lda)
library(igraph)
library(networkD3)

data("sampson")

# take matrix SAMPLK2 and store it inside a variable
monk_mat <- sampson$SAMPLK2
