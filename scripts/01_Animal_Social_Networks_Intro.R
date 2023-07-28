# Animal social networks

# Constructing networks from:
# 1) interactions
# 2) associations

# The most important question here (and whenever you are making networks in 
# general) is: what are your edges?

# 1) Network from interaction
library(igraph)
idat = read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_interactions.csv")
head(idat)

# Data set of dominance interactions between pairs of individuals. 
# Each individual has a 5 digit id number.

# generate a directed network
ig = graph_from_data_frame(idat, directed = T)
plot(ig)

# simplify the multiple interactions between same pairs by treating them as 
# weighted edges
E(ig)$weight = 1 # make all edge weights = 1
ig2 = simplify(ig, remove.multiple = T, edge.attr.comb = list(weight = sum))
plot(ig2, 
     edge.width = E(ig2)$weight/4, 
     edge.curved = T, 
     layout = layout_in_circle(ig2))

# Note: arrows go from winner to loser

# Network from association

# Individuals that are observed to be in the same flock, group, or herd may be 
# considered to be associated in some way. Keep in mind though that this is an 
# assumption, and the inference of social networks from patterns of group 
# association is often called the ‘gambit of the group’ (see Franks et al. 2010)

# Typically, association data would be first be gathered as an 
# individual-by-group matrix (or group-by-individual matrix). That is, 
# individuals are listed in rows and groups (e.g., flocks/herds/schools) listed 
# in columns (or vice-versa). The cell value is 1 if that individual is in that 
# group, and 0 if not.

assoc = as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", 
                           header = T, row.names = 1))
assoc

# Using an association index, note that in this type of data, the probability 
# that we draw a connection between any two nodes is partially dependent on how 
# often we saw each individual. For example, if two individuals are simply 
# observed more often, they are more likely to be observed together. 
# Conversely, if an individual is rarely seen, it is likely to be seen only 
# with a small subset of other individuals. We may like to take this into 
# account. One popular solution to this is to define network edges based on 
# an ‘association index’ that accounts for the frequency of observation of 
# each node. One simple association index is called the Simple Ratio Index 
# (Cairns & Schwager 1987; this is the same as the Jaccard Index).

# Association Index = |A∩B|/|A∪B| = |A∩B| /|A|+|B|−|A∩B|
# where
# |A∩B| is the number of times A and B were seen together
# |A| is the total number of times A was seen (together or separate from B)
# |B| is the total number of times B was seen (together or separate from A)

# Construct a social network from associations using the asnipe package

library(asnipe)

# assoc is in individual-by-group format, but this package likes 
# group-by-individual, so we will transpose the matrix.
gbi = t(assoc) 

#this function converts the association matrix into an adjacency matrix using 
# the Simple Ratio Index.
adj_m = get_network(t(assoc), association_index = "SRI") 

#create a graph object
assoc_g = graph_from_adjacency_matrix(adj_m, "undirected", weighted = T) 

plot(assoc_g, 
     edge.width = E(assoc_g)$weight*10, 
     vertex.label="", 
     vertex.color="orange", edge.color="#21918c", vertex.frame.color="#440154")



