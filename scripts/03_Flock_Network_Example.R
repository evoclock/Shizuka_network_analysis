library(dplyr)
library(tidyr)
library(magrittr)
library(asnipe)
library(igraph)

# Dryad has changed the way data is accessed. It must be downloaded directly
# from the webpage to your local directory (Dryad does not appear to have an 
# ftp server or API for programmatic downloads) 

flockdata = read.csv("data/Flock_Season2_Dryad.csv", header = T)
str(flockdata)
head(flockdata)
# Now convert raw data to individual-by-group matrix (a.k.a an affiliation
# matrix)

# retrieve the columns that have Bird IDs
birdcols = grep("Bird", colnames(flockdata))

# then get the bird ID data and remove NAs
flockdata[ ,birdcols]
bird_IDs = unique(gather(flockdata[ ,birdcols])$value %>% na.omit())

# get the IBG matrix by going through all the columns in flockdata and matching
# any IDs in our bird_IDs vector

m1 = apply(flockdata[ ,birdcols], 1, function(x) match(bird_IDs, x))

# convert NAs to 0 (i.e convert to a sparse matrix)
m1 %<>% replace(is.na(.), 0) %>% replace(. > 0, 1)

# or in base R
# m1[is.na(m1)] = 0
# m1[m1 > 0] = 1

#rows are bird ids
rownames(m1) = bird_IDs 
#columns are flock IDs (just "flock_#")
colnames(m1) = paste('flock', 1:ncol(m1), sep="_") 
m1

# We are now ready to make our network using the simple ratio index
# Recall that asnipe produces a GBI matrix so we need to transpose it

# 1) get the adjacency matrix
adj = get_network(t(m1), 
                  data_format="GBI", 
                  association_index = "SRI") 

# Generate the undirected (but weighted) network as an igraph object
g = graph_from_adjacency_matrix(adj, "undirected", weighted = T) 

l = layout_with_gem(g)
set.seed(2)
plot(g, 
     laylout = l,
     edge.width = E(g)$weight*10, 
     vertex.label = "", 
     vertex.size = 5,
     vertex.color="orange", 
     edge.color="#21918c") 


# filtering individuals seen less than 3 times per flock
m2 = m1[which(rowSums(m1) > 2),]
adj = get_network(t(m2), data_format = "GBI", "SRI")

# Generate the undirected (but weighted) network as an igraph object
g = graph_from_adjacency_matrix(adj, "undirected", weighted = T) 

l = layout_with_gem(g)
set.seed(2)
plot(g, 
     laylout = l,
     edge.width = E(g)$weight*10, 
     vertex.label = "", 
     vertex.size = 5,
     vertex.color="orange", 
     edge.color="#21918c") 

# Colour the network based on community membership

com = fastgreedy.community(g) #community detection method
node_colors = membership(com) #assign node color based on community membership
set.seed(2)
plot(g, 
     laylout = l,
     edge.width = E(g)$weight*10, 
     vertex.label = "", 
     vertex.size = 5, 
     vertex.color = node_colors)
