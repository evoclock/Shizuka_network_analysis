# Analysing the sparrow flock networks across two years
library(dplyr)
library(tidyr)
library(magrittr)
library(asnipe)
library(igraph)

# Dryad has changed the way data is accessed. It must be downloaded directly
# from the webpage to your local directory (Dryad does not appear to have an 
# ftp server or API for programmatic downloads) 

flockdata_1 = read.csv("data/Flock_Season2_Dryad.csv", header = T)
str(flockdata_1)
head(flockdata_1)

flockdata_2 = read.csv("data/Flock_Season3_Dryad.csv", header = T)
str(flockdata_2)
head(flockdata_2)

flock_list = list(flockdata_1, flockdata_2)

# This part of the code differs only in that we are applying the same operations
# we did to the season 2 data but to data from two different seasons.


m_list = lapply(flock_list, function(x) {
  birdcols = grep("Bird", colnames(x))
  x[ ,birdcols]
  bird_IDs = unique(gather(x[ ,birdcols])$value %>% na.omit())
  m1 = apply(x[ ,birdcols], 1, function(y) match(bird_IDs, y))
  m1 %<>% replace(is.na(.), 0) %>% replace(. > 0, 1)
  rownames(m1) = bird_IDs
  colnames(m1) = paste('flock', 1:ncol(m1), sep="_")
  m2 = m1[which(rowSums(m1) > 3), ]
  m2
})
  

# 1) get the adjacency matrix

adjs = lapply(m_list, 
              function(x) get_network(t(x), # transposing so that we get IBG instead
                                      data_format="GBI", 
                                      association_index = "SRI"))

# 2) Generate the igraph object for each adjacency matrix

gs = lapply(adjs, 
            function(x) graph_from_adjacency_matrix(x, 
                                                    "undirected", 
                                                    weighted = T))

gs

# 3) Plot networks
main = "Golden-crowned Sparrow Network"
seasons = c("Season 2", "Season 3") # plot title
default = par() #save default graphical parameters first 
par(mfrow = c(1,2)) #set up to plot networks side-by-side 

for(i in 1:2){
  set.seed(2)
  plot(gs[[i]], 
       edge.width = E(gs[[i]])$weight*10, 
       #vertex.label = V(gs[[i]]), 
       vertex.label = " ",
       vertex.color = "orange", 
       vertex.size = 10,
       edge.color = "#21918c",
       main = paste(main),
       sub = paste(seasons[i]))
}
par(default)

# Network modularity

# apply community detection function to each network
coms = lapply(gs, function(x) cluster_fast_greedy(x))
#calculate modularity based on community assignments
mods = sapply(coms, modularity) 

library(viridisLite)
com_cols = list(viridis(4, alpha = 0.7, option = "D"), 
                viridis(4, direction = -1, alpha = 0.7, option = "D"))
# com.colors=list(c("blue", "yellow", "green", "red"), c("green", "blue", "red", "yellow")) # assign colors to communities. Community colors are in different order for each year because community ID number depends on the order of nodes that belong to them.
set.seed(10) 
par(mfrow = c(1,2))
for(i in 1:2){ 
  l = layout_with_fr(gs[[i]])
  V(gs[[i]])$color = com_cols[[i]][membership(coms[[i]])]
  plot(gs[[i]], 
       layout = l, 
       edge.width=E(gs[[i]])$weight*10, 
       #vertex.label = V(gs[[i]]),
       vertex.label="", 
       vertex.size = 10,
       edge.color="gray10", 
       main = paste(seasons[i], ": Modularity=", round(mods[[i]], 2)))
}
par(default)

# Testing modularity of empirical network against randomised networks