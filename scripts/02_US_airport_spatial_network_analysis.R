# Centrality and community structure of US air transportation network example
# Script based in huge part on Dr. Dai Shizuka's but with some much needed
# updates to the spatial network overlay part of his tutorial (it was broken) 

# The air transportation network–a network of airports connected by airline 
# routes–is a great example of a spatial network. In a now-classic study, 
# Guimerà et al. (2005) used the world-wide air transportation network to 
# tackle the question of how to measure the importance of nodes in a network, 
# and how different measures can tell us different things. They also propose 
# some alternative measures of position that tells a little bit more about the 
# roles that different nodes play in a complex network. In this exercise we 
# will be a little bit further to think about methods for testing the 
# resilience of networks to disturbance using virtual knockout experiments. 
# Researchers in a wide variety of fields use these virtual knockouts as one 
# tool to understand things like:
# - the potential effects of node failure on power grids, transportation 
# networks and information flow
# - the effects of the loss of particular individuals in a social network
# - the effects of habitat alteration on spatial networks of organisms

library(igraph) 
library(igraphdata) 
library(ggmap)
# remotes::install_github("dyerlab/popgraph")
library(popgraph)
# To run simmulated annealing
# If on linux, run:
# sudo apt-get install libgsl-dev
# install.packages("https://cran.r-project.org/src/contrib/Archive/rnetcarto/rnetcarto_0.2.4.tar.gz",
#                 repos = NULL, type = "source", configure.args = '--host=host')
# library(rnetcarto)
library(viridisLite)
data("USairports") 
USairports

# replace the wrong airport codes and coordinates by using the following 
# compiled dataset

airports = read.csv('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat', 
                    header = F)

# match to the correct lat (col 7) and lon (col 8) for each airport as a node
# attribute

V(USairports)$lat = airports[match(V(USairports)$name, airports[,5]), 7] 
V(USairports)$long = airports[match(V(USairports)$name, airports[,5]), 8]

# Tidy the data as follows
# simplify the network by removing loops and make undirected
usair = as.undirected(simplify(USairports))

# remove airports whose codes didn't match the OpenFlights database 
# (and hence returned "NA" for latitude)
usair = delete.vertices(usair, which(is.na(V(usair)$lat) == TRUE))

# remove nodes in the Eastern and Southern Hemispheres (US territories). 
# This will make the plot easier to see.
usair = delete.vertices(usair, 
                        which(is.na(V(usair)$lat)
                              | V(usair)$lat < 0 | 
                                V(usair)$long > 0))

# keep only the largest connected component of the network 
# ("giant component"). this also makes the network easier to see.
decomp = decompose.graph(usair)
usair = decomp[[1]]

set.seed(3)
l = layout_with_gem(usair)
par(mar=c(1,1,1,1))
plot(usair, 
     layout = l, 
     vertex.label = "", 
     vertex.size = 3,
     vertex.color="orange", 
     edge.color="#21918c")
dev.off()

# Dai Shizuka asks the following: How should we define the ‘importance’ of an 
# airport in this network? 
# Degree and betweenness are the two most commonly used measure of node 
# centrality (i.e., measures of node position in a network), and in this case, 
# they represent two different measures of importance. Degree centrality tells 
# us how many airports one fly to directly from a given airport. In contrast, 
# betweenness centrality (which is the number of geodesic paths go through 
# the node) gives us a sense for how important the airport is as a transfer 
# location for people getting from one part of the country to another. Note 
# that we are using a simplified network here that only counts unique flight 
# routes, so we are not accounting for the number of replicated flights or 
# the size of the planes.

# Relationship betweenness and degree 
plot(degree(usair), betweenness(usair), pch = 19)
# There are a few nodes that have high betweenness despite intermediate degree
# but the overall trend is a strong linear relationship between both of these
# variables

# Retrieve degree centrality and betweeeness centrality
degree_cent = degree(usair)
betweenness_cent = betweenness(usair)

# order() gives us the element number in order of ranking so that we can
# see what some of these points are in terms of node degree.
top_degree_airports = V(usair)$name[order(degree_cent, 
                                          decreasing = TRUE)][1:10]
# [1] "ATL" "DEN" "ORD" "MSP" "DFW" "DTW" "LAS" "IAH" "CLT" "LAX"

# Most of these are in the lower 48 state because we are not including
# international flights in the network.

# Now let's see the top 10 airports in terms of node betweenness
top_betweenness_airports = V(usair)$name[order(betweenness_cent, 
                                               decreasing = TRUE)][1:10]

# [1] "ANC" "SEA" "DEN" "MSP" "ORD" "FAI" "DTW" "ATL" "BET" "LAX"

# Why the discrepancy between top 10 nodes by degree and betweenness?
# If plotted in space using coordinate data you can see the outline of the USA. 
# Now note how many airports there are in Alaska alone! This makes sense 
# because there are so few roads in Alaska relative to landmass that flying is 
# the main mode of transportation between remote towns. Thus, the airports in 
# Alaska are not well connected in terms of the number of places you can get 
# to by direct flight, but the hubs that connect Alaska to the rest of the 
# U.S. (e.g., Anchorage and Seattle) are disproportionately important in terms 
# of betweenness centrality. 

#set up layout matrix
longlat = matrix(c(V(usair)$long, V(usair)$lat), ncol = 2)  
par(mar=c(1,1,1,1))
plot(usair, 
     layout = longlat, 
     vertex.label = "", 
     vertex.size = 3,
     vertex.color="orange", 
     edge.color="#21918c")


# Let's overlay the network on a map

# Convert the igraph object to a data frame
usair_df = data.frame(id = V(usair)$name, long = V(usair)$long, lat = V(usair)$lat)

# Compute the edges' start and end coordinates
edges_df = as.data.frame(get.edgelist(usair))
colnames(edges_df) = c("from", "to")
edges_df = merge(edges_df, usair_df, by.x = "from", by.y = "id", sort = FALSE)
colnames(edges_df)[3:4] = c("from_long", "from_lat")
edges_df = merge(edges_df, usair_df, by.x = "to", by.y = "id", sort = FALSE)
colnames(edges_df)[5:6] = c("to_long", "to_lat")

# Plot the map
location = c(mean(usair_df$long), mean(usair_df$lat))
map = get_stamenmap(bbox = c(left = -179, 
                             bottom = 15, 
                             right = -65, 
                             top = 75), 
                    zoom = 5, 
                    maptype = "terrain-background", 
                    source = "stamen")

# Plot the spatial network with edges and nodes on the map
p = ggmap(map) +
  geom_segment(aes(x = from_long, 
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat), 
               data = edges_df, 
               color = "#21918c", 
               alpha = 0.3, 
               linewidth = 0.3) +
  geom_point(aes(x = long, 
                 y = lat), 
             data = usair_df, 
             color = "orange", 
             size = 1) +
  theme_void()

print(p)

# Community structure in the U.S. air transportation network

fg = fastgreedy.community(usair)
# so we know how many discrete colours we need
length(fg)
modularity(fg)
# [1] 0.3644685

colours_fg = c(turbo(14, alpha = 0.8))
par(mar = c(1,1,1,1))
plot(usair, 
     layout = longlat, 
     vertex.label = "", 
     vertex.color = colours_fg[fgmembership], 
     vertex.size = 3.5)
# dev.off()

# Dr. Shizuka does not generate an overlay on a map in his example but here
# is one.
# Plot the spatial network with community colors on the map
p = ggmap(map) +
  geom_segment(aes(x = from_long, 
                   y = from_lat, 
                   xend = to_long, 
                   yend = to_lat), 
               data = edges_df, 
               color = "black", 
               alpha = 0.2, 
               linewidth = 0.2) +
  geom_point(aes(x = long, 
                 y = lat, 
                 color = factor(fg$membership)), 
             data = usair_df,
             alpha = 0.8,
             size = 1.5) +
  scale_color_manual(name = "Group Membership", values = colours_fg) +  
  labs(title = "U.S. Air Transportation Network Community Structure") +
  theme_void()

print(p)

# Calculating within-Community Degree (z) and Participation Coefficient (P).

# For the within-community Degree of node i:
# ki is the number of links of node i to other nodes in its community (si)
# then this value is the z-score of the node’s degree within it’s community, 
# standardized across communities.

# For the participation Coefficient:
# kis is the number of links of node i to nodes in community s, and ki is the 
# total degree of node i. NM is the total number of communities. Thus, the 
# participation coefficient is close to one if the links of a node is evenly 
# distributed across all communities and zero if it links are exclusively with 
# nodes of their own community.

# calculate within-module degree, z
z = list()
for (i in 1:max(fgmembership)){ 
  newg = delete.vertices(usair, which(fgmembership!=i)) 
  wideg = degree(newg) 
  z[[i]] = (wideg - mean(wideg))/sd(wideg)
}
z = unlist(z)

# calculate participation coefficient
p = vector(length = vcount(usair))
for(i in 1:vcount(usair)){
  #get all neighbors of node i 
  nei = neighbors(usair, i) 
  #get table of community membership for neighbors
  tab = table(fgmembership[names = nei])  
  p[i] = 1-sum((tab/sum(tab))^2)
}

#create dataframe with airport name, z and p
dat = data.frame(name = V(usair)$name,
                 z = z[match(V(usair)$name, names(z))], 
                 p = p)

# The roles identified in the tutorial as as follows:
# * Most nodes are considered “Peripherals”, with relatively low z and low P
# * “Connector” nodes may have high P without having a high z: they don’t play 
# a strong role in their own community but tend to bridge communities. 
# These tend to be lesser-known airports (e.g., Topeka, Ketchikan).
# * “Provincial Hubs” are nodes that have high degree within their community 
# but low connections across communities. These include airports like Fairbanks 
# and Bethel in Alaska.
# * “Connector Hubs” are strongly connected within their community but also 
# bridge their community with other communities. These include aiports like 
# Anchorage and Seattle.

# Categorize airports based on z and p values
dat$Category = "Peripheral"
dat$Category[dat$z > 2 & dat$p < 0.21] = "Provincial Hub"
dat$Category[dat$z > 2.1 & dat$p >= 0.21 & dat$p <= 0.7] = "Connector Hub"
dat$Category[dat$z < 1.8 & dat$p >= 0.51 & dat$p <= 0.8] = "Connector"

# This is a reconstruction of what is on Dr. Shizuka's tutorial (he does not
# provide the code)
library(ggplot2)

# Define colors for each category
viridis(n = 4, alpha = 0.8)
category_colors = c("Peripheral" = "#440154B3", 
                     "Provincial Hub" = "#31688EB3", 
                     "Connector Hub" = "#35B779B3", 
                     "Connector" = "#FDE725B3")

# Plot the z vs. p scatter plot with colored points
ggplot(dat, aes(y = z, x = p, color = Category)) + 
  geom_point(size = 2, alpha = 0.5) +
  scale_color_manual(values = category_colors) +
  theme_light() + 
  theme(text = element_text(family = "sans", 
                            face = "plain", 
                            color = "#000000", 
                            size = 15, 
                            hjust = 0.5, 
                            vjust = 0.5)) + 
  scale_size(range = c(1, 3)) + 
  xlab("Participation Coefficient (p)") + 
  ylab("Within-Community Degree (z)") +
  labs(title = "U.S. Air Transportation Network Node Classification")


# Rnetcarto analysis (using the simulated annealing method) 
# This part was also slightly modified to generate the resulting plot at the end

# make the igraph object into an adjacency matrix (the format rnetcarto likes)
usair_mat = as_adjacency_matrix(usair, sparse=F) 
rnc = netcarto(usair_mat) 
# modularity
rnc[[2]]

# number of modules
length(unique(rnc[[1]]$module))
plot(rnc[[1]]$participation, rnc[[1]]$connectivity, pch = 19)

# make the igraph object into an adjacency matrix (the format rnetcarto likes)
usair_mat = as_adjacency_matrix(usair, sparse = FALSE) 
rnc = netcarto(usair_mat) 

# Extract z and p values from netcarto output
z_values = rnc[[1]]$within.module.degree.z
p_values = rnc[[1]]$participation.coefficient

# Categorize airports based on z and p values
dat = data.frame(name = V(usair)$name,
                  z = z_values,
                  p = p_values)

dat$Category = "Peripheral"
dat$Category[dat$z > 2.2 & dat$p < 0.2] = "Provincial Hub"
dat$Category[dat$z > 2.1 & dat$p >= 0.22 & dat$p <= 0.6] = "Connector Hub"
dat$Category[dat$z < 1.7 & dat$p >= 0.55 & dat$p <= 0.66] = "Connector"

# Define colors for each category with alpha value 0.3
category_colors = c("Peripheral" = "#440154B3", 
                     "Provincial Hub" = "#31688EB3", 
                     "Connector Hub" = "#35B779B3", 
                     "Connector" = "#FDE725B3")

# Plot the z vs. p scatter plot with colored points
ggplot(dat, aes(y = z, x = p, color = Category)) + 
  geom_point(size = 2, alpha = 0.5) +
  scale_color_manual(values = category_colors) +
  theme_light() + 
  theme(text = element_text(family = "sans", 
                            face = "plain", 
                            color = "#000000", 
                            size = 15, 
                            hjust = 0.5, 
                            vjust = 0.5)) + 
  scale_size(range = c(1, 3)) + 
  xlab("Participation Coefficient (p)") + 
  ylab("Within-Community Degree (z)") +
  labs(title = "U.S. Air Transportation Network Node Classification - Simulated annealing")
