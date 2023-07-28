
# load the manual
library(igraph)
library(help='igraph')

g = make_graph(~A-B-C-A, D-E-F-D, A-F) 
plot(g)
g

V(g) #look up vertices 
E(g) #look up edges
V(g)$name # vertex attributes

V(g)$color=c("white", "red", "green", "blue", "orange", "yellow") 
#a random set of colors 
plot(g)

# add edge attributes
E(g)$width=1:7
E(g)$color=rainbow(7) #rainbow() function chooses a specified number of colors 
plot(g)
g

# Data formats for networks
# adjacency matrix, edge list, and adjacency list.

# Adjacency matrix
# rows and columns represent different node.
# in unweighted adjacency matrices, the edges are represented by 0 and 1
# 1 = connection between two nodes
# in a weighted matric you can have different values which indicate edge
# qualities.

# Extract the adjacency matrix of the 'g' object
g_adj = as_adjacency_matrix(g, sparse=F)
g_adj

# Edge list
# Two-column list of two nodes that are connected in a network
# In directed networks, the edge goes from the vertex in the first column to
# the vertex in the second column.
# In undirected networks, the order of vertices don't matter.
# In weighted networks a third column indicates the edge weight.

g_edgeL = as_edgelist(g)
g_edgeL

# Affiliation matrix (incidence matrix, or individual-by-group matrix)
# For instances where co-membership/co-occurrence in groups matter.
# We need data in a matrix where rows are individuals/samples/species and
# where columns represent groups or populations (ideally)

A = c(1,1,0,0) 
B = c(1,0,1,0) 
C = c(1,0,1,0) 
D = c(0,1,0,1) 
E = c(0,0,1,1) 

aff = matrix(c(A,B,C,D,E), nrow=5, byrow=TRUE) 

dimnames(aff)= list(c("A","B","C","D","E"), 
                    c("Group1","Group2","Group3","Group4"))

aff #The individual-by-group matrix

# One-mode projection of the affinity matrix to convert it to a social network
# multiply the matrix with the transpose of itself

m2 = aff %*% t(aff)

g2 = graph_from_adjacency_matrix(m2, "undirected", weighted=T, diag=F)

plot(g2, edge.width=E(g2)$weight)

# Adjacency lists (a.k.a node list)
# Presents the focal node on the first column and then all the other nodes
# that connect to it as columns to the right of it

g_adj_list = as_adj_list(g)
g_adj_list

# Directed networks
# The adjacency matrix is not symmetrical for these
# If the cell value is 1, the edge goes from the row vertex to the column vertex

dir_g_net = make_graph(~A-+B-+C-+A, D-+E-+F-+D, A+-+F)
plot(dir_g_net)

as_adjacency_matrix(dir_g_net, sparse=F)

# For directed networks with mutual edges, the edge list lists both directions
# separately. In our example that means we have 8 rows, whereas an undirected
# version of the network there would be 7
as_edgelist(dir_g_net)

# Weighted networks code provided example does not actually store edge weights

as_data_frame(g)[,c(1:3)]


# Creating networks from edge lists

edge_dat = read.csv("https://dshizuka.github.io/network2018/NetworkWorkshop_SampleData/sample_edgelist.csv")
edge_dat

# Create the network using the edge list and edge values
set.seed(2)
eg = graph_from_data_frame(edge_dat, directed=FALSE) 
eg

plot(eg, edge.width = E(eg)$weight)

# Networks from adjacency matrices

am = as.matrix(read.csv("https://dshizuka.github.io/network2018/NetworkWorkshop_SampleData/sample_adjmatrix.csv", 
                        header=T, row.names=1))
am

# convert this adjacency matrix to an igraph object. Since the edges have 
# weights associated with them we need to set weighted=T

g = graph_from_adjacency_matrix(am, mode="undirected", weighted=T)

plot(g, edge.width = E(g)$weight)

# Network plots: layouts and attributes
# Two main topics: 
# (1) layout functions and how to merge network data  
# (2) attribute data to illustrate the network.

# Layout
#option 1
plot(g, layout=layout_in_circle(g))

#option 2:
l=layout_in_circle(g) 
class(l)
plot(g, layout=l)

# Force-directed layouts
# To create reproducible layouts set a seed
set.seed(10) 
l = layout_with_fr(g) 
plot(g, layout=l)

# Other force-directed algorithms
# Kamada & Kawai = kk
# Davidson & Harel = dh
# Frick et al = GEM

set.seed(10)
layouts = c("layout_with_fr", "layout_with_kk", "layout_with_dh", 
            "layout_with_gem", "layout_as_star", "layout_as_tree", 
            "layout_in_circle", "layout_on_grid")
par(mfrow=c(2,4), mar=c(1,1,1,1))

for(layout in layouts){
  l = do.call(layout, list(g))
  plot(g, layout=l, edge.color="black", vertex.label="", main=layout)
}

# clear the plotting region
dev.off()

# custom layouts
l = matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,6,7), ncol=2)
# vertices on a line and curved edges
plot(g, layout=l, edge.curved=TRUE, vertex.label="")

# Vertex attributes
# Usually stored on a separate file as node attributes

attrib = read.csv("https://dshizuka.github.io/networkanalysis/SampleData/sample_attrib.csv")
attrib

# sort the attributes to assign to the correct vertex with the match() function

V(g)$sex
# [1] M F M M F M F
# Levels: F M
V(g)$name
# [1] "Adam"    "Betty"   "Charles" "Daniel"  "Esther"  "Frank"   "Gina"   
V(g)$age
# [1] 30 28 35 15 12 10  8

V(g)$sex=factor(attrib[match(V(g)$name, attrib$Name), "Sex"]) 
# factor() preserves data as M/F
V(g)$age=attrib[match(V(g)$name, attrib$Name), "Age"]
g

# Assign colours to each sex
V(g)$color=c("gold","slateblue")[as.numeric(V(g)$sex)]
g

set.seed(10)
l = layout_with_gem(g)
plot(g, layout=l, vertex.label="", vertex.size=V(g)$age, edge.width=E(g)$weight, 
     edge.color="black")
legend("topleft", 
       legend=c("Female", "Male"), 
       pch=21, 
       pt.bg=c("gold", "slateblue"))

# Measuring networks
# 1) Nodes = centrality measures (position of nodes within a system for example)
# 2) Network = global measures (structure of the system as whole as an example)

library(asnipe)
library(igraph)

degree = igraph::degree
betweenness = igraph::betweenness
closeness = igraph::closeness
assoc = as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", 
                           header=T, row.names=1))
# group by data transpose
gbi = t(assoc) 

# adjacency matrix with "simple ratio index"
mat = get_network(t(assoc), association_index="SRI") 

#create a graph object
g = graph_from_adjacency_matrix(mat, "undirected", weighted=T) 

# plot the network
set.seed(10)
l = layout_with_gem(g)
plot(g, layout=l, 
     vertex.label="", 
     vertex.color="gold", 
     edge.color="slateblue", 
     edge.width=E(g)$weight*5)

# Node-level measures
# 1) Degree
# 2) Strength
# 3) Betweenness

degree(g)

# Vary node sizes proportional to degree centrality (number of edges connected
# to a given node)

set.seed(10)
de = igraph::degree(g)
plot(g, vertex.label="", 
     vertex.color="gold", 
     edge.color="slateblue", 
     vertex.size=de*2, 
     edge.width=E(g)$weight*5)

# Node strength (sum of weights of edges connected to the node)
# plot the node sizes proportional to these values

set.seed(10)
st = graph.strength(g)
plot(g,  vertex.label="", 
     vertex.color="gold", 
     edge.color="slateblue", 
     edge.width=E(g)$weight*5, 
     vertex.size=st*5)

# Betweenness: number of geodesic paths (shortest paths) that go through a given
# node. Nodes with high betweenness might be influential in a network.

be = betweenness(g, normalized=T)
plot(g,  vertex.label="", 
     vertex.color="gold", 
     edge.color="slateblue", 
     vertex.size=be*50, 
     edge.width=E(g)$weight*5)

# This example shows three nodes with qualitatively higher values of betweenness
# that act as bridges between different clusters of nodes

# Other centrality measures

# Closeness: Number of steps required to access every other node from a 
# given node
# function = closeness()
# Eigenvector centrality 	
# function = eigen_centrality() 	
# Values of the first eigenvector of the graph adjacency matrix. 
# The values are high for vertices that are connected to many other vertices 
# that are, in turn, connected many others, etc.

# Node-level measures
# In most analyses we will want to measure and compare node centrality with
# other traits, which requires putting together a dataframe combining
# vertex attributes and centrality measures

names = V(g)$name
de = degree(g)
st = graph.strength(g)
be = betweenness(g, normalized=T)

# assemble dataset
d = data.frame(node.name=names, degree=de, strength=st, betweenness=be) 
head(d)

# strength is the weighted version of degree, hence they will be correlated
plot(strength~degree, data=d)

# betweenness and strength are not since they describe something different
plot(betweenness~strength, data=d)

# Network-level measures
# 1) Size
# 2) Density
# 3) Components
# 4) Degree distributions
# 5) Average path length and diameter
# 6) Clustering coefficient

# number of vertices
n = vcount(g)
# number of edges
m = ecount(g)

n
m

# Density: density=[# edges that exist]/[# edges that are possible]
# In an undirected network with no loops, the number of edges that are possible
# is exactly the number of dyads that exist in the network.
# dyads= n(n-1)/2

dyads = n*(n-1)/2
density = m/dyads
density

# or alternatively
edge_density(g)

# Components
# If a network is composed of multiple components that aren't connected to one
# another, we can get this information with the components() function.
# For fully connected networks you can follow edges from any given vertex to
# all other vertices

components(g)
# The output will be:
# node membership, component sizes, and number of components
## $membership
## 23820 23726 23831 23763 23772 23770 23771 23777 23774 23860 23779 23773 
##     1     1     1     1     2     2     2     2     2     2     2     2 
## 23862 23857 23871 23853 23732 23734 23756 23759 23768 23758 23781 23815 
##     2     2     2     1     2     2     2     2     2     2     2     2 
## 23809 
##     2 
## 
## $csize
## 5 20
## 
## $no
## 2

# Degree distributions
# Statistical distribution of node degrees in a network

hist(degree(g), breaks=10, col="gray")

# When comparing multiple networks it is more useful to plot probability 
# densities of each degree

pk = degree.distribution(g)
plot(pk, pch=19)

# Average path length and diameter
# path = geodesic path or shortest path. The average path length and the 
# diameter (i.e. the maximum path length) are useful measures of a network.
# The average path length can be considered as the average degrees of separation
# whilst the diameter is the max degree of separation

paths = distances(g, algorithm="unweighted")
paths
# The Inf values are due to the fact that the network is not fully connected
# To deal with this you can ignore pairs of nodes that are in different 
# components and only measure avg lengths of the paths that do exist.
# Alternatively, you can measure each component separately which does permit
# calculation of diameter.

# Option 1
paths[paths=="Inf"] = NA
# we want either the upper or lower triangle of the matrix
mean(paths[upper.tri(paths)], na.rm=T)
# or alternatively (however, it is not returning the mean distance)
mean_distance(g)

# Option 2
# decompose the network into a list that contains each component as separate
# graph objects. Then calculate the separate path length matrice and the mean
# and max for each matrix

comps = decompose(g)
comps # a list object consisting of each component as graph object

# make a list object with two path length matrices
path_list = lapply(comps, function(x) distances(x, algorithm="unweighted")) 
avg_paths = sapply(path_list, mean) #average path length of each component
diams = sapply(path_list, max) #diameter of each component
avg_paths
diams


# Clustering coefficient (transitivity)
# It deals with the probability of two nodes that are connected to a common node
# being connected themselves (e.g the probability of two of your friends
# knowing each other)

# Global clustering coefficient = ratio of triangles to connected triples
# Local clustering coefficient = for each node, the proportion of their 
# neighbours that are connected to each other

g_cluster = transitivity(g, "global")
l_cluster = transitivity(g, "local")
av_l_cluster = transitivity(g, "localaverage")

g_cluster
l_cluster
av_l_cluster

# Community structure and Assortment
# How to detect the presence of clusters or communities and how can we quantify
# the degree of community structure

library(igraph)
library(assortnet)

# Modularity and community detection
# Modularity (Q) quantifies the degree to which clusters are discrete. Q is a 
# measure of the proportion of edges occurring within communities relative to
# the expected proportion if all edges were placed randomly.

# Modularity-optimisation techniques come in two main flavours agglomerative vs 
# divisive methods. These methods have limitations of course, and much like
# density based clustering algorithms such as DBSCAN they tend to miss small,
# well-defined communities/clusters when there are other larger communities.
# They also make a basic assumption that each node can belong to just one 
# community.

# Community detection methods in igraph

# Function 	
# edge.betweenness.community() 	
# One of the first in the class of “modularity optimization” algorithms. 
# It is a “divisive” method. Cut the edge with highest edge betweenness, 
# and recalculate. Eventually, you end up cutting the network into different 
# groups. 	
# Newman & Girvan 2004

# fastgreedy.community() 	
# Hierarchical agglomerative method that is designed to run well in large 
# networks. Creates “multigraphs” where you lump groups of nodes together in 
# the process of agglomeration in order to save time on sparse graphs. 	
# Clauset et al. 2004

# walktrap.community() 	
# Uses random walks to calculate distances, and then use agglomerative method 
# to optimize modularity 	
# Pons & Latapy 2005

# spinglass.community() 	
# This method uses the analogy of the lowest-energy state of a collection of 
# magnets (a so-called spin glass model). 	
# Reichardt & Bornholdt 2006

# leading.eigenvector.community() 	
# This is a “spectral partitioning” method. You first define a 
# ‘modularity matrix’, which sums to 0 when there is no community structure. 
# The leading eigenvector of this matrix ends up being useful as a community 
# membership vector. 	
# Newman 2006

# label.propagation.community() 
# Raghavan et al. 2007

# cluster_louvain() 	
# The “Louvain” method, so-called because it was created by a group of 
# researchers at Louvain University in Belgium. Community aggregation 	
# Blondel et al. 2008

# rnetcarto::netcarto() 	
# Simulated Annealing method. Thought to be useful for smaller networks. 
# Available through ‘rnetcarto’ package (Doulcier 2015). 	
# Guimera & Amaral 2005

# A simple example of modularity and community detection

g3 = make_graph(~A:B:C:D:E-A:B:C:D:E, F:G:H:I:J-F:G:H:I:J, A-F, B-G)
set.seed(7)
l = layout_with_gem(g3)
plot(g3, layout=l, edge.color="black")

# The community division in this example is clear so any of the functions
# described above would provide the same answer

eb = edge.betweenness.community(g2) 
eb
# IGRAPH clustering edge betweenness, groups: 2, mod: 0.41
# + groups:
#   $`1`
# [1] "A" "B" "C" "D" "E"
# 
# $`2`
# [1] "F" "G" "H" "I" "J"

# number of communities
length(eb)

# modularity (Q)
modularity(eb)

# membership
membership(eb)

# and finally, this eb object can be used to produce a plot with the community
# structure
plot(eb, g2, layout=l)

# Example: community detection with sample sparrow network (individual-by-group
# matrix)

library(asnipe)
library(igraph)

#import individual-by-group data
assoc = as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", 
                           header=T, row.names=1)) 
# group-by-individual transpose
gbi = t(assoc) 

# create an adjacency matrix with "simple ratio index"
mat = get_network(t(assoc), association_index="SRI") 

# make into igraph object
g_sparrow = graph_from_adjacency_matrix(mat, "undirected", weighted=T) 

plot(g_sparrow, edge.width=E(g_sparrow)$weight*5, vertex.label="")

# community detection with all different algorithms
# 1)
com_eb = edge.betweenness.community(g_sparrow)
com_eb

set.seed(2)
plot(com_eb, g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)

# 2)
com_fast_g = fastgreedy.community(g_sparrow)
com_fast_g

set.seed(2)
plot(com_fast_g, g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)

# 3)
com_walktrap = walktrap.community(g_sparrow)
com_walktrap

set.seed(2)
plot(com_walktrap, g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)

# 4) Spinglass does not work with unconnected graphs

# 5)
com_leading_eigenv = leading.eigenvector.community(g_sparrow)
com_leading_eigenv

set.seed(2)
plot(com_leading_eigenv, g_sparrow, vertex.label="", 
     edge.width=E(g_sparrow)$weight*5)

# 6)
com_lab_prop = label.propagation.community(g_sparrow)
com_lab_prop

set.seed(2)
plot(com_lab_prop, g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)

# 7)
com_louv = cluster_louvain(g_sparrow)
com_louv

set.seed(2)
plot(com_louv, g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)


library(RColorBrewer)
library(viridisLite)
colours_rcbrewer = brewer.pal(length(com_louv),'Accent') #make a color palette
colours_viridis = viridis(n = length(com_louv), option = "D")

#assign each vertex a color based on the community assignment
V(g_sparrow)$color = colours_rcbrewer[membership(com_louv)] 

set.seed(2)
plot(g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)

# Now with viridis
#assign each vertex a color based on the community assignment
V(g_sparrow)$color = colours_viridis[membership(com_louv)] 

set.seed(2)
plot(g_sparrow, vertex.label="", edge.width=E(g_sparrow)$weight*5)

# Assortment (a.k.a homophily)
# The tendency for nodes that share a trait to be connected
# The assortment coefficient (r) is similar to the modularity index (Q) but the
# assortment coefficient is used when we know a priori the 'type' or 'value' of 
# nodes. For example, we can use this coefficient to examine whether discrete
# node types (e.g., species, ethnicity, etc) are more or less connected to each
# other. It can also be used with "scalar attributes" (i.e continously varying
# traits)
# The coefficient is large (approaches 1) if nodes with similar values are more
# connected and small when similar nodes are less connected (approaches 0). The 
# value is 0 when edges are random with respect to node values

# The assortativity() function in igraph can calculate assortment in directed
# or undirected networks but it cannot handle weighted networks

# To measure assortativity on weighted networks we can use the 
# assortment.discrete() or assortment.continuous() function in the assortnet
# package.

# igraph method
set.seed(3)
# assign sizes to nodes using two normal distributions with different means
# as a way to assign node types, this allows us to measure the degree 
# to which this network exhibits assortment by node size
V(g3)$size = c(rnorm(5, mean=20, sd=5), rnorm(5, mean=30, sd=5)) 
plot(g3, layout=l, edge.color="black")
assortativity(g3, V(g3)$size, directed=F)
# [1] 0.5605718

# We can convert the size into a discrete trait and calculate the assortment
# coefficient

# make the values = 1 if large individual and 0 if small individual, 
# with cutoff at size = 2
V(g3)$size.discrete = (V(g3)$size>25)+0 
assortativity(g3, V(g3)$size.discrete, directed=F)
# [1] 0.8181818

# By way of comparison we can create a node attribute that varies randomly 
# across all nodes in the network on which we can then measure the assortativity
# coefficient.

set.seed(3)
#create a node trait that varies randomly for all nodes 
V(g3)$random = rnorm(10, mean=20, sd=5) 
assortativity(g3, V(g3)$random, directed=F)
## [1] -0.06392816
plot(g3, layout=l, edge.color="black", vertex.size=V(g3)$random)
# There is little assortment based on this "trait"

# Using the assortnet package for weighted networks

adj = as_adjacency_matrix(g3, sparse=F)
assortment.continuous(adj, V(g3)$size)
# $r
# [1] 0.5605718

# the mixing matrix allows us to know the cumulative edge weights that occur
# between individuals with the same vs different vertex labels
assortment.discrete(adj, V(g3)$size.discrete)
# $r
# [1] 0.8181818
#
# $mixing_matrix
#             0          1  ai
# 0  0.45454545 0.04545455 0.5
# 1  0.04545455 0.45454545 0.5
# bi 0.50000000 0.50000000 1.0


# Since we can do weighted edges let's add edge weights randomly and calculate
# the assortativity coefficient. We need an adjacency matrix as input which
# we need to generate after adding the weights

E(g3)$weight = runif(length(E(g3)), min=0, max=1)

adj = as_adjacency_matrix(g3, sparse=F, attr="weight")

assortment.continuous(adj, V(g3)$size)
# $r
# [1] 0.5545792
assortment.discrete(adj, V(g3)$size.discrete)
# $r
# [1] 0.498065
# 
# $mixing_matrix
#            0         1        ai
# 0  0.4628330 0.1218816 0.5847146
# 1  0.1218816 0.2934038 0.4152854
# bi 0.5847146 0.4152854 1.0000000

# Statistical testing
# Permutations and randomisations against the null hypothesis

# Because most networks are complex and relational data do not conform to the
# requirement of non-independence of data, we often use randomisations
# or permutations to generate null models against which we can compare our
# empirical data

# Return to this after reading Farine's 2017 article on this.

# Network regression (MRQAP)
# Relational data are not independent, e.g. if we want to predict how the 
# similarity between two nodes affects the probability of an edge, or how 
# network relations predict the transmission of pathogens we require methods 
# that account for this non-independence.
# The Mantel test is a non-parametric matrix correlation test that uses 
# permutations (node-permutations in this case) which is used to get around
# the problem of non-independence. A version focused on ecological applications
# is available on the 'ecodist' package.

# The Quadratic Assignment Procedure (QAP) is an extension of the Mantel Test 
# in a regression framework. The Multiple Regression Quadratic Assignment 
# Procedure (MRQAP) is an extension of this approach to allow for multiple 
# covariate matrices. Essentially, MRQAP allows you to determine the influence 
# of one matrix on another, controlling for the effects of one or more 
# covariate matrices. There are several forms of MRQAP, but the most popular 
# method is called the Double Semipartialling (DSP) method. This method 
# permutes the matrix of residuals from the ordinary least regression of the 
# dependent matrix on the independent matrices, to estimate error and calculate 
# the effects. MRQAP can be implement in 'sna' which is itself bundels in the
# 'statnet' and 'asnipe' packages.

# What the QAP does is to scramble the dependent variable data through several
# permutations (1000 is the default in these packages but can be adjusted).
# By taking the data and permuting it, multiple random datasets are generated
# and multiple analyses can then performed on these.
# Those datasets and analyses form an empirical sampling distribution, the 
# coefficients of which we can compare our coefficient against. In other words,
# we are preserving the dependence within rows and columns but we are removing
# the relationship between the dependent and independent variables.

# Small p-values evidently would suggest that coefficients are significant.
# In this case, they aren't.
# R-squared reports the percentage of the variance in the dependent variable(s)


library(sna) 
library(asnipe)

#generate 3 random adjacency matrices using the rgraph() function within sna
set.seed(2)
m1 = rgraph(10, m=1, tprob=0.5, mode="graph")
m2 = rgraph(10, m=1, tprob=0.5, mode="graph") 
m3 = rgraph(10, m=1, tprob=0.5, mode="graph")

#test the effect of m2 on m1, controlling for m3 (with the sna package)
netlm(m1, m2+m3, mode="graph", nullhyp="qap", test.statistic="t-value")

# OLS Network Model
#
# Coefficients:
#   Estimate   Pr(<=b) Pr(>=b) Pr(>=|b|)
# (intercept)  0.6245211 1.000   0.000   0.000    
# x1          -0.0862069 0.205   0.798   0.394    
#
# Residual standard error: 0.5044 on 43 degrees of freedom
# F-statistic: 0.6778 on 1 and 43 degrees of freedom, p-value: 0.4149 
# Multiple R-squared: 0.01552 	Adjusted R-squared: -0.007378 

#test the effect of m2 on m1 controlling for m3, 
# and effect of m3 on m1, controlling for m2 (with the asnipe package)

mrqap.dsp(m1~m2+m3, directed="undirected")

# MRQAP with Double-Semi-Partialing (DSP)
# 
# Formula:  m1 ~ m2 + m3 
# 
# Coefficients:
#           Estimate    P(β>=r) P(β<=r) P(|β|<=|r|)
# intercept  0.62001925 1.000   0.000   0.000      
# m2         0.05813282 0.678   0.322   0.664      
# m3        -0.23561116 0.064   0.936   0.123      
# 
# Residual standard error: 0.5002 on 42 degrees of freedom
# F-statistic: 1.202 on 2 and 42 degrees of freedom, p-value: 0.3106 
# Multiple R-squared: 0.05416  Adjusted R-squared: 0.00912 
# AIC: -40.77726

# Simulation of asocial changes in state (e.g. asocial learning)
# Each individual in a social network has an inherent probability to adopt an
# innovation at any given time.
# First we will make a random graph consisting of 100 nodes. We will set the
# initial 'innovation adoption' status for all individuals (all initially 0)

set.seed(4)
n = 100
g = erdos.renyi.game(n, p=0.05)

# the 'innovation adoption status' for each individual. All initially 0.
V(g)$status = 0 

l = layout_with_fr(g)
plot(g, vertex.label="", vertex.size=8, vertex.color="darkgray", layout=l)

# Set the asocial learning parameter, x to be 0.1. This is the probability that
# any given individual will come up with the innovation-e.g., how to forage for
# a new prey item.
x = 0.1

# which individuals have not adopted yet?
naive = which(V(g)$status == 0) 

# based on the probabilities, flip a coin for each naive individual and 
# determine if the individual adopts the innovation in the current time step
adopt = sample(c(1,0), length(naive), prob=c(x, 1-x), replace=T) 

# change status of individuals whose status is 0 and is in the list of new 
# adopters
V(g)$status[naive][which(adopt == 1)]=1 

plot(g, vertex.label="", vertex.color=c("darkgray", "red")[V(g)$status+1], 
     vertex.size=8, layout=l)

# We assume that any given individual can only adopt an innovation once (it 
# can't go back), it's akin to an SI model in which individuals do not recover
# from infection or go back to a susceptible state. In practical terms this
# means that we will ignore coin flips for individuals whose status=1.

t = 35
g_time=list()
V(g)$status = 0
for(j in 1:t){
  naive = which(V(g)$status == 0) 
  adopt = sample(c(1,0), length(naive), prob=c(x, 1-x), replace=T)
  V(g)$status[naive][which(adopt == 1)]=1 
  g_time[[j]]= g
}

g_time
# we end up with n=t igraph objects in a list called g_time. In each graph,
# the only thing that changes across t=n is the individual status. There will 
# be more individuals that adopt the innovation across each time step.

# for each time step, count the number of individuals that have adopted the 
# innovation
n_adopt_asocial = sapply(g_time, function(x) length(which(V(x)$status == 1)))

plot(n_adopt_asocial, 
     type="b", 
     las=1, 
     ylab="Cumulative number of nodes adopted", 
     xlab="Time", 
     ylim=c(0,100))
# There is of course a steady decelerating rate of adoption as we run out of 
# individuals that can adopt the innovation

# Plot the first 10 time points for example

def_par = par(no.readonly = TRUE)
layout(matrix(1:15, byrow=T, nrow=3))
par(mar = c(1,1,1,1))
for(i in 1:15){
  v_col = c("darkgray", "red")[V(g_time[[i]])$status+1]
  plot(g_time[[i]], 
       vertex.label = "", 
       vertex.color = v_col, 
       layout = l, 
       main=paste("Time",i))
}
dev.off()

# Simulating the social transmission of whatever state in a random graph

set.seed(4)
n = 100
g2 = erdos.renyi.game(n, p=0.05)
l2 = layout_with_fr(g2)

# Create the status vertex attribute where everyone will start with state=0,
# then randomly pick 2 nodes which will have state=1

V(g2)$status = 0 
seed = sample(V(g2), 2)
#These 'seed' individuals get a status of 1 at the beginning.
V(g2)$status[seed] = 1 

plot(g2, 
     vertex.label="", 
     vertex.size=8, 
     vertex.color=c("darkgray", "red")[V(g2)$status+1], 
     layout=l2)

# set a social transmission parameter, s. Think of it as the linear increase 
# in the probability that an individual will take on a new state when it has
# a neighbour that has that state.
# Since it is a probability 0 =< s =< 1
# set tau to 0.1 for now

tau = 0.1

# Now we will simulate 35 time steps of the spread of this innovation. 
# We will save the network for each time point. 
# The for-loop routine will be as follows:
# 1) first, we will use the neighbors() function to identify the neighbors 
# (i.e., nodes connected to) of each node. We will use this to add up the 
# status of each node’s neighbors.
# 2) Next, we will implement a social learning process in which the 
# probability p that an individual that has not yet adopted the innovation 
# will adopt in that time step = 1−e−τ∗s, where τ is the parameter that 
# describes the influence of social learning, and s is the number of 
# neighbors of an individual that has already adopted the innovation.
# 3) Based on the calculated probability p for each individual, we then use 
# the sample() function to “flip a biased coin” to see if the focal individual 
# adopts the innovation or not.We do this for every individual that has not yet 
# adopted the innovation (i.e., status = 0). Note that if the focal individual 
# has already adopted the innovation, then we just ignore that individual and 
# move on.
# 4) We then change the status of each individual that got a “1” in the coin 
# flip to status=1
# 5) Return to step 1.

t = 35
g2_time = list() #empty list to store the output networks
for(j in 1:t){
  nei_adopt = sapply(V(g2), function(x) sum(V(g2)$status[neighbors(g2,x)]))
  # here, we multiply the probabilities by 0 if node is already adopted, 
  # and 1 if not yet adopted
  p = (1-exp(-tau*nei_adopt))*abs(V(g2)$status-1) 
  adopters = sapply(p, function(x) sample(c(1,0), 1, prob=c(x, 1-x)))
  V(g2)$status[which(adopters == 1)]= 1
  g2_time[[j]] = g2
}

# Plot the accumulation curve

#for each time step, count the number of adopters.
n_adopt_social = sapply(g2_time, function(x) length(which(V(x)$status==1))) 

plot(n_adopt_social, 
     type="b", 
     las=1, 
     ylab="Cumulative number of nodes adopted", 
     xlab="Time", 
     ylim=c(0,100))

# Plot the asocial and social case together
plot(n_adopt_social, 
     type="l", 
     lty=1, 
     col="black",
     las=1, 
     ylab="Cumulative number of nodes adopted", 
     xlab="Time", 
     ylim=c(0,100))
points(n_adopt_asocial, 
       type="l", 
       las=1, 
       lty=2, 
       col="red")
legend("topleft", 
       lty=c(1,2), 
       col=c("black", "red"), 
       legend=c("asocial", "social"))

# Which is the same as the theoretical expectation. Asocial learning (e.g. 
# trial and error) produces decelerating curves, while social learning 
# (e.g by observing other group members) results in sigmoid-shape curves

# Animating the social transmission simulation
# If on linux (ubuntu or debian) you need to install Magick++ first
# sudo apt-get install -y libmagick++-dev

# library(remotes)
# install_github("yihui/animation")

library(animation)
saveGIF( 
  {for (i in 1:35) {
    plot(g2_time[[i]], 
         layout=l2, 
         vertex.label="", 
         vertex.size=5, 
         vertex.color=c("darkgray", "red")[V(g2_time[[i]])$status+1], 
         main=paste("time",i,sep=" "))
  }
  }, movie.name="social_transmission_sim.gif", 
  interval=0.2, 
  nmax=35, 
  ani.width=600, 
  ani.height=600)


# Effect of network structure on transmission dynamics
# Make a network with 100 nodes and strong community structure.
# Divide the nodes between 4 communities and set the probability of edges to
# be much higher within communities (0.5) compared to across communities (0.005)


# Set the random seed to ensure reproducibility
set.seed(2)

# Define the number of nodes in the graph
n = 100

# Create a vector 'mod_assign' that assigns 25 nodes to each of the 
# 4 modules (groups)
mod_assign = c(rep(1, 25), rep(2, 25), rep(3, 25), rep(4, 25))

# Create a binary matrix 'same_mod' where each entry (i, j) is 1 if nodes i 
# and j belong to the same module, and 0 otherwise
same_mod = outer(mod_assign, mod_assign, FUN = "==")

# 'same_mod' is a symmetric matrix because the relationship between nodes 
# i and j is the same as nodes j and i. The diagonal of 'same_mod' will be 1 
# as each node is in the same module as itself.

# Create a probability matrix 'p_mat' based on the 'same_mod' matrix,
# where the value at (i, j) represents the probability of an edge between 
# nodes i and j.
p_mat = apply(same_mod, c(1, 2), function(x) c(0.005, 0.5)[x + 1])

# For each entry in 'p_mat', a random Bernoulli trial is performed to determine 
# the presence or absence of an edge.
# The adjacency matrix 'adj' is created with 1s representing edges and 0s 
# representing no edges.
adj = apply(p_mat, c(1, 2), function(x) sample(c(1, 0), 1, prob = c(x, 1 - x)))

# Make the adjacency matrix symmetric (undirected graph) by setting the 
# lower triangular elements to 0 and copying the upper triangular elements.
adj[lower.tri(adj)] = 0
diag(adj) = 0
adj = adj + t(adj)

# Create an undirected graph 'g3' from the adjacency matrix using the 
# igraph package
g3 = graph_from_adjacency_matrix(adj, "undirected")

l3 = layout_with_fr(g3)

# Plot the graph 
plot(g3, 
     vertex.label = "",      # No vertex labels will be shown
     vertex.size = 3,        # Set the vertex size to 3
     layout = l3)            # Use the previously computed layout for plotting

# Create a vertex attribute for adoption status. 1 if the node has adopted the 
# innovation. 0 if not.
V(g3)$status = 0 

#select 2 innovators
seed = sample(V(g3),2) 

#These 'seed' individuals get a status of 1 at the beginning.
V(g3)$status[seed] = 1 

plot(g3, 
     vertex.label="", 
     vertex.size=8, 
     vertex.color=c("darkgray", "red")[V(g3)$status+1], 
     layout=l3)

# Now run a simulation with the same tau parameter as before (tau = 0.1)

# Set the value of tau, which represents a parameter controlling the 
# influence of neighbors on adoption probability
tau = 0.1

# Set the number of time steps to run the simulation
t = 30

# Create an empty list 'g3_time' to store the output networks at each time step
g3_time = list()

# Run the simulation for 't' time steps
for (j in 1:t) {
  # Calculate the number of adopted neighbors for each node in the graph 'g3'
  # 'V(g3)' returns the vertex sequence of the graph, and 'neighbors(g3, x)' 
  # returns the neighbours of vertex 'x'
  # 'nei_adopt' is a vector representing the count of adopted neighbors for 
  # each node
  nei_adopt = sapply(V(g3), function(x) sum(V(g3)$status[neighbors(g3, x)]))
  
  # Calculate the adoption probability for each node using the influence of 
  # its neighbors
  # 'abs(V(g3)$status - 1)' is used to convert the 'status' vector to a binary 
  # vector (0s and 1s)
  # Nodes that have already adopted (status=1) will have a probability of 0, 
  # and nodes that have not adopted (status=0) will use the formula 
  # (1-exp(-tau*nei_adopt))
  p = (1 - exp(-tau * nei_adopt)) * abs(V(g3)$status - 1)
  
  # Randomly decide which nodes will adopt based on the calculated probabilities
  # For each node, generate a random sample from {0, 1} using the probabilities 
  # in vector 'p'
  adopters = sapply(p, function(x) sample(c(1, 0), 1, prob = c(x, 1 - x)))
  
  # Update the 'status' of nodes that have newly adopted (adopters=1) to 
  # 1 (adopted)
  V(g3)$status[which(adopters == 1)] = 1
  
  # Save the current state of the graph 'g3' in the list 'g3_time' at the 
  # current time step 'j'
  # This allows us to keep track of the graph's state at each time step
  g3_time[[j]] = g3
}

# and plot

# Calculate the number of nodes that have adopted the behavior ('status' is 1) 
# at each time step in 'g3_time'
# 'V(x)$status == 1' returns a logical vector indicating whether the 'status' 
# attribute of each node is equal to 1 (adopted)
# 'which()' returns the indices of 'TRUE' values in the logical vector, and 
# 'length()' counts the number of such indices,
# representing the number of nodes that have adopted the behavior in graph 'x'
n_adopt_social = sapply(g3_time, function(x) length(which(V(x)$status == 1)))

plot(n_adopt_social, 
     type="l", 
     las=1, 
     ylab="Cumulative number of nodes adopted", 
     xlab="Time", 
     ylim=c(0,100))

# However, animating the network shows that the spread is heavily influenced
# by community structure

library(animation)

saveGIF( 
  {for (i in 1:30) {
    plot(g3_time[[i]], 
         layout=l3, 
         vertex.label="", 
         vertex.size=5, 
         # 'vertex.color=c("darkgray", "red")[V(g3_time[[i]])$status+1]' 
         # assigns colors to the vertices based on their 'status' attribute
         # Nodes with 'status' 0 are colored dark gray, and nodes with 'status' 
         # 1 are colored red
         vertex.color=c("darkgray", "red")[V(g3_time[[i]])$status+1], 
         main=paste("time",i,sep=" "))
  }
  }, movie.name="05_community_structure_sim.gif", 
  interval=0.2, 
  nmax=30, 
  ani.width=600, 
  ani.height=600)

# Network based diffusion analysis (NBDA)
# This modelling approach deal with the question of whether the order or
# timing of acquisition of a behaviour or change of state of an individual
# is indicative of difussion on a network. 
# To do this it uses a maximum likelihood model fitting.

# install_github("whoppitt/NBDA")

library(NBDA)

# Convert the adjacency matrix of the graph 'g3' to a 3D array 'adj_array'
# The third dimension of the array is set to 1, as there is only one time 
# step (static network)
adj_mat = as_adj(g3, sparse = FALSE)
adj_array = array(dim = c(nrow(adj_mat), ncol(adj_mat), 1))
adj_array[, , 1] = adj_mat

# Create a matrix 'solve_mat' containing information about node adoption 
# status at each time step. Each column represents a time step, and each row 
# represents a node's adoption status at that time step
solve_mat = sapply(g3_time, function(x) {
  V(x)$status
})

# Calculate the time of acquisition 'ta' for each node
# 'ta' represents the time step when a node adopts the behavior for the first 
# time (status changes from 0 to 1)
# 'which(x == 1)' returns the indices where 'x' is equal to 1 
# (indices of adopted nodes)
# 'min(which(x == 1))' returns the index of the first adoption, i.e., 
# the time of acquisition
ta = apply(solve_mat, 1, function(x) min(which(x == 1)))
# If 'min(which(x == 1))' returns Inf (no adoption), set the time of 
# acquisition to the last time step (30)
ta[is.infinite(ta)] = 30   

# Determine the order of acquisition 'oa' by sorting nodes based on their 
# time of acquisition 'ta'
oa = order(ta)

# Create an NBDA data object 'diffdat' using the prepared data
# The 'nbdaData' function creates an object with all the information needed 
# for NBDA
# 'assMatrix' is the 3D array representing the network at each time step
# 'orderAcq' is the order of acquisition of nodes, and 'timeAcq' is the time 
# of acquisition for each node
diffdat = nbdaData("try1", assMatrix = adj_array, orderAcq = oa, timeAcq = ta)

# oa.fit_social = oadaFit(diffdat, type="social")
# oa.fit_social@outputPar
# oa.fit_social@aic
# data.frame(Variable = oa.fit_social@varNames,
# MLE = oa.fit_social@outputPar,
# SE = oa.fit_social@se)

# Fit a model to the NBDA data using the 'tadaFit' function with type "social"
# 'ta_fit_social' is the fitted NBDA model object for the social influence type
ta_fit_social = tadaFit(diffdat, type = "social")
# ta.fit_social@outputPar

# Extract and display the Maximum Likelihood Estimates (MLE) and standard 
# errors for the model parameters
result_df = data.frame(
  Variable = ta_fit_social@varNames,
  MLE = round(ta_fit_social@outputPar, 3),
  SE = round(ta_fit_social@se, 3)
)

result_df
#                  Variable     MLE      SE
# 1         Scale (1/rate): 603.449 603.234
# 2 1 Social transmission 1 101.839 102.600

# Now let's look how the asocial learning case looks

adj_mat = as_adj(g, sparse=F)
adj_array = array(dim = c(nrow(adj_mat), ncol(adj_mat), 1))
adj_array[,,1] = adj_mat

# get list of individuals that solved at each time
# solve_mat is a matrix containing information about node adoption status 
# at each time step. Each column represents a time step, and each row 
# represents a node's adoption status at that time step
solve_mat = sapply(g_time, function(x){
  V(x)$status
})

#time of acquisition
ta = apply(solve_mat, 1, function(x) which.max(x == 1))
# If 'which.max(x == 1)' returns Inf (no adoption), set the time of acquisition 
# to the last time step (30)
ta[is.infinite(ta)] = 30
#order of acquisition
oa = order(ta)

diffdat = nbdaData("try1",  assMatrix = adj_array, orderAcq = oa, timeAcq = ta)

# oa.fit_social = oadaFit(diffdat, type="social")
# oa.fit_social@outputPar
# oa.fit_social@aic
# data.frame(Variable = oa.fit_social@varNames,
# MLE = oa.fit_social@outputPar,
# SE = oa.fit_social@se)

ta_fit_social2 = tadaFit(diffdat, type="social")
# ta.fit_social2@outputPar

result_df = data.frame(
  Variable = ta_fit_social2@varNames,
  MLE = round(ta_fit_social2@outputPar, 3),
  SE = ta_fit_social2@se
)
result_df

#                  Variable   MLE  SE
# 1         Scale (1/rate): 0.000 NaN
# 2 1 Social transmission 1 9.744 NaN

# Random graphs

# Erdös-Renyí Random Graph
# 1) G(n,m) model, in which n nodes are randomly connected by m edges.
# 2) G(n,p) model, in which we have a graph of n nodes, and each pair of nodes 
# has probability p of being connected.

# The main property of an Erdös-Renyí Random Graph is that, given n nodes and m 
# edges (or probability p of an edge between each pair of nodes), everything 
# else is unconditioned–i.e., random.

library(igraph)
# G(n,m) model
g1 = erdos.renyi.game(20, 38, type = "gnm") 
graph.density(g1) 
plot(g1,layout = layout.circle)
# density will always remain the same (#edges/[#dyads] = 38/[20*19/2] = 0.2)

# G(n,p) model
g2 = erdos.renyi.game(20, 0.2, type = "gnp") 
graph.density(g2)
plot(g2, layout = layout.circle)

# Your output will look approximately like mine, but it’ll be a bit different. 
# This is because now the number of edges is a probabilistic outcome of having 
# p = 0.2 chance of each dyad being connected.

# Ensembles of random graphs
# Calculating the densities for 100 random graphs of same n and p

# empty vector
densities = vector(length=100) 
for (i in 1:100){
  # random graph
  r = erdos.renyi.game(20, 0.2, type = "gnp") 
  # store the density of random graph as the ith element of the vector
  densities[i] = graph.density(r) 
}
#print the resulting vector
densities 
mean(densities)
#[1] 0.2009474

hist(densities)
abline(v = 0.2, lwd = 3, lty = 2, col = "red")

# Make a plot of 9 random graphs
par(mfrow = c(3,3), mar = c(1,1,1,1))
for (i in 1:9){
  r = erdos.renyi.game(20, p=0.2)
  plot(r, 
       layout = layout.circle,
       edge.color = "black",
       edge.width = 2,
       vertex.color = "red",
       vertex.label="")}

# Properties of random graphs
# As with Hardy-Weinberd Equilibrium in population genetics, Erdos-Renyi random
# graphs serve as a good null hypothesis of what the structure of a basic 
# system of n components and m connections or p probability of connections 
# looks like, all else being equal.

# Basic properties
# total number of edges = (n*(n−1)/2)*p
# mean degree = (n−1)*p
# clustering coefficient = p

# Those properties can of course be confirmed by generating random graphs and
# calculating these basic properties.

# First, create a set of vectors in which you'll store the results of the 
# simulations
m = vector(length = 100)
mean_k = vector(length = 100)
C_loc = vector(length = 100)
C_glob = vector(length = 100)

# Now, use a For-loop to create 100 random graphs, each time calculating the m, 
# mean degree and clustering coefficient
n = 20
p = 0.2
for (i in 1:100){
  r = erdos.renyi.game(n, p = p)
  m[i] = ecount(r)
  mean_k[i] = mean(degree(r))
  C_loc[i] = transitivity(r, type = "localaverage")
  C_glob[i] = transitivity(r, type = "global")
}

par(mfrow = c(1,3)) 
hist(m)
# expected number of edges, which is simply the number of dyads times p
abline(v = (n*(n-1)/2)*p, lty = 2, col = "red") 
hist(mean_k)
# expected mean degree, which is (n-1)*p
abline(v = (n-1)*p, lty = 2, col = "red") 
hist(C_glob)
#expected global clustering coefficient, which is simply p
abline(v = p, lty = 2, col = "red") 
dev.off()

# Path length and network size
# How does average path length of a network changes as we add more nodes?
seq(from = 10, to = 100, by = 10)

paths = matrix(ncol = 10, nrow = 500) 
for (n in seq(10, 100, 10)){
  for(i in 1:500){ 
    # let’s say each person knows 5 people then p = 5/(n-1). 
    r = erdos.renyi.game(n, p = (5/(n-1))) 
    paths[i,n/10] = average.path.length(r)
  }
}

paths_avg = apply(paths, 2, mean) 


x = seq(10, 100, 10)
plot(x, paths_avg,
     type = "b",
     pch = 20, 
     xlab = "N", 
     ylab = "Average Path Length", 
     las = 1)

plot(x, paths_avg,
     type = "b",
     pch = 20,
     xlab = "N",
     ylab = "Average Path Length",
     log = "x",
     las = 1)

# Path length does not increase as fast as network size. In fact, the average
# path length increases in proportion to the log of the network size in a 
# random graph. Number of nodes that are 's' steps away scales exponentially
# with the average degree, so, for example, if everyone knows 5 people, you
# can potentially reach up to 5^2 = 25 people in two steps, and 5^3 = 125 
# people in three steps, etc.

rm(list = ls())
# Simulating networks

library(asnipe)
library(igraph)

# scenario: individuals vary in how often they joing groups (e.g., flocks).
# These individuals also vary in a trait (e.g., body size) that correlates with
# gregariousness (i.e. the tendency to join flocks). The question is whether we
# will find correlation between the trait and centrality?

set.seed(2)
n = 50
# Sort that largest individual is first
trait = sort(rnorm(n, mean = 20, sd = 5), decreasing = T) 

# Assign gregariousness and sort. This will create a correlation between trait
# and gregariousness as specified in the scenario
p = sort(runif(n, min = 0.01, max = 0.1))

plot(trait, p, ylab = "Gregariousness", xlab = "Trait Value")

# Now simulate flock observations
f = 100
# simulation will generate an individual-by-group matrix (50 x 100)
ibg = matrix(0, nrow = n, ncol = f)

# For each cell, the probability that the individual (rows) is found in that 
# flock (column) depends on that individual’s gregariousness value, p
for(i in 1:n){
  for (j in 1:f){
    ibg[i,j] = sample(c(1,0), 1, prob = c(p[i], 1-p[i]))
  }
}

# Create the adjacency matrix and then the graph from this adjacency matrix
adj = get_network(t(ibg), data_format = "GBI", association_index = "SRI")

g = graph_from_adjacency_matrix(adj, "undirected", weighted = T)

# Plot the resulting network with node size proportional to the individual's
# trait value
plot(g,
     vertex.size = trait/2, 
     vertex.label = "", 
     edge.width = E(g)$weight*5)

# Now plot the relationship betweent trait value and the individual's degree
# centrality
plot(trait, 
     degree(g), 
     pch = 19, 
     col = "tomato")

cor.test(trait, degree(g))
# there is quite a strong correlation between trait and social position of 
# the individual