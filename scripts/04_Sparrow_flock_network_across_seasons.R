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

# Timer function
timer = function(start, end){
  return(difftime(end, start, units = "secs"))
}

# Function to print total runtime
print_total_runtime = function(operation_name, runtime){
  cat("Total runtime for", operation_name, ":", runtime, "seconds\n")
}

# This part of the code differs only in that we are applying the same operations
# we did to the season 2 data but to data from two different seasons.
start_time_matrix_operations = Sys.time()
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
end_time_matrix_operations = Sys.time()

# Print matrix operations time
matrix_operations_time = timer(start_time_matrix_operations, 
                               end_time_matrix_operations)
print_total_runtime("matrix operations", 
                    matrix_operations_time)

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
dev.off()
# Testing modularity of empirical network against randomised networks

# There are generally two ways to generate a P-value using the group membership 
# swapping algorithm. First, one could repeat the swaps until the test statistic 
# of interest stabilizes to a range of values corresponding to a randomized 
# matrix, and then repeat this procedure a large number of times to calculate a 
# distribution of the test statistic under the null model ('global test').
# Alternatively, one can run a large number of swaps from a single initial 
# matrix, calculating a test statistic after each ‘swap’ of the matrix, and 
# compare this distribution against the empirical test statistic 
# (‘serial test’). Manly (1995) discusses why the serial method is a valid 
# method for testing whether the empirical matrix is non-random as long as we 
# conduct a very large number of swaps. 

# Global test

# Each swap randomly switches the edges in the network while preserving the 
# number of edges for each node. The resulting adjacency matrix after the swaps 
# is stored in the swap_m list at the index k. The $Association_index part 
# extracts the adjacency matrix from the output of the network_swap function.

# We then convert each adjacency matrix in the swap_m list into an undirected
# weighted graph. Finally, the modularity of the communities is calculated
# on each graph and stored in the mod_swap vector

gbi = t(m_list[[1]])
swap_m = list()
#iterations (doing the same as in the paper, 1000 iters and 5000 swaps)
times = 1000
start_time_modularity_test = Sys.time()
swap_m = lapply(1:times, function(k) {
  network_swap(gbi, swaps = 5000)$Association_index
})

swap_g = lapply(swap_m, function(x) graph_from_adjacency_matrix(x, 
                                                                "undirected", 
                                                                weighted = TRUE))
mod_swap = sapply(swap_g, function(x) modularity(cluster_fast_greedy(x)))

end_time_modularity_test = Sys.time()
modularity_test_time = timer(start_time_modularity_test,
                             end_time_modularity_test)
print_total_runtime("modularity test",
                    modularity_test_time)

# Extract and plot the empirical modularity vs the distribution of values from
# the modularity swap
library(ggplot2)

# Create a data frame for ggplot
df = data.frame(mod_swap = mod_swap)

# Calculate the modularity threshold (mods[[1]])
mod_threshold = mods[[1]]

# Plot the histogram with ggplot
ggplot(df, aes(x = mod_swap)) +
  geom_histogram(binwidth = 0.01, 
                 color = "black", 
                 fill = "lightblue") +
  geom_vline(xintercept = mod_threshold, 
             color = "red", 
             linetype = "dashed", 
             size = 1.5) +
  labs(x = "Modularity", 
       y = "Frequency", 
       title = "Empirical Modularity Distribution") +
  theme_minimal()

# Calculate p-value
p1 = (length(which(mod_swap >= mod_threshold)) + 1) / (times + 1)
p1

# Deprecated base R option
# hist(mod_swap, xlim = c(min(mod_swap), mods[[1]]))
# abline(v = mods[[1]], col = "red", lty = 2, lwd = 2)
# p = (length(which(mod_swap >= mods[[1]])) + 1) / (times + 1)

# The empirical modularity value is much larger than expected by random chance


# Deprecated version
# for (k in 1:times){
#   swap_m[[k]] = network_swap(gbi, swaps = 500)$Association_index }
# 
# swap_g = lapply(swap_m, function(x) graph_from_adjacency_matrix(x,
#                                                                 "undirected",
#                                                                 weighted = T))
# mod_swap = sapply(swap_g,
#                   function(x) modularity(cluster_fast_greedy(x)))
# 
# end_time_modularity_test = Sys.time()
# modularity_test_time = timer(start_time_modularity_test,
#                              end_time_modularity_test)
# print_total_runtime("modularity test",
#                     modularity_test_time)
# 
# hist(mod_swap, xlim=c(min(mod_swap), mods[[1]]))
# abline(v=mods[[1]], col="red", lty=2, lwd=2)
# p = (length(which(mod_swap>=mods[[1]]))+1)/(times+1)
# p

# Now the serial method

gbi2 = t(m_list[[1]])
assoc2 = get_network(gbi2)
start_time_modularity_test = Sys.time()
net_perm = network_permutation(gbi2, 
                               permutations = 100000, 
                               returns = 1, 
                               association_matrix = assoc2)

swap_g2 = apply(net_perm, 1, function(x) graph_from_adjacency_matrix(x, 
                                                                     "undirected", 
                                                                     weighted=T))
mod_swap2 = sapply(swap_g2, function(x) modularity(cluster_fast_greedy(x))) 
end_time_modularity_test = Sys.time()
modularity_test_time = timer(start_time_modularity_test,
                             end_time_modularity_test)
print_total_runtime("modularity test",
                    modularity_test_time)

df2 = data.frame(mod_swap = mod_swap2)

# Plot 
ggplot(df2, aes(x = mod_swap)) +
  geom_histogram(binwidth = 0.01, 
                 color = "black", 
                 fill = "lightblue") +
  geom_vline(xintercept = mods[[1]], 
             color = "red", 
             linetype = "dashed", 
             size = 1.5) +
  labs(x = "Modularity", 
       y = "Frequency", 
       title = "Serial Method Modularity Distribution") +
  theme_minimal()

# Calculate the p-value
p2 = (length(which(mod_swap2 >= mods[[1]])) + 1)/100001
p2

p_vals = cbind(p1, p2)
print(p_vals)
#               p1          p2
# [1,] 0.000999001 8.99991e-05


# Deprecated base R version
# hist(mod_swap2, 
#      xlim = c(min(mod_swap2), mods[[1]]), main = "Serial Method") 
# abline(v = mods[[1]], col="red", lty=2, lwd=2) 

# Bootstrapping to account for sampling error

com_boot = vector(length = 100) #set up empty vector 
for (i in 1:100){
  s_col = sample(1:ncol(m_list[[1]]), 
                 ncol(m_list[[1]]),
                 # sample columns with replacement
                 replace=T) 
  # create new matrix using resampled columns
  nm1 = m_list[[1]][,s_col]
  # bootstrapped network
  g_boot = graph_from_adjacency_matrix(get_network(t(nm1)), 
                                       "undirected", 
                                       weighted = T) 
  # calculate modularity using fast_greedy community detection
  com_boot[i] = modularity(cluster_fast_greedy(g_boot))
}

df_boot = data.frame(Modularity = com_boot)

ggplot(df_boot, aes(x = "", y = Modularity)) +
  geom_violin(fill = "lightblue", 
              color = "black", 
              scale = "width", 
              trim = FALSE) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               color = "black", 
               # Add boxplot inside the violin plot
               outlier.shape = NA) +  
  labs(x = "", 
       y = "Modularity", 
       title = "Bootstrapped Modularity") +
  theme_minimal()

# Compare 
boxplot(mod_swap2, 
        mod_swap,
        com_boot, 
        col = "gray", 
        las = 1, 
        names=c("Null (serial method)", "Null(global method)", "Empirical \n(bootstrap)"), 
        ylab="Modularity")

