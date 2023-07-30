# Analysing the sparrow flock networks across two years
library(dplyr)
library(tidyr)
library(magrittr)
library(broom)
library(readr)
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

# 1) get the adjacency matrix for both seasons

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

# Is the pattern of association between years consistent?
# i.e. do birds re-create the social networks year after year?
# To test this we need to filter the individuals seen both years and perform a
# Mantel test to see if association indices are correlated across years.

library(ecodist)

# Restrict comparison to individuals that were seen in two sequential years 

# get IDs of birds that were present in both networks
id12 = rownames(adjs[[1]])[rownames(adjs[[1]]) %in% rownames(adjs[[2]])]

#get row/columns of those individuals in matrix 1 
ids_m1 = match(id12, rownames(adjs[[1]])) 

# and those in matrix 2
ids_m2 = match(id12,rownames(adjs[[2]])) 

# matrix 1 of association indices of only returning individuals
m12 = adjs[[1]][ids_m1, ids_m1] 

# matrix 2 of association indices of only returning individuals
m21 = adjs[[2]][ids_m2, ids_m2] 

# reorder the rows/columns by alphanumeric order
m12 = m12[order(rownames(m12)), order(rownames(m12))] 
m21 = m21[order(rownames(m21)), order(rownames(m21))] 

mantel12 = mantel(as.dist(m12) ~ as.dist(m21)) 

mantel12
#  mantelr      pval1      pval2      pval3    llim.2.5%  ulim.97.5% 
#  0.7393206  0.0010000  1.0000000  0.0010000  0.6463771  0.8514992 
# There's a high correlation of association indices given the Mantel r 
# coefficient.

plot(m12[upper.tri(m12)], 
     m21[upper.tri(m21)], 
     pch = 19, 
     col = rgb(1,0,0,0.5), 
     cex = 2, 
     xlab="Season 2 Association Index", 
     ylab="Season 3 Association Index")

# Or with ggplot
# Create a data frame for ggplot
data = data.frame(
  x = m12[upper.tri(m12)],
  y = m21[upper.tri(m21)]
)

# Plot 
ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.5, 
             color = "red", 
             size = 3, 
             shape = 19) +
  labs(x = "Season 2 Association Index", 
       y = "Season 3 Association Index") +
  theme_minimal()

# Could this effect be driven by fidelity to home ranges rather than social 
# associates? 
# There is no doubt that spatial overlap influences associations, since 
# associations are measured as co-occurrence in time and space.
# Importing a matrix of spatial overlap between each pair of each individual
# (measured as the proportion of joint home range that is shared) and plot the
# association index relative to this measure we get this:

# Read the CSV data and convert it to a matrix
s3 = as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/GCSPspaceoverlap3.csv", 
                        header = TRUE, row.names = 1))

# Match row names between s3 (the spatial overlap data) and adjs[[2]] (the 
# association index for season 2) and remove NAs
ids_s3 = match(rownames(s3), rownames(adjs[[2]]))
ids_s3 = na.omit(ids_s3)
ids_adj2 = match(rownames(adjs[[2]]), rownames(s3))
ids_adj2 = na.omit(ids_adj2)

# Subset the data using matched indices and remove NAs
s3_use = s3[ids_s3, ids_s3]
adj_use = adjs[[2]][ids_adj2, ids_adj2]
s3_use = s3_use[complete.cases(s3_use), ]
adj_use = adj_use[complete.cases(adj_use), ]

# Reorder the rows and columns of the matrices (c)
s3_use = s3_use[order(rownames(s3_use)), order(rownames(s3_use))]
adj_use = adj_use[order(rownames(adj_use)), order(rownames(adj_use))]

# Create the plot with the updated data
plot(s3_use[upper.tri(s3_use)], adj_use[upper.tri(adj_use)], 
     pch = 19, 
     cex = 2, 
     col = rgb(0, 0, 1, 0.5), 
     xlab = "Spatial Overlap", 
     ylab = "Association Index")

# For ggplot you need to create a data frame from the upper triangle of 
# both matrices
data = data.frame(Spatial_Overlap = c(s3_use[upper.tri(s3_use)]),
                   Association_Index = c(adj_use[upper.tri(adj_use)]))

# Plot
ggplot(data, aes(x = Spatial_Overlap, y = Association_Index)) +
  geom_point(size = 3, 
             alpha = 0.5, 
             color = "blue") +
  labs(x = "Spatial Overlap", 
       y = "Association Index") +
  theme_minimal()

# How can we test for the effect of the previous year's association on the 
# current year's association while taking into account the amount of spatial
# overlap in home ranges? We use MRQAP (a.k.a 'network regression'). To do this
# we use the spatial overlap matrix and the previous year adjacency matrix as 
# covariates to test their effects on the current year adjacency matrix.

# restrict the spatial overlap matrix to the individuals included in both 
# seasons, sorted in the same way as the adjacency matrices

# The match() function is used twice here, once for the row indices and once 
# for the column indices. This ensures that we get a square sub-matrix from 
# s3_use with matching rows and columns that correspond to the row names in m21.
s3_match = s3_use[match(rownames(m21),rownames(s3_use)), 
                  match(rownames(m21),rownames(s3_use))] 

mrqap.dsp(m21 ~ m12 + s3_match)
# m21 is the adjacency matrix of season 3 (dependent variable)
# m12 is the adjacency matrix of season 2 (independent variable 1)
# s3_match is the spartial overlap matrix of season 3

# MRQAP with Double-Semi-Partialing (DSP)
# 
## Formula:  m21 ~ m12 + s3_match 
# 
# Coefficients:
#            Estimate   P(β>=r) P(β<=r) P(|β|<=|r|)
# intercept -0.01749305 0.068   0.932   0.069      
# m12        0.45328523 1.000   0.000   0.000      
# s3_match   0.12343038 1.000   0.000   0.000      
# 
# Residual standard error: 0.04051 on 88 degrees of freedom
# F-statistic: 98.65 on 2 and 88 degrees of freedom, p-value:     0 
# Multiple R-squared: 0.6915   Adjusted R-squared: 0.6845 
# AIC: -35.74197

# Both m12 and s3_match are significant predictors of the current year's 
# adjacency matrix. The overall fit of the model is also very good (0.68), 
# what we cannot do is compare the magnitudes of effect here because the two 
# independent matrices were not scaled before running the model. So, let's
# do it like so:

mrqap.dsp(scale(m21) ~ scale(m12) + scale(s3_match))

# MRQAP with Double-Semi-Partialing (DSP)
# 
# Formula:  scale(m21) ~ scale(m12) + scale(s3_match) 
# 
# Coefficients:
#                 Estimate   P(β>=r) P(β<=r) P(|β|<=|r|)
# intercept       0.01269215 0.551   0.449   0.722      
# scale(m12)      0.43393270 1.000   0.000   0.000      
# scale(s3_match) 0.53650775 1.000   0.000   0.000      
# 
# Residual standard error: 0.6302 on 88 degrees of freedom
# F-statistic: 83.06 on 2 and 88 degrees of freedom, p-value:     0 
# Multiple R-squared: 0.6537   Adjusted R-squared: 0.6458 
# AIC: -88.04498

# The results suggest that the effect of home range overlap is slightly greater
# than that of the effect of associations in the previous year (0.54 vs. 0.43)
# but it is still a significant pattern for preferentially flocking with last
# year's flock-mates.
# This is some really cool analysis by the Shizuka lab, right?
# It was fun tweaking things and running through it.

# Save the large data outputs: m_list, adjs, gs, mods, coms, mod_swap, swap_g, 
# mod_swap2, swap_g2, df, df2, df_boot.

# Save large data outputs
saveRDS(m_list, file = "./outputs/m_list.Rds")
saveRDS(adjs, file = "./outputs/adjs.Rds")
saveRDS(gs, file = "./outputs/gs.Rds")
saveRDS(mods, file = "./outputs/mods.Rds")
saveRDS(coms, file = "./outputs/coms.Rds")
saveRDS(mod_swap, file = "./outputs/mod_swap.Rds")
saveRDS(swap_g, file = "./outputs/swap_g.Rds")
saveRDS(mod_swap2, file = "./outputs/mod_swap2.Rds")
saveRDS(swap_g2, file = "./outputs/swap_g2.Rds")
saveRDS(df, file = "./outputs/df.Rds")
saveRDS(df2, file = "./outputs/df2.Rds")
saveRDS(df_boot, file = "./outputs/df_boot.Rds")

# Save s3 data locally
write.csv(s3, file = "./outputs/s3_data.csv", row.names = TRUE)


# #If you wish to load them and avoid having to run all of the processes do
# #so like this:
# load_rds_files = function() {
#   rds_files = list.files("./outputs/", pattern = "\\.Rds$", full.names = TRUE)
#   data_list = lapply(rds_files, readRDS)
#   return(data_list)
# }
# 
# #Usage:
# rds_data_list = load_rds_files()
# 
# # Load s3 data separately
# s3_data = read.csv("./outputs/s3_data.csv", row.names = 1)