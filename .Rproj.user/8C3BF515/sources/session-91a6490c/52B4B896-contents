# This is a summary from R. Hijmans somewhat messy (imo) data prep tutorial
# on species distribution modelling on rspatial.org

# I would introduce it with a step by step summary before actually going for it
# like so:

# 1) filter rows with NA on lat and lon
# 2) Visualise with a map for the geographic distribution of data points,
# if you know (you should) the subject you are working with, you should
# have no problem spotting incongruences due to absence of that subject 
# in a given geographical location.
# 3) Look for duplicates and errors in latitude and longitude entered into
# a dataset. You can opt to relocate entries to the correct location (if the
# error is one of incorrect sign in lon or lat), and drop duplicates.
# 4) Visual check for lat-lon correction. (Not performed by R Hijmans)
# 5) Cross-check with Spatial Points Dataframe
# if you want a more accurate result change the crs to a better one than 
# 'wrld_simpl' below
# library(sp)
# coordinates(acgeo) = ~lon+lat
# crs(acgeo) = crs(wrld_simpl)
# ovr = over(acgeo, wrld_simpl)
# 6) Reduce potential sampling bias with gridSample on a raster layer
# of your dataset (if warranted, you can also use it to split the dataset
# for model evaluation later on)
# I left out georeferencing because it seems frankly to not be worth the effort.

library(dismo)
# rgdal, rgeos, and maptools are being removed from CRAN
library(sf)
library(dplyr)

# download data from GBIF

acaule = gbif("Solanum", "acaule*", geo = FALSE)

data(acaule)
dim(acaule)
str(acaule)
colnames(acaule)

# Filter those that do not have coordinates

acgeo = acaule %>% filter(lat != "NA", lon != "NA")

library(maptools)
data("wrld_simpl")
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), 
     axes=TRUE, col="light green")
# restore the box around the map
box()
# add the points
points(acgeo$lon, acgeo$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(acgeo$lon, acgeo$lat, col='black', cex=0.75)

# Alternatively if you prefer a more up to date package
library(geodata)

world = world(resolution=1, level=0, path = "data/", version="latest")
# if you wanna download specific countries
# gadm(country = "GBR", level = 1, path = "data/", 
# version = "latest", resolution = 1)

plot(world, xlim=c(-80,70), ylim=c(-80,40), 
     axes=TRUE, col="light green")
box()

points(acgeo$lon, acgeo$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(acgeo$lon, acgeo$lat, col='black', cex=0.75)

# Results are fairly similar although maptools provides better labels imo
# however, it is being removed from CRAN soon

# An example of common mistakes contained in GBIF data (mix ups with
# signs, lon and lat)
acaule[c(303,885),1:10]

lonzero = subset(acgeo, lon==0)
lonzero[, 1:13]
# issues with longitude and duplication of entries

dups = duplicated(lonzero[, 1:10])

lonzero = lonzero[dups, ]
lonzero[,1:13]

# Dealing with duplicates by lon and lat
dups2 = duplicated(acgeo[, c('lon', 'lat')])
sum(dups2)

# Correct records that are geographically incorrect
i = acgeo$lon > 0 & acgeo$lat > 0
# if records are greater than 0 for longitude correct to -1 * the longitude
# for that entry
acgeo$lon[i] <- -1 * acgeo$lon[i]
# likewise for latitude, this places them in the correct location
acgeo$lat[i] <- -1 * acgeo$lat[i]

acgeo = acgeo[acgeo$lon < -50 & acgeo$lat > -50, ]

# Regenerate the world plot to check
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), 
     axes=TRUE, col="light green")
# restore the box around the map
box()
# add the points
points(acgeo$lon, acgeo$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(acgeo$lon, acgeo$lat, col='black', cex=0.75)

# it worked, no records in Antarctica, the Indian ocean or in the Atlantic
# ocean

# Cross check with a SpatialPointsDataDrame

library(sp)
coordinates(acgeo) = ~lon+lat
crs(acgeo) = crs(wrld_simpl)
ovr = over(acgeo, wrld_simpl)

# The ovr object now contains a matching record from wrld_simpl for each point.

# Retrieve the 'NAME' variable from 'ovr' and store to a "country" object
cntr = ovr$NAME

# Which records do not match any country (there should be zero after the lat, 
# lon correction)
i = which(is.na(cntr))
i

# Which points have coordinates that do not match the GBIF record
j = which(cntr != acgeo$country)
j 

# We can take cntr, acgeo$country, and j and column bind them to correct
# mismatches

cbind(cntr, acgeo$country)[j,]

# Misattribution could be due to proximity between the Bolivian and neighbouring
# country borders as shown above

plot(acgeo)
plot(wrld_simpl, add=T, border='blue', lwd=2)
points(acgeo[j, ], col='red', pch=20, cex=2)

##########

# Dealing with the absence of coordinates. Georeferencing (requires
# a Google API key, so personally I would rather ask for lon, lat 
# data or drop the data point altogether)

# This is a description of how much of a headache it can be
# "Before using the geocode function it is best to write the records to a 
# table and “clean” them in a spreadsheet. Cleaning involves translation, 
# expanding abbreviations, correcting misspellings, and making duplicates 
# exactly the same so that they can be georeferenced only once. Then read 
# the the table back into R, and create unique localities, georeference 
# these and merge them with the original data" = Hard pass for me. If the
# people generating the data cannot be bothered to observe best practices
# it is hard to tell what other questionable practices in sampling they may
# have indulged in (Garbage in = Garbage out and all that)

georef = subset(acaule, (is.na(lon) | is.na(lat)) & !is.na(locality))
georef$cloc[4]

# Reducing sampling bias

# create a RasterLayer with the extent of acgeo
r = raster(acgeo)

# set the resolution of the cells to (for example) 1 degree
res(r) = 1

# expand (extend) the extent of the RasterLayer a little
r = extend(r, extent(r)+1)

# sample:
acsel = gridSample(acgeo, r, n=1)

# If you choose the chess-board sampling method 
# gridSample(xy, r, n=1, chess='')
# this can be useful to split the dataset into training and testing sets
# for model eval.

# to illustrate the method and show the result
p = rasterToPolygons(r)
plot(p, border='gray')
points(acgeo)

# selected points in red
points(acsel, cex=1, col='red', pch='x')

# Save the clean dataset 
write.csv(acaule, file = "outputs/acaule.csv")

