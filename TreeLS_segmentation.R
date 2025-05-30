# script to segment trees with treeLS

#install.packages("TreeLS")
#remotes::install_github('tiagodc/TreeLS')
library(TreeLS)

setwd("F:/Sonja/Msc_Thesis/data/8_preprocessedData/bp/")

# open plot file
bp_feb6 = readTLS("backpack_feb6_subsampled_0_05_Normalize by Ground Points.las")

map = treeMap(bp_feb6, map.hough(min_density = 0.1), 0)
x = plot(map)
add_treeMap(x, map, color='yellow', size=2)

# classify tree regions
tls = treePoints(bp_feb6, map, trp.crop())
add_treePoints(x, tls, size=4)
add_treeIDs(x, tls, cex = 2, col='yellow')

# classify stem points
tls = stemPoints(tls, stm.hough())
add_stemPoints(x, tls, color='red', size=8)

# make the plot's inventory
inv = tlsInventory(tls, d_method=shapeFit(shape='circle', algorithm = 'qr'))
add_tlsInventory(x, inv)


# extract stem measures
seg = stemSegmentation(tls, sgt.ransac.circle(n = 20))
add_stemSegments(x, seg, color='white', fast=T)

# plot everything once
tlsPlot(tls, map, inv, 
        seg, fast=T)

# check out only one tree
tlsPlot(tls, inv,
        seg, tree_id = 30)

