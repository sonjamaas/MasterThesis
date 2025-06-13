################################################################################
########## Script for clipping individual trees in UAV point cloud ############# 
################################################################################

# Workflow:
# 1. Load data (backpack .las and uav .las)
# 2. get extend of each tree from backpack data and store as polygon (lidR tree segmentation -> crown area)
# 3. clip uav data with these polygons

############################## Prerequisites ###################################

library(lidR)

# read data
setwd("E:/Sonja/Msc_Thesis/data/8_preprocessedData/")
bp <- readLAS("bp/backpack_feb6_subsampled_0_05_Normalize by Ground Points.las")
uav <- readLAS("E:/Sonja/Msc_Thesis/data/uav/UAV_feb2- shifted.las")

# make canopy height model
bp_chm <- rasterize_canopy(bp, 0.5, pitfree(subcircle = 0.2), pkg = "terra")
kernel <- matrix(1,3,3)
bp_chm_smoothed <- terra::focal(bp_chm, w = kernel, fun = median, na.rm = TRUE)
bp_chm_smoothed_ttops <- locate_trees(bp_chm_smoothed, lmf(5))

col = height.colors(50)
plot(bp_chm_smoothed, main = "CHM Pitfree", col = col)
plot(sf::st_geometry(bp_chm_smoothed_ttops), add = T, pch = 3)

# plot(bp_chm, bg = "white", size = 4)

f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

bp_ttops <- locate_trees(bp, lmf(f))
plot(bp_chm, col = height.colors(50))
plot(sf::st_geometry(bp_ttops), add = TRUE, pch = 3)

x <- plot(bp, bg = "white", size = 4)
add_treetops3d(x, bp_ttops)

algo <- li2012()
algo2 <- dalponte2016(bp_chm_smoothed, bp_chm_smoothed_ttops)
bp_trees <- segment_trees(bp, algo, attribute = "ID")
bp_trees_dalponte <- segment_trees(bp, algo2)

x <- plot(bp_trees, bg = "white", size = 4, color = "ID", pal = pastel.colors(200))
plot(bp_trees_dalponte, bg = "white", size = 4, color = "treeID")

crowns <- crown_metrics(bp_trees_dalponte, func = .stdtreemetrics, geom = "concave")

plot(sf::st_geometry(crowns), reset = FALSE)
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

# export the crown shapes to an sf object
library(sf)

st_write(crowns, "crowns.shp")

# try doing crown delineation with one tree .csv file
tree <- read.csv("E:/Sonja/Msc_Thesis/data/9_individualTrees/tree1.csv")
tree <- tree[1:3]
tree <- as.data.frame(tree)
tree <- LAS(tree)

xy <- tree@data[, c("X", "Y")]

hull_indices <- chull(xy)
hull_coords <- xy[c(hull_indices, hull_indices[1]),]

# convert to sf polygon
polygon_sf <- st_sf(geometry = st_sfc(st_polygon(list(as.matrix(hull_coords)))),
                    crs = st_crs(tree))
plot(polygon_sf)

# do same thing for all csv files
csv_files <- list.files(path = "E:/Sonja/Msc_Thesis/data/9_individualTrees/", pattern = "\\.csv$", full.names = TRUE)

get_crown_polygon <- function(file) {
  df <- read.csv(file)
  xy <- df[, c("X", "Y")]  # Adjust column names if needed
  hull_indices <- chull(xy)
  hull_coords <- xy[c(hull_indices, hull_indices[1]), ]  # Close the polygon
  st_polygon(list(as.matrix(hull_coords)))
}

polygons_list <- lapply(csv_files, get_crown_polygon)


# clip uav data with those polygons

for (i in seq_along(polygons_list)) {
  # Get polygon geometry
  poly <- polygons_list[[i]]
  
  # Clip LAS with this polygon (returns a LAS object)
  las_clip <- clip_roi(uav, poly)
  
  # Only write if the result is not empty
  if (!is.null(las_clip) && length(las_clip@data$X) > 0) {
    # Define output filename
    out_file <- sprintf("treeclip%d.las", i)
    writeLAS(las_clip, out_file)
  }
}

plot(las_clip)
library(stringr)
# merge tree files from uav and backpack
las_files_uav <- list.files("E:/Sonja/Msc_Thesis/data/8_preprocessedData/", pattern = "^treeclip\\d+\\.las$")
csv_files_bp <- list.files("E:/Sonja/Msc_Thesis/data/9_individualTrees/", pattern = "^tree\\d+\\.csv$")

las_nums <- str_extract(las_files_uav, "\\d+")
csv_nums <- str_extract(csv_files_bp, "\\d+")


# make the right files match ( file names dont match!!!)