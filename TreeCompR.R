devtools::install_github("juliarieder/TreeCompR")
library(TreeCompR)
library(lidR)

setwd("E:/Sonja/Msc_Thesis/data/not_normalized/")

tls <- readLAS("tls.las")

las_clip <- clip_rectangle(tls, xleft = 523951.8143544365884736, 
                           ybottom = 5537692.5082834409549832, 
                           xright =  524023.2143544366117567, 
                           ytop = 5537746.5082834409549832)

tls_sub <- decimate_points(las_clip, homogenize(1000, res = 0.5))

chm <- rasterize_canopy(tls_sub, res = 0.5, algorithm = p2r(0.15))
ttops <- locate_trees(chm, lmf(ws = 5))

tls_seg <- segment_trees(tls_sub, dalponte2016(chm = chm, treetops = ttops))
plot(tls_seg, color = "treeID")
tree26 <- filter_poi(tls_seg, treeID == 26)
plot(tree26, color = "treeID")

length(unique(tls_seg$treeID[!is.na(tls_seg$treeID)]))

pc <- read_pc(tls_sub)
tree <- read_pc(tree25)
tree_positions <- tree_pos(pc)
competition_results <- compete_pc(pc, tree, method = "cylinder")  # or method 




# normalized point cloud
# read files
setwd("E:/Sonja/Msc_Thesis/data/8_preprocessedData/")

las_bp <- 
  readLAS("bp/backpack_feb5_subsampled_0_05_Normalize by Ground Points.las")
tree <- read.csv("E:/Sonja/Msc_Thesis/data/9_individualTrees/tree10.csv")

plot(las_bp)

chm <- rasterize_canopy(las_tls, res = 0.5, algorithm = p2r(0.15))
ttops <- locate_trees(chm, lmf(ws = 5))

# get paths to trees
tree_paths <- list.files("E:/Sonja/Msc_Thesis/data/9_individualTrees/")

library(tidyverse) # for purrr() and bind_rows()
CI_data <- map(
  tree_paths,
  ~compete_pc(
    forest_source = read_pc(las_bp),
    tree_source = file.path("E:/Sonja/Msc_Thesis/data/9_individualTrees/", .x),
    tree_name = .x,
    comp_method = "cone"
  )
) %>%
  bind_rows()


pc <- read_pc(las_bp)
tree <- read_pc(tree)
tree_positions <- tree_pos(pc)
competition_results <- compete_pc(pc, tree, method = "cylinder")  # or method 
