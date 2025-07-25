################################################################################
################## Script for comparing canopy cover rasters ###################
################################################################################

#### Workflow ####
# 1. Prerequisites
# 2. Plot original data
# 3. Raster value comparison
# 4. Layer summaries
# 5. Calculate difference rasters
# 6. Difference raster metrics
# 7. Paired t-tests
# 8. Boxplots
# 9. Raster comparison plot


########################## 1. Prerequisites ####################################

# load required libraries
library(raster)
library(terra)
library(spatialEco)
library(tidyr)
library(ggplot2)
library(grid)
library(tidyr)
library(gridExtra)
library(cowplot)
library(patchwork)
library(dplyr)

# load data
setwd("E:/Sonja/Msc_Thesis/data/Metrics/CanopyCover/")

cover_uav <- rast("cover_uav.tif")
cover_als <- rast("cover_als.tif")
cover_tls <- rast("cover_tls.tif")
cover_bp <- rast("cover_bp.tif")
cover_uav_s <- rast("cover_uav_s.tif")
cover_tls_s <- rast("cover_tls_s.tif")
cover_bp_s <- rast("cover_bp_s.tif")


########################### 2. Plot original data ##############################

common_scale <- scale_fill_gradient2(
  low = "white",
  high = "darkgreen",
  limits = c(0, 100),
  oob = scales::squish # avoids warnings from small differences
)

cover_uav <- rename(as.data.frame(cover_uav, xy = TRUE, na.rm = TRUE), 
                  CanopyCover = cover_uav)
cover_uav_s <- rename(as.data.frame(cover_uav_s, xy = TRUE, na.rm = TRUE), 
                    CanopyCover = cover_uav_s)
cover_als <- rename(as.data.frame(cover_als, xy = TRUE, na.rm = TRUE), 
                    CanopyCover = cover_als )
cover_tls <- rename(as.data.frame(cover_tls, xy = TRUE, na.rm = TRUE), 
                    CanopyCover = cover_tls )
cover_tls_s <- rename(as.data.frame(cover_tls_s, xy = TRUE, na.rm = TRUE), 
                    CanopyCover = cover_tls_s )
cover_bp <- rename(as.data.frame(cover_bp, xy = TRUE, na.rm = TRUE), 
                   CanopyCover = cover_bp )
cover_bp_s <- rename(as.data.frame(cover_bp_s, xy = TRUE, na.rm = TRUE), 
                   CanopyCover = cover_bp_s )

u <- ggplot(data = cover_uav, 
       aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  #labs(title = "Canopy Cover derived from UAV data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_blank())+
  annotate("label", x = 523955, y = 5537700, label = "UAV Winter", 
           hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover [%]')

u_s <- ggplot(data = cover_uav_s, 
            aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  #labs(title = "Canopy Cover derived from UAV data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  annotate("label", x = 523955, y = 5537700, label = "UAV Summer", 
           hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover [%]')

a <- ggplot(data = as.data.frame(cover_als, xy = TRUE), 
       aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  # labs(title = "Canopy Cover derived from ALS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        #axis.text.x = element_blank(),
        #axis.text.y = element_blank()
        )+
  annotate("label", x = 523955, y = 5537700, label = "ALS", 
           hjust = "left", fill = "white")+
  labs(fill = 'Canopy Cover [%]')

t <- ggplot(data = as.data.frame(cover_tls, xy = TRUE), 
       aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  #labs(title = "Canopy Cover derived from TLS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_blank()
        )+
  annotate("label", x = 523955, y = 5537700, label = "TLS Winter", 
           hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover [%]')

t_s <- ggplot(data = as.data.frame(cover_tls_s, xy = TRUE), 
            aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  #labs(title = "Canopy Cover derived from TLS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  annotate("label", x = 523955, y = 5537700, label = "TLS Summer", 
           hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover [%]')

b <- ggplot(data = as.data.frame(cover_bp, xy = TRUE), 
       aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  #labs(title = "Canopy Cover derived from Backpack data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        axis.text.x = element_blank())+
  annotate("label", x = 523955, y = 5537700, label = "Backpack Winter", 
           hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover [%]')

b_s <- ggplot(data = as.data.frame(cover_bp_s, xy = TRUE), 
            aes(x = x, y = y, fill = CanopyCover))+
  geom_tile()+
  common_scale +
  coord_equal() +
  theme_minimal()+
  #labs(title = "Canopy Cover derived from Backpack data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15),
        axis.text.y = element_blank())+
  annotate("label", x = 523955, y = 5537700, label = "Backpack Summer", 
           hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover [%]')

layout <- "
AB
CD
EF
G#
"
combined <- (
  u + u_s + t + t_s + b + b_s + a +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "bottom")
)

# combined +
#   plot_annotation(
#     title = "Canopy Cover measurements"
#   )
# 

####################### 3. Raster value comparison #############################


# 
# # plot values to check them
# plot(vals_uav[valid], vals_als[valid],
#      xlab="UAV Cover", ylab="ALS Cover",
#      main="Pixel-wise Comparison (UAV + ALS)")
# abline(0, 1, col="red")
# 
# plot(vals_uav[valid], vals_tls[valid],
#      xlab="UAV Cover", ylab="TLS Cover",
#      main="Pixel-wise Comparison (UAV + TLS)")
# abline(0, 1, col="red")
# 
# plot(vals_uav[valid], vals_bp[valid],
#      xlab="UAV Cover", ylab="Backpack Cover",
#      main="Pixel-wise Comparison (UAV + Backpack)")
# abline(0, 1, col="red")
# 
# plot(vals_als[valid], vals_tls[valid],
#      xlab="ALS Cover", ylab="TLS Cover",
#      main="Pixel-wise Comparison (ALS + TLS)")
# abline(0, 1, col="red")
# 
# plot(vals_als[valid], vals_bp[valid],
#      xlab="ALS Cover", ylab="Backpack Cover",
#      main="Pixel-wise Comparison (ALS + Backpack)")
# abline(0, 1, col="red")
# 
# plot(vals_tls[valid], vals_bp[valid],
#      xlab="TLS Cover", ylab="Backpack Cover",
#      main="Pixel-wise Comparison (TLS + Backpack)")
# abline(0, 1, col="red")


########################### 4. Layer summaries #################################
cover_uav <- rast("cover_uav.tif")
cover_als <- rast("cover_als.tif")
cover_tls <- rast("cover_tls.tif")
cover_bp <- rast("cover_bp.tif")
cover_uav_s <- rast("cover_uav_s.tif")
cover_tls_s <- rast("cover_tls_s.tif")
cover_bp_s <- rast("cover_bp_s.tif")

# Layer summaries
metrics <- tibble(
  Metric = c("Mean", "Median", "SD", "Min", "Max"),
  UAV_winter = c(
    global(cover_uav, mean, na.rm = TRUE)[[1]],
    global(cover_uav, median, na.rm = TRUE)[[1]],
    global(cover_uav, sd, na.rm = TRUE)[[1]],
    global(cover_uav, range, na.rm = TRUE)[1],
    global(cover_uav, range, na.rm = TRUE)[2]
  ),
  UAV_summer = c(
    global(cover_uav_s, mean, na.rm = TRUE)[[1]],
    global(cover_uav_s, median, na.rm = TRUE)[[1]],
    global(cover_uav_s, sd, na.rm = TRUE)[[1]],
    global(cover_uav_s, range, na.rm = TRUE)[1],
    global(cover_uav_s, range, na.rm = TRUE)[2]
  ),
  ALS = c(
    global(cover_als, mean, na.rm = TRUE)[[1]],
    global(cover_als, median, na.rm = TRUE)[[1]],
    global(cover_als, sd, na.rm = TRUE)[[1]],
    global(cover_als, range, na.rm = TRUE)[1],
    global(cover_als, range, na.rm = TRUE)[2]
  ),
  TLS_winter = c(
    global(cover_tls, mean, na.rm = TRUE)[[1]],
    global(cover_tls, median, na.rm = TRUE)[[1]],
    global(cover_tls, sd, na.rm = TRUE)[[1]],
    global(cover_tls, range, na.rm = TRUE)[1],
    global(cover_tls, range, na.rm = TRUE)[2]
  ),
  TLS_summer = c(
    global(cover_tls_s, mean, na.rm = TRUE)[[1]],
    global(cover_tls_s, median, na.rm = TRUE)[[1]],
    global(cover_tls_s, sd, na.rm = TRUE)[[1]],
    global(cover_tls_s, range, na.rm = TRUE)[1],
    global(cover_tls_s, range, na.rm = TRUE)[2]
  ),
  BP_winter = c(
    global(cover_bp, mean, na.rm = TRUE)[[1]],
    global(cover_bp, median, na.rm = TRUE)[[1]],
    global(cover_bp, sd, na.rm = TRUE)[[1]],
    global(cover_bp, range, na.rm = TRUE)[1],
    global(cover_bp, range, na.rm = TRUE)[2]
  ),
  BP_summer = c(
    global(cover_bp_s, mean, na.rm = TRUE)[[1]],
    global(cover_bp_s, median, na.rm = TRUE)[[1]],
    global(cover_bp_s, sd, na.rm = TRUE)[[1]],
    global(cover_bp_s, range, na.rm = TRUE)[1],
    global(cover_bp_s, range, na.rm = TRUE)[2]
  )
)

print(metrics)


################### 5. Calculate difference rasters ############################

# difference map
cover_uav_resampled <- resample(cover_uav, cover_als, method = "bilinear")
cover_tls_resampled <- resample(cover_tls, cover_als, method = "bilinear")
cover_bp_resampled <- resample(cover_bp, cover_als, method = "bilinear")
cover_tls_resampled2 <- resample(cover_tls, cover_uav, method = "bilinear")
cover_bp_resampled2 <- resample(cover_bp, cover_uav, method = "bilinear")
cover_bp_resampled3 <- resample(cover_bp, cover_tls, method = "bilinear")

cover_uav_s_resampled <- resample(cover_uav_s, cover_als, method = "bilinear")
cover_tls_s_resampled <- resample(cover_tls_s, cover_als, method = "bilinear")
cover_bp_s_resampled <- resample(cover_bp_s, cover_als, method = "bilinear")
cover_tls_s_resampled2 <- resample(cover_tls_s, cover_uav_s, method = "bilinear")
cover_bp_s_resampled2 <- resample(cover_bp_s, cover_uav_s, method = "bilinear")
cover_bp_s_resampled3 <- resample(cover_bp_s, cover_tls_s, method = "bilinear")


diff_raster_als_uav <- cover_als - cover_uav_resampled
diff_raster_als_tls <- cover_als - cover_tls_resampled
diff_raster_als_bp <- cover_als - cover_tls_resampled
diff_raster_uav_tls <- cover_uav - cover_tls_resampled2
diff_raster_uav_bp <- cover_uav - cover_bp_resampled2
diff_raster_tls_bp <- cover_tls - cover_bp_resampled3

diff_raster_als_uav_s <- cover_als - cover_uav_s_resampled
diff_raster_als_tls_s <- cover_als - cover_tls_s_resampled
diff_raster_als_bp_s <- cover_als - cover_tls_s_resampled
diff_raster_uav_tls_s <- cover_uav_s - cover_tls_s_resampled2
diff_raster_uav_bp_s <- cover_uav_s - cover_bp_s_resampled2
diff_raster_tls_bp_s <- cover_tls_s - cover_bp_s_resampled3

# extract values
vals_uav <- values(cover_uav_resampled)
vals_als <- values(cover_als)
vals_tls <- values(cover_tls_resampled)
vals_bp <- values(cover_bp_resampled)
valid <- complete.cases(vals_uav, vals_als, vals_bp, vals_tls)

vals_uav_s <- values(cover_uav_s_resampled)
vals_als <- values(cover_als)
vals_tls_s <- values(cover_tls_s_resampled)
vals_bp_s <- values(cover_bp_s_resampled)
valid_s <- complete.cases(vals_uav_s, vals_als, vals_bp_s, vals_tls_s)


##################### 6. Difference raster metrics #############################

# extract diff raster values
vals_diff_als_uav <- values(diff_raster_als_uav)
vals_diff_als_tls <- values(diff_raster_als_tls)
vals_diff_als_bp <- values(diff_raster_als_bp)
vals_diff_uav_tls <- values(diff_raster_uav_tls)
vals_diff_uav_bp <- values(diff_raster_uav_bp)
vals_diff_tls_bp <- values(diff_raster_tls_bp)

vals_diff_als_uav_s <- values(diff_raster_als_uav_s)
vals_diff_als_tls_s <- values(diff_raster_als_tls_s)
vals_diff_als_bp_s <- values(diff_raster_als_bp_s)
vals_diff_uav_tls_s <- values(diff_raster_uav_tls_s)
vals_diff_uav_bp_s <- values(diff_raster_uav_bp_s)
vals_diff_tls_bp_s <- values(diff_raster_tls_bp_s)

vals_diff_clean_als_uav <- vals_diff_als_uav[!is.na(vals_diff_als_uav)]
vals_diff_clean_als_tls <- vals_diff_als_tls[!is.na(vals_diff_als_tls)]
vals_diff_clean_als_bp <- vals_diff_als_bp[!is.na(vals_diff_als_bp)]
vals_diff_clean_uav_tls <- vals_diff_uav_tls[!is.na(vals_diff_uav_tls)]
vals_diff_clean_uav_bp <- vals_diff_uav_bp[!is.na(vals_diff_uav_bp)]
vals_diff_clean_tls_bp <- vals_diff_tls_bp[!is.na(vals_diff_tls_bp)]

vals_diff_clean_als_uav_s <- vals_diff_als_uav_s[!is.na(vals_diff_als_uav_s)]
vals_diff_clean_als_tls_s <- vals_diff_als_tls_s[!is.na(vals_diff_als_tls_s)]
vals_diff_clean_als_bp_s <- vals_diff_als_bp_s[!is.na(vals_diff_als_bp_s)]
vals_diff_clean_uav_tls_s <- vals_diff_uav_tls_s[!is.na(vals_diff_uav_tls_s)]
vals_diff_clean_uav_bp_s <- vals_diff_uav_bp_s[!is.na(vals_diff_uav_bp_s)]
vals_diff_clean_tls_bp_s <- vals_diff_tls_bp_s[!is.na(vals_diff_tls_bp_s)]


metrics_combi_winter <- tibble(
  Metric = c("Mean", "Median", "SD", "Range Min", "Range Max", "RSME", "MAE"),
  ALS_UAV = c(
    mean(vals_diff_clean_als_uav)[[1]],
    median(vals_diff_clean_als_uav)[[1]],
    sd(vals_diff_clean_als_uav)[[1]],
    range(vals_diff_clean_als_uav)[1],
    range(vals_diff_clean_als_uav)[2],
    sqrt(mean((vals_als[valid] - vals_uav[valid])^2)),
    mean(abs(vals_als[valid] - vals_uav[valid]))
  ),
  ALS_TLS = c(
    mean(vals_diff_clean_als_tls)[[1]],
    median(vals_diff_clean_als_tls)[[1]],
    sd(vals_diff_clean_als_tls)[[1]],
    range(vals_diff_clean_als_tls)[1],
    range(vals_diff_clean_als_tls)[2],
    sqrt(mean((vals_als[valid] - vals_tls[valid])^2)),
    mean(abs(vals_als[valid] - vals_tls[valid]))
  ),
  ALS_BP = c(
    mean(vals_diff_clean_als_bp)[[1]],
    median(vals_diff_clean_als_bp)[[1]],
    sd(vals_diff_clean_als_bp)[[1]],
    range(vals_diff_clean_als_bp)[1],
    range(vals_diff_clean_als_bp)[2],
    sqrt(mean((vals_als[valid] - vals_bp[valid])^2)),
    mean(abs(vals_als[valid] - vals_bp[valid]))
  ),
  UAV_TLS = c(
    mean(vals_diff_clean_uav_tls)[[1]],
    median(vals_diff_clean_uav_tls)[[1]],
    sd(vals_diff_clean_uav_tls)[[1]],
    range(vals_diff_clean_uav_tls)[1],
    range(vals_diff_clean_uav_tls)[2],
    sqrt(mean((vals_uav[valid] - vals_tls[valid])^2)),
    mean(abs(vals_uav[valid] - vals_tls[valid]))
  ),
  UAV_BP = c(
    mean(vals_diff_clean_uav_bp)[[1]],
    median(vals_diff_clean_uav_bp)[[1]],
    sd(vals_diff_clean_uav_bp)[[1]],
    range(vals_diff_clean_uav_bp)[1],
    range(vals_diff_clean_uav_bp)[2],
    sqrt(mean((vals_uav[valid] - vals_bp[valid])^2)),
    mean(abs(vals_uav[valid] - vals_bp[valid]))
  ),
  TLS_BP = c(
    mean(vals_diff_clean_tls_bp)[[1]],
    median(vals_diff_clean_tls_bp)[[1]],
    sd(vals_diff_clean_tls_bp)[[1]],
    range(vals_diff_clean_tls_bp)[1],
    range(vals_diff_clean_tls_bp)[2],
    sqrt(mean((vals_tls[valid] - vals_bp[valid])^2)),
    mean(abs(vals_tls[valid] - vals_bp[valid]))
  )
)

metrics_combi_summer <- tibble(
  Metric = c("Mean", "Median", "SD", "Range Min", "Range Max", "RSME", "MAE"),
  ALS_UAV = c(
    mean(vals_diff_clean_als_uav_s)[[1]],
    median(vals_diff_clean_als_uav_s)[[1]],
    sd(vals_diff_clean_als_uav_s)[[1]],
    range(vals_diff_clean_als_uav_s)[1],
    range(vals_diff_clean_als_uav_s)[2],
    sqrt(mean((vals_als[valid_s] - vals_uav_s[valid_s])^2)),
    mean(abs(vals_als[valid_s] - vals_uav_s[valid_s]))
  ),
  ALS_TLS = c(
    mean(vals_diff_clean_als_tls_s)[[1]],
    median(vals_diff_clean_als_tls_s)[[1]],
    sd(vals_diff_clean_als_tls_s)[[1]],
    range(vals_diff_clean_als_tls_s)[1],
    range(vals_diff_clean_als_tls_s)[2],
    sqrt(mean((vals_als[valid_s] - vals_tls_s[valid_s])^2)),
    mean(abs(vals_als[valid_s] - vals_tls_s[valid_s]))
  ),
  ALS_BP = c(
    mean(vals_diff_clean_als_bp_s)[[1]],
    median(vals_diff_clean_als_bp_s)[[1]],
    sd(vals_diff_clean_als_bp_s)[[1]],
    range(vals_diff_clean_als_bp_s)[1],
    range(vals_diff_clean_als_bp_s)[2],
    sqrt(mean((vals_als[valid_s] - vals_bp_s[valid_s])^2)),
    mean(abs(vals_als[valid_s] - vals_bp_s[valid_s]))
  ),
  UAV_TLS = c(
    mean(vals_diff_clean_uav_tls_s)[[1]],
    median(vals_diff_clean_uav_tls_s)[[1]],
    sd(vals_diff_clean_uav_tls_s)[[1]],
    range(vals_diff_clean_uav_tls_s)[1],
    range(vals_diff_clean_uav_tls_s)[2],
    sqrt(mean((vals_uav_s[valid_s] - vals_tls_s[valid_s])^2)),
    mean(abs(vals_uav_s[valid_s] - vals_tls_s[valid_s]))
  ),
  UAV_BP = c(
    mean(vals_diff_clean_uav_bp_s)[[1]],
    median(vals_diff_clean_uav_bp_s)[[1]],
    sd(vals_diff_clean_uav_bp_s)[[1]],
    range(vals_diff_clean_uav_bp_s)[1],
    range(vals_diff_clean_uav_bp_s)[2],
    sqrt(mean((vals_uav_s[valid_s] - vals_bp_s[valid_s])^2)),
    mean(abs(vals_uav_s[valid_s] - vals_bp_s[valid_s]))
  ),
  TLS_BP = c(
    mean(vals_diff_clean_tls_bp_s)[[1]],
    median(vals_diff_clean_tls_bp_s)[[1]],
    sd(vals_diff_clean_tls_bp_s)[[1]],
    range(vals_diff_clean_tls_bp_s)[1],
    range(vals_diff_clean_tls_bp_s)[2],
    sqrt(mean((vals_tls_s[valid_s] - vals_bp_s[valid_s])^2)),
    mean(abs(vals_tls_s[valid_s] - vals_bp_s[valid_s]))
  )
)

print(metrics)


########################## 7. Paired t-tests ###################################

# paired t-test
t.test(vals_als[valid], vals_uav[valid], paired=TRUE)
t.test(vals_als[valid], vals_tls[valid], paired=TRUE)
t.test(vals_als[valid], vals_bp[valid], paired=TRUE)
t.test(vals_uav[valid], vals_tls[valid], paired=TRUE)
t.test(vals_uav[valid], vals_bp[valid], paired=TRUE)
t.test(vals_tls[valid], vals_bp[valid], paired=TRUE)

t.test(vals_als[valid_s], vals_uav_s[valid_s], paired=TRUE)
t.test(vals_als[valid_s], vals_tls_s[valid_s], paired=TRUE)
t.test(vals_als[valid_s], vals_bp_s[valid_s], paired=TRUE)
t.test(vals_uav_s[valid_s], vals_tls_s[valid_s], paired=TRUE)
t.test(vals_uav_s[valid_s], vals_bp_s[valid_s], paired=TRUE)
t.test(vals_tls_s[valid_s], vals_bp_s[valid_s], paired=TRUE)



############################# 8. Boxplots ######################################

### make values df into long format
data <- data.frame()
data <- rbind(data, vals_als)
data <- cbind(data, vals_tls)
data <- cbind(data, vals_uav)
data <- cbind(data, vals_bp)

data_long <- pivot_longer(data, 
                          cols = c("cover_als", 
                                   "cover_tls", 
                                   "cover_uav", 
                                   "cover_bp"), 
                          names_to = "Source", values_to = "Cover")

ggplot(data_long, aes(x = Source, y = Cover))+
  geom_jitter(color = "grey",
              alpha = 0.7,
              size = 1)+
  geom_boxplot(color = "darkolivegreen",
               fill = "darkolivegreen4",
               alpha = 0.3,
               notch = TRUE,
               notchwidth = 0.8,
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=2)+
  labs(title = "Canopy Cover Measurement Comparison", )+
  xlab("") +
  ylab("Canopy Cover [%]") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 6.28 6.4, cubes quadratic


##################### 9. Raster comparison plot ################################

# for winter

# define common scale
common_scale <- scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "red",
  midpoint = 0,
  limits = c(-100, 100),
  oob = scales::squish # avoids warnings from small differences
)

diff_raster_als_uav <- rename(as.data.frame(diff_raster_als_uav, 
                                            xy = TRUE, na.rm = TRUE), 
                              canopy_diff = cover_als)
diff_raster_als_tls <- rename(as.data.frame(diff_raster_als_tls, 
                                            xy = TRUE, na.rm = TRUE), 
                              canopy_diff = cover_als)
diff_raster_als_bp <- rename(as.data.frame(diff_raster_als_bp, 
                                           xy = TRUE, na.rm = TRUE), 
                             canopy_diff = cover_als)
diff_raster_uav_tls <- rename(as.data.frame(diff_raster_uav_tls, 
                                            xy = TRUE, na.rm = TRUE), 
                              canopy_diff = cover_uav  )
diff_raster_uav_bp <- rename(as.data.frame(diff_raster_uav_bp, 
                                           xy = TRUE, na.rm = TRUE), 
                             canopy_diff = cover_uav  )
diff_raster_tls_bp <- rename(as.data.frame(diff_raster_tls_bp, 
                                           xy = TRUE, na.rm = TRUE), 
                             canopy_diff = cover_tls)

# make individual plots
a <- ggplot(data = diff_raster_als_uav, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("UAV") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "ALS - UAV", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

b <- ggplot(data = diff_raster_als_tls, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("TLS") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "ALS - TLS", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

c <- ggplot(data = diff_raster_als_bp, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("Backpack") +
  xlab("ALS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "ALS - Backpack", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')


d <- ggplot(data = diff_raster_uav_tls, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "UAV - TLS", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

e <- ggplot(data = diff_raster_uav_bp, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("UAV")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "UAV - Backpack", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

f <- ggplot(data = diff_raster_tls_bp, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("TLS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "TLS - Backpack", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

# define layout
layout <- "
A##
BD#
CEF
"

# make one plot
combined <- (
  a + b + c + d + e + f +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right") 
)

# combined +
#   plot_annotation(
#     title = "Pairwise Comparison of Canopy Cover measurements"
#   )


# for summer
diff_raster_als_uav_s <- rename(as.data.frame(diff_raster_als_uav_s, 
                                            xy = TRUE, na.rm = TRUE), 
                              canopy_diff = cover_als)
diff_raster_als_tls_s <- rename(as.data.frame(diff_raster_als_tls_s, 
                                            xy = TRUE, na.rm = TRUE), 
                              canopy_diff = cover_als)
diff_raster_als_bp_s <- rename(as.data.frame(diff_raster_als_bp_s, 
                                           xy = TRUE, na.rm = TRUE), 
                             canopy_diff = cover_als)
diff_raster_uav_tls_s <- rename(as.data.frame(diff_raster_uav_tls_s, 
                                            xy = TRUE, na.rm = TRUE), 
                              canopy_diff = cover_uav_s)
diff_raster_uav_bp_s <- rename(as.data.frame(diff_raster_uav_bp_s, 
                                           xy = TRUE, na.rm = TRUE), 
                             canopy_diff = cover_uav_s)
diff_raster_tls_bp_s <- rename(as.data.frame(diff_raster_tls_bp_s, 
                                           xy = TRUE, na.rm = TRUE), 
                             canopy_diff = cover_tls_s)

# make individual plots
a <- ggplot(data = diff_raster_als_uav_s, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("UAV") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "ALS - UAV", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

b <- ggplot(data = diff_raster_als_tls_s, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("TLS") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "ALS - TLS", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

c <- ggplot(data = diff_raster_als_bp_s, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("Backpack") +
  xlab("ALS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "ALS - Backpack", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')


d <- ggplot(data = diff_raster_uav_tls_s, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "UAV - TLS", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

e <- ggplot(data = diff_raster_uav_bp_s, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("UAV")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "UAV - Backpack", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

f <- ggplot(data = diff_raster_tls_bp_s, aes(x = x, y = y, fill = canopy_diff))+
  geom_tile()+
  coord_equal() +
  ylab("") +
  xlab("TLS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        legend.position = "none")+ 
  annotate("label", x = 523955, y = 5537700, 
           label = "TLS - Backpack", hjust = "left", fill = "white")+ 
  labs(fill = 'Canopy Cover\nDifference [%]')

# define layout
layout <- "
A##
BD#
CEF
"

# make one plot
combined <- (
  a + b + c + d + e + f +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right") 
)
