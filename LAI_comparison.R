################################################################################
######################## Script for LAI comparison #############################
################################################################################

# - alignment of the data
# - statistical comparison (pearson/spearman R, RMSE, MAE, Cohens Kappa)
# - 

install.packages("spatialEco")
library(raster)
library(terra)
library(spatialEco)


setwd("E:/Sonja/Msc_Thesis/data/Metrics/")


# LAI
lai_uav <- raster("uav/forest_metrics_LiDAR360/UAV_feb2_shifted_clipped_LeafAreaIndex.tif")
lai_als <- raster("als/forest_metrics_lidar360/ALS_first_last_merged_shifted_LeafAreaIndex.tif")

uav_clipped <- crop(lai_uav, lai_als)
las_clipped <- crop(lai_als, uav_clipped)
plot(uav_clipped)
plot(las_clipped)

uav_resampled <- resample(uav_clipped, las_clipped, method = "ngb")

stack_metrics <- stack(uav_resampled, las_clipped)
cor_matrix <- layerStats(stack_metrics, 'pearson', na.rm = TRUE)
# pearson correlation coefficient
#            UAV LAI       ALS LAI
# UAV LAI    1.0000000     0.0902783
# ALS LAI    0.0902783     1.0000000
#
# mean
# UAV        ALS
# 2.109456   20.899692 

corr_coeff <- cor_matrix$`pearson correlation coefficient`[2,1]
# [1] 0.0902783

p_value <- corLocal(uav_resampled, las_clipped, test = TRUE)[[2]]
plot(p_value)

cellStats(abs(las_clipped - uav_resampled), mean)
# [1] 18.80724

lai_uav_rast <- rast(uav_resampled)
lai_als_rast <- rast(las_clipped)
lai_difference <- raster.change(lai_als_rast, lai_uav_rast, s = 5, stat ="t.test")
plot(lai_difference)

lm_model <- lm(as.matrix(lai_als_rast) ~ as.matrix(lai_uav_rast))
summary(lm_model)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -32.956   1.595   2.023   2.421   3.544 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              19.46768    0.17868  108.95   <2e-16 ***
# as.matrix(lai_uav_rast)  0.64704     0.07463    8.67   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.381 on 8481 degrees of freedom
# (2197 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.008786,	Adjusted R-squared:  0.008669 
# F-statistic: 75.17 on 1 and 8481 DF,  p-value: < 2.2e-16



# Canopy Cover
cc_als <- raster("als/forest_metrics_lidar360/ALS_first_last_merged_shifted_CanopyCover.tif")
plot(cc_als)
cc_uav <- raster("uav/forest_metrics_LiDAR360/UAV_feb2_shifted_clipped_CanopyCover.tif")
plot(cc_uav)

cc_uav_resampled <- resample(cc_uav, cc_als, method = "bilinear")

stack_metrics <- stack(cc_uav_resampled, cc_als)
cor_matrix <- layerStats(stack_metrics, 'pearson', na.rm = TRUE)
# pearson correlation coefficient
#         UAV CC       ALS CC
# UAV CC  1.0000000    0.3107444
# ALS CC  0.3107444    1.0000000
#
# mean
# UAV CC        ALS CC 
# 0.4912051     0.6175862 

plot(cc_als-cc_uav_resampled, main = "Difference Map")
spplot(stack_metrics, col.regions = heat.colors(100))

corr_coeff <- cor_matrix$`pearson correlation coefficient`[2,1]
# [1] 0.3107444

p_value <- corLocal(cc_uav_resampled, cc_als, test = TRUE)[[2]]
plot(p_value)

cellStats(abs(cc_als - cc_uav_resampled), mean)
# [1] 0.1749809

cc_uav_rast <- rast(cc_uav_resampled)
cc_als_rast <- rast(cc_als)
cc_difference <- raster.change(cc_als_rast, cc_uav_rast, s = 5, stat ="t.test")
plot(cc_difference)

lm_model <- lm(as.matrix(cc_als_rast) ~ as.matrix(cc_uav_rast))
summary(lm_model)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51866 -0.11326  0.01373  0.12254  0.57201 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.408054   0.005997   68.05   <2e-16 ***
# as.matrix(cc_uav_rast)   0.407749   0.011206   36.39   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1679 on 9636 degrees of freedom
# (1736 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.1208,	Adjusted R-squared:  0.1207 
# F-statistic:  1324 on 1 and 9636 DF,  p-value: < 2.2e-16





# gap fraction
gf_als <- raster("als/forest_metrics_lidar360/ALS_first_last_merged_shifted_GapFraction.tif")
plot(gf_als)
gf_uav <- raster("uav/forest_metrics_LiDAR360/UAV_feb2_shifted_clipped_GapFraction.tif")
plot(gf_uav)

gf_uav_resampled <- resample(gf_uav, gf_als, method = "bilinear")
plot(gf_uav_resampled)

stack_metrics <- stack(gf_uav_resampled, gf_als)
cor_matrix <- layerStats(stack_metrics, 'pearson', na.rm = TRUE)
# pearson correlation coefficient
#         UAV GF       ALS GF
# UAV GF  1.0000000    0.5146394
# ALS GF  0.5146394    1.0000000
# 
# mean
# UAV GF       ALS GF 
# 0.5124771    0.4542397 

corr_coeff <- cor_matrix$`pearson correlation coefficient`[2,1]
# [1] 0.5146394

p_value <- corLocal(gf_uav_resampled, gf_als, test = TRUE)[[2]]
plot(p_value)

cellStats(abs(gf_als - gf_uav_resampled), mean)
# [1] 0.1773367

gf_uav_rast <- rast(gf_uav_resampled)
gf_als_rast <- rast(gf_als)
gf_difference <- raster.change(gf_als_rast, gf_uav_rast, s = 5, stat ="t.test")
plot(gf_difference)

lm_model <- lm(as.matrix(gf_als_rast) ~ as.matrix(gf_uav_rast))
summary(lm_model)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.55723 -0.14965 -0.03082  0.12317  0.76572 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.083750   0.006323   13.24   <2e-16 ***
# as.matrix(gf_uav_rast) 0.716713   0.011625   61.65   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2074 on 10414 degrees of freedom
# (958 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.2674,	Adjusted R-squared:  0.2673 
# F-statistic:  3801 on 1 and 10414 DF,  p-value: < 2.2e-16
