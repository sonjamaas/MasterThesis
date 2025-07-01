################################################################################
######################## Script for LAI comparison #############################
################################################################################

# - alignment of the data
# - statistical comparison 

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

setwd("E:/Sonja/Msc_Thesis/data/Metrics/LAI_leafR")


# LAI
lai_uav <- rast("lai_uav_leafR.tif")
lai_als <- rast("lai_als_leafR.tif")
lai_tls <- rast("lai_tls_leafR.tif")
lai_bp <- rast("lai_bp_leafR.tif")

plot(lai_uav)
ggplot(data = as.data.frame(lai_uav, xy = TRUE), aes(x = x, y = y, fill = lai_uav_leafR ))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    name = "LAI"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "LAI derived from UAV data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(lai_als)
ggplot(data = as.data.frame(lai_als, xy = TRUE), aes(x = x, y = y, fill = lai_als_leafR ))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    name = "LAI"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "LAI derived from ALS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(lai_tls)
ggplot(data = as.data.frame(lai_tls, xy = TRUE), aes(x = x, y = y, fill = lai_tls_leafR ))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    name = "LAI"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "LAI derived from TLS data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

plot(lai_bp)
ggplot(data = as.data.frame(lai_bp, xy = TRUE), aes(x = x, y = y, fill = lai_bp_leafR ))+
  geom_tile()+
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    name = "LAI"
  )+
  coord_equal() +
  theme_minimal()+
  labs(title = "LAI derived from backpack data", )+
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 15))

# extract values
vals_uav <- values(lai_uav)
vals_als <- values(lai_als)
vals_tls <- values(lai_tls)
vals_bp <- values(lai_bp)
valid <- complete.cases(vals_uav, vals_als, vals_bp, vals_tls)

# plot values to check them
plot(vals_uav[valid], vals_als[valid],
     xlab="UAV LAI", ylab="ALS LAI",
     main="Pixel-wise Comparison (UAV + LAI)")
abline(0, 1, col="red")

plot(vals_uav[valid], vals_tls[valid],
     xlab="UAV LAI", ylab="TLS LAI",
     main="Pixel-wise Comparison (UAV + TLS)")
abline(0, 1, col="red")

plot(vals_uav[valid], vals_bp[valid],
     xlab="UAV LAI", ylab="Backpack LAI",
     main="Pixel-wise Comparison (UAV + Backpack)")
abline(0, 1, col="red")

plot(vals_als[valid], vals_tls[valid],
     xlab="ALS LAI", ylab="TLS LAI",
     main="Pixel-wise Comparison (ALS + TLS)")
abline(0, 1, col="red")

plot(vals_als[valid], vals_bp[valid],
     xlab="ALS LAI", ylab="Backpack LAI",
     main="Pixel-wise Comparison (ALS + Backpack)")
abline(0, 1, col="red")

plot(vals_tls[valid], vals_bp[valid],
     xlab="TLS LAI", ylab="Backpack LAI",
     main="Pixel-wise Comparison (TLS + Backpack)")
abline(0, 1, col="red")



# Layer summaries
metrics <- tibble(
  Metric = c("Mean", "Median", "SD", "Min", "Max"),
  UAV = c(
    global(lai_uav, mean, na.rm = TRUE)[[1]],
    global(lai_uav, median, na.rm = TRUE)[[1]],
    global(lai_uav, sd, na.rm = TRUE)[[1]],
    global(lai_uav, range, na.rm = TRUE)[1],
    global(lai_uav, range, na.rm = TRUE)[2]
  ),
  ALS = c(
    global(lai_als, mean, na.rm = TRUE)[[1]],
    global(lai_als, median, na.rm = TRUE)[[1]],
    global(lai_als, sd, na.rm = TRUE)[[1]],
    global(lai_als, range, na.rm = TRUE)[1],
    global(lai_als, range, na.rm = TRUE)[2]
  ),
  TLS = c(
    global(lai_tls, mean, na.rm = TRUE)[[1]],
    global(lai_tls, median, na.rm = TRUE)[[1]],
    global(lai_tls, sd, na.rm = TRUE)[[1]],
    global(lai_tls, range, na.rm = TRUE)[1],
    global(lai_tls, range, na.rm = TRUE)[2]
  ),
  BP = c(
    global(lai_bp, mean, na.rm = TRUE)[[1]],
    global(lai_bp, median, na.rm = TRUE)[[1]],
    global(lai_bp, sd, na.rm = TRUE)[[1]],
    global(lai_bp, range, na.rm = TRUE)[1],
    global(lai_bp, range, na.rm = TRUE)[2]
  )
)

print(metrics)

values1 <- values(lai_uav)
values2 <- values(lai_als)
values3 <- values(lai_tls)
values4 <- values(lai_bp)

# Exclude NAs
valid <- complete.cases(values1, values2, values3, values4)

hist(values1, breaks=50, col=rgb(1,0,0,0.5), main="Histogram of UAV LAI Layers",
     xlab="LAI", xlim=range(c(values1, values2), na.rm=TRUE))
hist(values2, breaks=50, col=rgb(0,0,1,0.5), add=TRUE)
hist(values3, breaks=50, col=rgb(0,1,0,0.5), add=TRUE)
hist(values4, breaks=50, col=rgb(1,1,0,0.5), add=TRUE)

legend("topright", legend=c("UAV", "ALS", "TLS", "Backpack"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,1,0,0.5), rgb(1,1,0,0.5)))



# DIFFERENCE OF ALS - UAV

# run linear model to check relationship
lm_model <- lm(vals_als[valid] ~ vals_uav[valid])
summary(lm_model)

vals_df <- data.frame(
  als = vals_als,
  uav = vals_uav
)

vals_df <- na.omit(vals_df)
# library(ggplot2)
ggplot(data = vals_df, aes(x=lai_als , y = lai_uav))+
  geom_tile()

# difference map
lai_uav_resampled <- resample(lai_uav, lai_als, method = "bilinear")
diff_raster_als_uav <- lai_als - lai_uav_resampled
x <- plot(diff_raster_als_uav, main = "ALS - UAV LAI")


global(diff_raster_als_uav, fun = mean, na.rm=TRUE)
global(diff_raster_als_uav, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff <- values(diff_raster_als_uav)
vals_diff_clean <- vals_diff[!is.na(vals_diff)]

# summary statistics
mean(vals_diff_clean)
# [1] 0.1965482
sd(vals_diff_clean)
# [1] 0.4924401
median(vals_diff_clean)
# [1] 0.1696863
range(vals_diff_clean)
# [1] -1.576113  1.825174

# RSME to quantify error magnitude
rmse <- sqrt(mean((vals_als[valid] - vals_uav[valid])^2))
# [1] 0.5348211

# MAE (Mean absolute error)
mae <- mean(abs(vals_als[valid] - vals_uav[valid]))
# [1] 0.4212459

# paired t-test
t.test(vals_als[valid], vals_uav[valid], paired=TRUE)

# correlation 
lm_model <- lm(vals_als[valid] ~ vals_uav[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - UAV)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_als[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)




# DIFFERENCE OF ALS - TLS

# run linear model to check relationship
lm_model <- lm(vals_als[valid] ~ vals_tls[valid])
summary(lm_model)

vals_df <- data.frame(
  als = vals_als,
  uav = vals_tls
)

vals_df <- na.omit(vals_df)
# library(ggplot2)
ggplot(data = vals_df, aes(x=lai_als , y = lai_tls))+
  geom_jitter()

# difference map
lai_tls_resampled <- resample(lai_tls, lai_als, method = "bilinear")

diff_raster_als_tls <- lai_als - lai_tls_resampled
plot(diff_raster_als_tls, main = "ALS - TLS LAI")

global(diff_raster_als_tls, fun = mean, na.rm=TRUE)
global(diff_raster_als_tls, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff <- values(diff_raster_als_tls)
vals_diff_clean <- vals_diff[!is.na(vals_diff)]

# summary statistics
mean(vals_diff_clean)
# [1] -0.2503554
sd(vals_diff_clean)
# [1] 0.9807483
median(vals_diff_clean)
# [1] -0.1131719
range(vals_diff_clean)
# [1] -5.321009 2.280832

# RSME to quantify error magnitude
rmse <- sqrt(mean((vals_als[valid] - vals_tls[valid])^2))
# [1] 1.078008

# MAE (Mean absolute error)
mae <- mean(abs(vals_als[valid] - vals_tls[valid]))
# [1] 0.775314

# paired t-test
t.test(vals_als[valid], vals_tls[valid], paired=TRUE)

# correlation 
lm_model <- lm(vals_als[valid] ~ vals_tls[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - TLS)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_als[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)




# DIFFERENCE OF ALS - BP

# run linear model to check relationship
lm_model <- lm(vals_als[valid] ~ vals_bp[valid])
summary(lm_model)

vals_df <- data.frame(
  als = vals_als,
  uav = vals_bp
)

vals_df <- na.omit(vals_df)
# library(ggplot2)
ggplot(data = vals_df, aes(x=lai_als , y = lai_bp))+
  geom_jitter()

# difference map
lai_bp_resampled <- resample(lai_bp, lai_als, method = "bilinear")

diff_raster_als_bp <- lai_als - lai_bp_resampled
plot(diff_raster_als_bp, main = "ALS - Backpack LAI")

global(diff_raster_als_bp, fun = mean, na.rm=TRUE)
global(diff_raster_als_bp, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff <- values(diff_raster_als_bp)
vals_diff_clean <- vals_diff[!is.na(vals_diff)]

# summary statistics
mean(vals_diff_clean)
# [1] 0.530186
sd(vals_diff_clean)
# [1] 0.6967018
median(vals_diff_clean)
# [1] 0.5724063
range(vals_diff_clean)
# [1] -2.679261  2.402156

# RSME to quantify error magnitude
rmse <- sqrt(mean((vals_als[valid] - vals_bp[valid])^2))
# [1] 0.8923286

# MAE (Mean absolute error)
mae <- mean(abs(vals_als[valid] - vals_bp[valid]))
# [1] 0.7342134

# paired t-test
t.test(vals_als[valid], vals_bp[valid], paired=TRUE)

# correlation 
lm_model <- lm(vals_als[valid] ~ vals_bp[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - Backpack)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_bp[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)






# DIFFERENCE OF UAV - TLS

# run linear model to check relationship
lm_model <- lm(vals_uav[valid] ~ vals_tls[valid])
summary(lm_model)

vals_df <- data.frame(
  als = vals_uav,
  uav = vals_tls
)

vals_df <- na.omit(vals_df)
# library(ggplot2)
ggplot(data = vals_df, aes(x=lai_uav , y = lai_tls))+
  geom_jitter()

# difference map
lai_tls_resampled <- resample(lai_tls, lai_uav, method = "bilinear")

diff_raster_uav_tls <- lai_uav - lai_tls_resampled
plot(diff_raster_uav_tls, main = "UAV - TLS LAI")

global(diff_raster_uav_tls, fun = mean, na.rm=TRUE)
global(diff_raster_uav_tls, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff <- values(diff_raster_uav_tls)
vals_diff_clean <- vals_diff[!is.na(vals_diff)]

# summary statistics
mean(vals_diff_clean)
# [1] -0.4426547
sd(vals_diff_clean)
# [1] 0.7846737
median(vals_diff_clean)
# [1] -0.2929525
range(vals_diff_clean)
# [1] -5.087441  1.617790

# RSME to quantify error magnitude
rmse <- sqrt(mean((vals_uav[valid] - vals_tls[valid])^2))
# [1] 1.019687

# MAE (Mean absolute error)
mae <- mean(abs(vals_uav[valid] - vals_tls[valid]))
# [1] 0.6877927

# paired t-test
t.test(vals_uav[valid], vals_tls[valid], paired=TRUE)

# correlation 
lm_model <- lm(vals_uav[valid] ~ vals_tls[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - Backpack)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_bp[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)






# DIFFERENCE OF UAV - BP

# run linear model to check relationship
lm_model <- lm(vals_uav[valid] ~ vals_bp[valid])
summary(lm_model)

vals_df <- data.frame(
  als = vals_uav,
  uav = vals_bp
)

vals_df <- na.omit(vals_df)
# library(ggplot2)
ggplot(data = vals_df, aes(x=lai_uav , y = lai_bp))+
  geom_jitter()

# difference map
lai_bp_resampled <- resample(lai_bp, lai_uav, method = "bilinear")

diff_raster_uav_bp <- lai_uav - lai_bp_resampled
plot(diff_raster_uav_bp, main = "UAV - Backpack LAI")

global(diff_raster_uav_bp, fun = mean, na.rm=TRUE)
global(diff_raster_uav_bp, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff <- values(diff_raster_uav_bp)
vals_diff_clean <- vals_diff[!is.na(vals_diff)]

# summary statistics
mean(vals_diff_clean)
# [1] 0.3351966
sd(vals_diff_clean)
# [1] 0.5067623
median(vals_diff_clean)
# [1] 0.4083022
range(vals_diff_clean)
# [1] -2.000405  1.775489

# RSME to quantify error magnitude
rmse <- sqrt(mean((vals_uav[valid] - vals_bp[valid])^2))
# [1] 0.6531309

# MAE (Mean absolute error)
mae <- mean(abs(vals_uav[valid] - vals_bp[valid]))
# [1] 0.5429159

# paired t-test
t.test(vals_uav[valid], vals_bp[valid], paired=TRUE)

# correlation 
lm_model <- lm(vals_uav[valid] ~ vals_bp[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - Backpack)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_bp[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)





# DIFFERENCE OF TLS - BP

# run linear model to check relationship
lm_model <- lm(vals_tls[valid] ~ vals_bp[valid])
summary(lm_model)

vals_df <- data.frame(
  als = vals_tls,
  uav = vals_bp
)

vals_df <- na.omit(vals_df)
# library(ggplot2)
ggplot(data = vals_df, aes(x=lai_tls , y = lai_bp))+
  geom_jitter()

# difference map
lai_bp_resampled <- resample(lai_bp, lai_tls, method = "bilinear")

diff_raster_tls_bp <- lai_tls - lai_bp_resampled
plot(diff_raster_tls_bp, main = "TLS - Backpack LAI")

global(diff_raster_tls_bp, fun = mean, na.rm=TRUE)
global(diff_raster_tls_bp, fun = sd, na.rm=TRUE)

# extract diff raster values
vals_diff <- values(diff_raster_tls_bp)
vals_diff_clean <- vals_diff[!is.na(vals_diff)]

# summary statistics
mean(vals_diff_clean)
# [1] 0.7812597
sd(vals_diff_clean)
# [1] 0.846069
median(vals_diff_clean)
# [1] 0.6210544
range(vals_diff_clean)
# [1] -2.636167  5.316502

# RSME to quantify error magnitude
rmse <- sqrt(mean((vals_tls[valid] - vals_bp[valid])^2))
# [1] 1.151602

# MAE (Mean absolute error)
mae <- mean(abs(vals_tls[valid] - vals_bp[valid]))
# [1] 0.3642269

# paired t-test
t.test(vals_tls[valid], vals_bp[valid], paired=TRUE)

# correlation 
lm_model <- lm(vals_uav[valid] ~ vals_bp[valid])
summary(lm_model)


# histogram of differences
hist(vals_diff_clean,
     breaks = 50,
     main = "Histogram of Differences (ALS - Backpack)",
     xlab = "Difference in LAI")

# density plot
plot(density(vals_diff_clean, na.rm=TRUE),
     main="Density of Differences",
     xlab="Difference in LAI")
abline(v=0, col="red", lty=2)

# scatter plot
plot(vals_uav[valid], vals_bp[valid],
     xlab="Scaled UAV LAI",
     ylab="ALS LAI",
     main="Pixel-wise Comparison")
abline(0,1,col="red")

residuals <- vals_als[valid] - vals_uav[valid]

plot(residuals, main="Residuals (ALS - UAV)",
     ylab="Difference (LAI)")
abline(h=0, col="red", lty=2)



### make values df into long format
data <- data.frame()
data <- rbind(data, vals_als)
data <- cbind(data, vals_tls)
data <- cbind(data, vals_uav)
data <- cbind(data, vals_bp)

data_long <- pivot_longer(data, cols = c("lai_als_leafR", "lai_tls_leafR", "lai_uav_leafR", "lai_bp_leafR"), names_to = "Source", values_to = "LAI")


## Viz
ggplot(data_long, aes(x = Source, y = LAI))+
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
  labs(title = "LAI Measurement Comparison", )+
  xlab("") +
  ylab("LAI") +
  theme_minimal()+
  theme(plot.title = element_text(size = 15))

# export in 6.28 6.4, cubes quadratic

#### 4. pairwise comparison

common_scale <- scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "red",
  midpoint = 0,
  limits = c(-3, 6),
  oob = scales::squish # avoids warnings from small differences
)
library(dplyr)
diff_raster_als_uav <- rename(as.data.frame(diff_raster_als_uav, xy = TRUE, na.rm = TRUE), lai_diff = lai_als_leafR)
diff_raster_als_tls <- rename(as.data.frame(diff_raster_als_tls, xy = TRUE, na.rm = TRUE), lai_diff = lai_als_leafR)
diff_raster_als_bp <- rename(as.data.frame(diff_raster_als_bp, xy = TRUE, na.rm = TRUE), lai_diff = lai_als_leafR)
diff_raster_uav_tls <- rename(as.data.frame(diff_raster_uav_tls, xy = TRUE, na.rm = TRUE), lai_diff = lai_uav_leafR )
diff_raster_uav_bp <- rename(as.data.frame(diff_raster_uav_bp, xy = TRUE, na.rm = TRUE), lai_diff = lai_uav_leafR )
diff_raster_tls_bp <- rename(as.data.frame(diff_raster_tls_bp, xy = TRUE, na.rm = TRUE), lai_diff = lai_tls_leafR)


a <- ggplot(data = diff_raster_als_uav, aes(x = x, y = y, fill = lai_diff))+
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
  annotate("text", x = 524020, y = 5537700, label = "ALS - UAV", hjust = "right")

b <- ggplot(data = diff_raster_als_tls, aes(x = x, y = y, fill = lai_diff))+
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
  annotate("text", x = 524020, y = 5537700, label = "ALS - TLS", hjust = "right")

c <- ggplot(data = diff_raster_als_bp, aes(x = x, y = y, fill = lai_diff))+
  geom_tile()+
  coord_equal() +
  ylab("Backpack") +
  xlab("ALS")+
  common_scale +
  theme_minimal()+
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        legend.position = "none")+ 
  annotate("text", x = 524020, y = 5537700, label = "ALS - Backpack", hjust = "right")

d <- ggplot(data = diff_raster_uav_tls, aes(x = x, y = y, fill = lai_diff))+
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
  annotate("text", x = 524020, y = 5537700, label = "UAV - TLS", hjust = "right")

e <- ggplot(data = diff_raster_uav_bp, aes(x = x, y = y, fill = lai_diff))+
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
  annotate("text", x = 524020, y = 5537700, label = "UAV - Backpack", hjust = "right")

f <- ggplot(data = diff_raster_tls_bp, aes(x = x, y = y, fill = lai_diff))+
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
  annotate("text", x = 524020, y = 5537700, label = "TLS - Backpack", hjust = "right")

# lay2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
# 
# grid.arrange(a, NULL, NULL, b, d, NULL, c, e, f, 
#              layout_matrix = lay2, widths = c(1,1,1), heights = c(1.01,0.94,1)
#              ,top = textGrob("Pairwise Comparison of LAI measurements", gp=gpar(fontsize =15))
# )
# export in 6.28 6.4, cubes quadratic


layout <- "
A##
BD#
CEF
"
combined <- (
  a + b + c + d + e + f +
    plot_layout(design = layout, guides = "collect") &
    theme(legend.position = "right")
)

combined +
  plot_annotation(
    title = "Pairwise Comparison of LAI measurements"
  )
