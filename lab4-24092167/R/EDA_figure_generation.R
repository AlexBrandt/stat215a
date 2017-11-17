# Library import
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(glmnet)
library(caret)
library(gbm)
library(grid)
library(fastAdaboost)
library(MASS)

# Set working directory
# setwd("~/Dropbox/STAT_215A/stat215a/lab4/")

# Simple method to load files and assign consistent
# column names
load_clouds <- function(filename) {
  tmp.df <- read.table(filename)
  names(tmp.df) <- c("y", "x",
                     "label", "NDAI", "SD", "CORR",
                     "RA.DR", "RA.CF", "RA.BF",
                     "RA.AF", "RA.AN")
  return(tmp.df)
}

# Load the images
img1 <- load_clouds("image1.txt")
img2 <- load_clouds("image2.txt")
img3 <- load_clouds("image3.txt")

# Plot the expert labels
plot(img1$x.coord, img1$y.coord, 
     col= ifelse(img1$expert == 1, "red", 
                 ifelse(img1$expert == 0, "black", "blue")))
plot(img2$x.coord, img2$y.coord, 
     col= ifelse(img2$expert == 1, "red", 
                 ifelse(img2$expert == 0, "black", "blue")))
plot(img3$x.coord, img3$y.coord, 
     col= ifelse(img3$expert == 1, "red", 
                 ifelse(img3$expert == 0, "black", "blue")))

# Helper function: https://github.com/tidyverse/ggplot2/wiki/share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

# Get the data for three images

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

# take a peek at the data from image1
head(image1)
summary(image1)

# The raw image (red band, from nadir).

ggplot(rbind(img1, img2, img3), aes(NDAI)) +
  geom_histogram(binwidth = .05) + 
  ggtitle("Distribution of NDAI Values") + 
  geom_vline(xintercept=-1) +
  geom_vline(xintercept=1)

# Plot the expert pixel-level classification
im1 <- ggplot(img1) + geom_point(aes(x = x, y = y, color = factor(label)),
                                 shape=15, size=.1) +
  scale_color_manual(name = "Expert label", breaks = c(-1, 0, 1),
                     labels = c("non-cloud", 'unlabeled', 'cloud'),
                     values=c("#56B4E9", "#999999", "#E69F00")) +
  theme_classic() +
  labs(title = "Image 1")

im2 <- ggplot(img2) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_manual(name = "Expert label", breaks = c(-1, 0, 1),
                     labels = c("non-cloud", 'unlabeled', 'cloud'),
                     values=c("#56B4E9", "#999999", "#E69F00")) +
  theme_classic() +
  labs(title = "Image 2")

im3 <- ggplot(img3) + geom_point(aes(x = x, y = y, color = factor(label))) +
  scale_color_manual(name = "Expert label", breaks = c(-1, 0, 1),
                     labels = c("non-cloud", 'unlabeled', 'cloud'),
                     values=c("#56B4E9", "#999999", "#E69F00")) +
  theme_classic() +
  labs(title = "Image 3")

# Plot the NDAI pixel-level plot
im1n <- ggplot(img1) + geom_point(aes(x = x, y = y, color = NDAI),
                                  size=0.0) +
  scale_color_gradientn(colors=rainbow(5)) +
  theme_classic() +
  labs(title = "Image 1")

im2n <- ggplot(img2) + geom_point(aes(x = x, y = y, color = NDAI),
                                  size=0.0) +
  scale_color_gradientn(colors=rainbow(5)) +
  theme_classic() +
  labs(title = "Image 2")

im3n <- ggplot(img3) + geom_point(aes(x = x, y = y, color = NDAI),
                                  size=0.0) +
  scale_color_gradientn(colors=rainbow(5)) +
  theme_classic() +
  labs(title = "Image 3")

d <- grid_arrange_shared_legend(im1n, im2n, im3n)
ggsave("ndai_labels.png", d, dpi = 300, width = 10, height = 4)

# Plot the SD pixel-level plot
im1s <- ggplot(img1) + geom_point(aes(x = x, y = y, color = SD), size=.1) +
  scale_color_gradientn(colors=rainbow(5)) +
  theme_classic() +
  labs(title = "Image 1")

im2s <- ggplot(img2) + geom_point(aes(x = x, y = y, color = SD)) +
  scale_color_gradientn(colors=rainbow(5)) +
  theme_classic() +
  labs(title = "Image 2")

im3s <- ggplot(img3) + geom_point(aes(x = x, y = y, color = SD), 
                                  shape=0, size=.01) +
  scale_color_gradientn(colors=rainbow(5)) +
  theme_classic() +
  labs(title = "Image 3")

d <- grid_arrange_shared_legend(im1, im2, im3)
ggsave("ndai_labels.png", d, dpi = 300, width = 10, height = 4)
d <- grid_arrange_shared_legend(im1n, im2n, im3n)
ggsave("sd_labels.png", d, dpi = 300, width = 10, height = 4)
d <- grid_arrange_shared_legend(im1s, im2s, im3s)
ggsave("sd_labels.png", d, dpi = 300, width = 10, height = 4)

# Merge the images afterword in a basic image editor to help save space/
# present nicely

