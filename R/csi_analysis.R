#### Import the data ####
df <- read.csv('data/CSI_data_small.csv')

#### Looking at the data ####
head(df)
dim(df)
summary(df)

#### Exploratory plots ####

# Names for the x labels
samples <- c(rep('car',9), rep('plant', 5), rep('scene', 6))

#### Ca ####
# Scatter plot
# Graphic device jpeg
jpeg('figs/scatter_ca.jpeg', res = 300, units = 'cm', height = 8, width = 8)
# The margins of the plot: bottom, left, top, right
par(mar = c(4,4,1,0.5))
plot(df$Ca, pch = 19, xaxt = 'n', # Disable the x-axis labels to use custom names
     type = 'o',
     xlab = 'Location', 
     ylab = "Ca [unknown]",
     col = as.factor(df$Sample))

axis(1, at = 1:nrow(df), labels = samples) # Custom axis labels
legend('topleft', c('car','plant','scene'), pch = c(19,19,19),
       col = c('black','red','green'))
dev.off()

# Boxplots
jpeg('figs/boxplot_ca.jpeg', res = 300, units = 'cm', height = 8, width = 8)
par(mar = c(4,2,1,0.5)) # Modified the left margin to make it look better
plot(df$Sample, df$Ca, xlab = 'Location', ylab = '') # No y-label because repetitive
dev.off()


#### Zn ####
# Scatter plot
# Graphic device jpeg
jpeg('figs/scatter_zn.jpeg', res = 300, units = 'cm', height = 8, width = 8)
# The margins of the plot: bottom, left, top, right
par(mar = c(4,4,1,0.5))
plot(df$Zn, pch = 19, xaxt = 'n', # Disable the x-axis labels to use custom names
     type = 'o',
     xlab = 'Location', 
     ylab = "Zn [unknown]",
     col = as.factor(df$Sample))

axis(1, at = 1:nrow(df), labels = samples) # Custom axis labels
legend('topleft', c('car','plant','scene'), pch = c(19,19,19),
       col = c('black','red','green'))
dev.off()

# Boxplots
jpeg('figs/boxplot_zn.jpeg', res = 300, units = 'cm', height = 8, width = 8)
par(mar = c(4,2,1,0.5)) # Modified the left margin to make it look better
plot(df$Sample, df$Zn, xlab = 'Location', ylab = '') # No y-label because repetitive
dev.off()

#### Cs ####
# Scatter plot
# Graphic device jpeg
jpeg('figs/scatter_cs.jpeg', res = 300, units = 'cm', height = 8, width = 8)
# The margins of the plot: bottom, left, top, right
par(mar = c(4,4,1,0.5))
plot(df$Cs, pch = 19, xaxt = 'n', # Disable the x-axis labels to use custom names
     type = 'o',
     xlab = 'Location', 
     ylab = "Cs [unknown]",
     col = as.factor(df$Sample))

axis(1, at = 1:nrow(df), labels = samples) # Custom axis labels
legend('bottomleft', c('car','plant','scene'), pch = c(19,19,19),
       col = c('black','red','green'))
dev.off()

# Boxplots
jpeg('figs/boxplot_cs.jpeg', res = 300, units = 'cm', height = 8, width = 8)
par(mar = c(4,2,1,0.5)) # Modified the left margin to make it look better
plot(df$Sample, df$Cs, xlab = 'Location', ylab = '') # No y-label because repetitive
dev.off()





#### ANOVA for Ca ####
df_aov_ca <- df[,c(1,5)]

result_ca <- aov(Ca ~ Sample, data = df_aov_ca)

summary(result_ca)

#### ANOVA for Zn ####
df_aov_zn <- df[,c(1,8)]

result_zn <- aov(Zn ~ Sample, data = df_aov_zn)

summary(result_zn)

#### ANOVA for Mg ####
df_aov_cs <- df[,c(1,12)]

result_cs <- aov(Cs ~ Sample, data = df_aov_cs)

summary(result_cs)




#### Cluster analysis ####
# Prepare the data for cluster analysis
df_temp <- df[,-1] # Remove the first column to discard the non-numeric column
df_temp <- data.matrix(df_temp) # Change to matrix format for heatmap analysis
row.names(df_temp) <- paste0(as.character(df[,1]), 1:nrow(df)) # Add the locations to the plot

jpeg('figs/heatmap_csi.jpeg', res = 300, units = 'cm', height = 16, width = 16)
# The margins of the plot: bottom, left, top, right
par(mar = c(2,2,2,2))
heatmap(df_temp)
dev.off()


## Cluster analysis using the complete method
# Linkage method is complete
jpeg('figs/hc_complete.jpeg', res = 300, units = 'cm', height = 8, width = 8)
# The margins of the plot: bottom, left, top, right
par(mar = c(2,2,2,2))
hc <- hclust(dist(df_temp), 
             method = 'complete')
plot(hc, labels = df$Sample, main = 'Complete', hang = -1)
dev.off()


# Linkage method is average
jpeg('figs/hc_average.jpeg', res = 300, units = 'cm', height = 8, width = 8)
# The margins of the plot: bottom, left, top, right
par(mar = c(2,2,2,2))
hc <- hclust(dist(df_temp), 
             method = 'average')
plot(hc, labels = df$Sample, main = 'Average', hang = -1)
dev.off()


plot(hc, labels = df$Sample)

#Optional: K-means clustering
# k_means <- kmeans(df_temp, 3, nstart = 20)
# k_means
# table(k_means$cluster, df$Sample)
#### PCA ####
#install.packages('car') # Only one time install
library(car) # load the package

csi.pca <- prcomp(df[,-1], # Remove the first column
                  center = TRUE, # Center the data, similar to scaling
                  scale. = TRUE) # Need to be scaled because of magnitude diff.

jpeg('figs/biplot_csi.jpeg', res = 300, units = 'cm', height = 16, width = 16)
# The margins of the plot: bottom, left, top, right
par(mar = c(4,4,1,1))
plot(csi.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20,        # The marker 
     col = df$Sample, # The color
     xlim = c(-5,10), ylim = c(-2.5,6)) # The limits of the axes
dataEllipse(csi.pca$x[,1],      # PC1
            csi.pca$x[,2],      # PC2
            groups = df$Sample, # Groups
            lwd = 1,            # line width
            group.labels = c('car','plant','scene'), 
            plot.points = FALSE, # Do not want to redraw points on top of old points
            levels = 0.5,      # 50% confidence level, similar to what is set in ggbiplot
            add = TRUE,
            fill=TRUE, 
            fill.alpha = 0.02,
            col = c('black','red','green'))

legend('bottomright', c('Car','Plant','Scene'), pch = c(20,20,20),
       col = c('black', 'red', 'green'))
dev.off()




