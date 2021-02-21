# ----------------
# REFERENCES
# ----------------

# interpret silhouette width
# # https://stats.stackexchange.com/questions/10540/how-to-interpret-mean-of-silhouette-plot

# https://towardsdatascience.com/let-us-understand-the-correlation-matrix-and-covariance-matrix-d42e6b643c22#:~:text=between%20two%20variables.-,%E2%80%9CCovariance%E2%80%9D%20indicates%20the%20direction%20of%20the%20linear%20relationship%20between%20variables,a%20function%20of%20the%20covariance.

# david robinson's cautionary analysis 
# http://varianceexplained.org/r/kmeans-free-lunch/#:~:text=k%2Dmeans%20assume%20the%20variance,then%20k%2Dmeans%20will%20fail.
# https://stats.stackexchange.com/questions/326685/is-it-true-that-k-means-has-an-assumption-each-cluster-has-a-roughly-equal-numb
# https://stats.stackexchange.com/questions/133656/how-to-understand-the-drawbacks-of-k-means
# https://stats.stackexchange.com/questions/11691/how-to-tell-if-data-is-clustered-enough-for-clustering-algorithms-to-produce-m

# k means non-normal distribution
# https://stats.stackexchange.com/questions/325676/should-k-means-only-be-applied-if-the-variables-are-normally-distributed

# k means general interpretation 
# https://stats.stackexchange.com/questions/149254/cluster-analysis-effectiveness-of-k-means-results-and-alternative-methods

# k means with many zero values (i.e. ruck contests)
# https://stackoverflow.com/questions/18063087/is-k-means-for-clustering-data-with-many-zero-values

    # sparse vector is a vector that has a large number of zeros 
    # k means will produce results which are not sensible 

# principal components first, then k-means analysis second
# https://stats.stackexchange.com/questions/183236/what-is-the-relation-between-k-means-clustering-and-pca
# https://www.r-bloggers.com/2014/06/pca-and-k-means-clustering-of-delta-aircraft/

# https://stackoverflow.com/questions/55456222/changing-ellipse-line-type-in-fviz-cluster

# which distance measure should you use?
# https://stats.stackexchange.com/questions/80377/which-distance-to-use-e-g-manhattan-euclidean-bray-curtis-etc

# General guide 
# https://uc-r.github.io/kmeans_clustering
# https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967
# https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff
# https://towardsdatascience.com/understanding-k-means-clustering-in-machine-learning-6a6e67336aa1


# clustering with categorical data
# https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

# when to scale your data
# https://stats.stackexchange.com/questions/89809/is-it-important-to-scale-data-before-clustering
# https://stackoverflow.com/questions/15777201/why-vector-normalization-can-improve-the-accuracy-of-clustering-and-classificati/15779107#15779107

      # K-means clustering is "isotropic" in all directions of space and therefore 
      # tends to produce more or less round (rather than elongated) clusters. 
      # In this situation leaving variances unequal is equivalent to putting more 
      # weight on variables with smaller variance.

# is my data not amenable to clustering? i.e. one homogeneous cluster
# https://www.quora.com/Machine-Learning/How-can-I-detect-if-my-dataset-is-clustered-or-unclustered-i-e-forming-one-single-cluster/answer/Franck-Dernoncourt

      # use the gap statistic 

# k-means only uses Euclidean distance 
# https://stats.stackexchange.com/questions/81481/why-does-k-means-clustering-algorithm-use-only-euclidean-distance-metric

# evaluating success of k-means
# http://www.socr.umich.edu/people/dinov/courses/DSPA_notes/12_kMeans_Clustering.html

# Feature importance
# https://cran.r-project.org/web/packages/FeatureImpCluster/readme/README.html
# https://stats.stackexchange.com/questions/77689/estimating-the-most-important-features-in-a-k-means-cluster-partition

# Measuring effectiveness of a cluster result
# https://stats.stackexchange.com/questions/76093/how-to-determine-which-method-is-the-most-valid-reasonable-clustering-results/76095#76095

      # To measure the quality of clustering results, there are two kinds of validity indices: 
      # external indices and internal indices.
      # An external index is a measure of agreement between two partitions 
      # where the first partition is the a priori known clustering structure, 
      # and the second results from the clustering procedure (Dudoit et al., 2002).

      # Internal indices are used to measure the goodness of a clustering structure 
      # without external information (Tseng et al., 2005).

      # For external indices, we evaluate the results of a clustering algorithm based 
      # on a known cluster structure of a data set (or cluster labels).

      # For internal indices, we evaluate the results using quantities and features 
      # inherent in the data set. The optimal number of clusters is usually determined 
      # based on an internal validity index.

# impact of dud features
# https://www.quora.com/Cluster-Analysis/Can-a-useless-feature-negatively-impact-the-clustering/answer/Franck-Dernoncourt

      # It should be noted that a variable not containing any relevant information 
      # (say, the telephone number of each person) is worse than useless, 
      # because it will make the clustering less apparent. 
      # The occurrence of several such “trash variables” will kill the whole clustering 
      # because they yield a lot of random terms in the distances,
      # thereby hiding the useful information provided by the other variables. 
      # Therefore, such noninformative variables must be given a zero weight in the 
      # analysis, which amounts to deleting them. 
      # A recent discussion of variable selection can be found in Fowlkes et al. (1988). 
      # In general, the selection of “good” variables is a nontrivial task and 
      # may involve quite some trial and error 
      # (in addition to subject-matter knowledge and common sense). 
      # In this respect, cluster analysis may be considered an exploratory technique

# ----------------
# DEPENDENCIES
# ----------------

# Set correct package library path 
# .libPaths(c("C:/NotBackedUp/Temp/software/R/3.4", .libPaths()))

# Data cleaning 
library(snakecase)
library(tidyr)
library(dplyr)
library(readr)
library(reshape)
library(knitr)

# Data visualisation
library(corrplot)
library(taucharts) # not available for ANZ version of R 
library(purrr) # required for map_dbl
library(GGally)
library(plotly)
library(factoextra)

# Data analysis 
library(ClustOfVar)
library(cluster)

# ----------------
# READ IN FILE 
# ----------------

# .csv from PC directory 
players <- read.csv("afl_player_statistics_pf.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE)

# ----------------
# CLEAN DATA 
# ----------------

# check df
str(players)

# convert games played to numeric
players$Games <- as.numeric(players$Games)

# filter to only players who have played at least an entire season worth of games 
players <- players %>% 
  dplyr::filter(Games >= 22)

# Aggregate
players.agg <- players %>% 
  group_by(PositionType) %>%
  summarise_at(.vars = colnames(.)[5:48], mean) # calculate mean for all stats 
# Ensure table is dataframe
players.agg <- as.data.frame(players.agg)
# round numeric columns to 2 decimal places 
players.agg <- players.agg %>% mutate_if(is.numeric, ~round(., 2))

# visualise ruck contests

# calculate means for plot
library(plyr)
means_ruck <- mean(players$ruck_contests, na.rm = TRUE)  

# code for plot 
ruck <- players %>% 
  ggplot(aes(ruck_contests)) + 
  geom_density(alpha = 0.2) +
  # geom_vline(data = means_ruck, 
  #            aes(xintercept = means_ruck), 
  #            linetype="dashed") +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Density of cases") +
  scale_color_manual(values=c("#C8423B")) +
  scale_fill_manual(values=c("#C8423B")) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title.x = element_text(vjust = -1, size = 10, face = "bold"),
        axis.title.y = element_text(vjust = 2, size = 10, face = "bold"),
        plot.title = element_text(vjust = 2, size = 10, face = "bold", hjust = 0.5)) + # move title away from axis 
  labs(color = "Survived", fill = "Survived") +
  ggtitle("Density plot of survival status x age")



# ----------------
# MODEL DATA 
# ----------------

set.seed(123)

# scale all of our variables to add into k-means model 
all_scaled <- as.data.frame(scale(players[,5:48], 
                                  center = TRUE, 
                                  scale = TRUE))

# raw data - only variables to include in model
all_numeric <- dplyr::select_if(players[,5:48], is.numeric)


M1 <- all_scaled %>%  
  select(
    # these ruck-related stats distinguish ruckmen entirely from the group
    # 90.6% variance explained with just these stats
    hitouts,
    hitouts_to_advantage,
    ruck_contests,
    score_launches,
    # this midfield related fields produces some separation of midfielders, reducing overall variance
    clearances,
    contested_possessions,
    uncontested_possessions,
    centre_clearances,
    stoppage_clearances,
    # forwards
    goals,
    tackles_inside_fifty,
    f50_ground_ball_gets,
    # defenders
    rebounds,
    # one_percenters, keep this out, makes too similar to rucks
    # intercepts, # really improves variance, but muddies model
    contest_def_one_on_ones,
    disposal_efficiency_percentage
  )

# ----------------
# PRINCIPAL COMPONENTS
# ----------------

# https://builtin.com/data-science/step-step-explanation-principal-component-analysis

# Principal components are new variables that are constructed as linear combinations 
# or mixtures of the initial variables. 
# These combinations are done in such a way that the new variables 
# (i.e., principal components) are uncorrelated and most of the information 
# within the initial variables is squeezed or compressed into the first components. 
# So, the idea is 10-dimensional data gives you 10 principal components, 
# but PCA tries to put maximum possible information in the first component, 
# then maximum remaining information in the second and so on, 
# until having something like shown in the scree plot below.

# https://www.r-bloggers.com/2014/06/pca-and-k-means-clustering-of-delta-aircraft/

# verify by plotting variance of columns

  # mar <- par()$mar
  # par(mar=mar+c(0,5,0,0))
  # barplot(sapply(all_numeric, var), horiz=T, las=1, cex.names=0.8)
  # barplot(sapply(all_numeric, var), horiz=T, las=1, cex.names=0.8, log='x')
  # par(mar=mar)


# Verify variance is uniform
  # all_numeric_scale <- data.frame(scale(all_numeric))
  # plot(sapply(all_scaled, var))

# Proceed with principal components
pc <- prcomp(M1)

# https://stat.ethz.ch/pipermail/r-help/2005-August/076610.html
pc$sdev
ev <- pc$sdev^2 #eigenvalues
ev

pc$x

plot(pc)
plot(pc, type='l')
fviz_eig(pc)

pc.df <- as.data.frame(pc)

fviz_pca_var(pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# ----------------
# PCA PLOT 
# ----------------

library(ggfortify)
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
p1 <- 
autoplot(pc, data = M1,
         loadings = TRUE, 
         loadings.colour = '#36332e',
         loadings.size = 0.5,
         loadings.label.colour = '#36332e',
         loadings.label = TRUE,
         loadings.label.size = 2,
         loadings.label.repel = T
         ) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(colour = "#E7B573") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.title.x = element_text(colour = "#4E4F4E",
                                    size = 9),
        axis.title.y = element_text(colour = "#4E4F4E",
                                    size = 9),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        # plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0)))
p1$layers[[2]]$aes_params$size <- 0.1 # change line thickness 
p1

# https://www.datacamp.com/community/tutorials/pca-analysis-r
# The relationship (correlation or anticorrelation, etc) between the initial 
# variables and the principal components
pc.df <- data.frame(pc$rotation)

# library(ggplot2)
# theme_set(theme_bw())
# # same plot in ggplot2
# pc.df.2 <- data.frame(pc$x[, 1:2]) # we only need the first two principal components
# ggplot(pc.df.2, aes(y = PC1, x = PC2)) + geom_point(col = 'tomato2')

# 4 components is both 'elbow' and explains >89% variance
summary(pc) 

# First for principal components
# coordinates for each player for first 3 principal components into dataframe
comp <- data.frame(pc$x[,1:3]) # first 3 principal components in a dataframe

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

# ----------------
# k MEANS
# ----------------
# ELBOW METHOD 
# Determine number of clusters
wss <- (nrow(M1)-1) * sum(apply(M1, 2, var))
for (i in 1:15) wss[i] <- sum(kmeans(M1,
                                     centers = i)$withinss)

wss.df <- as.data.frame(wss)

wss.df %>% 
  ggplot(aes(y=wss, x=1:15)) +
  geom_line(colour="#E4796A") +
  geom_point(colour="#E4796A") +
  scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
    labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  scale_y_continuous(
    breaks=c(0,2000,4000,6000,8000,1000),
    labels=c(0,2000,4000,6000,8000,1000)) +
  labs(x = "within cluster sum of squares",
       y = "k",
       title = "Elbow plot",
       subtitle = "Depicting within cluster sum of squares against number of clusters") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.title.x = element_text(colour = "#4E4F4E",
                                    size = 9,
                                    face = "bold",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "#4E4F4E",
                                    size = 9,
                                    face = "bold",
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        plot.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0)))

plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

# SILHOUETTE METHOD 
# function to compute average silhouette for k clusters
M1.sil <- function(k) 
{
  km.res <- kmeans(M1, 
                   centers = k, 
                   nstart = 25)
  ss <- silhouette(km.res$cluster, 
                   dist(M1))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
M1_sil_values <- map_dbl(k.values, M1.sil)

plot(k.values, M1_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# compute gap statistic
gap_stat <- clusGap(M1, 
                    FUN = kmeans, 
                    nstart = 25,
                    K.max = 10, 
                    B = 50, # integer, number of Monte Carlo (“bootstrap”) samples.
                    d.power = 2) # preventing only a single cluster being the preferred selection
# Print the result
print(gap_stat, 
      method = "firstmax")

# Investigation of issue
k <- maxSE(gap_stat$Tab[, "gap"], 
           gap_stat$Tab[, "SE.sim"], 
           method="Tibs2001SEmax")
fit <- kmeans(M1, k)

# Plot the results.
pch <- ifelse(fit$cluster == 1,24,16); col <- ifelse(fit$cluster == 1,"Red", "Black")
plot(M1, 
     asp = 1, 
     main = title, 
     pch = pch, 
     col = col)

plot(gap_stat, 
     main = paste("Gap stats"))
abline(v = k, # vertical line for k 
       lty = 3, # line type
       lwd = 2, # line width
       col = "Blue")

# Optimal == 5


# From scree plot elbow occurs at k = 4
# Apply k-means with k = 4
k <- kmeans(comp, 4, 
            nstart = 25, 
            iter.max=1000)
k

# library(RColorBrewer)
# library(scales)
# palette(alpha(brewer.pal(9,'Set1'), 0.5))
# plot(comp, col=k$clust, pch=16)

# ----------------
# K MEANS PLOT 1
# ----------------

# join cluster groupings back to data
M1[,"Cluster"] <- k$clust
# coerce to factor 
M1$Cluster <- as.factor(M1$Cluster)
# plot now with k means clusters

colour_theme <- c("#E4796A", "#E7B573", "#f5a253", "#638DCB")
d$layers[[2]]$aes_params$size <- 0.5

# https://stackoverflow.com/questions/52648355/plotting-pca-biplot-with-autoplot-modify-arrow-thickness
p <-
autoplot(pc, 
         data = M1,
         colour = "Cluster", # need to join k$clust back to M1
         loadings = TRUE, 
         loadings.colour = '#666257',
         loadings.size = 0.5,
         loadings.label.colour = '#666257',
         loadings.label = TRUE,
         loadings.label.size = 2,
         loadings.label.repel = T
         ) +
  scale_colour_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.title.x = element_text(colour = "#4E4F4E",
                                    size = 9),
        axis.title.y = element_text(colour = "#4E4F4E",
                                    size = 9),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        # plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0)))
p$layers[[2]]$aes_params$size <- 0.1 # change line thickness 
p

# ----------------
# K MEANS PLOT 2
# ----------------

plot.data <- fviz_cluster(k, data = M1) # save to access $data
data <- plot.data$data # this is all you need
# calculate the convex hull using chull(), for each cluster
hull_data <- data %>%
  group_by(cluster) %>%
  slice(chull(x, y))
# plot: you can now customize this by using ggplot sintax
ggplot(data, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_polygon(data = hull_data, 
               alpha = 0.5, 
               aes(fill = cluster, linetype = cluster)) +
  scale_colour_manual(values = colour_theme) +
  scale_fill_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01) +
  labs(title = "Cluster Plot",
       subtitle = "Depicting of k = 4 for first two principal components & variable loadings") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.title.x = element_text(colour = "#4E4F4E",
                                    size = 9),
        axis.title.y = element_text(colour = "#4E4F4E",
                                    size = 9),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        # plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0)))


# fviz_cluster(k, data = M1,
#              palette = c("#2E9FDF", "#00AFBB", "#E7B800","#2E9FDF"), 
#              geom = "point",
#              ellipse.type = "convex", 
#              ggtheme = theme_bw()
# )

# ----------------
# K MEANS PLOT 3
# ----------------

# join cluster groupings back to data
M1[,"Position"] <- players$PositionType
# coerce to factor 
M1$Position <- as.factor(M1$Position)
# plot now with k means clusters
autoplot(pc, 
         data = M1,
         colour = "Position", # need to join k$clust back to M1
         loadings = TRUE, 
         loadings.colour = 'blue',
         loadings.label = TRUE, 
         loadings.label.size = 2,
         loadings.label.repel= T)

# ggplot2 visualisation 
# join cluster groupings back to data
M1[,"Player"] <- players$Player_Name

M1.P <- fviz_cluster(k, 
                  data = M1[1:15]) # save to access $data
data <- M1.P$data # this is all you need
# calculate the convex hull using chull(), for each cluster
hull_data <- data %>%
  group_by(cluster) %>%
  slice(chull(x, y))
# colour theme
colour_theme <- c("#E4796A", "#E7B573", "#B0AA96", "#638DCB")
# plot: you can now customize this by using ggplot sintax
ggplot(data, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_polygon(data = hull_data, 
               alpha = 0.5, 
               aes(fill = cluster, linetype = cluster)) +
  scale_color_manual(values = colour_theme) +
  scale_fill_manual(values = colour_theme) +
  
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        plot.background = element_rect(fill = "#fcf9f0"),
        panel.background = element_blank(),
        legend.background = element_rect(fill = "#fcf9f0"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right







k # 78.5% 

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))
# First cluster
row.names(data[k$clust==clust[1],])
# Second Cluster
row.names(data[k$clust==clust[2],])
# Third Cluster
row.names(data[k$clust==clust[3],])

# Compare accommodation by cluster in boxplot
boxplot(M1$ruck_contests ~ k$cluster,
        xlab='Cluster', ylab='Accommodation',
        main='ruck_contests by Cluster')

boxplot(M1$clearances ~ k$cluster,
        xlab='Cluster', ylab='Accommodation',
        main='clearances by Cluster')


# ----------------
# CORR PLOTS 
# ----------------

# create matrix of correlations
M <- cor(M1)
# round data to 2 decimal places
M <- round(M, 2)

# Plot with colour 
CM1 <- 
  corrplot(M,
           method = "square", 
           type = "upper",
           tl.col= "black", 
           tl.cex = 0.6, # Text label color and rotation
           cl.cex = 0.6 # Correlation label color and rotation
  )
# Plot with labels 
CM2 <- 
  corrplot(M,
           method = "number", 
           type = "upper",
           tl.col="black", 
           # tl.srt=45, 
           tl.cex = 0.6, # Text label color and rotation
           cl.cex = 0.6, # Correlation label color and rotation
           number.cex = .6
  )

set.seed(123)

# ----------------
# VISUALISE  
# ----------------

distance <- get_dist(players_scaled2)

# Complex, & awesome looking plot that I don't quite know how to interpret

  # This starts to illustrate which states have large dissimilarities (red) versus 
  # those that appear to be fairly similar (teal).
  
  # get_dist: for computing a distance matrix between the rows of a data matrix. 
  # The default distance computed is the Euclidean

fviz_dist(distance, 
          gradient = list(low = "#00AFBB", 
                          mid = "white", 
                          high = "#FC4E07"))

# ----------------
# DETERMINE CLUSTER #
# ----------------

# Elbow Method
  # the basic idea behind cluster partitioning methods, such as k-means clustering, 
  # is to define clusters such that the total intra-cluster variation 
  # (known as total within-cluster variation or total within-cluster sum of square) 
  # is minimized

  # The total within-cluster sum of square (wss) measures the compactness of the clustering 
  # and we want it to be as small as possible. 

  # Thus, we can use the following algorithm to define the optimal clusters:

    # 1. Compute clustering algorithm (e.g., k-means clustering) for different values of k.
      # For instance, by varying k from 1 to 10 clusters
    # 2. For each k, calculate the total within-cluster sum of square (wss)
    # 3. Plot the curve of wss according to the number of clusters k.
    # 4. The location of a bend (knee) in the plot is generally considered as an indicator of
      # the appropriate number of clusters.

# Plot "Elbow Method"

# function to compute total within-cluster sum of square 
wss <- function(k) 
{
  kmeans(players_scaled2, k, nstart = 10)$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type = "b", 
     pch = 19, # dots
     frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# Looking at elbow plot, optimal cluster # == 3

# Average Silhouette Method
  # In short, the average silhouette approach measures the quality of a clustering. 
  # That is, it determines how well each object lies within its cluster. 
  # A high average silhouette width indicates good clustering. 

  # The average silhouette method computes the average silhouette of observations for different values of k. 
  # The optimal number of clusters k is the one that maximizes the average silhouette 
  # over a range of possible values for k.2

  # We can use the silhouette function in the cluster package to compute the average silhouette width. 
  # The following code computes this approach for 1-15 clusters. 

# function to compute average silhouette for k clusters
avg_sil <- function(k) 
{
  km.res <- kmeans(players_scaled2, 
                   centers = k, 
                   nstart = 25)
  ss <- silhouette(km.res$cluster, 
                   dist(players_scaled2))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# Clearly 3 is the optimal number of clusters 

# Gap Statistic Method
  # The gap statistic has been published by R. Tibshirani, G. Walther, and T. Hastie (Standford University, 2001). 
  # The approach can be applied to any clustering method (i.e. K-means clustering, hierarchical clustering). 
 
 # The gap statistic compares the total intracluster variation for different values of k 
  # with their expected values under null reference distribution of the data 
  # (i.e. a distribution with no obvious clustering). 
  # The reference dataset is generated using Monte Carlo simulations of the sampling process

# compute gap statistic
gap_stat <- clusGap(players_scaled2, 
                    FUN = kmeans, 
                    nstart = 25,
                    K.max = 10, 
                    B = 50, # integer, number of Monte Carlo (“bootstrap”) samples.
                    d.power = 2) # preventing only a single cluster being the preferred selection
# Print the result
print(gap_stat, 
      method = "firstmax")

# logW   E.logW      gap     SE.sim
# [1,] 8.020106 9.153953 1.133847 0.02141738
# [2,] 7.475990 8.780787 1.304797 0.02508783
# [3,] 6.904827 8.506184 1.601357 0.01576409
# [4,] 6.644467 8.289482 1.645015 0.01809680
# [5,] 6.520366 8.205012 1.684646 0.01874720
# [6,] 6.421560 8.132868 1.711308 0.01658225
# [7,] 6.332389 8.074128 1.741739 0.01593255
# [8,] 6.283606 8.022555 1.738949 0.01531380
# [9,] 6.210026 7.979049 1.769023 0.01501867
# [10,] 6.131487 7.941745 1.810258 0.01513251

# ****** NOTE - Gap Statistic continues to increase!
  # selecting maximum cluster # is not parsimonious 

# https://stats.stackexchange.com/questions/95290/how-should-i-interpret-gap-statistic

  # in many real-world datasets, the clusters are not as well-defined, 
  # and we want to be able to balance maximizing the gap statistic 
  # with parsimony of the model. Case in point: OP's first image. 
  # If we're maximizing the gap statistic alone, 
  # then we should choose the model with 30 (or even more!) clusters. 
  # Assuming that that plot is just going to continue to increase, 
  # of course, the results are less useful. 

  # So Tibshirani suggests the 1-standard-error method (i.e. equivalent to elbow of plot)

# Another issue is if k = 1 returns the highest gap statistic 

# EXAMPLE

  # RAW DATA 
  # logW   E.logW       gap      SE.sim
  # [1,] 11.49051 12.10033 0.6098169 0.017811513 *** OPTIMAL GAP == 1 
  # [2,] 11.02448 11.47285 0.4483720 0.015241535
  # [3,] 10.75075 11.14025 0.3894970 0.012054764
  # [4,] 10.56459 10.93300 0.3684069 0.012821011
  # [5,] 10.39440 10.79567 0.4012696 0.009944292
  # [6,] 10.30568 10.69603 0.3903530 0.008902703
  # [7,] 10.20430 10.62220 0.4178987 0.007334922
  # [8,] 10.11937 10.56748 0.4481090 0.007684777
  # [9,] 10.06688 10.52331 0.4564284 0.007468296
  # [10,] 10.02640 10.48887 0.4624717 0.006771964

# Reasons this may occur
  # Huge amount of within-group variance -- aka noise 
  # Significant differences in scale, especially metres gained 
  # compared to a null distribution, it seems likely that there is 
  # NOT the presence of legitimate clusters in this data
  # corroborated by response to this question 
  # https://stats.stackexchange.com/questions/51556/choosing-clusters-for-k-means-the-1-cluster-case

# d.power
  # a positive integer specifying the power 
  # which is applied to the euclidean distances (dist) 
  # before they are summed up 
    # d.power = 1, corresponds to the “historical” R implementation, 
    # d.power = 2 corresponds to what Tibshirani et al had proposed. 

# Investigation of issue
k <- maxSE(gap_stat$Tab[, "gap"], 
           gap_stat$Tab[, "SE.sim"], 
           method="Tibs2001SEmax")
fit <- kmeans(players_scaled2, k)

# Plot the results.
pch <- ifelse(fit$cluster == 1,24,16); col <- ifelse(fit$cluster == 1,"Red", "Black")
plot(players_scaled2, 
     asp = 1, 
     main = title, 
     pch = pch, 
     col = col)

plot(gap_stat, 
     main = paste("Gap stats"))
abline(v = k, # vertical line for k 
       lty = 3, # line type
       lwd = 2, # line width
       col = "Blue")

# Optimal == 7
fviz_gap_stat(gap_stat)


# ----------------
# K-MEANS USING DAISY 
# ----------------

# Daisy 
# Dissimilarity Matrix Calculation
  # Compute all the pairwise dissimilarities (distances) between observations in the data set. 
  # The original variables may be of mixed types. 
  # In that case, or whenever metric = "gower" is set, 
  # a generalization of Gower's formula is used, see ‘Details’ below.

# The metrics used for each data type are described below:
  # quantitative (interval): range-normalized Manhattan distance
  # ordinal: variable is first ranked, then Manhattan distance is used with a special adjustment for ties
  # nominal: variables of k categories are first converted into k binary columns and then the Dice coefficient is used

# ----------------
# GOWERS DISTANCE
# ----------------

# Gower’s Distance can be used to measure how different two records are. 
# The records may contain combination of logical, categorical, numerical or text data. 
# The distance is always a number between 0 (identical) and 1 (maximally dissimilar). 

# Create Dissimilarity Matrix 
d <- 
  players_scaled2 %>% 
  daisy(metric = "gower") # gower used when some metrics are non-numeric

# fit model into 4 clusters 
kfit <- kmeans(d, 2)

# The output of kmeans is a list with several bits of information. The most important being:
  
  # cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
  # centers: A matrix of cluster centers.
  # totss: The total sum of squares.
  # withinss: Vector of within-cluster sum of squares, one component per cluster.
  # tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
  # betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
  # size: The number of points in each cluster.

# this will print out cluster mean for each observation 
kfit2

# ---------
# Visualise
# ---------

# Clusters 
clusplot(as.matrix(d), 
         kfit$cluster, 
         color = T, 
         shade = F, 
         labels = 1, 
         lines = 0,
         cex = 0.7,
         cex.txt = 0.8,
         cex.axis = 0.8,
         cex.lab = 0.8,
         cex.main = 1,
         lwd = 0.8,
         main = '2D PCA-plot of k-means clustering using Gowers Distance')

# Clusters with facto-extra 
fviz_cluster(kfit, 
             data = players_scaled2)

# join back groupings
kfit$cluster
players_scaled[,"Cluster"] <- kfit$cluster

# summarizes statistics for groupings 
player_gowers <- players_scaled2 %>% 
  group_by(Cluster) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1)

# ----------------
# EUCLIDEAN DISTANCE
# ----------------

# The Euclidean distance is appropriate for continuous numerical variables 
# Use to reflect absolute distances. 
# Distance takes into account every variable and doesn’t remove redundancies, 
# so if I had three variables that explain the same (are correlated), I would weight this effect by three

d2 <- 
  players_scaled2 %>% 
  daisy(metric = "euclidean") 

# fit model into 3 clusters 
kfit2 <- kmeans(d2, 3)

# Investigate output 
str(kfit2)

# ---------
# Visualise
# ---------

# visualise Clusters 
clusplot(as.matrix(d2), 
         kfit2$cluster, 
         color = T, 
         shade = F, 
         labels = 1, 
         lines = 0,
         cex = 0.7,
         cex.txt = 0.8,
         cex.axis = 0.8,
         cex.lab = 0.8,
         cex.main = 1,
         lwd = 0.8,
         main = '2D PCA-plot of k-means clustering using Euclidean Distance')

# Clusters with facto-extra 
fviz_cluster(kfit2, 
             data = players_scaled2)

# join back groupings
kfit2$cluster
players_numeric[,"Cluster2"] <- kfit2$cluster

# summarises statistics for groupings 
playergroup2 <- players_numeric %>% 
  group_by(Cluster2) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1)

# ----------------
# MANHATTAN DISTANCE
# ----------------

# The use of the Manhattan distance is advised in those situations where for example 
  # a difference of 1 in the first variable,and of 3 in the second variable is the same as 
  # a difference of 2 in the first variable and of 2 in the second

d3 <- 
  players_scaled2 %>% 
  daisy(metric = "manhattan",
        stand = FALSE) # scaled prior to analysis 


# fit model into 3 clusters 
kfit3 <- kmeans(d3, 
                centers = 3)
kfit3 # 78.0%

# ---------
# Visualise
# ---------

# visualise 3 clusters
clusplot(as.matrix(d3), 
         kfit3$cluster, 
         color = T, 
         shade = F, 
         labels = 1, 
         lines = 0,
         cex = 0.7,
         cex.txt = 0.8,
         cex.axis = 0.8,
         cex.lab = 0.8,
         cex.main = 1,
         lwd = 0.8,
         main = '2D PCA-plot of k-means clustering using Manhattan Distance')

# Clusters with facto-extra 
fviz_cluster(kfit3, 
             data = players_scaled2)

# We can plot clusters against specific variables to spot-check 
players_scaled2 %>%
  as_tibble() %>%
  mutate(cluster = kfit3$cluster,
         player = players2$Player_Name) %>%
  ggplot(aes(clearances, 
             contested_possessions, 
             color = factor(cluster), 
             label = player)) +
  geom_text()

# join back groupings
kfit3$cluster
players_numeric2[,"Cluster_Manhattan"] <- kfit3$cluster

# join back groupings
players[,"Cluster_Manhattan"] <- kfit3$cluster

# summarises statistics for groupings 
PlayerGroup_Manhattan <- players_numeric2 %>% 
  group_by(Cluster_Manhattan) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1)



final <- kmeans(players_scaled2, 
                3, 
                nstart = 25)
print(final)




















# ----------------
# K - MEANS WITH SILHOUETTE 
# ----------------
# https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967

# As the initial centroids are defined randomly,
# we define a seed for purposes of reprodutability
set.seed(123)

# The nstart parameter indicates that we want the algorithm to be executed 20 times.
# This number is not the number of iterations, it is like calling the function 20 times and then
# the execution with lower variance within the groups will be selected as the final result.
kmeans(players_scaled2, 
       centers = 3, 
       nstart = 20)

# The kmeans() function outputs the results of the clustering. 
# We can see the centroid vectors (cluster means), 
# the group in which each observation was allocated (clustering vector) and a percentage (78.2%) 
# that represents the compactness of the clustering, that is, how similar are the members within the same group. 
# If all the observations within a group were in the same exact point in the n-dimensional space, 
# then we would achieve 100% of compactness.

# Plots a chart showing the sum of squares within a group for each execution of the kmeans algorithm. 
# In each execution the number of the initial groups increases by one up to the maximum number of centers passed as argument.
#
# @param data The dataframe to perform the kmeans 
# @param nc The maximum number of initial centers
#
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(players_scaled2, nc = 20)

# Run again with k = 4, based upon scree plot 
set.seed(123)
clustering <- kmeans(players_scaled2, 
                     centers = 5, 
                     nstart = 20)
clustering


# replot this with ggplot2
# identify "mean" value for each cluster (aka centroid) - plot this with high transparency for other values 
# https://stackoverflow.com/questions/34308109/controlling-alpha-in-ggparcoord-from-ggally-package

players_scaled2$cluster <- as.factor(clustering$cluster)

p <- ggparcoord(data = players_scaled2, 
                columns = c(2:11), 
                groupColumn = "cluster", 
                scale = "std") + 
  labs(x = "Stat Differential", 
       y = "value (in standard-deviation units)", 
       title = "Clustering")
ggplotly(p)

MatchGroup4 <- Matches_Numeric %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1)



# https://uc-r.github.io/kmeans_clustering

















# Alternatives

pr <- prcomp(Matches_Numeric[c(1:17)], 
             center = TRUE, 
             scale = TRUE)

summary(pr)

screeplot(pr, 
          type = "l", 
          npcs = 15, 
          main = "Screeplot of the first 10 PCs")
abline(h = 1, 
       col="red", 
       lty=5)
legend("topright", 
       legend=c("Eigenvalue = 1"),
       col=c("red"), 
       lty=5, 
       cex=0.6)

cumpro <- cumsum(pr$sdev^2 / sum(pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)


library(factoextra)

fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = wdbc$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))



# join players back
M1[,"Player"] <- df.players$Player_Name
# as character
M1$Cluster <- as.character(M1$Cluster)
# Change cluster names to ~ position
M1$Cluster[M1$Cluster == "1"] <- "Forward"
M1$Cluster[M1$Cluster == "2"] <- "Defender"
M1$Cluster[M1$Cluster == "3"] <- "Midfield"
M1$Cluster[M1$Cluster == "4"] <- "Ruck"
# look for mismatches 
M1$Match <- M1$Cluster == M1$Position





