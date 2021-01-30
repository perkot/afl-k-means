# ----------------
# REFERENCES
# ----------------

# https://stats.stackexchange.com/questions/80377/which-distance-to-use-e-g-manhattan-euclidean-bray-curtis-etc
# https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff

# https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

# https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967

# ----------------
# DEPENDENCIES
# ----------------

# Set correct package library path 
.libPaths(c("C:/NotBackedUp/Temp/software/R/3.4", .libPaths()))

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

# Data analysis 
library(ClustOfVar)
library(cluster)

# ----------------
# READ IN FILES 
# ----------------

# .csv from PC directory 
Matches <- read.csv("Matches.csv", 
                header = TRUE, 
                stringsAsFactors=FALSE)

# .csv from PC directory 
Matches_Numeric <- read.csv("Matches_Numeric.csv", 
                    header = TRUE, 
                    stringsAsFactors=FALSE)

# ----------------
# CLEAN DATA 
# ----------------

Matches_Numeric$FA_Diff <- NULL # FA a mirror of FF 
Matches_Numeric$CCL_Diff <- NULL # already have clearance data
Matches_Numeric$SCL_Diff <- NULL # already have clearance data
Matches_Numeric$D_Diff <- NULL # already have kick & handball data
Matches_Numeric$M_Diff <- NULL # selected contested marking instead
Matches_Numeric$TO_Diff <- NULL # selected contested marking instead
Matches_Numeric$SI_Diff <- NULL # in effect, a component of margin
Matches_Numeric$GA_Diff <- NULL # in effect, a component of margin

# Ensures all columns are numeric for analysis 
Matches_Numeric[,1:17] <- sapply(Matches_Numeric[,1:17],as.numeric)

# Consider scaled data 
  # center=true just means remove the mean
  # scale=TRUE stands for divide by SD
Matches_Numeric_Scale <- scale(Matches_Numeric, 
                               center = FALSE, 
                               scale = TRUE)

# ----------------
# CORR PLOTS 
# ----------------

# create matrix of correlations
M <- cor(Matches_Numeric)
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

# ----------------
# K-MEANS USING DAISY 
# ----------------

# Daisy 
# Dissimilarity Matrix Calculation
# Compute all the pairwise dissimilarities (distances) between observations in the data set. 
# The original variables may be of mixed types. In that case, or whenever metric = "gower" is set, 
# a generalization of Gower's formula is used, see ‘Details’ below.

# The metrics used for each data type are described below:
  # quantitative (interval): range-normalized Manhattan distance
  # ordinal: variable is first ranked, then Manhattan distance is used with a special adjustment for ties
  # nominal: variables of k categories are first converted into k binary columns and then the Dice coefficient is used

str(Matches_Numeric)

# ----------------
# GOWERS DISTANCE
# ----------------

# Gower’s Distance can be used to measure how different two records are. 
# The records may contain combination of logical, categorical, numerical or text data. 
# The distance is always a number between 0 (identical) and 1 (maximally dissimilar). 

d <- 
  Matches_Numeric %>% 
  daisy(metric = "gower") # gower used when some metrics are non-numeric

# fit model into 3 clusters 
kfit <- kmeans(d, 3)

# visualise 3 clusters
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

# join back groupings
kfit$cluster
Matches_Numeric[,"Cluster"] <- kfit$cluster

# summarises statistics for groupings 
MatchGroup <- Matches_Numeric %>% 
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
  Matches_Numeric_Scale %>% 
  daisy(metric = "euclidean") 

# fit model into 3 clusters 
kfit2 <- kmeans(d2, 3)

# Investigate output 
str(kfit2)

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

# Elbow Method
  # Recall that, the basic idea behind cluster partitioning methods, such as k-means clustering, 
  # is to define clusters such that the total intra-cluster variation (known as total within-cluster variation 
  # or total within-cluster sum of square) is minimized

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
# set seed
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) 
  {
  kmeans(Matches_Numeric_Scale, k, nstart = 10 )$tot.withinss
  }
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Average Silhouette Method
  # In short, the average silhouette approach measures the quality of a clustering. 
  # That is, it determines how well each object lies within its cluster. 
  # A high average silhouette width indicates a good clustering. 
  # The average silhouette method computes the average silhouette of observations for different values of k. 
  # The optimal number of clusters k is the one that maximizes the average silhouette 
  # over a range of possible values for k.2

  # We can use the silhouette function in the cluster package to compuate the average silhouette width. 
  # The following code computes this approach for 1-15 clusters. 
  # The results show that 2 clusters maximize the average silhouette values with 
  # 4 clusters coming in as second optimal number of clusters.

# function to compute average silhouette for k clusters
avg_sil <- function(k) 
  {
  km.res <- kmeans(Matches_Numeric_Scale, 
                   centers = k, 
                   nstart = 25)
  ss <- silhouette(km.res$cluster, 
                   dist(Matches_Numeric_Scale))
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

# Gap Statistic Method
  # The gap statistic has been published by R. Tibshirani, G. Walther, and T. Hastie (Standford University, 2001). 
  # The approach can be applied to any clustering method (i.e. K-means clustering, hierarchical clustering). 
  # The gap statistic compares the total intracluster variation for different values of k 
  # with their expected values under null reference distribution of the data 
  # (i.e. a distribution with no obvious clustering). 
  # The reference dataset is generated using Monte Carlo simulations of the sampling process

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(Matches_Numeric_Scale, 
                    FUN = kmeans, 
                    nstart = 25,
                    K.max = 10, 
                    B = 50,
                    d.power = 2) # preventing only a single cluster being the preferred selection
# Print the result
print(gap_stat, method = "firstmax")

# d.power
  # a positive integer specifying the power 
  # which is applied to the euclidean distances (dist) before they are summed up to give . 
  # The default, d.power = 1, corresponds to the “historical” R implementation, 
  # whereas d.power = 2 corresponds to what Tibshirani et al had proposed. 
  # This was found by Juan Gonzalez, in 2016-02.

# RAW DATA 
# logW   E.logW       gap      SE.sim
# [1,] 11.49051 12.10033 0.6098169 0.017811513 *** OPTIMAL GAP == 1, RESULT OF NON-STANDARDISED DATA 
# [2,] 11.02448 11.47285 0.4483720 0.015241535
# [3,] 10.75075 11.14025 0.3894970 0.012054764
# [4,] 10.56459 10.93300 0.3684069 0.012821011
# [5,] 10.39440 10.79567 0.4012696 0.009944292
# [6,] 10.30568 10.69603 0.3903530 0.008902703
# [7,] 10.20430 10.62220 0.4178987 0.007334922
# [8,] 10.11937 10.56748 0.4481090 0.007684777
# [9,] 10.06688 10.52331 0.4564284 0.007468296
# [10,] 10.02640 10.48887 0.4624717 0.006771964

# https://stats.stackexchange.com/questions/140711/why-does-gap-statistic-for-k-means-suggest-one-cluster-even-though-there-are-ob

# SCALE = TRUE, d.power = 2 
# logW   E.logW      gap      SE.sim
# [1,] 8.601743 9.795964 1.194221 0.011609600
# [2,] 8.447080 9.618037 1.170957 0.008690646
# [3,] 8.383363 9.538859 1.155496 0.009542233
# [4,] 8.333660 9.478960 1.145300 0.009205868
# [5,] 8.286366 9.439993 1.153627 0.009239606
# [6,] 8.251598 9.405771 1.154173 0.009125238
# [7,] 8.220015 9.376283 1.156269 0.009432784
# [8,] 8.190867 9.349882 1.159015 0.009402437
# [9,] 8.168479 9.326815 1.158336 0.009382302
# [10,] 8.145895 9.305444 1.159549 0.009473067

# Investigation of issue
k <- maxSE(gap_stat$Tab[, "gap"], 
           gap_stat$Tab[, "SE.sim"], 
           method="Tibs2001SEmax")
fit <- kmeans(Matches_Numeric, k)

# Plot the results.
pch <- ifelse(fit$cluster==1,24,16); col <- ifelse(fit$cluster==1,"Red", "Black")
plot(Matches_Numeric, 
     asp=1, 
     main=title, 
     pch=pch, 
     col=col)
plot(gap_stat, 
     main=paste("Gap stats"))
abline(v=k, lty=3, lwd=2, col="Blue")

# My notes
  # Huge amount of within-group variance -- aka noise 
  # Significant differences in scale, especially metres gained 
  # compared to a null distribution, it seems likely that there is NOT the presence of legitimate clusters in this data
  # corroborated by response to this question 
  # https://stats.stackexchange.com/questions/51556/choosing-clusters-for-k-means-the-1-cluster-case

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

# join back groupings
kfit2$cluster
Matches_Numeric[,"Cluster2"] <- kfit2$cluster

# summarises statistics for groupings 
MatchGroup2 <- Matches_Numeric %>% 
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
  Matches_Numeric_Scale %>% 
  daisy(metric = "manhattan") 

# fit model into 3 clusters 
kfit3 <- kmeans(d3, 3)

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

# join back groupings
kfit3$cluster
Matches_Numeric[,"Cluster3"] <- kfit3$cluster

# summarises statistics for groupings 
MatchGroup3 <- Matches_Numeric %>% 
  group_by(Cluster3) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1)

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
kmeans(Matches_Numeric, 
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

wssplot(Matches_Numeric, nc = 20)

# Run again with k = 4, based upon scree plot 
set.seed(123)
clustering <- kmeans(Matches_Numeric, 
                     centers = 4, 
                     nstart = 20)
clustering

library(GGally)
library(plotly)
# replot this with ggplot2
# identify "mean" value for each cluster (aka centroid) - plot this with high transparency for other values 
# https://stackoverflow.com/questions/34308109/controlling-alpha-in-ggparcoord-from-ggally-package

Matches_Numeric$cluster <- as.factor(clustering$cluster)

p <- ggparcoord(data = Matches_Numeric, 
                columns = c(2:17), 
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