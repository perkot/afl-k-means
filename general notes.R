General notes

the performance of k-means algorithm tends to be affected by skewed data distributions, i.e., imbalanced data. They often produce clusters of relatively uniform sizes, even if input data have varied cluster size, which is called the “uniform effect.”
# https://link.springer.com/chapter/10.1007/978-3-319-13731-5_54

Because the number of clusters (k) must be set before we start the algorithm, it is often advantageous to use several different values of k and examine the differences in the results. 

An additional disadvantage of K-means is that it’s sensitive to outliers and different results can occur if you change the ordering of your data. The Partitioning Around Medoids (PAM) clustering approach is less sensititive to outliers and provides a robust alternative to k-means
https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/
  
  Evaluate feature importance
https://cran.r-project.org/web/packages/FeatureImpCluster/readme/README.html

Cluster analysis as such is not an automatic task, but an iterative process of knowledge discovery or interactive multi-objective optimization that involves trial and failure. It is often necessary to modify data preprocessing and model parameters until the result achieves the desired properties.

k means assumptions
k-means assume the variance of the distribution of each attribute (variable) is     spherical;

all variables have the same variance;

the prior probability for all k clusters are the same, i.e. each cluster has         roughly equal number of observations; If any one of these 3 assumptions is           violated, then k-means will fail.

## Premise

*"A model is a simplification or approximation of reality and hence will not reflect all of reality"*
  
  This aphorism summarizes the difficulty of generating statistical models that adequately represent all the nuance & complexity of the real world. At the core of a statistical models success is the quality of data that builds it.

If a data-set inadequately represents the nuance of an area of study, it is unlikely for a statistical model to be able to compensate for this absent variance. This may result in models that are statistically sensible, but otherwise inadequate representations of reality

But how can we know how in touch our model is with reality? Sometimes we will be fortunate that the results of a model can be reconciled against some known external parameter

This was the motivation behind this analysis - to explore & reconcile a statistical model with a known external parameter, and discuss the issues there in. My approach was to use k-means - a technique to explore sub-groups of observations within a data set

The quality of clustering/grouping in k-means can be measured with two types of validity indices 

**Internal indices** - to measure the goodness of a clustering structure without external information (Tseng et al., 2005). In this case, relying upon statistical outputs

**External indices** - to evaluate the results of a clustering algorithm based 
on a known cluster structure of a data set. In other words, already known, real-world groupings

My conduit for this question is a data-set I have curated myself that summarizes average game-day statistics of Australian Rules Football players (i.e. average kicks, handballs, tackles per game)

The goal here is to generate two models - the first optimizing for internal indices  (statistical outputs), the second optimizing for external indices (already known-groupings). 

### Model [1] - Internal Indices

Model 1 aims to find the best combination of statistics,to maximize separation of groupings. Thus, success is defined by internal indicators such as minimizing within-cluster variation, & silhouette plots. Thus, a "good model" is defined strictly by optimizing group-separation

This is important, because in the absence of *any* external indicator, how would we know if the classifications derived from the statistical model are sensible? The statistics may clearly articulate how well the model performs on the data, but not how well the data represents its source-area of inquiry


****** answer - the stats are the best representation of the models data ******
  
  ### Model [2] - External Indices 
  
  Of course this can be ameliorated by having an external indicator

AFL players, and sports-people more generally, can be categorized based upon their *field position*. The players position is included in this data, & as such, this model can be evaluated against this external indicator 

Broadly, AFL players fulfill one of four positions:
  
  * Forwards
* Midfielders
* Rucks
* Defenders

Which means our target for this model is four groups (k = 4)

Thus, in contrast to model [1] which we are treating as being agnostic of any external indicator, this model will look to group players into these four field positions. It carries with it the assumption that a players position *is* associated with match-day statistics. This seems intuitive to me... a high average number of *spoils* per game should be associated with players who are defenders.

But as real and true as these field positions may be, it may be that this data doesn't have the sophistication to model them accurately

Which brings us back to the heart of the matter: 

*"A model is a simplification or approximation of reality and hence **will not reflect all of reality**"*

We are trying to condense the complexity & fluidity of an elite sport with a selection of ~ 40 match day statistics. 

So how good can our approximation be? 

If model [1] does not match known field-positions, yet is statistically "superior", how do we interpret it?

Are their statistics absent from this data, that may improve the outputs?

Let's find out

## Some background on clustering 

**"Clustering"** is a technique used to explore sub-groups of observations within a data set. Observations are grouped in such a way that they are more similar to one another (by some statistical criteria), than to observations in other groups

**k-means clustering** is probably the most common algorithm for partitioning 

k-means 'groups' observations (i.e. players) such that the observations within the group are as similar as possible. Clusters are formed via observations that demonstrate high *intra-class similarity*, measured by the clusters *'centroid'* (the mean value of points assigned to the cluster). 

*'Principal components'* are derived as *combinations of the variables* in the data-set

In k-means clustering, 'k' denotes the number of clusters we will separate the data into. This eventually needs to be *pre-specified* to perform the analysis. As mentioned, the # of groups can be estimated statistically (internal indices) or based on known-groupings (external indices). 

For more information on k-means/principal components analysis, [this towards data science](https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff) article is an easily digestible summary 