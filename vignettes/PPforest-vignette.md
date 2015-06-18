---
title: "Projection pursuit classification random forest "
author: "N. da Silva, E. Lee & D. Cook"
date: "2015-06-17"
output: rmarkdown::html_vignette 
   
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::docco_linear}
  %\VignetteEncoding{UTF-8}
---

## Abstract

A random forest is an ensemble learning method, built on bagged trees. The bagging provides power for classification because it yields information about variable importance, predictive error and proximity of observations. This research adapts the random forest to utilize combinations of variables in the tree construction, which we call the projection pursuit classification random forest (`PPforest`). In a random forest each split is based on a single variable, chosen from a subset of predictors. In the PPforest, each split is based on a linear combination of randomly chosen variables. The linear combination is computed by optimizing a projection pursuit index, to get a projection of the variables that best separates the classes. The PPforest uses the PPtree algorithm, which fits a single tree to the data. Utilizing linear combinations of variables to separate classes takes the correlation between variables into account, and can outperform the basic forest when separations between groups occurs on combinations of variables. Two projection pursuit indexes, LDA and PDA, are used for PPforest. 

##Introduction
Random Forest was proposed by Leo Bieman (2001), there are two main concept used in random forest, bootstrap aggregation  and random feature selection  to individual classification or regression trees for prediction.  The most popular random forest uses univariate decision trees (CART or C4.5), these trees select a single feature variable in each split. A not very popular version of random forest that is also presented in the original paper, uses  oblique trees where the feature space is selected by random selected hyperplanes.

The approach presented here (`PPforest`) goes in the spirit of the second random forest version. Our porpouse works well when the data are collinear with correlated features. In these cases hyperplanes that are oblique to the axis maybe do a better work in separeate the feature space.

###Motivation Example
Australian crab data set contains measurements on rock crabs of the genus Leptograpsus. There are 200 observations from two species (blue and orange) and for each specie (50 in each one) there are 50 males and 50 females. Class variable has 4 classes with the combinations of specie and sex (BlueMale, BlueFemale, OrangeMale and OrangeFemale). The data were collected on site at Fremantle, Western Australia. For each specimen, five measurements were made, using vernier calipers.
 
1. FL the size of the frontal lobe length, in mm
2. RW rear width, in mm
3. CL length of midline of the carapace, in mm
4. CW maximum width of carapace, in mm
5. BD depth of the body; for females, measured after displacement of the abdomen, in mm

To visualize this data set we use a scatterplot matrix.

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 


To better understand the main characteristics of PPtree algorithm we will use a subset of crab data. We have selected two feature variables, RW and BD. With this reduced data set we run a `rpart` and a `PPtree` and we plot the decision boundaries for each tree.

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
The  left panel shows the `rpart` decision boundaries while the right panel shows the `PPtree` decision boundaries.
Decision boundaries from  `rpart` are orthogonal to the axis and for this data set several splits are needed in order to capture the data structure. On the other hand, `PPtree` decition boundaries requiere less splits to capture the data structure.

##Projection pursuit classification forest
 `PPforest` implements a projection pursuit classification random forest. `PPforest` adapts random forest to utilize combinations of variables in the tree construction.  Projection pursuit classification trees  are used to build the forest, ( from `PPtreeViz` package )

`PPforest` is generated form `PPtree` algorithm. `PPtree` combines tree structure methods with projection pursuit dimensional reduction.

\code{PPtree} projection pursuit classification tree algorithm:

1. In each node a projection pursuit index is maximized to find the optimal $1-D$ projection, $\alpha^*$, for separating all classes in the current data.
2. Reduce the number of classes to two, by comparing means and assign new labels, $G_1$ or $G_2$ ($y_i^*$) to each observation
3. Re-do projection pursuit with these new group labels finding the $1-D$ projection, $\alpha$ using $(x_i,y^*)$
4. Calculate the decision boundary c, keep $\alpha$ and $c$
5. Separate data into two groups using new group labels $G_1$ and $G_2$
6. For each group stop if there is only one class else repeat the procedure, the splitting steps are iterated util the last two classes are separate.

Trees from `PPtree` algorithm are simple, they use the association between variables to find separation. If a linear boundary exists, `PPtree` produces a tree without misclassification.
One class is assigned only to one final node, depth of the tree is at most the number of classes.
Finally the projection coefficient of the node represent the variable importance.

The following plots shows the tree structure from `rpart` and `PPtree`. Trees from `PPtree` are smaller and simpler than trees from `rpart`. For this example `PPtree` present three splits while `rpart` tree did  13 splits. 

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png) 

Projection pursuit random forest algorithm description

1. Let N the number of cases in the training set, $B$ bootstrap samples from the training set are drawn (samples of size N with replacement)

2. For each bootstrap sample a PPtree type algorithm is grown to the largest extent possible.

3. Let M the number of input variables, a number of $m<<M$ variables are selected at random at each node and each split is based on a linear combination of these randomly chosen variables. The linear combination is computed by optimizing a projection pursuit index, to get a projection of the variables that best separates the classes.
4.  Predict the classes of each case not included in the bootstrap sample and compute oob error.
5. Based on majority vote predict the class for new dat.

We present two examples using real data to show how PPforest package works.

###Overview PPforest package
`PPforest` package implements a classification random forest using projection pursuit classification trees. The following table present all the functions in `PPforest` package. 

| Function |Description |
| ------ | ------ | -----: |
|PPforest|Run a Projection pursuit random forest|
|predict.PPforest|Vector with predicted values from a PPforest objec|
|ppf_importance| Plot a global measure of imortance in PPforest.|
|pproxy_plot| Proximity matrix visualization|
|ppf_oob_error| OOB error visualization|
|var_select  |Index id for variables set, sample variables without replacement with constant sample proportion |
|train_fn |Index id for training set, sample in each class with constant sample proportion. |
|PPtree_split|Projection pursuit classification tree with random variable selection in each split|
|trees_pp| Projection pursuit trees for bootstrap samples|
|tree_ppred|Vector with predicted values from a PPtree object.|
|ppf_bootstrap| Draws bootstrap samples with strata option.|
|print.PPforest| Print PPforest object|

To run the `PPforest` we first split the data in training (2/3) and test (1/3).
To do that we use train_fn function, this function has two arguments, class is a vector with the class variable and size.p is the sample proportion we want. The sample is stratified by each class with the same proportion. This function returns a vector with the sample ids.



```r
training.id <- PPforest::train_fn(crab[, 1], 2/3)
str(training.id)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	132 obs. of  1 variable:
##  $ id: int  51 52 53 57 58 59 62 63 64 65 ...
```

```r
training.id
```

```
## Source: local data frame [132 x 1]
## 
##    id
## 1  51
## 2  52
## 3  53
## 4  57
## 5  58
## 6  59
## 7  62
## 8  63
## 9  64
## 10 65
## .. ..
```
If we want all the selected id's we use `training.id$id`

```PPforest``` function runs a projection pursuit random forest,  using this function we have the option to split the data in training and test using size.tr directly. `size.tr` is the size proportion of the training then the proportion of this will be 1- `size.tr`.
The number of trees in the forest is specified using the argument m. size.p is the sample proportion of the variables used in each node split, `PPmethod` is the projection pursuit index to be optimized there are two options LDA and PDA. Finally strata=TRUE indicate that the bootstrap samples are stratified by class variable. 


```r
set.seed(146)
pprf.crab <- PPforest::PPforest(data = crab, size.tr = 2/3, m = 500, size.p = .7,  
              PPmethod = 'LDA', strata = TRUE)
str(pprf.crab, max.level = 1)
```

```
## List of 19
##  $ prediction.training: chr [1:132] "BlueMale" "BlueFemale" "BlueMale" "BlueMale" ...
##  $ training.error     : num 0.0606
##  $ prediction.test    : chr [1:68] "BlueMale" "BlueMale" "BlueFemale" "BlueMale" ...
##  $ error.test         : num 0.0147
##  $ oob.error.forest   : num 0.0682
##  $ oob.error.tree     : num [1:500] 0.2 0.156 0.229 0.333 0.214 ...
##  $ boot.samp          :Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	132 obs. of  6 variables:
##   ..- attr(*, "indices")=List of 500
##   .. .. [list output truncated]
##   ..- attr(*, "drop")= logi TRUE
##   ..- attr(*, "group_sizes")= int [1:500] 132 132 132 132 132 132 132 132 132 132 ...
##   ..- attr(*, "biggest_group_size")= int 132
##   ..- attr(*, "labels")='data.frame':	500 obs. of  1 variable:
##   ..- attr(*, "vars")=List of 1
##  $ output.trees       :Classes 'rowwise_df', 'tbl_df' and 'data.frame':	500 obs. of  2 variables:
##  $ proximity          :'data.frame':	8778 obs. of  3 variables:
##  $ votes              : num [1:132, 1:4] 0.323 0.566 0.246 0.244 0.842 ...
##   ..- attr(*, "dimnames")=List of 2
##  $ prediction.oob     : chr [1:132] "BlueMale" "BlueFemale" "BlueMale" "BlueMale" ...
##  $ n.tree             : num 500
##  $ n.var              : num 3
##  $ type               : chr "Classification"
##  $ confusion          : num [1:4, 1:5] 31 4 0 0 2 29 0 0 0 0 ...
##   ..- attr(*, "dimnames")=List of 2
##  $ call               : language PPforest::PPforest(data = crab, size.tr = 2/3, m = 500, PPmethod = "LDA",      size.p = 0.7, strata = TRUE)
##  $ train              :'data.frame':	132 obs. of  6 variables:
##  $ test               :'data.frame':	68 obs. of  5 variables:
##  $ vote.mat           : chr [1:500, 1:132] "BlueFemale" "BlueMale" "BlueFemale" "BlueFemale" ...
##  - attr(*, "class")= chr "PPforest"
```

`PPforest` function returns the predicted values of the training data, training error, test error and predicted test values are returned. Also there is the information about out of bag error for the forest and also for each tree in the forest. Bootstrap samples, output of all the trees in the forest from trees_pp function, proximity matrix and vote matrix,  number of trees grown in the forest, number of predictor variables selected to use for splitting at each node. Confusion matrix of the prediction (based on OOb data), the training data and test data and vote matrix are also returned.

The printed version of a PPforest object follows the randomForest printed version to make them comparable. Based on confusion matrix, we can observe that the biggest error is for BlueMale class. Most of the wrong classified values are between BlueFemale and BlueMale.

```r
pprf.crab
```

```
## 
## Call:
##  PPforest::PPforest(data = crab, size.tr = 2/3, m = 500, PPmethod = "LDA",      size.p = 0.7, strata = TRUE) 
##                Type of random forest: Classification
##                      Number of trees: 500
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 6.82%
## Confusion matrix:
##              BlueFemale BlueMale OrangeFemale OrangeMale class.error
## BlueFemale           31        2            0          0        0.06
## BlueMale              4       29            0          0        0.12
## OrangeFemale          0        0           30          3        0.09
## OrangeMale            0        0            0         33        0.00
```

If we compare the results with the randomForest function for this data set the results are the following:

```r
rf.crab <- randomForest::randomForest(Type~.,data=crab,proximity=TRUE)
rf.crab
```

```
## 
## Call:
##  randomForest(formula = Type ~ ., data = crab, proximity = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 19.5%
## Confusion matrix:
##              BlueFemale BlueMale OrangeFemale OrangeMale class.error
## BlueFemale           41        3            6          0        0.18
## BlueMale              3       41            0          6        0.18
## OrangeFemale          4        0           41          5        0.18
## OrangeMale            2        5            5         38        0.24
```
We can see that for this data set the `PPforest` performance is much better than using `randomForest`. `PPforest` works well since the classes can be separated by linear combinations of variables. 
This is a clear case where oblique hyperplanes are more adecuate in this case than hyperplanes horizontal to the axis.

We can get the predicted values for training and test using the output of `PPforest`

```r
 pred.training <- pprf.crab$prediction.training
 pred.test <- pprf.crab$prediction.test
 pred.test[1:10]
```

```
##  [1] "BlueMale"   "BlueMale"   "BlueFemale" "BlueMale"   "BlueMale"  
##  [6] "BlueMale"   "BlueMale"   "BlueMale"   "BlueMale"   "BlueMale"
```

From `PPforest` object we can plot a heat map of the proximity matrix using the function `pproxy_plot`.

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The data are ordered by class (BlueFemale, BlueMale, OrangeFemale and OrangeMale), in the heat map we can observe a colored block diagonal structure, this means that observations form the same clase are similar, but also we can observe that observations from BlueMale and BlueFemale are similar (big blue square in the left top corner).

Additionaly `pproxy_plot` can be used to plot the MDS using proximity matrix information.

If we select k=2 the output plot is as follows:


```r
PPforest::pproxy_plot(pprf.crab, type="MDS", k=2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

We can observe a spacial separation between classes.
If we select k>2,  we can observe that using two dimensions is enought to see the spacial separation.


```r
PPforest::pproxy_plot(pprf.crab, type="MDS",k = 3)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

A global measure of importance is computed. In PPtree the projection coefficient of each node represent the importance of variables to class separation in each node. To get a global measure in PPforest we need to take into account the importance in each node and combine the results for all the trees in the forest. The importance measure of PPforest is a  weighted mean of the absolute value of the projection coefficients across all nodes in every tree. The weights are  the projection pursuit index in each node, and 1-the out of bag error of each tree.

Using the \code{ppf_importance} function we can plot the global variable importance measure in PPforest.


```r
PPforest::ppf_importance(crab, pprf.crab, global = TRUE, weight = TRUE) 
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

We can see that the most important variable in this example is RW while the less important is CL

Finally a cumulative out of eerror plot can be done using `ppf_oob_error`


```r
PPforest::ppf_oob_error(pprf.crab, nsplit = 20)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

The oob-error rate decrease when we increase the numbers of trees but gets constant with less than  250 trees. 
 
##NCI60 example

Gene expression data set, contains 61 observations and 30 feature variables. Class variable has 8 different tissue types, 9 breast, 5central nervous system (CNS), 7 colon, 6 leukemia, 8 melanoma, 9 non- small-cell lung carcinoma (NSCLC), 6 ovarian and 9 renal. There are 6830 genes.

To visualize this data set we use a scatterplot matrix selecting some of the variables.

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

Since this data set present a considerable big number of predictive variables we will explore the data set using `PPTreeViz` package first.

We can explore the optimal projection for classification in q-dimensional space. We select q=1 and  PDA  projection pursuit index. Whith q=1 `PPopt.Viz` returns an histogram with the projected data and the plot  with the coefficients of projection. 



```r
library(ggplot2)

PPtreeViz::PPopt.Viz(PPtreeViz::PDAopt(NCI60[, 1], as.matrix(NCI60[, -1]), q = 1))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 
In the histogram we can observe each class distribution and we can observe that leukemia is sparate from the other clases. In the plot with the coefficient calues we can observe that Gene 30 and Gene 28 are the most important to separate the classes in this view.  

If we select q=2, `PPopt.Viz shows the best 2 dimensional projection coefficients and the scatter plot of the projected data. We can observe that colon and leukemia are separable.

```r
library(ggplot2)
PPtreeViz::PPopt.Viz(PPtreeViz::PDAopt(NCI60[, 1], as.matrix(NCI60[, -1]), q = 2))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

In the same way than for crab data we split the data in training and test(size.tr=2/3) and we run a ```PPforest```. In this case PDA index are more appropiate since the number of observationis small related to the number of feature variables. We select $\lambda=.7$, it is a penalty parameter in PDA index and is between 0 to 1. size.p=.7 is the random sample variable proportion used in each node split.


```r
set.seed(123)
pprf.nci60 <- PPforest::PPforest(data = NCI60, size.tr = 2/3, m = 500, size.p = .7, PPmethod = 'PDA', strata = TRUE, lambda=.5)
pprf.nci60
```

```
## 
## Call:
##  PPforest::PPforest(data = NCI60, size.tr = 2/3, m = 500, PPmethod = "PDA",      size.p = 0.7, strata = TRUE, lambda = 0.5) 
##                Type of random forest: Classification
##                      Number of trees: 500
## No. of variables tried at each split: 21
## 
##         OOB estimate of  error rate: 37.5%
## Confusion matrix:
##          BREAST CNS COLON LEUKEMIA MELANOMA NSCLC OVARIAN RENAL
## BREAST        2   0     0        0        1     1       1     1
## CNS           0   2     0        0        0     1       0     0
## COLON         0   0     5        0        0     0       0     0
## LEUKEMIA      0   0     0        5        0     0       0     0
## MELANOMA      2   0     0        0        3     0       0     0
## NSCLC         1   0     0        0        0     3       2     0
## OVARIAN       0   0     0        0        0     2       0     2
## RENAL         0   1     0        0        0     0       0     5
##          class.error
## BREAST          0.67
## CNS             0.33
## COLON           0.00
## LEUKEMIA        0.00
## MELANOMA        0.40
## NSCLC           0.50
## OVARIAN         1.00
## RENAL           0.17
```

We can compare the results with randomForest

```r
set.seed(123)
rf.nci60 <- randomForest::randomForest(Type~., data = NCI60, proximity = TRUE)
rf.nci60
```

```
## 
## Call:
##  randomForest(formula = Type ~ ., data = NCI60, proximity = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 45.9%
## Confusion matrix:
##          BREAST CNS COLON LEUKEMIA MELANOMA NSCLC OVARIAN RENAL
## BREAST        3   1     0        0        2     2       1     0
## CNS           0   0     0        0        0     1       0     4
## COLON         0   0     6        0        0     1       0     0
## LEUKEMIA      0   0     0        8        0     0       0     0
## MELANOMA      0   0     0        0        6     2       0     0
## NSCLC         0   1     1        0        3     3       1     0
## OVARIAN       0   0     0        0        1     1       0     4
## RENAL         0   1     0        0        0     0       1     7
##          class.error
## BREAST     0.6666667
## CNS        1.0000000
## COLON      0.1428571
## LEUKEMIA   0.0000000
## MELANOMA   0.2500000
## NSCLC      0.6666667
## OVARIAN    1.0000000
## RENAL      0.2222222
```

In this case PPforest also shows a better performance than randomForest.

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png) 

We can also plot the MDS of the proximity matrix using the function \code{pproxy_plot}.

If we select k=2 the output plot is as follows:

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png) 
If we select k>2 the graphical display will be:

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22-1.png) 


![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png) 

We can observe that the most important variables are Gene28, Gene14 and Gene25 while Gene9 and Gene15 are the less important for the classification PPforest.


 render("PPforest-vignette.md")     
