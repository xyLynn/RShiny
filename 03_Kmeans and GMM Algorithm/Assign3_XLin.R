library(mclust)     #GMM
library(NbClust)    #kmeans_k_optimization
library(cluster)    #kmeans_k_optimization
library(factoextra) #kmeans_k_optimization
library(vegan)      #kmeans_k_optimization
library(plotly)     #3D plot
library(caret)      #cross validation
library(class)      #knn

#' Cluster Calculation
#'
#' Computes GMM and Kmeans clustering of the dataset, and return a new dataset with cluster numbers. (Question 4.b)
#' @param algorithm c('Kmeans', 'PureKmeans', 'GMM'). Method of clustering. If method is 'PureKmeans', the returned dataset will not include the centroid of each cluster. Returned dataset will not include the centroid of each cluster by using 'Kmeans' method.
#' @param dt Dataframe. The column names must include 'heiht', 'weight', 'age', 'male', 'gender'.
#' @param n Either the number of clusters, say k, or a set of initial (distinct) cluster centres.
#' @param normalize If TRUE, the data in returned dataset is scaled. (by using 'scale()' function).
#' @return Return a dataset with original data or data after being scaled with cluster number.
#' @keywords cluster calculation
#' @examples
#' gmm_norm <- cluster_calculation("GMM", train, 3, normalize=TRUE)
#' gmm_unnorm <- cluster_calculation("GMM", train, 3, normalize=FALSE)
#' kmeans_norm <- cluster_calculation("Kmeans", train, 3, normalize = TRUE)
#' kmeans_unnorm <- cluster_calculation("Kmeans", train, 3, normalize = FALSE)
#' purek_norm <- cluster_calculation("PureKmeans", train, 3, normalize = TRUE)
#' purek_unnorm <- cluster_calculation("PureKmeans", train, 3, normalize = FALSE)
#' @export
cluster_calculation <- function(algorithm, dt, n, normalize=FALSE){
  require(mclust)
  dt.org <- dt[,c('height','weight','age')]
  dt.scale <- scale(dt.org)
  if (algorithm == 'Kmeans'){
    kmeans_clust <- kmeans(dt.scale, centers = n, nstart = 100)
    #calculate centroid for each cluster
    centers <- as.data.frame(t(apply(as.matrix(kmeans_clust$centers), 1, function(r)r*attr(dt.scale,'scaled:scale') + attr(dt.scale, 'scaled:center'))))
    centers$cluster <- paste("cluster", row.names(centers), "centroid")
    #combine 2 datasets
    dt_with_kmeans_cluster <- cbind(dt.org, kmeans_clust$cluster)
    names(dt_with_kmeans_cluster)[4] <- "cluster"
    dt_with_kmeans_cluster <- rbind(dt_with_kmeans_cluster, centers)

    return(dt_with_kmeans_cluster)
  } else if (algorithm == 'PureKmeans'){
    kmeans_clust <- kmeans(dt.scale, centers = n, nstart = 100)
    if (normalize==TRUE){
      dt_with_kmeans_cluster <- cbind(dt.scale, dt$male, kmeans_clust$cluster)
    } else if (normalize==FALSE){
      dt_with_kmeans_cluster <- cbind(dt[,c('height','weight','age','male')], kmeans_clust$cluster)
    }
    dt_with_kmeans_cluster <- as.data.frame(dt_with_kmeans_cluster)
    names(dt_with_kmeans_cluster)[4:5] <- c("male","cluster")

    return(dt_with_kmeans_cluster)

  } else if (algorithm == 'GMM'){
    gmm_clust <- mclust::Mclust(dt.scale, n)
    if (normalize==TRUE){
      dt_with_gmm_cluster <- cbind(dt.scale, dt$male, as.factor(gmm_clust$classification))
    } else if (normalize==FALSE){
      dt_with_gmm_cluster <- cbind(dt[,c('height','weight','age','male')], as.factor(gmm_clust$classification))
    }
    dt_with_gmm_cluster <- as.data.frame(dt_with_gmm_cluster)
    names(dt_with_gmm_cluster)[4:5] <- c("male","cluster")

    return(dt_with_gmm_cluster)
  }
}

#' Optimum k(Kmeans)
#'
#' Compute the optimum number of clusters given the datasets. (Question 3.b)
#' @param method c('NbClust', 'GapStat', 'Calisky'). These are 3 method to calculate the optimum number of clusters. Check this website for details: https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
#' @param startNum The minimum number of clusters. (>2)
#' @param endNum The maximum number of clusters.
#' @param iter The number of random starting configurations for each value of K. Using NULL in method 'NbClust'.
#' @param dt Dataframe. The column names must include 'heiht', 'weight', 'age', 'male', 'gender'.
#' @return The objection of this function is to meet requirement in assignment 4.b, getting optimum number of clusters. Here, the returns of
#' each method are, NbClust:2, GapStat:4, Calisky Criterion:2. In conclusion, the optimum number of clusters for this dataset is 2.
#' Method 'Calisky Criterion' is faster than method 'NbClust'.
#' @keywords Question 3.b: optimum number of clusters
#' @examples
#' kmeans_k_optimization('GapStat', 1, 10, 500, train)
#' kmeans_k_optimization('Calisky', 1, 10, 1000, train)
#' kmeans_k_optimization('NbClust', 2, 10, NULL, train)
#' @export
kmeans_k_optimization <- function(method, startNum, endNum, iter, dt){
  require(NbClust)    #kmeans_k_optimization
  require(cluster)    #kmeans_k_optimization
  require(factoextra) #kmeans_k_optimization
  require(vegan)      #kmeans_k_optimization
  dt.org <- dt[,c('height','weight','age')]
  dt.scale <- scale(dt.org)
  if (method == 'NbClust'){
  ##NbClust --> 2 (takes long time)
  nb_clust <- NbClust(dt.scale,  distance = "euclidean",
                      min.nc=startNum, max.nc=endNum, method = "kmeans",
                      index = "all", alphaBeale = 0.1)
  return(nb_clust)
  } else if (method == 'GapStat'){
  ##Gap Statistic --> 4
  gap_clust <- clusGap(dt.scale, kmeans, endNum, B = iter, verbose = interactive())
  print(gap_clust)
  fviz_gap_stat(gap_clust)
  } else if (method == 'Calisky'){
  ##Calisky Criterion --> 2 (fast)
  ca_clust <- cascadeKM(dt.scale, startNum, endNum, iter = iter)
  print(ca_clust$results)
  calinski.best <- as.numeric(which.max(ca_clust$results[2,]))
  cat("Optimum number of clusters:", calinski.best, "\n")
  }
}

#' 3D Plot
#'
#' Getting basic 3D scatter plot under 3 methods. (Question 2, 3.a, 4.a)
#' @param method c('Origin', 'Kmeans', 'GMM'). If method is 'Origin', then the graph will show the plot in the Age, Height and Weight of
#' the training data. The same 3D plot that mark the centroid of each cluster will be generated under 'Kmeans'.
#' @param n Cluster numbers. Use NULL in 'Origin' method.
#' @param dt Dataframe. The column names must include 'heiht', 'weight', 'age', 'male', 'gender'.
#' @param colors The color code. The amount of color code should be doubled (n*2) while using 'Kmeans' method. Check this website for color codes: http://html-color.org/
#' @return 3D plot
#' @keywords Question 2, 3.a, 4.a: 3D Plot
#' @examples
#' plot_3d('Origin', NULL, train, c('#BF382A', '#0C4B8E'))
#' plot_3d('Kmeans', 3, train, c('#BF382A', '#0C4B8E','#f50521', '#7105f5', '#51f505', '#e9f505'))
#' plot_3d('GMM', 3, train, c('#BF382A', '#0C4B8E','#f50521'))
#' @export
plot_3d <- function(method, n, dt, colors){
  require(plotly)
  if (method == 'Origin'){
    plotly::plot_ly(dt, x = ~weight, y = ~height, z = ~age, color = ~Gender, colors = colors) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Weight'),
                          yaxis = list(title = 'Height'),
                          zaxis = list(title = 'Age')))
  } else if (method == 'Kmeans'){
    dt_with_kmeans_cluster <- cluster_calculation('Kmeans', dt, n, normalize = FALSE)
    plotly::plot_ly(dt_with_kmeans_cluster, x = ~weight, y = ~height, z = ~age, color = ~cluster, colors = colors) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Weight'),
                          yaxis = list(title = 'Height'),
                          zaxis = list(title = 'Age')))
  } else if (method == 'GMM'){
    dt_with_gmm_cluster <- cluster_calculation('GMM', dt, n, normalize = FALSE)
    plotly::plot_ly(dt_with_gmm_cluster, x = ~weight, y = ~height, z = ~age, color = ~cluster, colors = colors) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Weight'),
                          yaxis = list(title = 'Height'),
                          zaxis = list(title = 'Age')))
  }
}

#' Optimum k(KNN)
#'
#' Compute the optimum k in KNN given the datasets by using cross validation.
#' @param dt Dataframe. The column names must include 'heiht', 'weight', 'age', 'male', 'gender'.
#' @param krange Range of k. (>1)
#' @keywords k computation
#' @examples
#' knn_k_optimization(train, 1:10)
#' @export
knn_k_optimization <- function(dt, krange){
  require(caret)      #cross validation
  require(class)      #knn
  y <- dt$male
  x <- scale(dt[,c('height','weight','age')])
  idx <- createFolds(y, k = 10)
  res <- sapply(krange, function(k) {
    ##try out each version of k from 1 to 12
    res.k <- sapply(seq_along(idx), function(i) {
      ##loop over each of the 10 cross-validation folds
      ##predict the held-out samples using k nearest neighbors
      pred <- knn(train=x[ -idx[[i]], ],
                  test=x[ idx[[i]], ],
                  cl=y[ -idx[[i]] ], k = k)
      ##the ratio of misclassified samples
      mean(y[ idx[[i]] ] != pred)
    })
    ##average over the 10 folds
    mean(res.k)
  })
  k_table <- as.data.frame(cbind(krange, res))
  return(k_table$krange[which.min(k_table$res)])
}

#' Gender prediction
#'
#' Predict the data point is male or female depending on other data points in the cluster by using KNN. (Question 3.c, 4.c)
#' @param method c('Kmeans', 'GMM') Two algorithm
#' @param n Number of clusters
#' @param train Train dataset.
#' @param test Test dataset
#' @param print If print = TRUE, then function will print out confusion matrix, accuracy, false negative rate and false positive rate.
#' @param normalize If normalize = TRUE, normalized (scaled) data will be used to predict gender.
#' @return A dataframe with 1 obs. of 4 vairables, method, accuracy, false negative rate and false positive rate.
#' @keywords Question 3.c, 4.c: Gender prediction
#' @examples
#' kmeans_pred <- predict_gender('Kmeans', 3, train, test, print=FALSE, normalize=TRUE)
#' gmm_pred <- predict_gender('GMM', 3, train, test, print=TRUE, normalize=TRUE)
#'           Reference
#' Prediction  0  1
#' 0 94 17
#' 1 27 80
#' Accuracy: 0.7981651
#' False Negative Rate: 0.1752577
#' False Positive Rate: 0.2231405
#' @export
predict_gender <- function(method, n, train, test, print=TRUE, normalize=FALSE){
  if (method == 'Kmeans'){
    pred_actual_data <- data.frame()
    if (normalize==TRUE){
      dt1 <- cluster_calculation('PureKmeans', train, n, normalize=TRUE)
      dt2 <- cluster_calculation('PureKmeans', test, n, normalize=TRUE)
    } else if (normalize==FALSE){
      dt1 <- cluster_calculation('PureKmeans', train, n, normalize=FALSE)
      dt2 <- cluster_calculation('PureKmeans', test, n, normalize=FALSE)
    }
    for (i in 1:n){
      train_data <- dt1[dt1$cluster == i,]
      test_data <- dt2[dt2$cluster == i,]
      k <- knn_k_optimization(train_data, 1:15)
      pred <- knn(train = train_data[,1:3], test = test_data[,1:3], cl=train_data$male, k=k)
      temp <- data.frame(test_data, pred)
      pred_actual_data <- rbind(pred_actual_data, temp)
    }
    names(pred_actual_data)[4]<-"Actual"
    con <- confusionMatrix(pred_actual_data$pred, pred_actual_data$Actual, positive = "1")
    accuracy.knn <- con$overall[c('Accuracy')] #accuracy
    fnr.knn <- 1-con$byClass[c('Sensitivity')] #false negative rate
    fpr.knn <- 1-con$byClass[c('Specificity')] #false positive rate
    table <- data.frame(method,accuracy.knn,fnr.knn,fpr.knn)
    row.names(table) <- NULL
    names(table) <- c('method', 'accuracy', 'fnr', 'fpr')
    if (print == TRUE){
      print(con$table)
      cat("Accuracy:", accuracy.knn, "\n")
      cat("False Negative Rate:", fnr.knn, "\n")
      cat("False Positive Rate:", fpr.knn, "\n")
    }
    return(table)
  } else if (method == "GMM"){
    if (normalize==TRUE){
     gmm_dt1 <- cluster_calculation('GMM', train, n, normalize=TRUE)
     gmm_dt2 <- cluster_calculation('GMM', test, n, normalize=TRUE)
    } else if (normalize==FALSE) {
     gmm_dt1 <- cluster_calculation('GMM', train, n, normalize=FALSE)
     gmm_dt2 <- cluster_calculation('GMM', test, n, normalize=FALSE)
    }
    gmm_pred_actual_data <- data.frame()
    for (i in 1:n){
      gmm_train_data <- gmm_dt1[gmm_dt1$cluster == i,]
      gmm_test_data <- gmm_dt2[gmm_dt2$cluster == i,]
      k <- knn_k_optimization(gmm_train_data, 1:10)
      pred <- knn(train = gmm_train_data[,1:3], test = gmm_test_data[,1:3], cl=gmm_train_data$male, k=k)
      gmm_temp <- data.frame(gmm_test_data, pred)
      gmm_pred_actual_data <- rbind(gmm_pred_actual_data, gmm_temp)
    }
    names(gmm_pred_actual_data)[4]<-"Actual"
    gmm_con <- confusionMatrix(gmm_pred_actual_data$pred, gmm_pred_actual_data$Actual, positive = "1")
    gmm_accuracy.knn <- gmm_con$overall[c('Accuracy')] #accuracy
    gmm_fnr.knn <- 1-gmm_con$byClass[c('Sensitivity')] #false negative rate
    gmm_fpr.knn <- 1-gmm_con$byClass[c('Specificity')] #false positive rate
    gmm_table <- data.frame(method,gmm_accuracy.knn,gmm_fnr.knn,gmm_fpr.knn)
    row.names(gmm_table) <- NULL
    names(gmm_table) <- c('method', 'accuracy', 'fnr', 'fpr')
    if (print == TRUE){
      print(gmm_con$table)
      cat("Accuracy:", gmm_accuracy.knn, "\n")
      cat("False Negative Rate:", gmm_fnr.knn, "\n")
      cat("False Positive Rate:", gmm_fpr.knn, "\n")
    }
    return(gmm_table)
  }
}

#' Comparing GMM and Kmeans
#'
#' Comparing the prediction accuracy in Kmeans clusters and GMM clusters. (Question 5)
#' @param method c('Kmeans', 'GMM') Cluster algorithm
#' @param seed Integer. The clusters might change in Kmeans algorithm. Find how much the accuracy will change if we change the
#' seed.
#' @param n Number of clusters.
#' @param normalize If normalize = TRUE, use normalized (scaled) data in predicting process.
#' @param plot If plot = TRUE, plot the line chart.
#' @return A dataframe with 5 variables, method, accuracy, false negative rate, false positive rate and seed (or ClusterAmount).
#' GMM works better for this dataset. There are 2 reasons. Firstly, the results from 'Kmeans' will change if seed is changed. And accuracy in Kmeans
#' changes a lot, from around 0.4 to 0.99. Further, accuracy will decreasing by increase the number of clusters. And accuracy from GMM is higher than
#' it from Kmeans, until the cluster number reach around 8. In conclusion, GMM works better in small cluster amount.
#' @keywords Question 5: Comparing GMM and Kmeans
#' @examples
#' p <- compare_method('Cluster', 12, 2:10, normalize = FALSE)
#' q <- compare_method('Cluster', 12, 2:10, normalize = TRUE)
#' r <- compare_method('Seed', 1:50, 2, normalize = FALSE)
#' s <- compare_method('Seed', 1:50, 2, normalize = FALSE)
#' @export
compare_method <- function(method, seed, n, normalize=FALSE, plot=TRUE){
  if (method == 'Seed'){
    kmeans_change_seed <- data.frame()
    gmm_change_seed <- data.frame()
    for (i in seed){
      if (normalize == FALSE){
        kmeans_pred <- predict_gender('Kmeans', n, train, test, print=FALSE, normalize=FALSE)
        gmm_pred <- predict_gender('GMM', n, train, test, print=FALSE, normalize=FALSE)
      } else if (normalize == TRUE){
        kmeans_pred <- predict_gender('Kmeans', n, train, test, print=FALSE, normalize=TRUE)
        gmm_pred <- predict_gender('GMM', n, train, test, print=FALSE, normalize=TRUE)
      }
      kmeans_pred$seed <- i
      gmm_pred$seed <- i
      kmeans_change_seed <- rbind(kmeans_pred, kmeans_change_seed)
      gmm_change_seed <- rbind(gmm_pred, gmm_change_seed)
    }
    combined_seed_table <- rbind(kmeans_change_seed, gmm_change_seed)
    if (plot == TRUE){
      print(ggplot(combined_seed_table, aes(x = seed, y = accuracy, colour = method)) +  geom_line(size = 1))
    }
    return(combined_seed_table)
  } else if (method == 'Cluster'){
    kmeans_change_cluster <- data.frame()
    gmm_change_cluster <- data.frame()
    for (i in n){
      set.seed(seed)
      if (normalize == FALSE){
        kmeans_pred <- predict_gender('Kmeans', i, train, test, print=FALSE, normalize=FALSE)
        gmm_pred <- predict_gender('GMM', i, train, test, print=FALSE, normalize=FALSE)
      } else if (normalize == TRUE){
        kmeans_pred <- predict_gender('Kmeans', i, train, test, print=FALSE, normalize=TRUE)
        gmm_pred <- predict_gender('GMM', i, train, test, print=FALSE, normalize=TRUE)
      }
      kmeans_pred$ClusterAmount <- i
      gmm_pred$ClusterAmount <- i
      kmeans_change_cluster <- rbind(kmeans_pred, kmeans_change_cluster)
      gmm_change_cluster <- rbind(gmm_pred, gmm_change_cluster)
    }
    combined_cluster_table <- rbind(kmeans_change_cluster, gmm_change_cluster)
    if (plot == TRUE){
      print(ggplot(combined_cluster_table, aes(x = ClusterAmount, y = accuracy, colour = method)) +  geom_line(size = 1))
    }
    return(combined_cluster_table)
  }
}

