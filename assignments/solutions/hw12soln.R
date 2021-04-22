library(cluster)

data <- test[,4:5]

dist_mat <- dist(data, method = 'euclidean')

hclust_cen <- hclust(dist_mat, method = 'centroid')
plot(hclust_cen)
rect.hclust(hclust_cen , k = 4, border = 2:6)

cut_cen <- cutree(hclust_cen, k = 2)
table(cut_cen, test$Gender)

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 5, border = 2:6)

cut_avg <- cutree(hclust_avg, k = 2)
table(cut_avg, test$Gender)

kCluster <- kmeans(data, 6, nstart = 20)
kCluster
age <- test$Age
for(i in 1:length(age)){
  if (age[i] <= 20) {age[i] = 20
  }else if (age[i] <= 30 && age[i]>20) {age[i] = 30
  }else if (age[i] <= 40 && age[i]>30) {age[i] = 40
  }else if (age[i] <= 50 && age[i]>40) {age[i] = 50
  }else if (age[i] <= 60 && age[i]>50) {age[i] = 60
  }else if (age[i] <= 70 && age[i]>60) {age[i] = 70
  }else {age[i] = 80
  }
}
table(kCluster$cluster, age)


table(kCluster)

k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=20,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

silhouette_score <- function(k){
  km <- kmeans(data, centers = k, nstart=20)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

