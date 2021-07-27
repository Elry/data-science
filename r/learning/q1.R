library(GGally);
library(cluster);
library(datasets);
library(corrplot);
library(tidyverse);
library(gridExtra);
library(factoextra);

# data exploration
help(USArrests);
head(USArrests);
help(USArrests);

# checking for missing data
any(is.na(USArrests));

# checking for non-numeric data
any(as.logical(sapply(USArrests, is.numeric )-1));

mydata <- data.frame(scale(USArrests));

summary(mydata);

sapply(mydata, var);
sapply(mydata, mean);
sapply(mydata, median);

# searching best k-group
RNGversion("3.5.2");
set.seed(1987);
wss <- 0;
bss <- 0;
for (i in 1:10) wss[i] = sum(kmeans(mydata,i, nstart=25)$withinss);
for (i in 1:10) bss[i] = sum(kmeans(mydata,i, nstart=25)$betweenss);

par(mfrow=c(1,2));
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares");
abline(v=4, col="red");

plot(1:10, bss, type="b", xlab="Number of Clusters", ylab="Betweenss groups sum of squares");
abline(v=4, col="red");

fit <- kmeans(mydata, 4, nstart=25);

# group sizes
set.seed(1987);
par(mfrow=c(2,2));

for (i in 3:6){
  fit <- kmeans(mydata, i, nstart=25)
  main_ = paste("Size groups for k=", i)
  barplot(fit$size, main = main_ )
};
fit = kmeans(mydata, 4, nstart=25)
fit;

# silhouette Method
set.seed(1987);
ss_m <- c(0);
for (i in 3:10){
  fit <- kmeans(mydata,  i, nstart=25)
  ss <- silhouette(fit$cluster, dist(mydata))
  ss_m[i] <-  mean(ss[,3])
};

par(mfrow=c(1,1));
plot(ss_m,
  type = "b", pch = 19, frame = FALSE, 
  xlab = "Number of clusters K",
  ylab = "Average Silhouettes",
  xlim=c(3,10)
);

abline(v=4,col="red");

set.seed(1987);
par(mfrow=c(2,2));
fit <- kmeans(mydata, 3, nstart=25);
ss <- silhouette(fit$cluster, dist(mydata));
plot(ss);

fit <- kmeans(mydata, 4, nstart=25);
ss <- silhouette(fit$cluster, dist(mydata));
plot(ss);

fit <- kmeans(mydata, 5, nstart=25);
ss <- silhouette(fit$cluster, dist(mydata));
plot(ss);

fit <- kmeans(mydata, 6, nstart=25);
ss <- silhouette(fit$cluster, dist(mydata));
plot(ss);

help(silhouette);

# group analysis
set.seed(1987);
fit <- kmeans(mydata, 4, nstart=25);

par(mfrow=c(1, 1));
ss <- silhouette(fit$cluster, dist(mydata));
plot(ss);
mean(ss[,3]);

# cluster visualization with main components
par(mfrow=c(1, 1));
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0);

# cluster visualization with two variables
p1 <- ggplot(mydata, aes(UrbanPop, Murder, color = as.factor(fit$cluster))) + geom_point();
p2 <- ggplot(mydata, aes(UrbanPop, Assault, color = as.factor(fit$cluster))) + geom_point();
p3 <- ggplot(mydata, aes(UrbanPop, Murder, color = as.factor(fit$cluster))) + geom_point();

grid.arrange(p1, p2, p3);

# clusters mean values
mydata$predict = fit$cluster

par(mfrow=c(2, 2));
for (i in 1:4){
  main_ = paste("Group =", i)
  barplot(sapply(mydata[mydata$predict==i,-5],mean),main=main_) 
};

# cluster visualization with different variables
ggpairs(
  cbind(mydata, Cluster = as.factor(fit$cluster)),
  columns=1:4,
  aes(colour=Cluster, alpha=0.5),
  lower=list(continuous="points"),
  upper=list(continuous="blank"),
  axisLabels="none", switch="both"
);

# PAM k-medoids
mydata_pam <- mydata[,-c(5)];

set.seed(1987);
fit <- pam(mydata_pam,4);

# PAM x K-Means
table(fit$cluster, mydata$predict);

ss <- silhouette(fit$cluster, dist(mydata_pam));

par(mfrow=c(1, 1));
plot(ss);
mean(ss[,3]);

# hierarchical cluster (hclust)
mydata_hclu <- mydata[,-c(5)];

set.seed(1987);

# distance matrix
d <- dist(mydata_hclu, method = "euclidean");

fit <- hclust(d, method = "complete");
# fit <- hclust(d, method = "single");
# fit <- hclust(d, method = "average");

# display dendogram
par(mfrow=c(1, 1));
plot(fit);

# cut tree into 4 clusters
groups <- cutree(fit, k=4);

# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit, k=4, border="red");

# silhouette
ss <- silhouette(groups, dist(mydata_hclu));
plot(ss);
mean(ss[,3]);

# comparing PAM and K-means with hclust
set.seed(1987);

mydata = mydata[,-c(5)];
mydata_pam = mydata[,-c(5)];
mydata_hclu = mydata[,-c(5)];


fitPAM <- pam(mydata_pam, 4);
hc <- hclust(d, method = "complete");
fitKmeans <- kmeans(mydata, 4, nstart=25);

table(fitPAM$cluster, groups);
table(fitKmeans$cluster, groups);

# see results like k-means 
par(mfrow=c(1, 1))
clusplot(
  mydata_hclu,
  groups,
  color=TRUE,
  shade=TRUE, 
  labels=2,
  lines=0
);

# centroid Plot against 1st 2 discriminant functions
plotcluster(mydata_hclu, groups)

ggpairs(
  cbind(mydata_hclu, Cluster=as.factor(groups)),
  columns=1:4,
  aes(colour=Cluster, alpha=0.5),
  lower=list(continuous="points"),
  upper=list(continuous="blank"),
  axisLabels="none", switch="both"
);

# using fviz_nbclust
mydata <- mydata[,-c(5)];

set.seed(1987);
fviz_nbclust(mydata, kmeans, method = "silhouette");

set.seed(1987);

# fviz_nbclust with hierarchical cluster
fviz_nbclust(
  mydata,
  FUNcluster = hcut,
  method = c("silhouette", "wss", "gap_stat"),
  diss = NULL,
  k.max = 20,
  nboot = 100,
  verbose = interactive(),
  barfill = "steelblue",
  barcolor = "steelblue",
  linecolor = "steelblue",
  print.summary = TRUE
)
