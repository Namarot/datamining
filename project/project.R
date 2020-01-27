library(dplyr)
library(skimr)
library(visdat)
library(cluster)
library(fpc)
library(factoextra)
library(ggplot2)
library(reshape2)


# library(mlbench)
# library(stats)
# library(e1071)
# library(LICORS)
# library(ggdendro)
# library(dbscan)
# library(FactoMineR)
# library(missMDA)
# library(tidyverse)
# library(Rtsne)

set.seed(123)

setwd("/home/utku/ceng464/CENG464-Project")
df<-read.csv('data/data2.csv', fileEncoding = "latin1")

str(df)
head(df)

# Missing values are "NULL" rather than NULL or NA, converting them to NA
df[(df == "NULL") | (df == "null") | (df == "Null") | (df == "NU") | (df == "NUL") | (df == "")] <- NA

# Converting mis-typed features to correct type
col_numeric <- c("LATITUDE", "LONGITUDE", "ADM_RATE", "ADM_RATE_ALL", "SATVR25", "SATVR75", "SATMT25", "SATMT75",
                 "SATWR25", "SATWR75", "SATVRMID", "SATMTMID", "SATWRMID", "ACTCM25",  "ACTCM75", "ACTEN25", "ACTEN75",
                 "ACTMT25", "ACTMT75", "ACTWR25", "ACTWR75", "ACTCMMID", "ACTENMID", "ACTMTMID", "ACTWRMID",
                 "SAT_AVG", "SAT_AVG_ALL")
for(i in col_numeric){
  df[, i] <- as.numeric(df[, i])
}

# dim(df)
# glimpse(df)
# summary(df)
# skim(df)
# vis_dat(df)

df_cols <- colnames(df)
# Removing features that are of no use, keeping UNITID for later comparison
cols_to_remove <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "ZIP", "INSTURL", "NPCURL", "ST_FIPS", "LOCALE2")

# Removing non-main campuses and related features
cols_nonmain <- c("NUMBRANCH", "ADM_RATE_ALL", "SAT_AVG_ALL")

df_cols <- df_cols[! df_cols %in% cols_to_remove]
df_cols <- df_cols[! df_cols %in% cols_nonmain]

df2 <- df[df_cols]

df_main <- df2[(df2$MAIN == 1),]
df_nonmain <- df2[(df2$MAIN == 0),]

# Removing the MAIN feature now that we're done with it
df_main <- df_main[-6]
df_nonmain <- df_nonmain[-6]

# dim(df_main)
# glimpse(df_main)
# summary(df_main)
skm <- skim(df_main)
skm
# vis_dat(df_main)

# Removing features with over 80% missing data
cols_not_missing <- skm$skim_variable[(skm$complete_rate > 0.6)]
cols_not_missing
df_main <- df_main[cols_not_missing]

# skim(df_main)
# vis_dat(df_main)

# Removing rows with missing data
df_main <- na.omit(df_main)

# summary(df_main)
# skim(df_main)
# vis_dat(df_main)

# write.csv(df_main, file="data/df_main.csv", row.names = FALSE)

# Features rejected due to high correlation (p > 0.9)
df_main_cols <- colnames(df_main)
rejected_cols <- c("HIGHDEG", "PREDDEG", "SCH_DEG")

df_main_cols <- df_main_cols[! df_main_cols %in% rejected_cols]
df_main <- df_main[df_main_cols]

# summary(df_main)
# skim(df_main)
# vis_dat(df_main)

cols_to_names <- c("CITY", "STABBR", "ACCREDAGENCY", "LOCALE", "CCBASIC", "CCUGPROF", "CCSIZSET", "HBCU", "PBI", "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", "MENONLY",
                   "WOMENONLY", "HCM2", "CONTROL", "REGION")
for(i in cols_to_names){
  df_main[, i] <- make.names(df_main[, i])
  df_main[, i] <- as.factor(df_main[, i])
}

# summary(df_main)
# skim(df_main)
# vis_dat(df_main)

df_main <- df_main[3:18]
# summary(df_main)
# skim(df_main)
# vis_dat(df_main)

gower.dist <- daisy(df_main, metric=c("gower"))

# Hierarchical Clustering

# Divisive
divisive.clust <- diana(gower.dist, diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")
wcss <- vector()
sils <- vector()
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = cutree(divisive.clust, k = i))
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, Divisive'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Value', main="Divisive")
# 7 seems good
divisive7 <- cutree(as.hclust(divisive.clust), k = 7)
divisive.cut <- hcut(gower.dist, k = 7, hc_func = "diana")
fviz_silhouette(divisive.cut)
divisive.results <- df_main %>%
  mutate(cluster = divisive.cut$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
divisive.results$the_summary
# Agglomerative
# ward.D linkage
aggl.clust.wardD=hclust(gower.dist, method='ward.D')
plot(aggl.clust.wardD, main = "Agglomerative, ward.D")

wcss <- vector()
sils <- vector()
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = cutree(aggl.clust.wardD, k = i))
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, ward.D'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Value', main="ward.D")
# 5 seems good

# complete linkage
aggl.clust.complete=hclust(gower.dist, method='complete')
plot(aggl.clust.complete, main = "Agglomerative, complete")

wcss <- vector()
sils <- vector()
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = cutree(aggl.clust.complete, k = i))
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, complete'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Value', main="complete")
# 13 is the obvious choice here


#
# Shows CC* features with X.2 (-2) meaning "Not Applicable" might be leading to lackluster results
#

df_final <- df_main[!(df_main$CCBASIC=="X.2" & df_main$CCSIZSET=="X.2" & df_main$CCUGPROF=="X.2"),]
write.csv(df_final, file="data/df_final.csv", row.names = FALSE)

vis_dat(df_final)

gower.dist <- daisy(df_final, metric=c("gower"))
# Hierarchical Clustering
library(dendextend)
# Divisive
divisive.clust <- diana(gower.dist, diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")
wcss <- vector()
sils <- vector()
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = cutree(divisive.clust, k = i))
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, Divisive'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Width', main="Silhouette, Divisive")
# 3 seems good
divisive.cut <- hcut(gower.dist, k = 3, hc_func = "diana")
divisive.cut$size
fviz_silhouette(divisive.cut)
divisive.results <- df_final %>%
  mutate(cluster = divisive.cut$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
divisive.results$the_summary

dendro <- as.dendrogram(divisive.clust)
dendro.col <- dendro %>%
  set("branches_k_color", k = 3, value = c("darkslategray", "gold3", "cyan3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)

ggd1 <- as.ggdend(dendro.col)

ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, Divisive, k = 3")

ggplot(ggd1, labels = F) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")
# 5
divisive.cut5 <- hcut(gower.dist, k = 5, hc_func = "diana")
divisive.cut5$size
fviz_silhouette(divisive.cut5)
divisive.results5 <- df_final %>%
  mutate(cluster = divisive.cut5$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
divisive.results5$the_summary

dendro <- as.dendrogram(divisive.clust)
dendro.col <- dendro %>%
  set("branches_k_color", k = 5, value = c("darkslategray", "gold3", "cyan3", "darkcyan", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)

ggd1 <- as.ggdend(dendro.col)

ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, Divisive, k = 5")

ggplot(ggd1, labels = F) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")
# Agglomerative
# ward.D linkage
aggl.clust.wardD=hclust(gower.dist, method='ward.D')
plot(aggl.clust.wardD, main = "Agglomerative, ward.D")

wcss <- vector()
sils <- vector()
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = cutree(aggl.clust.wardD, k = i))
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, ward.D'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Width', main="Silhouette, ward.D")
# 2 seems good
aggl.cut.wardD <- hcut(gower.dist, k = 2, hc_func = "hclust", hc_method = "ward.D")
aggl.cut.wardD$size
fviz_silhouette(aggl.cut.wardD)
aggl.wardD.results <- df_final %>%
  mutate(cluster = aggl.cut.wardD$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
aggl.wardD.results$the_summary

dendro <- as.dendrogram(aggl.clust.wardD)
dendro.col <- dendro %>%
  set("branches_k_color", k = 2, value = c("darkslategray", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)

ggd1 <- as.ggdend(dendro.col)

ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, Aggl. ward.D, k = 2")

ggplot(ggd1, labels = F) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

# 3
aggl.cut.wardD3 <- hcut(gower.dist, k = 3, hc_func = "hclust", hc_method = "ward.D")
aggl.cut.wardD3$size
fviz_silhouette(aggl.cut.wardD3)
aggl.wardD3.results <- df_final %>%
  mutate(cluster = aggl.cut.wardD3$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
aggl.wardD3.results$the_summary

dendro <- as.dendrogram(aggl.clust.wardD)
dendro.col <- dendro %>%
  set("branches_k_color", k = 3, value = c("darkslategray", "gold3", "cyan3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)

ggd1 <- as.ggdend(dendro.col)

ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, Aggl. ward.D, k = 3")

ggplot(ggd1, labels = F) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

# complete linkage
aggl.clust.complete=hclust(gower.dist, method='complete')
plot(aggl.clust.complete, main = "Agglomerative, complete")

wcss <- vector()
sils <- vector()
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = cutree(aggl.clust.complete, k = i))
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, complete'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Width', main="Silhouette, complete")
# 2
aggl.cut.complete <- hcut(gower.dist, k = 2, hc_func = "hclust", hc_method = "complete")
aggl.cut.complete$size
# 6
aggl.cut.complete <- hcut(gower.dist, k = 6, hc_func = "hclust", hc_method = "complete")
aggl.cut.complete$size
fviz_silhouette(aggl.cut.complete)

# k-medoids clustering
wcss <- vector()
sils <- vector() 
for(i in 2:20){
  stats <- cluster.stats(d = gower.dist, clustering = pam(gower.dist, diss = TRUE, k = i)$clustering)
  wcss[i-1] <- stats$within.cluster.ss
  sils[i-1] <- stats$avg.silwidth
}
plot(2:20, wcss, type="b", main=paste('Elbow Method, k-medoids'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
plot(2:20, sils, type="b", main = "Silhouette, k-medoids",xlab = "k (number of clusters)", ylab = "Average Silhouette Width")

# k = 2 
pam.fit <- pam(gower.dist, diss = TRUE, k = 2)
pam.results <- df_final %>%
  mutate(cluster = pam.fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam.results$the_summary
fviz_silhouette(pam.fit)


####

df_final$cluster <- divisive.cut$cluster
df_final$cluster <- make.names(df_final$cluster)  
df_final$cluster <- as.factor(df_final$cluster)
write.csv(df_final, file="data/df_final_clusters.csv", row.names = FALSE)


# Classification
library(caret)
library(pROC)
library(class)
library(C50)
# create test and training sets

inTrain = createDataPartition(y = df_final$cluster, p = .80, list = FALSE)
training = df_final[inTrain,]
testing = df_final[-inTrain,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats=5, savePredictions = TRUE, classProbs = TRUE)

# rpart
model1 <- train(cluster ~ ., data = training, 
                method = "rpart",
                trControl= fitControl,
                na.action = na.omit)
model1
pred1 <- predict(model1, testing[, 1:16])
confusionMatrix(data = pred1, reference = testing$cluster)

pred1_probs <- predict(model1, testing[, 1:16], type = "prob")
roc1 <- multiclass.roc(testing$cluster, pred1_probs, plot=TRUE, 
                       auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                       print.auc=TRUE)
roc1[["auc"]]

# knn

model2 <- train(cluster ~ ., data = training, 
                method = "knn",
                trControl= fitControl,
                tuneLength = 10,
                na.action = na.omit)
model2
pred2 <- predict(model2, testing[, 1:16])
confusionMatrix(data = pred2, reference = testing$cluster)

pred2_probs <- predict(model2, testing[, 1:16], type = "prob")
roc2 <- multiclass.roc(testing$cluster, pred2_probs, plot=TRUE, 
                       auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                       print.auc=TRUE)
roc2[["auc"]]

# c5.0

model3 <- train(cluster ~ ., data = training, 
                method = "C5.0",
                trControl= fitControl,
                na.action = na.omit)
model3
pred3 <- predict(model3, testing[, 1:16])
confusionMatrix(data = pred3, reference = testing$cluster)

pred3_probs <- predict(model3, testing[, 1:16], type = "prob")
roc3 <- multiclass.roc(testing$cluster, pred3_probs, plot=TRUE, 
                       auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, 
                       print.auc=TRUE)
roc3[["auc"]]

summary(model3$finalModel)
model3.foo <- C5.0(cluster ~ ., data = training, trials = 10)
model3.foo
pred3.foo <- predict(model3.foo, testing[, 1:16])
confusionMatrix(data = pred3.foo, reference = testing$cluster)
plot(model3.foo)
