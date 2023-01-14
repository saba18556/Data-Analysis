data <- read.csv(file.choose(), header=TRUE)
str(data)

sum(is.na(data))

final <- data
final$ID <- NULL
final$Date_Cust <- NULL

means = apply(final, 2, mean)
sds = apply(final,2,sd)
nor = scale(final, center = means, scale = sds)
nor <- data.frame(nor)
View(nor)
wss <- (nrow(nor)-1)*sum(apply(nor,2,var)) #taking n-1 rows to cal wss
for(i in 2:20) wss[i] <- sum(kmeans(nor, centers = i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#clustering
set.seed(123)
kc2 <- kmeans(nor[,c(6,18,19,20,21,22)],2)
kc2
set.seed(123)
kc3 <- kmeans(nor[,c(6,18,19,20,21,22)],3)
kc4 <- kmeans(nor[,c(6,18,19,20,21,22)],4)
kc5 <- kmeans(nor[,c(6,18,19,20,21,22)],5)
kc6<- kmeans(nor[,c(6,18,19,20,21,22)],6)
kc7 <- kmeans(nor[,c(6,18,19,20,21,22)],7)
kc8 <- kmeans(nor[,c(6,18,19,20,21,22)],8)
kc9 <- kmeans(nor[,c(6,18,19,20,21,22)],9)

library(cluster)
# very poor representation since the variability is 32.7%
clusplot(data, kc7$cluster, color = TRUE, shade = TRUE, labels = 2,lines = 0)

#choosing target market

library(dplyr)
member <- final[,c(6,18,19,20,21,22)]
member%>%
  mutate(Cluster =kc7$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(mean)
