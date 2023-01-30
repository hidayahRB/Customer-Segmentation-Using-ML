# import libraries
library(clustertend)
library(hopkins)
library(factoextra)
library(NbClust)
library(cluster)

# import dataset
cust_profile <- read.csv("Mall_Customers.csv")
# check variables
names(cust_profile)

# rename columns
colnames(cust_profile)[4] <- "AnnualIncome" 
colnames(cust_profile)[5] <- "SpendingScore"
names(cust_profile)

# check null values
colSums(is.na(cust_profile))

# check for outliers using boxplot
boxplot(AnnualIncome ~ Gender,data = cust_profile)
boxplot(SpendingScore ~ Gender,data = cust_profile)

# Calculate the quartiles
Q1 <- quantile(cust_profile$AnnualIncome, 0.25)
Q3 <- quantile(cust_profile$AnnualIncome, 0.75)

# Calculate the IQR
IQR <- Q3 - Q1

# Define the upper and lower bounds
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Filter the data to keep only values within the bounds
filtered_data <- cust_profile[cust_profile$AnnualIncome >= lower_bound 
                              & cust_profile$AnnualIncome <= upper_bound, ]
cust_profile <- filtered_data

# re-check for outliers using boxplot
boxplot(AnnualIncome ~ Gender,data = filtered_data)

# drop unwanted column; customerID
demogdf = subset(cust_profile, select = -c(CustomerID))

# Age distribution with histogram
ggplot(demogdf,aes(x=Age)) +
  geom_histogram(fill="#69b3a2") +
  labs(title="Histogram: Dispersion of Age")

# Create the bar plot
# Count the occurrences of each label
counts <- table(demogdf$Gender)
barplot(counts, col = "blue", border = "black")


### cluster tendency


# Create seperate dataframe for cluster validation purpose
# drop unwanted column; customerID, Gender
prof_df = subset(demogdf, select = -c(Gender))
# rename column
names(prof_df)

# Random data generated from the df_custValid dataframe to show importance of cluster validation
random_df <- apply(prof_df, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)

# Standardize the data sets
prof_df <- scale(prof_df)
random_df <- scale(random_df)

res.pca <- prcomp(prof_df, scale = F)

# Plot faithful data set
fviz_pca_ind(res.pca, title = "PCA - Customer Profile dataset", 
             habillage = cust_profile$Gender,  palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")
# Plot the random df
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data", 
             geom = "point", ggtheme = theme_classic())

set.seed(123)
# K-means on customer profile dataset
# n = 2 because assume have 2 classes of gender
km.res1 <- kmeans(prof_df, 2)
fviz_cluster(list(data = prof_df, cluster = km.res1$cluster),
             title = "K-means Cluster Plot on Customer Profile Dataset",
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())
# Hierarchical clustering on the customer profile dataset
fviz_dend(hclust(dist(prof_df)),
          title = "Cluster Dendogram on Customer Profile Dataset",
          k = 2, k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)

# K-means on the random dataset
km.res2 <- kmeans(random_df, 2)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
             title = "Cluster Plot on Random dataset",
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(random_df)), k = 2, k_colors = "jco",  
          as.ggplot = TRUE, show_labels = FALSE)

# Compute Hopkins statistic for customer profile dataset
res_prof <- get_clust_tendency(prof_df, n = nrow(prof_df)-1, graph = FALSE)


# Compute Hopkins statistic for a random dataset
res_random <- get_clust_tendency(random_df, n = nrow(random_df)-1,
                          graph = FALSE)
res_prof$hopkins_stat
res_random$hopkins_stat

# Correlation-based distance method
res.dist <- get_dist(prof_df, method = "pearson")
# Visualize the dissimilarity matrix
fviz_dist(res.dist, lab_size = 8)

# Correlation-based distance method
random.dist <- get_dist(random_df, method = "pearson")
# Visualize the dissimilarity matrix
fviz_dist(random.dist, lab_size = 8)

# Elbow method
fviz_nbclust(prof_df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(prof_df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap Stats k-mean
fviz_nbclust(prof_df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
# Gap Stats hclust
fviz_nbclust(prof_df, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")



#### CLUSTERING: K_MEANS 

names(demogdf)

#Creating the customer clusters with KMeans
# consists of Age, AnnualIncome & SpendingScore only
# k = 6
k6<-kmeans(demogdf[, 3:4], 6, iter.max = 100, nstart=50,
           algorithm = "Lloyd")

#Printing the result
k6

#Showing the six KMeans clusters
clusplot(demogdf, k6$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

#Perform Principal Component Analysis
pcclust<-prcomp(demogdf[, 3:4], scale=FALSE)

#Checking the summary of the PCA model
summary(pcclust)

# Applying the PCA model on the data
pcclust$rotation[, 1:2]
#Set seed to 1
set.seed(1)

#Create a plot of the customers segments
ggplot(demogdf, aes(x = AnnualIncome , y = SpendingScore)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Low Income, High Spending", 
                                "High Income, High Spending", 
                                "Low Income, Low Spending", 
                                "High Income, Low Spending", 
                                "Medium Income, Medium Spending",
                                "Medium Income, Medium Spending")) +
  labs(x="Annual Income", y="Spending Score") +
  ggtitle("Segments of Mall X Customers", 
          subtitle = "Using K-means Clustering")

# create data frame
data <- demogdf
# cluster data using k-means
set.seed(123)
clusters <- kmeans(data[,3:4], 6)
# add cluster assignments to data
data$cluster <- clusters$cluster

# write data to CSV file
write.csv(data, "clustered_data.csv", row.names = FALSE)

#### CLUSTERING: AGGLOMRATIVE

# Perform the clustering
fit <- hclust(dist(demogdf[,3:4]), method = "ward.D2")

# Cut the tree into k clusters
agg_clusters <- cutree(fit, k = 6)
# Number of members in each cluster
table(agg_clusters)

plot(fit, hang = -1, cex = 0.6)
rect.hclust(fit, k = 6, border = 2:5)

library(factoextra)
fviz_cluster(list(data = demogdf[,3:4], cluster = agg_clusters))

# create data frame
data <- cust_profile
# Add the cluster assignments to the original data frame
data$cluster <- agg_clusters
# Write the data frame to a CSV file
write.csv(data, "agg_clust_data.csv", row.names = FALSE)

