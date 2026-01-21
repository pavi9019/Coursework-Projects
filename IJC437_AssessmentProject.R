install.packages("rgl")
install.packages(("patchwork"))
install.packages("reshape2")
install.packages("fpc") 
install.packages("clusterCrit")
install.packages("psych")
library(tidyverse)
library(MASS)
library(rgl)
library(patchwork)
library(reshape2)
library(fpc)
library(clusterCrit)
library(cluster)
library(psych)
library(RColorBrewer)



df1<-read.csv("acoustic_features_updated.csv")
View(df1)

nrow(df1)
ncol(df1)

head(df1)
tail(df1)

describe(df1)

# Create a categorical variable Rank group:
df1 <-   df1 %>% 
  mutate( 
    rank_group = case_when(
      is.na(max_rank) ~ "Non-Billboard",
      max_rank >= 1  & max_rank <= 10  ~ "Rank 1–10",
      max_rank >= 11 & max_rank <= 50  ~ "Rank 11–50",
      max_rank >= 51 & max_rank <= 100 ~ "Rank 51–100",
      TRUE ~ NA_character_
    ),
      rank_group = factor(
      rank_group,
      levels = c("Non-Billboard", "Rank 1–10", "Rank 11–50", "Rank 51–100")
    )
  ) %>%
  filter(!is.na(rank_group))


#extracting numeric columns from the dataframe:
num_df <- subset(df1, select= -c(song_id,max_rank,B_NB,duration_ms,rank_group,key,mode,time_signature))

#generating correlation matrix:
corr_mat <- cor(num_df, use = "pairwise.complete.obs")

#converting Matrix to Data frame for plotting:
corr_df <- melt(corr_mat, varnames = c("Var1", "Var2"), value.name = "cor")

#plotting the correlation matrix:
ggplot(corr_df, aes(Var1, Var2, fill = cor)) +
  geom_tile(colour="black") +
  scale_fill_gradient2(low="lightblue", high="darkblue",limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation matrix")+
  xlab("")+
  ylab("")


#Scaling the dataset:
scaled_df<-scale(num_df)

#performing PCA and obtaining scores pc1 and pc2
pca<-prcomp(scaled_df)
df1$pca1<-pca$x[,1]
df1$pca2<-pca$x[,2]


#scaling PCA columns
num_df1<- subset(df1, select=c(pca1,pca2))
scaledpca_df<-scale(num_df1)

#Clustering:
n_clusters <- 20

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)
sil_widths <- numeric(n_clusters)
sil_widthspca<-numeric(n_clusters)
set.seed(123)

#Clustering on Dataset without PCA:
# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(scaled_df, centers = i, nstart = 20, iter.max=20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
  sil <- silhouette(km.out$cluster, dist(scaled_df))
  if (!is.na(sil[1])) {
    sil_widths[i] <- mean(sil[, 3])  # 3rd column = sil_width
  } else {
    sil_widths[i] <- NA  # k=1 always gives NA silhouette
  }
}

#Plotting Silhouette widths for all Clusters:

sil_df <- data.frame(
  k = 1:n_clusters,
  sil_width = sil_widths
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    x = "Number of clusters (k)",
    y = "Average silhouette width",
    title = "Silhouette analysis on Original data"
  ) +
  theme_minimal()


# Plotting WSS for all cluster values

wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  geom_line() +
  labs(
    x = "Number of clusters (k)",
    y = "WSS",
    title = "WSS analysis on Original data"
  ) +
  theme_minimal()

scree_plot +geom_hline(
  yintercept = wss, 
  linetype = 'dashed')



##Clustering on Dataset after PCA:
# Look over 1 to n possible clusters
wss_pca <- numeric(n_clusters)
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out.pca <- kmeans(scaledpca_df, centers = i, nstart = 20, iter.max=20)
  # Save the within cluster sum of squares
  wss_pca[i] <- km.out.pca$tot.withinss
  sil_pca <- silhouette(km.out.pca$cluster, dist(scaledpca_df))
  if (!is.na(sil_pca[1])) {
    sil_widthspca[i] <- mean(sil_pca[, 3])  # 3rd column = sil_width
  } else {
    sil_widthspca[i] <- NA  # k=1 always gives NA silhouette
  }
}

#Plotting Silhouette widths for all Clusters:
sil_df_pca <- data.frame(
  k = 1:n_clusters,
  sil_widthpca = sil_widthspca
)
#Plotting the Sihouette widths for the Clusters:
ggplot(sil_df_pca, aes(x = k, y = sil_widthpca)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    x = "Number of clusters (k)",
    y = "Average silhouette width",
    title = "Silhouette analysis on PCA-transformed data"
  ) +
  theme_minimal()


# Plotting WSS for all cluster values

wss_df_pca <- tibble(clusters = 1:n_clusters, wss = wss_pca)

scree_plot <- ggplot(wss_df_pca, aes(x = clusters, y = wss_pca, group = 1)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  geom_line() +
  labs(
    x = "Number of clusters (k)",
    y = "WSS",
    title = "WSS analysis on PCA-Transformed data"
  ) +
  theme_minimal()

scree_plot +geom_hline(
  yintercept = wss_pca, 
  linetype = 'dashed')



# deciding on Cluster size:
set.seed(123)

km.out <- kmeans(scaled_df, centers = 8, nstart = 20, iter.max=20)
km.out.pca <- kmeans(scaledpca_df, centers = 5, nstart = 20, iter.max=20)

#Evaluating the Clusters with the Calinski-Harabasz Index:
#dataset Without PCA
ch1 <- calinhara(scaled_df, km.out$cluster)
ch1

#dataset with PCA
ch2 <- calinhara(scaledpca_df, km.out.pca$cluster)
ch2

#Evaluating the Clusters with the Dunn Index:


#Plotting Cluster distribution on the dataset:

#Dataset Without PCA
df1$cluster_id <- factor(km.out$cluster)
ggplot(df1, aes(x=cluster_id)) +
  geom_bar(fill="darkblue") +
  labs(
    x = "Cluster ID",
    y = "Count of Songs",
    title = "Cluster distribution of Original data"
  ) +
  theme_minimal()

#Dataset with PCA
df1$cluster_id_pca <- factor(km.out.pca$cluster)
ggplot(df1, aes(x=cluster_id_pca)) +
  geom_bar(fill="darkblue") +
  labs(
    x = "Cluster ID",
    y = "Count of Songs",
    title = "Cluster distribution of PCA Transformed data"
  ) +
  theme_minimal()

#Plotting orginal principle components against clusters:
ggplot(df1, aes(x = pca1, y = pca2, fill = cluster_id_pca)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(
    x = "PC 1",
    y = "PC 2",
    title = "Distribution of Clusters between Principal components"
  ) +
  theme_minimal() 


#Plotting Cluster against Billboard ranked songs to see if pattern emerges:
#Extracting Billboard ranked song from dataset:
df_billboard<- df1 |>
  filter(rank_group != "Non-Billboard")

#plotting based on Cluster Id generated with out PCA:
ggplot(df_billboard, aes(x = cluster_id, fill = rank_group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    x = "Group",
    fill = "Cluster ID",
    title = "Distribution of Billboard Ranked Songs"
  ) +
  theme_minimal()

#plotting based on Cluster Id generated with PCA:
ggplot(df_billboard, aes(x = cluster_id_pca, fill = rank_group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(
    x = "Group",
    fill = "Cluster ID",
    title = "Distribution of Billboard Ranked Songs"
  ) +
  theme_minimal()






#working only on Billboard data:
df_bb<-subset(df1, select= -c(pca1,pca2,cluster_id,cluster_id_pca))%>%
  filter(rank_group != "Non-Billboard")
View(df_bb)
num_df2<-subset(df_bb, select =-c(song_id,max_rank,B_NB,duration_ms,rank_group,key,mode,time_signature))
scaled_df2<-scale(num_df2)
#PCA:
pca2<-prcomp(scaled_df2)
df_bb$pca1<-pca2$x[,1]
View(df_bb)
df_bb$pca2<-pca2$x[,2]
ggplot(df_bb, aes(x = rank_group, y = pca1, fill = rank_group)) +
  geom_boxplot() +
  labs(
    x = "Group",
    y = "PC1 score",
    fill = "Group"
  ) +
  theme_minimal()

num_df3<- subset(df_bb, select=c(pca1,pca2))
scaledpca_df2<-scale(num_df3)


#Clustering without PCA:
bb.km.out <- kmeans(scaled_df2, centers = 10, nstart = 20,iter.max=20)
bb.km.out
#Clustering with PCA :
bb.km.out.pca <- kmeans(scaledpca_df2, centers = 10, nstart = 20,iter.max=20)
bb.km.out.pca

n_clusters <- 20

# Initialize total within sum of squares error: wss
bb.wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  bb.km.out <- kmeans(scaled_df2, centers = i, nstart = 20, iter.max=20)
  # Save the within cluster sum of squares
  bb.wss[i] <- bb.km.out$tot.withinss
}

# Look over 1 to n possible clusters
bb.wss_pca <- numeric(n_clusters)
for (i in 1:n_clusters) {
  # Fit the model: km.out
  bb.km.out.pca <- kmeans(scaledpca_df2, centers = i, nstart = 20, iter.max=20)
  # Save the within cluster sum of squares
  bb.wss_pca[i] <- bb.km.out.pca$tot.withinss
}
# Produce a scree plot
bb.wss_df <- tibble(clusters = 1:n_clusters, wss = bb.wss)
bb.wss_df_pca <- tibble(clusters = 1:n_clusters, wss = bb.wss_pca)
#without PCA:
scree_plot <- ggplot(bb.wss_df, aes(x = clusters, y = bb.wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  
  xlab('Number of clusters')

scree_plot +geom_hline(
  yintercept = bb.wss, 
  linetype = 'dashed',  )

#with PCA:
scree_plot <- ggplot(bb.wss_df_pca, aes(x = clusters, y = bb.wss_pca, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  
  xlab('Number of clusters')

scree_plot +geom_hline(
  yintercept = bb.wss_pca, 
  linetype = 'dashed',  )

# deciding on Cluster size:

set.seed(123)
# Build model with k clusters: km.out
bb.km.out <- kmeans(scaled_df2, centers = 6, nstart = 20, iter.max=20)
bb.km.out.pca <- kmeans(scaledpca_df2, centers = 4, nstart = 20, iter.max=20)

bb_ch1 <- calinhara(scaled_df2, bb.km.out$cluster)
bb_ch1

bb_ch2 <- calinhara(scaledpca_df2, bb.km.out.pca$cluster)
bb_ch2

df_bb$cluster_id <- factor(bb.km.out$cluster)
ggplot(df_bb, aes(x=cluster_id)) +
  geom_bar() +
  xlab("cluster_id") 

df_bb$cluster_id_pca <- factor(bb.km.out.pca$cluster)
ggplot(df_bb, aes(x=cluster_id_pca)) +
  geom_bar() +
  xlab("cluster_id after PCA") 


ggplot(df_bb, aes(x = cluster_id, fill = rank_group)) +
  geom_bar() +
  labs(
    x = "Group",
    fill = "Cluster ID"
  ) +
  theme_minimal()

ggplot(df_bb, aes(x = cluster_id_pca, fill = rank_group)) +
  geom_bar() +
  labs(
    x = "Group",
    fill = "Cluster ID"
  ) +
  theme_minimal()

ggplot(df_bb, aes(x = cluster_id_pca, y =max_rank )) +
  geom_boxplot() +
  labs(
    x = "Group",
    y = "PC1 score",
    fill = "Group"
  ) +
  theme_minimal()





