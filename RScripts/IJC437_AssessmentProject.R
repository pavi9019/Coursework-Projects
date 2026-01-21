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
  sil <- silhouette(km.out$cluster, dist(scaled_df)) #Calculating Silhouette widths for each k value
  if (!is.na(sil[1])) {
    sil_widths[i] <- mean(sil[, 3])  
  } else {
    sil_widths[i] <- NA  #saving Silhouette widths
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
  sil_pca <- silhouette(km.out.pca$cluster, dist(scaledpca_df)) #Calculating Silhouette widths for each k value
  if (!is.na(sil_pca[1])) {
    sil_widthspca[i] <- mean(sil_pca[, 3]) # Saving Silhouette widths
  } else {
    sil_widthspca[i] <- NA  
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
#Extracting Billboard ranked songs from dataset:
df_billboard<- df1 %>%
  filter(rank_group != "Non-Billboard")

#Plotting based on Cluster Id generated with out PCA:
ggplot(df_billboard, aes(x = cluster_id, fill = rank_group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(
    x = "Group",
    fill = "Cluster ID",
    title = "Distribution of Billboard Ranked Songs"
  ) +
  theme_minimal()

#Plotting based on Cluster Id generated with PCA:
ggplot(df_billboard, aes(x = cluster_id_pca, fill = rank_group)) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(
    x = "Group",
    fill = "Cluster ID",
    title = "Distribution of Billboard Ranked Songs"
  ) +
  theme_minimal()


#Plotting Clusters againts technical features:
#1.Danceability:
p1 <- ggplot(df_billboard, aes(x = song_id, y = danceability, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73",
                                "#F0E442", "#0072B2", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Danceability", title =" Raw Data",color = "Cluster ID") +
  theme(legend.position = "bottom")
  
p2 <- ggplot(df_billboard, aes(x = song_id, y = danceability, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#E69F00", "#56B4E9", "#009E73", 
                                "#F0E442", "#0072B2", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Danceability", title =" PCA Data",color = "Cluster ID") +
  theme(legend.position = "bottom")
  

#2.Energy
p3 <- ggplot(df_billboard, aes(x = song_id, y = energy, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Energy", title =" Raw Data",color = "Cluster ID") +
  theme(legend.position = "bottom")

p4 <- ggplot(df_billboard, aes(x = song_id, y = energy, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Energy", title =" PCA Data",color = "Cluster ID") +
  theme(legend.position = "bottom")


#3.Liveness
p5 <- ggplot(df_billboard, aes(x = song_id, y = liveness, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Liveness",title =" Raw Data", color = "Cluster ID") +
  theme(legend.position = "bottom")

p6 <- ggplot(df_billboard, aes(x = song_id, y = liveness, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Liveness",title =" PCA Data", color = "Cluster ID") +
  theme(legend.position = "bottom")

p1/p2|p3/p4|p5/p6

#4.Loudness
p7 <- ggplot(df_billboard, aes(x = song_id, y = loudness, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Loudness", title =" Raw Data",color = "Cluster ID") +
  theme(legend.position = "bottom")

p8 <- ggplot(df_billboard, aes(x = song_id, y = loudness, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Loudness",title =" PCA Data", color = "Cluster ID") +
  theme(legend.position = "bottom")


#5.Speechiness:
p9 <- ggplot(df_billboard, aes(x = song_id, y = speechiness, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Speechiness",title =" Raw Data", color = "Cluster ID") +
  theme(legend.position = "bottom")

p10 <- ggplot(df_billboard, aes(x = song_id, y = speechiness, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Speechiness",title =" PCA Data", color = "Cluster ID") +
  theme(legend.position = "bottom")



#6.Valence:
p11 <- ggplot(df_billboard, aes(x = song_id, y = valence, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Valence", title =" Raw Data",color = "Cluster ID") +
  theme(legend.position = "bottom")

p12 <- ggplot(df_billboard, aes(x = song_id, y = valence, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Valence", title =" PCA Data",color = "Cluster ID") +
  theme(legend.position = "bottom")

p7/p8|p9/p10|p11/p12

#7.Tempo:
p13 <- ggplot(df_billboard, aes(x = song_id, y = tempo, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Tempo", title =" Raw Data",color = "Cluster ID") +
  theme(legend.position = "bottom")

p14 <- ggplot(df_billboard, aes(x = song_id, y = tempo, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Tempo", title =" PCA Data",color = "Cluster ID") +
  theme(legend.position = "bottom")



#8.Instrumentalness:
p15 <- ggplot(df_billboard, aes(x = song_id, y = instrumentalness, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Instrumentalness",title =" Raw Data", color = "Cluster ID") +
  theme(legend.position = "bottom")

p16 <- ggplot(df_billboard, aes(x = song_id, y = instrumentalness, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Instrumentalness",title =" PCA Data", color = "Cluster ID") +
  theme(legend.position = "bottom")



#9.Acousticness:
p17 <- ggplot(df_billboard, aes(x = song_id, y = acousticness, color = cluster_id)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Acousticness",title =" Raw Data", color = "Cluster ID") +
  theme(legend.position = "bottom")

p18 <- ggplot(df_billboard, aes(x = song_id, y = acousticness, color = cluster_id_pca)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_manual(values = c("#D55E00", "#F0E442", "#56B4E9", "#9ACD32", 
                                "#A52A2A", "#556B2F", "#000000", "#CC79A7"))+
  theme_minimal() +
  labs(x = "Song ID", y = "Acousticness", title =" PCA Data",color = "Cluster ID") +
  theme(legend.position = "bottom")

p13/p14|p15/p16|p17/p18


