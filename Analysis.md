# IJC437: Clustering Analysis of Acoustic Music Features: Identifying Patterns in Billboard-Ranked Songs
**Unsupervised analysis of 20k songs to identify features predictive of Billboard Hot 100 Top 10 success**

## Research Questions
1.	RQ1: Can we identify distinct acoustic-based clusters in the song dataset, and how many clusters optimally represent the data?
2.	RQ2: Does applying PCA to acoustic features improve clustering quality compared to clustering on raw scaled features?
3.	RQ3: Are acoustic clusters meaningfully associated with Billboard chart performance?

## Methodology
- Data Loading and EDA
- Feature engineering
- Correlation analysis
- Standardization
- Dimensionality reduction
- K-Means Clustering
- CLuster optimization
- Model evaluation


## Key Findings
- PC1 and PC2 capture most of the variance in the data.
- Clustering on raw dataset and PCA transformed dataset, both produce clear clustering.
- Optimal clustering for raw data : **k=8** (WSS - Elbow analysis) and Calinski-Harabasz Index  	**3250.67**
- Optimal Clustering for PCA Transformed data is : **k=5** (WSS - Elbow analysis) and Calinski-Harabasz Index 	**13413.75**
- PCA transformed data produces tighter Clusters making it good for interpretability and separation.
- Raw data produces clusters for granular analysis.
- Cluster analysis across technical feature highlights Billboard ranking songs have Danceability, Energy, Loudness, Valence and Tempo
- Clusters 7,8,2 and 3 (for Raw data) and clusters 2,3 (for PCA transformed data) Contain the majority of the Billboard ranked songs.

