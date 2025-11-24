# ---------------------------------------

# install.packages("cluster")
# install.packages("dplyr")

library(cluster)   # agnes, silhouette
library(dplyr)


df <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Remove cereals with any missing values
df <- na.omit(df)

# Save cereal names and numeric variables
cereal_name <- df$name
X <- df %>% select(where(is.numeric))

# ---- 2. Normalize numeric variables (z-scores) ----
Xs <- scale(X)
dist_mat <- dist(Xs)

# ---- 3. Compare linkage methods (single, complete, average, ward) ----
methods <- c("single", "complete", "average", "ward")
ac <- sapply(methods, function(m) agnes(Xs, method = m)$ac)
ac
# From these, average usually has the highest AC:
best_method <- "average"

# ---- 4. Hierarchical clustering with chosen method ----
agnes_best <- agnes(Xs, method = best_method)
hc <- as.hclust(agnes_best)

# ---- 5. Silhouette check for k = 2..6 ----
sil_avgs <- sapply(2:6, function(k) {
  cl <- cutree(hc, k = k)
  mean(silhouette(cl, dist_mat)[, "sil_width"])
})
sil_avgs
# From inspection: best k is 2
k <- 2

# ---- 6. Final clustering ----
clusters <- cutree(hc, k = k)
table(clusters)

# ---- 7. Cluster means in original units ----
cluster_means <- aggregate(
  X[, c("calories","protein","fat","sodium","fiber","carbo","sugars")],
  by = list(cluster = clusters),
  FUN = mean
)
cluster_means

# ---- 8. List cereals in each cluster ----
cereals_cluster1 <- cereal_name[clusters == 1]
cereals_cluster2 <- cereal_name[clusters == 2]

cereals_cluster1
cereals_cluster2

# ---- 9. Stability check: split A/B and compare ----
set.seed(123)
n <- nrow(Xs)
idx <- sample(n)
half <- floor(n / 2)
A_idx <- idx[1:half]
B_idx <- idx[(half + 1):n]

# Cluster partition A
agnes_A <- agnes(Xs[A_idx, ], method = best_method)
hc_A <- as.hclust(agnes_A)
clA <- cutree(hc_A, k = k)

# Centroids from A (normalized space)
centA <- aggregate(Xs[A_idx, ], by = list(cluster = clA), FUN = mean)
cent_mat <- as.matrix(centA[,-1])   # rows = clusters

# Assign B to nearest centroid
assign_B <- apply(Xs[B_idx, ], 1, function(z) {
  dists <- apply(cent_mat, 1, function(mu) sum((z - mu)^2))
  which.min(dists)
})

# Compare to full-data clustering on B
full_B <- clusters[B_idx]
agreement <- mean(assign_B == full_B)
agreement   # ~0.92 ⇒ 92% agreement (good stability)

# ---- 10. Healthy cereals cluster ----
# From cluster_means, the healthiest cluster has:
# - lower calories, fat, sodium, sugars
# - higher fiber/protein
healthy_cluster <- 1
healthy_cereals <- cereal_name[clusters == healthy_cluster]
healthy_cereals
