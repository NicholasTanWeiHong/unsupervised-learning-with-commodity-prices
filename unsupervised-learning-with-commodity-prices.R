## ----install_packages, message = FALSE, warning = FALSE------------------
library(cluster)
library(dendextend)
library(knitr)
library(naniar)
library(Quandl)
library(tidyverse)
library(xts)


## ----import_quandl-------------------------------------------------------
# Load the Quandl API Key
Quandl.api_key("rn2xyN_hG9XfxN_9ibFJ")

# Define a vector of Commodity Names
names <- c("Brent_Crude", "WTI_Crude", "Dubai_Crude", "Canadian_Crude", "HHB_Gas", "NBP_Gas", "RBOB_Gasoline", "European_Naphtha", "GulfCoast_Jet", "Singapore_FO", "CME_Copper", "SHFE_Copper", "TSI_Iron_Ore", "SGX_Iron_Ore", "Newcastle_Coal", "Indonesian_Coal", "CME_Aluminium", "SHFE_Aluminium")

# Define a vector of Quandl Codes per Commodity
codes <- c("CHRIS/ICE_B1", "CHRIS/CME_CL1", "CHRIS/CME_DC1", "CHRIS/CME_WCC1", "CHRIS/CME_NG1", "CHRIS/ICE_M1","CHRIS/CME_RB1", "CHRIS/CME_UN1", "CHRIS/CME_GE1", "CHRIS/CME_UA1", "CHRIS/CME_HG1", "CHRIS/SHFE_CU1", "CHRIS/CME_TIO1", "CHRIS/SGX_FEF1", "CHRIS/ICE_NCF1", "CHRIS/CME_MCC1", "CHRIS/CME_ALI1", "CHRIS/SHFE_AL1")

# Create a data.frame to store commodity contract codes
commod_info <- data.frame(
  name = names,
  code = codes,
  stringsAsFactors = FALSE
)

# View the first 6 rows of commodities
head(commod_info)

## ----import_data---------------------------------------------------------
# Create an empty list to store historical prices per commodity: df_list
df_list <- list()

# Iterate through commod_info and add to df_list
for (i in 1:nrow(commod_info)) {
  df_list[[i]] <- Quandl(commod_info$code[i])
}


## ----merge_datasets------------------------------------------------------
# Reduce all data.frames in df_list into a single object: com
com <- df_list %>% 
  reduce(left_join, by = "Date") %>% 
  select(matches("Date|Settle")) %>% 
  select(-"Pre Settle.x", -"Pre Settle.y")

# Define column names as full commodity name
names(com) <- c("Date", commod_info$name)

# Impute missing NA values with LOCF
com <- na.locf(com)

# View the first 10 observations of com
head(com, n = 10)

## ----observe_str---------------------------------------------------------
# Observe the structure of the joined data.frame
str(com)


## ----scale_prices--------------------------------------------------------
# Scale all prices in the data.frame: scaled_com
scaled_com <- com %>%
  select(-Date) %>%
  scale() %>%
  as.data.frame() %>%
  mutate(Date = com$Date) %>%
  select(Date, everything())

# Observe the first 6 observations of scaled_com
head(scaled_com)

## ----convert_to_xts------------------------------------------------------
# Convert the data.frame to an xts object: com_xts
com_xts <- as.xts(scaled_com[, -1], order.by = as.Date(com$Date))

# Subset the xts object for data after 2016: com_xts_sub
com_xts_sub <- com_xts['2016-01/2019-01']

# Convert the daily series to a monthly series without OHLC: com_xts_monthly
com_xts_monthly <- to_period(com_xts_sub, period = "months", OHLC = FALSE)

# View the first 6 observations of the xts object
head(com_xts_monthly)

## ----viz_prices, fig.height=5, fig.width=8, fig.align="center"-----------
# Convert the xts object back to a data.frame: com_monthly_df
com_monthly_df <- as.data.frame(com_xts_monthly)

# Visualize the price action of all commodities
com_monthly_df %>%
  mutate(Date = as.Date(rownames(com_monthly_df))) %>%
  select(Date, everything()) %>%
  gather(Commodity, Price, -Date) %>%
  ggplot(aes(Date, Price, color = Commodity)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = "Historical Prices of Commodity Contracts",
      subtitle = "Scaled price action of 18 commodities from 2016 to 2019",
      caption = "Source: Quandl Continuous Futures Wiki",
      x = "Date",
      y = "Scaled Price"
    )


## ----transpose_df--------------------------------------------------------
# Transpose the data.frame s.t. Dates are columns: com_final
com_t <- as.data.frame(t(com_monthly_df))

head(com_t)


## ----generate_dendrogram, fig.height=5, fig.width=8, fig.align="center"----

# Define a theoretical number of clusters: k
K = 4

# Calculate a Distance Matrix between each Commodity: dist_com
dist_com <- dist(com_t, method = "euclidean")

# Generate an average linkage analysis: hc_com
hc_com <- hclust(dist_com, method = "average")

# Create a dendrogram from hc_com: dend_com
dend_com <- as.dendrogram(hc_com)

# Plot the dendrogram
dend_colored <- color_branches(dend_com, k = K)
plot(dend_colored, main = "Clustered Dendrogram of Commodity Price Action")

## ----tidy_with_hc--------------------------------------------------------
# Convert commodity IDs from rownames to a new column: Commodity
df_com <- rownames_to_column(as.data.frame(com_t), var = "Commodity")

# Create cluster assignments with h = 5
cut_com <- cutree(hc_com, k = K)

# Create a new column reflecting cluster assignments from cut_com
clustered_com_hc <- mutate(df_com, Cluster = cut_com)

# Create a tidy data.frame by gathering the Month and Values into two columns
tidy_com_hc <- gather(
  data = clustered_com_hc,
  key = Month,
  value = Price,
  -Commodity,
  -Cluster
)

# View the first 6 observations of tidy_com
head(tidy_com_hc)

## ----view_clusters_hc----------------------------------------------------
# View cluster assignments
sort(cut_com)


## ----viz_hc_results, fig.height=5, fig.width=8, fig.align="center"-------
# Visualize each time series by Cluster Assignment
tidy_com_hc %>% 
  mutate(Month = as.Date(Month)) %>% 
  ggplot(aes(Month, Price, color = factor(Cluster))) +
    geom_line(aes(group = Commodity)) +
    scale_color_manual(values = c(
      "#999999", 
      "#E69F00", 
      "#56B4E9", 
      "#009E73", 
      "#F0E442")) +
    labs(
      title = "Hierarchical Clusters of Traded Commodity Contracts",
      subtitle = "Based on Historical Price Action from 2016 to 2019",
      caption = "Source: Quandl Continuous Futures Wiki",
      x = "Date",
      y = "Scaled Price",
      color = "Cluster"
    ) +
    theme_classic()


## ----calculate_totwithinss-----------------------------------------------
# Estimate Total Within Sum of Squares per value of k
tot_withinss <- map_dbl(1:10, function(k) {
  model <- kmeans(x = com_t, centers = k)
  model$tot.withinss
})

tot_withinss

## ----viz_elbow-----------------------------------------------------------
# Create a data.frame containing both k and tot_withinss: elbow_df
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Visualize the Elbow Plot with ggplot
elbow_df %>% 
  ggplot(aes(k, tot_withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Elbow Plot of Estimated Commodity Clusters",
    subtitle = "Total Within Sum of Squares per value k",
    caption = "Source: Quandl Continuous Futures Wiki",
    x = "Num of Clusters",
    y = "Total Within Sum of Squares"
  ) +
  theme_minimal()


## ----calculate_sil-------------------------------------------------------
# Estimate Silhouette Width per unit k
sil_width <- map_dbl(2:10, function(k) {
  model <- pam(com_t, k = k)
  model$silinfo$avg.width
})

sil_width

## ----viz_sil-------------------------------------------------------------
# Create a data.frame containing both average silhouette wdith and k: sil_df
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

# Visualize the relationship between k and silhouette width with ggplot
sil_df %>% 
  ggplot(aes(k, sil_width)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Silhouette Width Plot of Estimated Commodity Clusters",
    subtitle = "Average Silhouette Width Per Number of Clusters k",
    caption = "Source: Quandl Continuous Futures Wiki",
    x = "Number of Clusters",
    y = "Average Silhouette Wdith"
  ) +
  theme_minimal()


## ----km_clustering-------------------------------------------------------
# Build a K-Means Model with k = 4
model_com_km <- kmeans(com_t, centers = 4)

# Extract the vector of cluster assignments from the model
clust_kmeans <- model_com_km$cluster

# View the results of cluster assignment with K-Means
clust_kmeans

## ----tidy_with_km--------------------------------------------------------
# Create a new column reflecting cluster assignments from clust_kmeans: clustered_com_km
clustered_com_km <- mutate(df_com, Cluster = clust_kmeans)

# Create a tidy data.frame by gathering the Month and Values into two columns
tidy_com_km <- gather(
  data = clustered_com_km,
  key = Month,
  value = Price,
  -Commodity,
  -Cluster
)

# View the first 6 observations of tidy_com_km
head(tidy_com_km)

## ----view_km_results-----------------------------------------------------
# View cluster assignments with K-Means Clustering
sort(clust_kmeans)

## ----viz_kmeans----------------------------------------------------------
# Visualize each time series by Cluster Assignment
tidy_com_km %>% 
  mutate(Month = as.Date(Month)) %>% 
  ggplot(aes(Month, Price, color = factor(Cluster))) +
    geom_line(aes(group = Commodity)) +
    scale_color_manual(values = c(
      "#999999", 
      "#E69F00", 
      "#56B4E9", 
      "#009E73", 
      "#F0E442")) +
    labs(
      title = "K-Means Clusters of Traded Commodity Contracts",
      subtitle = "Based on Historical Price Action from 2016 to 2019",
      caption = "Source: Quandl Continuous Futures Wiki",
      x = "Date",
      y = "Scaled Price",
      color = "Cluster"
    ) +
    theme_classic()


## ----include=FALSE-------------------------------------------------------
purl("unsupervised-learning-with-commodity-prices.Rmd")

