import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import time

# Start timing
start_time = time.time()

# Maximum number of clusters to consider
MaxK = 10
k = range(2, MaxK + 1)

# Load the data
df = pd.read_csv("computers.csv", sep=';')
# Delete the 'id' column (similar to dataframe index)
df.drop('id', axis=1, inplace=True)
# Substitute yes/no answers into 1/0
df['cd'] = np.where(df['cd'] == 'yes', 1, 0)
df['multi'] = np.where(df['multi'] == 'yes', 1, 0)
df['premium'] = np.where(df['premium'] == 'yes', 1, 0)
# Copy column prices and speeds (useful for later)
prices = df['price'].copy()
speeds = df['speed'].copy()


# Get renormalized data
renorm = StandardScaler()
renormalized = renorm.fit_transform(df)
# Substitute new data into dataframe
df.loc[:, :] = renormalized

# Create a list to store the errors of each KMeans
inertias = []
#Loop for every cluster number
for i in range(len(k)):
    # Set the model
    KM = KMeans(n_clusters=k[i])
    # Fit the data to the model
    KM.fit(df)
    # Estimate the errors
    inertias.append(KM.inertia_)


# Take k=4 as the optimal parameter
BestK = 4
# Fit KMeans for the optimal parameter
BestKMeans = KMeans(n_clusters=BestK).fit(df)
# Get the clusters
BestClusters = BestKMeans.predict(df)
# Calculate the centroids
centroids = BestKMeans.cluster_centers_

#Use Principal Component Analysis to get 2D coordinates
PCA_fit = PCA(n_components=2).fit(df)
PCA_coords = PCA_fit.transform(df)
centroid_coords = PCA_fit.transform(centroids)

# Calculate the average price of each cluster
avg_prices = np.zeros(BestK)
num_points = np.zeros(BestK)
for i in range(0, len(df.index)):
    avg_prices[BestClusters[i]] += prices[i]
    num_points[BestClusters[i]] += 1
avg_prices = avg_prices / num_points
price_max = np.amax(avg_prices)
index_max = list(avg_prices).index(price_max)+1

# Stop timing
stop_time = time.time()

# Print the biggest average price
print("The biggest average price is: " + str(price_max) + " which corresponds with cluster number: " + str(index_max))

# Print time
print("Serial execution time: " + str(stop_time - start_time))

# PLOTTING THE RESULTS
# Plot the elbow graph
plt.plot(k, inertias, marker='o')
plt.xlabel('Number of Clusters')
plt.ylabel('Inertia')
plt.title('Elbow Graph')
plt.show()

# Plot the data in 2D grouped by clusters
plt.scatter(PCA_coords[:,0], PCA_coords[:,1], c=BestClusters)
plt.scatter(centroid_coords[:,0], centroid_coords[:,1], marker='*', c='red')
plt.xlabel('PCA x')
plt.ylabel('PCA y')
plt.title('Clusters in 2D')
plt.show()

# Plot the heatmap
fig, ax = plt.subplots()
im = ax.imshow(centroids)
ax.set_xticks(np.arange(len(df.columns)))
ax.set_yticks(np.arange(len(centroids)))
ax.set_xticklabels(df.columns)
ax.set_yticklabels(["Centroid 0", "Centroid 1", "Centroid 2", "Centroid 3"])
plt.colorbar(im, orientation='horizontal')
plt.title('Heatmap')
fig.tight_layout()
plt.show()
