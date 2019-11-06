#Loading the required Library
library(tidyverse)


#Loading the dataset
arg=read_csv('argentina.csv')
head(arg)

#GDP Per Population
arg$gdp_per_cap=arg$gdp/arg$pop

#Province with highest gdp
rich_province=arrange(arg,-gdp_per_cap)
top_n(rich_province,4)

#Province with highest Population
bigger_province=arrange(arg,-pop)
bigger_province=bigger_province[bigger_province$pop>1000000,][,c('province','pop')]

arg_matrix=arg
arg_matrix$province=NULL
arg_matrix=as.matrix(arg_matrix)
head(arg_matrix)


#Training the model for clustering
library(FactoMineR)
arg_pca=PCA(arg_matrix,scale.unit = TRUE)
print(arg_pca)


library(factoextra)
pca_var_plot=fviz_pca_var(arg_pca)
pca_var_plot

variance_first_two_components=sum(arg_pca$eig[1,3],arg_pca$eig[2,3])

fviz_pca_ind(arg_pca,title='Province-PCA')

set.seed(1234)

# Create an intermediate data frame with pca_1 and pca_2
argentina_comps <- tibble(pca_1 = arg_pca$ind$coord[ ,1],  
                          pca_2 = arg_pca$ind$coord[ ,2])

# Cluster the observations using the first 2 components and print its contents
argentina_km <- kmeans(argentina_comps,centers = 4,
                       nstart = 20,iter.max = 50))

cluster_as_factor=factor(argentina_km$cluster)

# Plot individulas colored by cluster
fviz_pca_ind(arg_pca,title = 'Cluster Province-PCA', 
             habillage =cluster_as_factor) 

library(ggrepel)
arg$Cluster=cluster_as_factor


# Scatterplot of gdp vs. cluster, colored by cluster
ggplot(arg, aes(gdp, Cluster, color = Cluster)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP")

# Scatterplot of GDP per capita vs. cluster, colored by cluster
ggplot(arg, aes(gdp_per_cap, Cluster, color = Cluster)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP per capita")

# Scatterplot of poverty vs. cluster, colored by cluster
ggplot(arg, aes(poverty,Cluster, color =Cluster)) +
  geom_point()+
  labs(x = "Cluster", y = "Poverty rate") +
  geom_text_repel(aes(label = province), show.legend = FALSE)

