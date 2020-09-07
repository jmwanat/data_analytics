#MSDS 498
#Capstone

#https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
set.seed(1680) # for reproducibility

install.packages('ISLR')
install.packages('Rtsne')

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(plyr)

setwd("~/Desktop/R/")

# read cardio_train
cardio<- read.csv("Cardio_Train_Final.csv")
head(cardio)
colnames(cardio)
str(cardio)

glimpse(cardio)

# remove NA records
complete_cardio <- cardio[complete.cases(cardio),]
sum(sapply(complete_cardio, is.infinite)) ##0
sum(sapply(complete_cardio, is.na)) ##0

#modify indicators as factors
complete_cardio$smoke <- as.factor(complete_cardio$smoke) 
complete_cardio$alcohol <- as.factor(complete_cardio$alcohol) 
complete_cardio$active <- as.factor(complete_cardio$active) 
complete_cardio$cardio <- as.factor(complete_cardio$cardio) 

#rename factors
levels(complete_cardio$smoke) <- c("non-smoker", "smoker")
levels(complete_cardio$alcohol) <- c("non-drinker", "drinker")
levels(complete_cardio$active) <- c("not-active", "active")
levels(complete_cardio$cardio) <- c("non-cardio", "cardio")
glimpse(complete_cardio)

#only want rows where there is cardio disease
cardio_yes <- complete_cardio[which(complete_cardio$cardio == "cardio"),]

# Remove id, age_days, height, cardio, geo, zip before clustering
complete_cardio_used <- cardio_yes[, c(-1,-2,-5,-15,-16, -17, -18)]

glimpse(complete_cardio_used)

#this is computationally expensive and takes a few minutes
gower_dist2 <- daisy(complete_cardio_used,
                    metric = "gower")
#this results in a large dissimiliarity (575096655 elements, 4.3 Gb)

# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
summary(gower_dist2)

#575096655 dissimilarities, summarized :
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.1326  0.2024  0.2087  0.2747  0.7656 
#Metric :  mixed ;  Types = I, N, I, I, I, I, I, I, N, N, N 
#Number of objects : 33915

#this is computationally expensive and takes a few minutes
gower_mat2 <- as.matrix(gower_dist2)

# Output most similar pair
complete_cardio_used[
  which(gower_mat2 == min(gower_mat2[gower_mat2 != min(gower_mat2)]),
        arr.ind = TRUE)[1, ], ]

#      age_years gender weight_kg      BMI ap_hi ap_lo cholesterol gluc      smoke     alcohol active
#31483        60 female        60 20.51913   120    80           1    1 non-smoker non-drinker active
#12377        60 female        60 20.76125   120    80           1    1 non-smoker non-drinker active


# Output most dissimilar pair
complete_cardio_used[
  which(gower_mat2 == max(gower_mat2[gower_mat2 != max(gower_mat2)]),
        arr.ind = TRUE)[1, ], ]

#      age_years gender weight_kg      BMI ap_hi ap_lo cholesterol gluc      smoke     alcohol     active
#54247        61 female        55 25.10957   100   160           3    3 non-smoker non-drinker     active
#26603        40   male       144 47.56243   120    80           1    1     smoker     drinker not-active



# Calculate silhouette width for many k using PAM
sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit2 <- pam(gower_dist2,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit2$silinfo$avg.width
  
}


# Plot silhouette width (higher is better)
png(filename="silhouette_mixed.png")
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
plot(silhouette_mixed)
dev.off()

#we can see that 6 clusters is optimal

pam_fit2 <- pam(gower_dist2, diss = TRUE, k = 6)

pam_results2 <- complete_cardio_used %>%
  mutate(cluster2 = pam_fit2$clustering) %>%
  group_by(cluster2) %>%
  do(the_summary = summary(.))

#summary of clusters
pam_results2$the_summary

#medoids serve as exemplars of each cluster
complete_cardio_used[pam_fit2$medoids, ]


#One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, or t-SNE. #This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D #visualization. While it typically utilizes Euclidean distance, it has the ability to handle a custom distance metric like the one we #created above. In this case, the plot shows the  clusters that PAM was able to detect. 

tsne_obj <- Rtsne(gower_dist2, is_distance = TRUE)

#merge the clusters with the cardio_yes
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit2$clustering),
         id = cardio_yes$id)

#plot the clusters
png("tsne_plot.png", 1200, 480, res=96)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
#plot(tsne_plot)
dev.off()

#merge cluster information with cardio=1 rows based on id column in both sets
cardio_with_cluster <- merge(x=tsne_data, y=cardio_yes, by = "id", all = TRUE)

#compare cluster #1 summary with summary from above
check2 <- cardio_with_cluster %>%
  group_by(cluster) %>%
  do(summary_check = summary(.))

#summary of clusters
check2$summary_check
#yes, this matches the summary above

#save cardio_with_cluster to csv file
write.csv(cardio_with_cluster, "~/Desktop/R/cardio_with_cluster.csv")
