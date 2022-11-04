
library(tidyverse)
library(factoextra)
library(factoextra)

# Import the csv file (Boston_house_prices.csv) and explore it.
# Note that all counties in this data are in Boston, MA.
#====================== Write R code HERE ==========================

cars <- read.csv('RDC_Inventory_Core_Metrics_State_History.csv')
summary(cars)

print(cars$quality_flag)

drop_flag<-cars[cars$quality_flag== 0,]

head(drop_flag)
summary(drop_flag)

agregatte <-drop_flag%>%
  group_by(state)%>% 
  summarize(mean_lis=mean(median_listing_price),
            active_count= sum(active_listing_count),
            mdm=mean( median_days_on_market),
            pic=sum(price_increased_count),
            prc=sum(price_reduced_count),
            plc=sum(pending_listing_count),
            sqft_price = mean(median_listing_price_per_square_foot),
            sqft = mean(median_square_feet),
            total_count= sum(total_listing_count))
(agregatte)
ggplot(agregatte, aes(x=mean_lis, y=total_count)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)

ggplot(agregatte, aes(x=mean_lis, y=mdm)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)



regression2 <- lm(mean_lis~active_count,data = agregatte )
summary(regression2)

regression3 <- lm(mean_lis~mdm,data = agregatte )
summary(regression3)

regression4 <- lm(mean_lis~pic,data = agregatte )
summary(regression4)

regression5 <- lm(mean_lis~prc,data = agregatte )
summary(regression5)

regression6 <- lm(mean_lis~plc,data = agregatte )
summary(regression6)

regression7 <- lm(mean_lis~ sqft,data = agregatte )
summary(regression7)



regression8 <- lm(mean_lis~total_count,data = agregatte )
summary(regression8)

## sqft_price = mean(median_listing_price_per_square_foot),
regression1 <- lm(sqft_price~mean_lis,data = agregatte )
summary(regression1)

regression2 <- lm(sqft_price~active_count,data = agregatte )
summary(regression2)

regression3 <- lm(sqft_price~mdm,data = agregatte )
summary(regression3)

regression4 <- lm(sqft_price~pic,data = agregatte )
summary(regression4)

regression5 <- lm(sqft_price~prc,data = agregatte )
summary(regression5)

regression6 <- lm(sqft_price~plc,data = agregatte )
summary(regression6)

regression7 <- lm(sqft_price~ sqft,data = agregatte )
summary(regression7)

regression8 <- lm(sqft_price~total_count,data = agregatte )
summary(regression8)

#Results
#   mean_lis     Estimate Std. Error t value Pr(>|t|)  
#active_count 8.634e-03  1.425e-02   0.606    0.547  Multiple R-squared:  0.007434,	Adjusted R-squared:  -0.01282 
#mdm            -2639       1157  -2.280    0.027 *  Multiple R-squared:  0.09589,	Adjusted R-squared:  0.07744  
#pic         3.371e-01  2.676e-01    1.26    0.214   Multiple R-squared:  0.03137,	Adjusted R-squared:  0.0116 
#prc         2.727e-02  4.542e-02    0.60    0.551   Multiple R-squared:  0.007303,	Adjusted R-squared:  -0.01296 
#plc         1.866e-02  2.486e-02   0.751    0.457   Multiple R-squared:  0.0116,	Adjusted R-squared:  -0.008991 
#sqft           -86.35      75.32  -1.146  0.25716   Multiple R-squared:  0.02612,	Adjusted R-squared:  0.006249 
#total_count 6.102e-03  9.094e-03   0.671    0.505   Multiple R-squared:  0.009103,	Adjusted R-squared:  -0.01112
+             
  
  
  
  # sqft_price   Estimate Std. Error t value Pr(>|t|)  
  #mean_lis     6.909e-04  4.348e-05  15.887  < 2e-16 ** Multiple R-squared:  0.8374,	Adjusted R-squared:  0.8341  
  #active_count 1.687e-06  1.080e-05   0.156    0.877    Multiple R-squared:  0.0004978,	Adjusted R-squared:  -0.0199 
  #mdm          -1.6339     0.8888  -1.838   0.0721 .    Multiple R-squared:  0.06452,	Adjusted R-squared:  0.04543 
  #pic         1.101e-04  2.046e-04   0.538    0.593     Multiple R-squared:  0.005871,	Adjusted R-squared:  -0.01442 
  #prc         5.546e-06  3.441e-05   0.161    0.873     Multiple R-squared:  0.00053,	Adjusted R-squared:  -0.01987 
  #plc         4.575e-06  1.887e-05   0.242    0.809     Multiple R-squared:  0.001223,	Adjusted R-squared:  -0.01958 
  #total_count 1.305e-06  6.894e-06   0.189    0.851     Multiple R-squared:  0.0007309,	Adjusted R-squared:  -0.01966 
#sqft         -0.1967     0.0503  -3.910 0.000283 ***  Multiple R-squared:  0.2378,	Adjusted R-squared:  0.2223 


#selected variables

regression9 <- lm(sqft_price~mdm+sqft,data = agregatte )
summary(regression9)

set.seed(645) 

#mdm relationships

mdm_clusters <- agregatte%>%
  select(state, sqft_price,mdm)%>%
  drop_na

mdm_cluster <- agregatte%>%
  select(sqft_price,mdm)%>%
  drop_na

result<-kmeans(mdm_cluster, centers = 4, iter.max = 10, nstart = 25)
fviz_nbclust(mdm_cluster, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(mdm_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

my_countries_cluster2 <- mdm_clusters %>% 
  mutate(cluster_kmeans = result$cluster)

my_countries_cluster2[my_countries_cluster2$state== "North Carolina",]

my_countries_cluster2[my_countries_cluster2$cluster_kmeans== 2,]



ggplot(my_countries_cluster2, aes(x=sqft_price, y=mdm)) + 
  geom_point(size=3, aes(color=cluster_kmeans))




#sqft

sqft_clusters <- agregatte%>%
  select(state, sqft_price,sqft)%>%
  drop_na

sqft_cluster <- agregatte%>%
  select(sqft_price,sqft)%>%
  drop_na

result<-kmeans(sqft_cluster, centers = 5, iter.max = 10, nstart = 25)
fviz_nbclust(sqft_cluster, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(sqft_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

my_countries_cluster2 <- sqft_clusters %>% 
  mutate(cluster_kmeans = result$cluster)

my_countries_cluster2[my_countries_cluster2$state== "North Carolina",]

........






ggplot(my_countries_cluster2, aes(x=sqft_price, y=sqft)) + 
  geom_point(size=3, aes(color=cluster_kmeans))

