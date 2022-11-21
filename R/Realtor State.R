
library(tidyverse)
library(factoextra)
library(factoextra)
library(openxlsx)


#----------------------------------------------------------
#Realtor State Data

realstate_state <- read.csv('RDC_Inventory_Core_Metrics_State_History.csv')
summary(realstate_state)

#Clean Data by quality flag
drop_flag<-realstate_state[realstate_state$quality_flag== 0,]

#Group information by state
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

ggplot(agregatte , aes(x=mean_lis, y=total_count)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)

ggplot(agregatte, aes(x=mean_lis, y=mdm)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)


#Variable regressions by mean listing price
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



#Variable regressions by sqft price
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

# Mean List price regression variables

# Variable    Estimate   Std. Error t value Pr(>|t|) 

#active_count 8.634e-03  1.425e-02   0.606   0.547     Multiple R-squared:  0.007434,	Adjusted R-squared:  -0.01282 
#mdm          -2639      1157        -2.280  0.027 *   Multiple R-squared:  0.09589,	Adjusted R-squared:  0.07744  
#pic          3.371e-01  2.676e-01   1.26    0.214     Multiple R-squared:  0.03137,	Adjusted R-squared:  0.0116 
#prc          2.727e-02  4.542e-02   0.60    0.551     Multiple R-squared:  0.007303,	Adjusted R-squared:  -0.01296 
#plc          1.866e-02  2.486e-02   0.751   0.457     Multiple R-squared:  0.0116,	  Adjusted R-squared:  -0.008991 
#sqft         -86.35     75.32      -1.146   0.25716   Multiple R-squared:  0.02612,	Adjusted R-squared:  0.006249 
#total_count  6.102e-03  9.094e-03   0.671   0.505     Multiple R-squared:  0.009103,	Adjusted R-squared:  -0.01112
            
  
  
  
# Variable    Estimate   Std. Error t value Pr(>|t|) 

#mean_lis      6.909e-04  4.348e-05   15.887  < 2e-16 ** Multiple R-squared:  0.8374,	Adjusted R-squared:  0.8341  
#active_count  1.687e-06  1.080e-05   0.156    0.877     Multiple R-squared:  0.0004978,	Adjusted R-squared:  -0.0199 
#mdm           -1.6339    0.8888      -1.838   0.0721 .  Multiple R-squared:  0.06452,	Adjusted R-squared:  0.04543 
#pic           1.101e-04  2.046e-04   0.538    0.593     Multiple R-squared:  0.005871,	Adjusted R-squared:  -0.01442 
#prc           5.546e-06  3.441e-05   0.161    0.873     Multiple R-squared:  0.00053,	Adjusted R-squared:  -0.01987 
#plc           4.575e-06  1.887e-05   0.242    0.809     Multiple R-squared:  0.001223,	Adjusted R-squared:  -0.01958 
#total_count   1.305e-06  6.894e-06   0.189    0.851     Multiple R-squared:  0.0007309,	Adjusted R-squared:  -0.01966 
#sqft          -0.1967    0.0503      -3.910   0.000283  Multiple R-squared:  0.2378,	Adjusted R-squared:  0.2223 


#selected variables
#Found that variables for SQFT Price, median days in market and Sqft seems to be
#the stronger predictors based on R squared data (Althought not extremely significant)

regression9 <- lm(sqft_price~log(mdm)+log(sqft),data = agregatte )
summary(regression9)

set.seed(645) 


total_clusters <- agregatte%>%
  select(state, sqft_price,mdm,sqft)%>%
  drop_na
total_clusters

#--------------------------------------------------------- 

#mdm relationships
mdm_agreggate <- agregatte%>%
  select(sqft_price,mdm)%>%
  drop_na

mdm_result<-kmeans(mdm_agreggate, centers = 4, iter.max = 10, nstart = 25)

fviz_nbclust(mdm_agreggate, kmeans, method = "wss") +
labs(subtitle = "Elbow method")

fviz_nbclust(mdm_agreggate, kmeans, method = "silhouette") +
labs(subtitle = "Silhouette method")

mdm_cluster_Agreggate <- total_clusters %>% 
mutate(cluster_kmeans = mdm_result$cluster)

# Cluster Identification
mdm_cluster_Num<- mdm_cluster_Agreggate[mdm_cluster_Agreggate$state== "North Carolina",]

nc_mdm_cluster<- mdm_cluster_Agreggate[mdm_cluster_Agreggate$cluster_kmeans== mdm_cluster_Num$cluster_kmeans,]

ggplot(mdm_cluster_Agreggate, aes(x=sqft_price, y=mdm)) + 
  geom_point(size=3, aes(color=cluster_kmeans))+
  labs(title = "Median Day Market State Clustering")


#----------------------------------------------------------


#sqft relationships
sqft_agreggate <- agregatte%>%
  select(sqft_price,sqft)%>%
  drop_na



result<-kmeans(sqft_agreggate, centers = 5, iter.max = 10, nstart = 25)

fviz_nbclust(sqft_agreggate, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(sqft_agreggate, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

sqft_cluster_Agreggate <- total_clusters %>% 
  mutate(cluster_kmeans = result$cluster)



#North Carolina on cluster #2
sqft_cluster_Num<- sqft_cluster_Agreggate[sqft_cluster_Agreggate$state== "North Carolina",]

nc_sqft_cluster<- sqft_cluster_Agreggate[sqft_cluster_Agreggate$cluster_kmeans== sqft_cluster_Num$cluster_kmeans,]

ggplot(sqft_cluster_Agreggate, aes(x=sqft_price, y=sqft)) + 
  geom_point(size=3, aes(color=cluster_kmeans))+
  labs(title = "SQFT State Clustering")

#----------------------------------------------------------

#Clustering based on both variables 

total_agreggate <-total_clusters%>%
  select(sqft_price,sqft,mdm)%>%
  drop_na()
  


result<-kmeans(total_agreggate, centers = 5, iter.max = 10, nstart = 25)

fviz_nbclust(total_agreggate, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(total_agreggate, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

total_cluster_Agreggate <- total_clusters %>% 
  mutate(cluster_kmeans = result$cluster)

#North Carolina on cluster #2
total_cluster_Num<-total_cluster_Agreggate[total_cluster_Agreggate$state== "North Carolina",]

nc_total_cluster<- total_cluster_Agreggate[total_cluster_Agreggate$cluster_kmeans== total_cluster_Num$cluster_kmeans,]

ggplot(total_cluster_Agreggate, aes(x=sqft, y=mdm)) + 
  geom_point(size=3, aes(color=cluster_kmeans))+
  labs(title = "Both Variables State Clustering")


#There is a relationship between median days in the market and sqft?

regression10<- lm(sqft~mdm,data = agregatte )
summary(regression10)

#----------------------------------------------------------
#Cluster Comparison

#MDM Cluster
nc_mdm_cluster




#SQFT Cluster
nc_sqft_cluster

#Total Cluster
nc_total_cluster

#Here we are checking the intersection of similar states shared by the 3 clusters 

common_elements <- Reduce(intersect, list(nc_mdm_cluster$state, nc_sqft_cluster$state, nc_total_cluster$state)) 


#Common Elements 
#Colorado 
#Idaho
#Montana 
#North Carolina 

#----------------------------------------------------------
#Common Cluster Analysis

common_cluster <- agregatte[agregatte$state %in% c(common_elements), ]  
common_cluster

ggplot(common_cluster , aes(x=sqft, y=sqft_price)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)

ggplot(common_cluster , aes(x=mdm, y=sqft_price)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)

ggplot(common_cluster , aes(x=mdm, y=sqft)) + 
  geom_point(size=3)+
  geom_text(aes(label = state), size=4)

#----------------------------------------------------------
      




#----------------------------------------------------------
#Realtor State history (Evolution of studied variables on previous regresssions)
#for NC and the states clustered allongside it

realstate_state <- read.csv('RDC_Inventory_Core_Metrics_State_History.csv')
head(realstate_state)
summary(realstate_state)

realstate_state<-realstate_state[realstate_state$quality_flag == 0,]


NC_history <- realstate_state[realstate_state$state_id == 'NC',]

Colorado_history <- realstate_state[realstate_state$state_id == 'CO',]

Idaho_history <- realstate_state[realstate_state$state_id == 'ID',]

Montana_history <- realstate_state[realstate_state$state_id == 'MT',] 

clustered_history <- realstate_state[realstate_state$state_id %in% c('NC','CO','ID', 'MT'),]
clustered_history


#Listing Price 

ggplot(NC_history, aes(x=month_date_yyyymm, y=median_listing_price))+
  geom_point(aes(color=state_id))+
  labs(title = "NC Median Listing price history")

ggplot(clustered_history, aes(x=month_date_yyyymm, y=median_listing_price))+
  geom_point(aes(color=state_id))+
  labs(title = "Clustered Median Listing price history")


#Average Listing Price 
ggplot(NC_history, aes(x=month_date_yyyymm, y=average_listing_price))+
  geom_point()+
  labs(title = "NC Average listing price History")

ggplot(clustered_history, aes(x=month_date_yyyymm, y=average_listing_price))+
  geom_point(aes(color=state_id))+
  labs(title = "Clustered Average listing price History")


#Active Count 
ggplot(NC_history, aes(x=month_date_yyyymm, y=active_listing_count))+
  geom_point()+
  labs(title = "NC Active Listing Count History")

ggplot(clustered_history, aes(x=month_date_yyyymm, y=active_listing_count))+
  geom_point(aes(color=state_id))+
  labs(title = "Clustered Active Listing Count History")

#Median Days on market

ggplot(NC_history, aes(x=month_date_yyyymm, y=median_days_on_market))+
  geom_point()+
  labs(title = "NC Median Days on market History")

ggplot(clustered_history, aes(x=month_date_yyyymm, y=median_days_on_market))+
  geom_point(aes(color=state_id))+
  labs(title = "Clustered Median Days on market History")

#Price Increase Count

ggplot(NC_history, aes(x=month_date_yyyymm, y=price_increased_count))+
  geom_point()+
  labs(title = "NC Price Increased count history")

ggplot(clustered_history, aes(x=month_date_yyyymm, y=price_increased_count))+
  geom_point(aes(color=state_id))+
  labs(title = "Clustered Price Increased count history")
#Price reduced count

ggplot(NC_history, aes(x=month_date_yyyymm, y=price_reduced_count))+
  geom_point()+
  labs(title = "NC Price Reduced Count History")

ggplot(clustered_history, aes(x=month_date_yyyymm, y=price_reduced_count))+
  geom_point(aes(color=state_id))+
  labs(title = "Clustered Price Reduced Count History")



#----------------------------------------------------------

library(openxlsx)

zillow <-read.xlsx("Archived Zillow Data.xlsx")

head(zillow)
summary(zillow)

zillow<-zillow%>%
  mutate(index = zillow$`Home.Value.Index.Monthly.by.State.(all.homes)`)%>%
  select(Date, RegionName, index)

head(zillow)

grouped<-zillow%>%
  group_by(RegionName)

NC_Zillow<-zillow[zillow$RegionName== 'North Carolina',]
head(NC_Zillow)
  
ggplot(NC_Zillow, aes(x=Date, y=index))+
  geom_point()+
  labs(title = "NC Zillow value Index")


       



