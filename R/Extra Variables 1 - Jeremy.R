library(readxl)
library(tidyverse)
library(dplyr)

inventory <- read.csv("RDC_Inventory_Core_Metrics_State_History.csv")
inventory_nc <- inventory %>% filter(state_id == "NC")
inventory_nc <- inventory_nc %>% rename(month = month_date_yyyymm)
inventory_nc$month <- paste0(inventory_nc$month, "01")
inventory_nc$month <- as.Date(inventory_nc$month, format = "%Y%m%d")

#Compare median days on the market to the median listing price/sq ft for all the US 
ggplot(inventory, aes(x=median_listing_price_per_square_foot, y=median_days_on_market))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price/sq. ft by Active Listings")

#Compare median listing price/sq ft to the number of active listings for all of the US
ggplot(inventory, aes(x=active_listing_count, y=median_listing_price_per_square_foot))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("US Median Listing Price/sq. ft by Active Listings")

#Compare median days on market to median listing price/sq. ft for North Carolina
ggplot(inventory_nc, aes(x=median_listing_price_per_square_foot, y=median_days_on_market, color=month))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price/sq. ft by Active Listings")

#Compare median listing price/sq. ft to active listings for North Carolina
ggplot(inventory_nc, aes(x=active_listing_count, y=median_listing_price_per_square_foot, color=month))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price/sq ft. by Active Listings")

###################
#Variables by month
###################

#Compare median listing price/sq. ft to the month for North Carolina
ggplot(inventory_nc, aes(y=median_listing_price_per_square_foot, x=month, color=month))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price/sq. ft by Month")

#Compare median listing price/sq. ft to the month for North Carolina
ggplot(inventory_nc, aes(y=median_days_on_market, x=month, color=month))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price/sq. ft by Month")

# It appears that the month/year and median_days_on_market are strong variables for mean_listing_price_per_square_foot
regression1 <- lm(median_listing_price_per_square_foot ~ active_listing_count + median_days_on_market + month, data=inventory_nc)
summary(regression1)

# There is a correlation between month/year and median_days_on_market but median_listing_price_per_square_foot is
# a much better predictor
regression2 <- lm(median_days_on_market ~ active_listing_count + median_listing_price_per_square_foot + month, data=inventory_nc)
summary(regression2)
