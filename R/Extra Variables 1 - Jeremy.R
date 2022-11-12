library(readxl)
library(tidyverse)
library(dplyr)

inventory <- read.csv("RDC_Inventory_Core_Metrics_State_History.csv")
inventory <- inventory %>% drop_na()
inventory <- inventory %>% rename(month = month_date_yyyymm)
inventory$month <- paste0(inventory$month, "01")
inventory$month <- as.Date(inventory$month, format = "%Y%m%d")
inventory_nc <- inventory %>% filter(state_id == "NC")

#Compare median days on the market to the median listing price/sq ft for all the US 
ggplot(inventory, aes(x=median_listing_price_per_square_foot, y=median_days_on_market))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("US Median Days on Market by Median Listing Price / Sq. ft")+
  xlab("Median Listing Price / Sq. ft")+
  ylab("Median Days on Market")

#Compare median listing price/sq ft to the number of active listings for all of the US
ggplot(inventory, aes(x=active_listing_count, y=median_listing_price_per_square_foot))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("US Median Listing Price / Sq. ft by Active Listings")+
  xlab("Active Listings")+
  ylab("Median Listing Price / Sq. Ft")

#Compare median days on market to median listing price/sq. ft for North Carolina
#This is the opposite effect that I would expect, possibly the month is a better determiner
ggplot(inventory_nc, aes(x=median_listing_price_per_square_foot, y=median_days_on_market, color=month))+
  geom_point()+
  scale_x_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Days on Market by Median Listing Price / Sq. Ft")+
  xlab("Median Listing Price / Sq. Ft")+
  ylab("Median Days on Market")

#Compare median listing price/sq. ft to active listings for North Carolina 
ggplot(inventory_nc, aes(x=active_listing_count, y=median_listing_price_per_square_foot, color=month))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price / Sq ft. by Active Listings")+
  xlab("Active Listings")+
  ylab("Median Listing Price / Sq. Ft")

  

###################
#Variables by month
###################

#Compare median listing price/sq. ft to the month for North Carolina
ggplot(inventory_nc, aes(y=median_listing_price_per_square_foot, x=month, color=month))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Listing Price / Sq. ft by Month")+
  xlab("Month")+
  ylab("Median Listing Price / Sq. ft")

#Compare median listing price/sq. ft to the month for North Carolina
ggplot(inventory_nc, aes(y=median_days_on_market, x=month, color=month))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  ggtitle("NC Median Days on Market / Sq. ft by Month")+
  xlab("Month")+
  ylab("Median Days on Market")

# It appears that the month/year and median_days_on_market are strong variables for mean_listing_price_per_square_foot
# Strong Variables:  Median Days on Market, Month/year
# Weak Variables: Active Listing Count
regression1 <- lm(median_listing_price_per_square_foot ~ active_listing_count + median_days_on_market + month, data=inventory_nc)
summary(regression1)

# There is a correlation between month/year and median_days_on_market but median_listing_price_per_square_foot is
# a much better predictor
#Strong Variables: Median Listing Price per Sq. Ft
# Weak Variables: Active Listing Count, Month/Year
regression2 <- lm(median_days_on_market ~ active_listing_count + median_listing_price_per_square_foot + month, data=inventory_nc)
summary(regression2)

##########
#Pending Ratio and new_listing_count
##########

# pending_ratio is also a strong predictor of median_listing_price_per_square_foot
ggplot(inventory_nc, aes(x=pending_ratio, y=median_listing_price_per_square_foot, color=month))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)

regression3 <- lm(median_listing_price_per_square_foot ~ active_listing_count + median_days_on_market + month + new_listing_count + pending_ratio, data=inventory_nc)
summary(regression3)