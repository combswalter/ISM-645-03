library(readxl)
RDC_Inventory_Core_Metrics_State_History <- read_excel("C:/Users/colem/Desktop/Intro to Data Analytics/Group Project/RDC_Inventory_Core_Metrics_State_History.xlsx")
View(RDC_Inventory_Core_Metrics_State_History)

library(tidyverse)
library(dplyr)
library(ggpubr)

ggarrange(Starplot, Dunkplot)

#I will be using the median listing price per square foot as my response variable
#The question I will be asking is can price per square foot be predicted by looking at the explanatory variables of:
#median days on market and active listing count

DaysMarket <- ggplot(RDC_Inventory_Core_Metrics_State_History, aes(x=median_days_on_market, y=median_listing_price_per_square_foot)) + 
  geom_jitter() +
  geom_smooth(method ="lm", se = FALSE)

ActiveCount <- ggplot(RDC_Inventory_Core_Metrics_State_History, aes(x=active_listing_count, y=median_listing_price_per_square_foot)) + 
  geom_jitter() +
  geom_smooth(method ="lm", se = FALSE)

ggarrange(DaysMarket, ActiveCount)

#There seems to be a potential linear relationship between the response and explanatory variables
#Price per square foot seems to increase as there are less active listings and the listings spend less days on market

RegressionMarket <- lm(median_listing_price_per_square_foot ~ median_days_on_market, data = RDC_Inventory_Core_Metrics_State_History)

summary(RegressionMarket)

#The intercept tells us that on average, a home that spends 0 days on market would have a median listing price of $206 per square foot
#Residuals follow a normal distribution with Median near zero and the first and third quartiles having similar absolute values
#P-value is less that .05,  showing statistical significance
#The multiple r-squared, or coefficient of determination, is the proportion of the variance in the response variable that is predictable from the explanatory variable
#The closer to 1 the multiple r-squared the better, the closer to 0 means that there is no explanation in the variance of the response from the explanatory variable
#0.74 hear shows a reasonable linear relationship between the days on market and price per square foot

RegressionCount <- lm(median_listing_price_per_square_foot ~ active_listing_count, data = RDC_Inventory_Core_Metrics_State_History)

summary(RegressionCount)

#The intercept tells us that on average, a home that if a home was the only active listing the price would be $197 per square foot
#Residuals follow a normal distribution with Median near zero and the first and third quartiles having similar absolute values
#P-value is less that .05,  showing statistical significance
#The multiple r-squared value of 0.90 seems to show that there's even more of a linear relationship between the number of active listings and the price per square foot

explanatory_data <- tibble(
  median_days_on_market = 150:160)

prediction_data <- explanatory_data %>% 
  mutate(median_listing_price_per_square_foot = predict(RegressionMarket, explanatory_data))

prediction_data

#Using the linear regression based off of the explanatory variable of days on market
#we can predict the response variable of price per square foot for days on market between 150-160

ggplot(RDC_Inventory_Core_Metrics_State_History, aes(x=median_days_on_market, y=median_listing_price_per_square_foot)) + 
  geom_point() +
  geom_smooth(method ="lm", se = FALSE) +
  geom_point(data = prediction_data, color = "red")

