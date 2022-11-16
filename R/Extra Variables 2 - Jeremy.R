library(readxl)
library(tidyverse)
library(dplyr)

zillow_raw <- read.csv("Zillow_SFH_County_cleaned.csv")

head(zillow_raw)
summary(zillow_raw)

zillow_clean <- zillow_raw %>% gather("month", "mean_cost", 4:72)
zillow_clean <- subset(zillow_clean, select=-X)
zillow_clean <- zillow_clean %>% mutate(month = gsub("X", "", month))
zillow_clean <- zillow_clean %>%
  mutate(month = as.Date(month, format = "%m.%d.%Y")

zillow_clean <- zillow_clean %>%
  mutate(month = as.Date(month, format = "%m.%d.%Y"))

head(zillow_clean)

zillow_sum <- zillow_clean %>% 
  group_by(month) %>%
  summarize(avg_cost = mean(mean_cost), median_cost = median(mean_cost), max_cost = max(mean_cost), min_cost = min(mean_cost))

ggplot(zillow_sum, aes(x=as.Date(month), y=avg_cost))+
  geom_point()+
  scale_x_date(date_breaks="3 month", date_labels="%B %Y")+
  scale_y_continuous(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("NC Average Sale Price by Month (01/2017 - 09/2022)")+
  xlab("Month")+
  ylab("Averate Sales Price")+
  geom_smooth(method = "lm", se=FALSE)
