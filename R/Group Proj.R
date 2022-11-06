getwd()
library(tidyverse)
library(dplyr)
library(ggplot2)

# First I can calculate statistics for each data set I'm looking at within various years or states
# eg. median, mean, etc. 

summary(Housing_Inventory_US_Census_tidied_)

# I also looked at 30 year mortgage rates over time. 

summary(X30_Year_Fixed_Mortgage_Rate_Historic_Table_FreddieMac_xls)

summary(Number_of_New_Houses_Sold_US_Census_tidied_)
  
# Then run linear regression models to analyze relationships between variables



# Prepare plots, like scatterplots, box plots, bar charts
# to visualize and analyze data

Housing_Inventory_US_Census_tidied_ %>%
  mutate("Percent Occupied Housing" = `Total Occupied Housing Units` / `All Housing Units`)

View(PercentOccupiedHousing)

PercentOccupiedHousing <- Housing_Inventory_US_Census_tidied_ %>%
  group_by(Year) %>%
  summarize(PercentOccupiedHousing = `Total Occupied Housing Units` / `All Housing Units`)

OccupiedHousingPlot <- ggplot(PercentOccupiedHousing, aes(x=Year, y=PercentOccupiedHousing)) + 
  geom_line() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x="Year",y="Percentage of Housing Occupied") + 
  theme_minimal() + 
  expand_limits(y=.70)

# This chart shows the change in occupied housing units since the 1970s. 
# There is an unusual dip in 1982, that I will be interested to see if is replicated in later years.
# You can also see the downturn around the 2008 crash. 

ThirtyYrAvgRate <- X30_Year_Fixed_Mortgage_Rate_Historic_Table_FreddieMac_xls %>%
  group_by(Year) 

ThirtyYearMortgageRatePlot <- ggplot(X30_Year_Fixed_Mortgage_Rate_Historic_Table_FreddieMac_xls, aes(x=Year, y=`Rate (avg)`)) + 
  geom_line() + 
  theme_bw() + 
  labs(x="Year", y="Average 30-year Mortgage Rate")

# Might be good to find a way to combine these to show the relationship between mortgage 
# rates and occupied housing units. 

# Looked at New Housing Units Sold by Year in the United States.

USNewHousing <- Number_of_New_Houses_Sold_US_Census_tidied_ 

ggplot(USNewHousing, aes(x=Year, y=`United States`)) + 
  geom_point() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line()) +
  labs(x="Year", y="New Housing Units Sold")
  
options(scipen = 999) 

ggplot(USNewHousing, aes(x=Year, y=`United States`)) + 
  geom_col(fill="cyan") + 
  theme_bw() +
  labs(x="Year", y="New Housing Units Sold")
  

# Looked at Zillow Data for a previous class...
  
HomeOwnership <- Home_Ownership_and_Vacancy_Rates_by_State %>%
  filter(Indicator=='Home Ownership Rates')
HomeVacancy <- Home_Ownership_and_Vacancy_Rates_by_State %>%
  filter(Indicator=='Home Vacancy Rates')
RentalVacancy <- Home_Ownership_and_Vacancy_Rates_by_State %>%
  filter(Indicator=='Rental Vacancy Rates')

NCOwnership <- Home_Ownership_and_Vacancy_Rates_by_State %>%
  filter(State=='North Carolina')

NCHomeOwnership <- NCOwnership %>%
  filter(Indicator=='Home Ownership Rates')
NCHomeVacancy <- NCOwnership %>%
  filter(Indicator=='Home Vacancy Rates')
NCRentalVacancy <- NCOwnership %>%
  filter(Indicator=='Rental Vacancy Rates')

HomeOwnershipChart <- ggplot(HomeOwnership, mapping = aes(x=`DATE`,y=`Rate`))
HomeOwnershipChartClean <- HomeOwnershipChart +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line())

HomeVacancyChart <- ggplot(HomeVacancy, mapping = aes(x=`DATE`,y=`Rate`))
HomeVacancyChartClean <- HomeVacancyChart +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line())

