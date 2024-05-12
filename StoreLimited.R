# load library 
  library(tidyverse)
  library(lubridate)
  
# load data 
  dt4c07L2 <- read_csv("C04S copy.csv")
  
# Verify if data is in the same folder 
  dir()
  
# Understand/ review data structure 
  names(dt4c07L2)
  glimpse(dt4c07L2)
  dt4c07L2 %>% slice_head(n=5)
  dt4c07L2 %>% slice_tail(n=5)
  
# Check for missing values 
  dt4c07L2 %>% 
    is.na() %>% 
    colSums()
  
# Summary Statistics for all variables 
  dt4c07L2 %>% 
    summary()
  
# Summary statistics for numeric variables 
  dt4c07L2 %>% 
    select(SalesQuantity,RetailPriceUnit,productCostUnit) %>% 
    summary()
  
# Repeat above to focus ONLY one - Sales Quantity 
  dt4c07L2 %>% 
    select(SalesQuantity) %>% 
  summary()

# Data preparation 
  names(dt4c07L2) 
dt1 <- dt4c07L2 %>% 
    mutate(productPriceUnit=RetailPriceUnit/1.13, 
           productCost=productPriceUnit*SalesQuantity, 
           ifelse(productPriceUnit>20,"Premium","Regular"), 
           Revenue = SalesQuantity*productPriceUnit, 
           grossProfit=Revenue-productCost, 
           Day = weekdays(SalesDate))
names(dt1)

dt1 %>% 
  summary()

# Outliers for gross profit 
dt1 %>%  
  select(grossProfit) %>% 
  summary()
  #Calculate lower and upper whisker 
  quantile(dt1$grossProfit,.25) - 1.5*IQR(dt1$grossProfit)
  quantile(dt1$grossProfit,.75) + 1.5*IQR(dt1$grossProfit)

# Add variable to indicate if an observation of gross profit is an outlier 
dt2 <- dt1 %>% 
  mutate(
    gpOutliers= 
      ifelse(
    (grossProfit < quantile(dt1$grossProfit, .25) - 
                               1.5*IQR(dt1$grossProfit) | # vertical line is OR
                               grossProfit > quantile(dt1$grossProfit, .75) + 
                               1.5*IQR(dt1$grossProfit)),"Yes","No"
      )
  )


# review result to make sure code works properly 
dt2 %>% 
  select(grossProfit, gpOutliers) %>% 
  slice_sample(n=10)

# filter just outliers 
dt2 %>% 
  select(grossProfit, gpOutliers) %>% 
  filter(gpOutliers == "Yes")
          

# Segment Analysis by store - ORIGINAL DATA 
# total sales quantity, revenue, product costs
dt1 %>% 
  select(Store, SalesQuantity, Revenue, productCost, grossProfit) %>% 
  group_by(Store) %>% 
  summarize(storeSales =sum(SalesQuantity), 
            storeRevenue = sum(Revenue), 
            storeCOGS = sum(productCost), 
            storeGP = sum(grossProfit), 
            storeGPM = storeGP/storeRevenue)*100

# gross profit for each store and total for all stores 
dt1 %>% 
  select(Store, SalesQuantity, Revenue, productCost, grossProfit) %>% 
  summarize(storeSales =sum(SalesQuantity), 
            storeRevenue = sum(Revenue), 
            storeCOGS = sum(productCost), 
            storeGP = sum(grossProfit), 
            storeGPM = storeGP/storeRevenue)*100

  