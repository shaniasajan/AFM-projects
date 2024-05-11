library(tidyverse)
options(scipen = 999) # sets penalties for scientific notation - it takes away scientific notation 
set.seed(123) # to get the same results (for everyone in the class) if we select random samples 

dt0 <- read_csv("industryAnalysis_2000_2022.csv")

dt0 %>% glimpse()
dt0 %>% slice_tail(n=5)

dt0 %>% filter(tic=="AAPL") %>% select(tic, naics)
#naics = north american industry code 

dt1_naics_334220 <- dt0 %>% filter(naics==334220 & fyear==2016) %>% select(conm, tic, sale, cogs, naics)
# companies that are in this industry code 
dt1_naics_334220 %>% summary()
# pre-revenue start ups exist 
# NA means there is nothing there -- there is no data 

dt1_naics_334220 %>% filter(sale==0)
# getting rid of this value because it adds no value 

dt1_naics_334220 %>% is.na() %>% colSums()
dt1_naics_334220 %>% filter(is.na(sale))
# getting rid of this value because it adds no value 

dt2 <- dt1_naics_334220 %>% filter(sale >=1)
summary(dt2)                            

dt2_grossMargin <- dt2 %>% mutate(grossMargin = (sale-cogs)/sale)
dt2_grossMargin %>% summary()

dt3_mediansales <-dt2_grossMargin %>% mutate(medianSales = median(sale))
dt3_mediansales <- dt3_mediansales %>% mutate(size = ifelse(sale >= medianSales, "large","small"))

dt3_mediansales %>% group_by(size) %>% summarise(number = n(), meanGrossMargin = mean(grossMargin))
# are we done? nope! we did not take the negative into consideration 

dt3_mediansales %>% group_by(size) %>% summarise(
  nCases = n(),
  meanGM = mean(grossMargin),
  minGM = min(grossMargin),
  q1GM = quantile(grossMargin, 0.25),
  medGM = median(grossMargin),
  q3GM = quantile(grossMargin, 0.75),
  maxGM = max(grossMargin)
)

# large  upper bound - max: 0.779 (there is an outlier)
0.512+1.5*(0.512-0.354) 
# large lower bound - min: 0.156 (there is an outlier)
0.354-1.5*(0.512-0.354) 

#small upper bound - max: 0.905 (there is an outlier)
0.553+1.5*(0.553-0.321)
#small lower bound - max: -3.58 (there is an outlier)
0.321-1.5*(0.553-0.321)

dt3_mediansales %>% filter(grossMargin > 0.512+1.5*(0.512-0.354 & size =='large'))
dt3_mediansales %>% filter(grossMargin>0.354-1.5*(0.512-0.354 & size =='large'))

dt3_mediansales %>% filter(grossMargin > 0.553+1.5*(size =='small'))


dt4_no_outlier <- dt3_mediansales %>% filter(tic!='PRKR')

dt4_no_outlier %>% group_by(size) %>% summarise(
  nCases = n(),
  meanGM = mean(grossMargin),
  minGM = min(grossMargin),
  q1GM = quantile(grossMargin, 0.25),
  medGM = median(grossMargin),
  q3GM = quantile(grossMargin, 0.75),
  maxGM = max(grossMargin)
)
