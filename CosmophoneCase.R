# AFM 244 - week 4

library(tidyverse)
dt0 <- read_csv("CosmoPhone_2022S.csv")

glimpse(dt0) 
dt0 %>% slice_head(n=5)

dt0 %>% is.na() %>% colSums() # shows which variables are missing information 
# In this dataset, totalCharges is missing 567 values! 

dt0 %>% filter(is.na(totalCharges)) %>% select (tenure, seniorCitizen, Churn) %>% 
  slice_sample(n=10)

dt0 %>%
  select(seniorCitizen) %>% filter(seniorCitizen==no)

dt0 %>% filter(is.na(totalCharges)) %>%
  select(tenure) %>% summary()



# We are trying to figure out who is churning or leaving the company 

# filter to see zero tenure customers
dt0 %>% select(tenure) %>% 
  filter(tenure == 0)

dt0 %>% mutate(notCitizens = filter(seniorCitizen))

dt0 %>% select(seniorCitizen, totalCharges) %>% group_by(seniorCitizen) %>% filter(seniorCitizen == yes) %>%  summary()

dt1 <- dt0 %>% mutate(contractLength=ifelse(Contract=="Month-to-month", "shortTerm", "longTerm"),
  autoPayment=ifelse(PaymentMethod=="Electronic check" |
  PaymentMethod=="Mailed check", "manual","automatic"))

dt2 <- 


dt0 %>%
  select(tenure, monthlyBill, totalCharges, seniorCitizen) %>% 
  summary()      # THQ3 Are there outliers in monthly bill
# Remember: IQR = Q3 - Q1
# Upper Whisker (UW) = Q3 + (1.5*IQR)
# Lower Whisker (LW) = Q1 - (1.5*IQR) -- no outliers! 

dt0 %>%
  group_by(Churn) %>%
  summarise(Q1Tenure = quantile(tenure, 0.25),
            medTenure = median(tenure),
            Q3Tenure = quantile(tenure, 0.75))

churnBoxplot <- dt0 %>%
  ggplot(aes(x = Churn, y = tenure)) +
  geom_boxplot() + 
  coord_flip()
churnBoxplot

dt0 %>%  count(Churn)

dt0 %>% count(Churn) %>% 
  mutate(relFreq = n / sum(n))

dt0 %>% select(Churn) %>% table()

dt0 %>% select(Churn) %>% 
  table() %>% 
  prop.table()

dt0 %>% select(seniorCitizen, Churn) %>%
  table() %>% prop.table()
# prop.table() shows how much of the population is senior citizen that has churned and not
# It is not conditional probability (If... then,,,)!! It is the probability of the entire population 

dt0 %>%
  count(seniorCitizen, Churn) %>% 
  mutate(relFreq = n / sum(n)) %>%
  select(-n) %>% #-n basically takes the count out (without n column)
  spread(seniorCitizen, relFreq) # selecting a key and a value 

dt0 %>%
  count(seniorCitizen, Churn) %>% 
  mutate(relFreq = n / sum(n)) %>% 
  group_by(seniorCitizen) %>% 
  mutate(condProbBySenior = relFreq/sum(relFreq))

dt0 %>% select(Churn,seniorCitizen) %>% 
  table() %>% prop.table(2) # the number indicates which variable from above is selected 
# Here, the 2 refers to the seniorCitizen
# "Given someone is a seniorCitizen,..."
# In conclusion, senior citizens churn more often than non-senior citizens 

# QUIZ 
model1 <- dt0 %>% 
  count(seniorCitizen, contract, autoPMT, Churn) %>% 
  mutate(p=n/sum(n)) %>% 
  group_by(seniorCitizen, contract, autoPMT) %>% 
  mutate(cp=p/sum(p))

# Probability of selecting a customer that is not senior, have M2M contracts, chose automatic payments, and have churned
model1 %>% select(Churn,seniorCitizen,contract, autoPMT) %>% 
  group_by(Churn, seniorCitizen, contract) %>% 
  table() %>% prop.table()

model1 %>% group_by(p)

