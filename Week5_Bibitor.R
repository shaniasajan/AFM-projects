library(tidyverse)

tStores <- read_csv('tStores.csv')
tSales <- read_csv('tSales.csv')
tEmployees <- read_csv('tEmployees.csv')

tStores
tSales
tEmployees
# Store 0 = headquarters 

tEmployees %>%  filter(Store==1)

tEmployees %>% slice_tail(n=10)
# To see which store Annie works, we would do: 
tStores %>%  filter(Store==60)
# We know that she works in store 60, but we don't know where that is 

# You can use the if.na, duplicate values, filters, etc. to find the unique identifiers
tSales %>%  filter(salesID==1)
# Code with 'tSales' will take long! Check that it works before midterm!
# Complete the code to verify the primary key for sales
tSales %>% select(salesID) %>% is.na() %>% sum()   
tSales %>% select(salesID) %>% count(salesID) %>% filter(n>1)

#######################

table_a <- read_csv('prices.csv')
table_b <- read_csv('qty.csv')

table_a
table_b

# To create table 
# left side of table, right side, common element = products 
table_innerjoin <- inner_join(table_a,table_b, by='Products')
# the common elements (products) in both tables will be put on the new table, and joins the tables together 
table_innerjoin
# you can rearrange sthe order of columns by rearranging the tables 

table_leftjoin <-left_join(table_a,table_b,by='Products')
table_leftjoin
# notice how all the data on the left side shows up, the others will be "NA" if it doesnt match up 

table_rightjoin <- right_join(table_a,table_b,by='Products')
table_rightjoin

# the cities that have more than one stores and that includes the number of stores and information about their sales, square footage, and employees
tStores # shows us each store and what city they belong in 
cityStores <- tStores %>% group_by(City) %>% 
  summarise(numOfStores = n_distinct(Store), totalSqFt = sum(SqFt))
# n_distinct counts the number of unique values in this column
cityStores 

citySales <- tSales %>% left_join(tStores %>% select(Store, City),by='Store')
# You cannot select "City" on its own because you are grouping 

citySales <- citySales %>% group_by(City) %>% 
  summarise(totalRev= sum(SalesQuantity*SalesPrice))

cityStores <- cityStores %>% left_join(citySales, by='City')

cityEmployees <- tEmployees %>% left_join(tStores %>% 
                                            select(Store, City), by='Store')
# There is no city value for store zero because that is headquarters!

cityEmployees %>%  filter(Store==1)

cityEmployees %>% group_by(City) %>% summarise(numOfEmpl = n_distinct(emp))
# something wrong here^

cityProfiles <- cityStores %>% 
  left_join(cityEmployees, by='City')
cityProfiles

# Template on how to join - inner or left 
nameoffile <- file1 %>% left_join(file1, by='whatevervariable')

# QUIZ TIME BABY 
# Q1: The following question assumes you have imported the tidyverse library and imported the tStores, tSales, and tEmployees datasets
# Run the following R script and use the results for store 22 to select the correct answer.

tStores %>% glimpse()
tStores %>% filter(Store==22) %>% count(Store)
tEmployees %>% glimpse()
tEmployees %>% filter(Store==22) %>% count(Store)

# Q2: Run the following R script and use the results for employee ID 04-91735 to select the correct answer.

tEmployees %>% glimpse()
tEmployees %>% filter(emplID=="04-91735")
tStores %>% glimpse()
tStores %>% filter(Store==0)

# Q3 What is the job title for employee Brian King
tEmployees %>% filter(LastName=="King")

# Q4 What is the square footage for the store Michael Hughes works at
tEmployees %>% filter(LastName=="Hughes") # He works at store 38
tStores %>% filter(Store==38)

# Q5 Are there outliers in the SalesPrice property in tSales?
tSales %>% select(SalesPrice) %>% summary(SalesPrice) 

# Q6 How many employees with the job title "Retail Store Clerk I" work in Store 60
tSales %>% filter(Store==60)

tEmployees

ClerkOne <- tStores %>% inner_join(tEmployees, by='Store')
ClerkOne %>% filter(Title=='Retail Store Clerk I') %>% count()

# Q7 -- How many total employees are there in stores 60 and 61
ClerkOne %>% filter(Store==60) %>%  count() #24 
ClerkOne %>% filter(Store==61) %>%  count() #11
24 + 11

#Q8 What is the name of the city with the smallest store?
tStores %>% select(SqFt) %>% order()
tStores %>% filter(Store==1)

