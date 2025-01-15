### SETTING THE WORKING DIRECTORATE ####
setwd("H:/2024-25-R-Analysis/2025-Analysis")

### IMPORTING THE NEEDED LIBRARIES ##
library(tidyverse)
library(tidyselect)
library(tidyr)
library(readxl)
library(readr)
library(ggplot2)

### Importing the required data set ###
df <- read_excel("H:/2024-25-R-Analysis/Data_set/Vegetable_Sales-1.xlsx")
head(df)

df %>% 
  view()

## Adding the variable total
df$Total <- df$`Quantity Sold (kg)` * df$`Unit Selling Price ($/kg)`

df %>% 
  head() %>% 
  view()

#### Exploring the data set
df %>%     # Observations ----------> 251
  dim()    # Variables    ----------> 8

## Checking the variables in the data set
df %>% 
  names() %>% 
  view()

## Checking for the data types
df %>% 
  glimpse()

## Exploring the sales and Discount Variables
# Sold and returned vegetables
df %>% 
  select(`Sale or Return`) %>% 
  unique() %>% 
  view()

df %>% 
  select(`Sale or Return`) %>%      # Sales ---------> 244
  table() %>%                       # Returns -------> 7
  sort(descending = FALSE) %>% 
  view()

# Sold on discount and not
df %>% 
  select(`Discount (Yes/No)`) %>%  # On discount ------------> 6
  table() %>%                      # Not on discount -------->
  sort(descending=FALSE) %>% 
  view()

## Filtering the data sets
#df_no_discount <- df[filter(df,df$`Discount (Yes/No)` == "No"),c("Item Name","Loss Rate (%)","Discount (Yes/No)","Quantity Sold (kg)","Total")]

df %>% 
  filter(`Discount (Yes/No)` == "No") %>% 
  select(c("Item Name","Loss Rate (%)","Discount (Yes/No)","Quantity Sold (kg)","Total")) %>% 
  view()

df %>% 
  filter(`Discount (Yes/No)` == "yes") %>% 
  select(c("Item Name","Loss Rate (%)","Discount (Yes/No)","Quantity Sold (kg)","Total")) %>% 
  view()

## Changing the variables discount and sales to factors ##
#Discount
class(df$`Discount (Yes/No)`)  # Character

df$`Discount (Yes/No)` <- as.factor(df$`Discount (Yes/No)`)
class(df$`Discount (Yes/No)`)  # factor

# Sale or return
class(df$`Sale or Return`)  # character

df$`Sale or Return` <- as.factor(df$`Sale or Return`)
class(df$`Sale or Return`)

class(df$Total)
df$Total <- as.double(df$Total)

## Finding out the maximum loss vegetable
df %>% 
  is.na() %>% 
  sum()

df$Total <- unlist(df$Total)

df %>% 
  mutate(Total = as.numeric(Total))

df_max <- df[which.max(df$`Sale or Return`),c("Item Name","Loss Rate (%)","Discount (Yes/No)","Total")]
view(df_max)

df_High <- df[which.max(df$`Sale or Return`),c("Item Name","Loss Rate (%)","Discount (Yes/No)","Total")]
view(df_High)

#### CLEANING OUR DATA ####
#checking for missing values

df %>% 
  is.na() %>%   ## Missing values -------> 0
  sum()

## Checking for duplicates
sum(duplicated(df))   # duplicates ------> 0

## Checking and removing outliers
## Summary stat
df %>% 
  select(`Wholesale Price ($/kg)`,`Quantity Sold (kg)`,`Loss Rate (%)`,`Unit Selling Price ($/kg)`,Total) %>% 
  summary() %>% 
  view()

## Outliers in Total
df %>% 
  select(Total) %>% 
  boxplot()

## Remove the outliers from the Total Variable
summary(df$Total)
# Q1 ----------------> 2.208
# Q3 ----------------> 4.265
TMin <- 0.798
TMax <- 21.691

## Calculating the IQR
IQR(df$Total)   # IQR --------> 2.0566
IQR = 2.0566

### Setting boundaries
lower <- 0.798 - IQR * 1.5
upper <- 21.691 + IQR * 1.5


## Removing the outliers
df$Total[which(df$Total < lower | df$Total > upper)]

#Checking if it is removed
boxplot(df$Total)


