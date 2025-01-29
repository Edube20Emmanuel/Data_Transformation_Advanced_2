### Setting my working directorate
setwd("H:/2024-25-R-Analysis/2025-Analysis")

### IMPORT THE REQUIRED LIBRARIES
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(data.table)
library(readxl)
library(readr)

### Importing the R data sets
data()
AirPassengers
class(AirPassengers)
starwars
dim(starwars)
view(starwars[,2])

## Transforming it to a data table
starwars <- as.data.table(starwars)
class(starwars)

### Carring out functions on the data
starwars %>% 
  view()

## Selecting the variables
starwars[,hair_color:skin_color]

## Using the piping 
starwars %>% 
  select(name,height,mass,ends_with("color")) %>% 
  view()


### Filtering the rows
starwars %>% 
  select(name,height,mass,ends_with("color")) %>% 
  filter(mass > 100) %>% 
  order("mass") %>% 
  view()

## REDING A DATA SET USING A DATA TABLE
df <- fread("Lessons/escs_trend.csv")
df

df %>%    #observations -----------> 1447787
  dim()   # Variables -------------> 10

## Viewing the head and tail
df %>% 
  head(4)

df %>% 
  tail(4)
## CHECKING FOR THE STRUCTURE
glimpse(df)

## SELECTING VARIABLES AND ROWS
df[,cycnt:schoolid]

### USING THE SELECT METHOD
df %>% 
  select(cycnt:schoolid) %>% 
  view()

## Filtering the rows from the data table
df[paredint_trend > 12,]
df %>% 
  filter(paredint_trend > 12) %>% 
  dim()

### Combining the conditions
df1 = df[paredint_trend >13 & cnt == "ARE",]

df %>% 
  filter(paredint_trend > 13 & cnt == "ARE")

df %>% 
  select(cnt) %>% 
  table()

unique(df$oecd)

### Ordering the code
df[order(paredint_trend > 13)]
