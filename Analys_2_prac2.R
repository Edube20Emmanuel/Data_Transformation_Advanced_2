### SETTTING THE WORKING DIRECTORATE ###
setwd("H:/2024-25-R-Analysis/2025-Analysis")

### LOADING THE REQUIRED LIBRARIES ###
library(tidyverse)
library(tidyselect)
library(tidyr)
library(readr)
library(readxl)

### LOADING THE DATA ###
data()
starwars %>% 
  view()

### Checking the dimensions in the data
starwars %>% 
  dim()

### Checking for structure of the data set ## 
starwars %>% 
  glimpse()

## ## Checking for variables ## ##
starwars %>% 
  names() %>% 
  view()

#### Summary statistics #####
starwars %>% 
  select(height,mass,birth_year) %>% 
  summary() %>% 
  view()

#### Analyzing variables  ###
table(starwars$gender)

### Sorting the table
view(sort(table(starwars$gender),descending = FALSE))

## Checking and changing the data type 
starwars %>% 
  glimpse()

starwars %>% 
  mutate(gender = as.factor(gender))
class(starwars$gender)

### CLEANING AND DEALING WITH MISSING ###
starwars %>% 
  select(name,height,mass, ends_with("color")) %>% 
  filter(!complete.cases(.)) %>% 
  view()

### Dropping the missing values in the data set 
starwars %>% 
  select(name, height, mass, ends_with("color")) %>% 
  drop_na(height) %>% 
  view()

## Replacing the missing values ##
starwars %>% 
  select(name, height, mass, ends_with("color")) %>% 
  mutate(hair_color = replace_na(hair_color, "none")) %>% 
  view()
