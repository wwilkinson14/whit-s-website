## Whit's First Project NPS Data

## Lets figure out how to use readr and read in the csv file
## You can also use read_csv to build a tibble and skip = n to skip lines of text before reading
## in your table.

library(tidyverse)
nps <- read_csv("np_visit.csv")

##Some interesting questions to answer about this data:

## 1. How many parks/monuments are there in CA?
## 2. Show the visitation over time for all parks/monuments in CA 
## 3. Which years in our history had the lowest total visitation?
## 4. Which regions have the most parks and monuments?
## 5. When were visitation for Yosemite the lowest/highest?
## 6. What was the most-visited park in 2016? monument? etc.

## 1. How many parks/monuments are there in CA?
## Filter only works for "numeric, logical, or complex types", so we could add the function
## "grepl" to ask filter to look for a string- its actually will do this without grepl

CA <- filter(nps, grepl("CA", state))

CA_parks <- unique(CA$park_name)
length(CA_parks)

##answer: [1] 26

## 2. Show the visitation over time for all parks/monuments in CA 

## Lets look at parks first, create a variable for parks in CA

CA_parks_df <- nps %>% 
  filter(state =="CA" & type =="National Park")

## and one for monuments

CA_mon_df <- nps %>% 
  filter( state == "CA" & type == "National Monument")

# plot CA parks visitors per year

ggplot(data = CA_parks_df, aes(x = year, y= visitors)) +
 geom_point(aes(color= park_name)) +
  xlab("Year") +
  ylab("Number of Visitors per Year") +
  ggtitle("CA Parks Visitation through Time")

## Lets look at just the last 10 years data for CA parks and plot it

CA_parks06_16 <- filter(CA_parks_df, year >= 2006)

## CA Parks data just for 2006-2008
 
CA_parks06_08 <- CA_parks_df %>% 
filter( year == "2006" | year == "2007" | year == "2008")

## and plot it

ggplot(data = CA_parks06_16, aes(x = year, y= visitors)) +
  geom_point(aes(color= park_name)) +
  xlab("Year") +
  ylab("Number of Visitors per Year") +
  ggtitle("CA Parks Visitation 2006-2016")+
  geom_line(aes(color = park_name)) +
  geom_point(aes(color =park_name))

## What is the average visitation for each park over the 10 year period, plot it

##Curious if Sequoia and Kings Canyon visitation are related

CA_parks06_16_SK <- filter(CA_parks_df, year >= 2006, park_name == "Sequoia National Park" | park_name == "Kings Canyon National Park")

ggplot(data = CA_parks06_16_SK, aes(x = year, y= visitors)) +
  geom_point(aes(color= park_name)) +
  xlab("Year") +
  ylab("Number of Visitors per Year") +
  ggtitle("Sequoia Kings Canyon Visitation 2006-2016")+
  geom_line(aes(color = park_name)) +
  geom_point(aes(color =park_name))

#What was the mean number of visitors at CA National Parks from 2006-2016
meanvisit <- CA_parks06_16 %>% group_by(park_name) %>% 
  summarize(mean(visitors))
  colnames(meanvisit)[2] <- "mean_visit"


 p <- ggplot(meanvisit, aes(park_name, mean_visit, color = park_name, fill = park_name))
  p +geom_bar(stat = "identity") +
    #xlab("Park")
    ylab("Mean Visitors per Year") +
    ggtitle("Mean Park Visitors 2006-2016") +
    #theme_classic()+
    theme(axis.text.x=element_blank(), axis.title.x=element_blank())
  
  CA_parks95_05 <- filter(CA_parks_df, between(year, 1995, 2005))
  meanvisit95 <- CA_parks95_05 %>% group_by(park_name) %>% 
    summarize(mean(visitors))
  colnames(meanvisit)[2] <- "mean_visit"
  
  #Same mean analysis for 1995-2005
  
  CA_parks95_05 <- filter(CA_parks_df, between(year, 1995, 2005))
  meanvisit95 <- CA_parks95_05 %>% group_by(park_name) %>% 
    summarize(mean(visitors)) 
  colnames(meanvisit95)[2] <- "mean_visit"
  
  
  visit95 <- ggplot(meanvisit95, aes(park_name, mean_visit, color = park_name, fill = park_name))
  visit95 +geom_bar(stat = "identity") +
    #xlab("Park")
    ylab("Mean Visitors per Year") +
    ggtitle("Mean Park Visitors 1995-2005") +
    #theme_classic()+
    theme(axis.text.x=element_blank(), axis.title.x=element_blank())
  
  
## Lets also look at the last 10 years for monuments

mons_CA06_16 <- filter(CA_mon_df, year >= 2006)

ggplot(data = mons_CA06_16, aes(x = year, y= visitors)) +
  geom_point(aes(color= park_name)) +
  xlab("Year") +
  ylab("Number of Visitors per Year") +
  ggtitle("CA Monument Visitation 2006-2016")+
  geom_line(aes(color = park_name)) +
  geom_point(aes(color =park_name))

## What is the average visitation for each monument over the 10 year period, plot it

Monmeanvisit <- mons_CA06_16 %>% group_by(park_name) %>% 
  summarize(mean(visitors))
colnames(Monmeanvisit)[2] <- "mean_visit"

p2 <- ggplot(Monmeanvisit, aes(park_name, mean_visit, color = park_name, fill = park_name))
p2 +geom_bar(stat = "identity") +
  #xlab("Park")
  ylab("Mean Visitors per Year") +
  ggtitle("Mean Monument Visitors 2006-2016") +
  #theme_classic()+
  theme(axis.text.x=element_blank(), axis.title.x=element_blank())

## 3. Which years in our history had the lowest total visitation in CA?
##Create a loop that 
##calculates the total visitors for each year

CA %>% group_by(year) %>% 
  summarise( total = sum(CA$visitors))

yr <- unique(CA$year)
total <- vector("double", length(yr))

for (i in seq_along(yr)){
   
    newcol <- filter(CA, year == yr[i]) 
    total[[i]] <-sum(newcol$visitors)
}

df <-data.frame(yr, total)
colnames(df) <- c("yr", "visitor_total")

low_visit <- min(total)
## [1] 1000

low_year <- df[df$visitor_total == low_visit,]
