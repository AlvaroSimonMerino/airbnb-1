## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(batman)


## ----loadData-----------------------------------------------------------------
RawData <- read.csv(file = '../00_DatosOriginales/listings.csv', header = TRUE)
#summary(RawData)
data <- RawData %>%
  select(id, host_id, host_since, host_is_superhost, neighbourhood_group_cleansed, property_type, room_type, accommodates, bedrooms, price, minimum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm, availability_365, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, reviews_per_month)
str(data)


## ----typeConversion-----------------------------------------------------------
data$host_since <- as.Date(data$host_since)
data$host_is_superhost <- to_logical(data$host_is_superhost)
data$price<-parse_number(data$price)


## ----checkTypes---------------------------------------------------------------
str(data)


## ----summary------------------------------------------------------------------
summary(data)


## -----------------------------------------------------------------------------
data %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  count()


