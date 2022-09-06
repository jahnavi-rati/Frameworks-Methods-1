###############################################################################
# KAGGLE PROJECT
################


################################################################################
# ANALYSIS DATA CLEANING
########################


# STORING DATA ----------------------------------------------------------------
setwd("C:/Users/jahna/OneDrive/Desktop/Columbia/Classes/Frameworks & Methods 1/Kaggle")
data <- read.csv("analysisData.csv")
str(data)

library(skimr)
library(mice)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggcorrplot)

skimr::skim(data)


# VARIABLES -------------------------------------------------------------------
names(data)

# REMOVING ----
# ...                               
#[2]  "name"                                        
#[3]  "summary"                                     
#[4]  "space"                                       
#[5]  "description"                                 
#[6]  "neighborhood_overview"                       
#[7]  "notes"                                       
#[8]  "transit"                                     
#[9]  "access"                                      
#[10] "interaction"
#[11] "house_rules"                                 
#[12] "host_name"                                   
#[13] "host_since"                                  
#[14] "host_location"                               
#[15] "host_about"                                  
#[16] "host_response_time"
# ...
#[20] "host_neighbourhood"                          
#[21] "host_listings_count"                         
#[22] "host_total_listings_count"                   
#[23] "host_verifications"
# ...
#[26] "street"                                      
#[27] "neighbourhood"
# ...                
#[30] "city"                                        
#[31] "state"                                       
#[32] "zipcode"                                     
#[33] "market"                                      
#[34] "smart_location"                              
#[35] "country_code"                                
#[36] "country"
# ...
#[45] "amenities"
# ...
#[48] "weekly_price"                                
#[49] "monthly_price"
# ...
#[56] "minimum_minimum_nights"                      
#[57] "maximum_minimum_nights"                      
#[58] "minimum_maximum_nights"                      
#[59] "maximum_maximum_nights"                      
#[60] "minimum_nights_avg_ntm"                      
#[61] "maximum_nights_avg_ntm"                      
#[62] "calendar_updated"                            
#[63] "has_availability"                            
#[64] "availability_30"                             
#[65] "availability_60"                             
#[66] "availability_90"                             
#[67] "availability_365"
# ...
#[69] "number_of_reviews_ltm"                       
#[70] "first_review"
# ...
#[79] "requires_license"                            
#[80] "license"                                     
#[81] "jurisdiction_names"
# ...
#[83] "is_business_travel_ready"
# ...
#[85] "require_guest_profile_picture"               
#[86] "require_guest_phone_verification"            
#[87] "calculated_host_listings_count"              
#[88] "calculated_host_listings_count_entire_homes" 
#[89] "calculated_host_listings_count_private_rooms"
#[90] "calculated_host_listings_count_shared_rooms"
#[91] "reviews_per_month"

# USING ----
#[1]  "id" 
# ...
#[17] "host_response_rate"                          
#[18] "host_acceptance_rate"                        
#[19] "host_is_superhost"
# ...
#[24] "host_has_profile_pic"                        
#[25] "host_identity_verified"
# ...
#[28] "neighbourhood_cleansed"
#[29] "neighbourhood_group_cleansed"
# ...
#[37] "is_location_exact"                           
#[38] "property_type"                               
#[39] "room_type"                                   
#[40] "accommodates"                                
#[41] "bathrooms"                                   
#[42] "bedrooms"                                    
#[43] "beds"                                        
#[44] "bed_type"
# ...                                   
#[46] "square_feet"
#[47] "price"
# ...
#[50] "security_deposit"                            
#[51] "cleaning_fee"                                
#[52] "guests_included"                             
#[53] "extra_people"                                
#[54] "minimum_nights"                              
#[55] "maximum_nights"
# ...
#[68] "number_of_reviews"
# ...
#[71] "last_review"                                 
#[72] "review_scores_rating"                        
#[73] "review_scores_accuracy"                      
#[74] "review_scores_cleanliness"                   
#[75] "review_scores_checkin"                       
#[76] "review_scores_communication"                 
#[77] "review_scores_location"                      
#[78] "review_scores_value"  
# ...
#[82] "instant_bookable"
# ...
#[84] "cancellation_policy" 

# NEW ----
# "days_since_last_review"


# CLEANING --------------------------------------------------------------------

# Select variables to clean and use in model ----

library(dplyr)
library(tidyr)
library(tidyverse)

data_cleaning <- data %>%
  
  select(id, host_response_rate, host_acceptance_rate, host_is_superhost,
         host_has_profile_pic, host_identity_verified, 
         neighbourhood_cleansed, neighbourhood_group_cleansed, zipcode, is_location_exact,    
         property_type, room_type, accommodates, bathrooms, bedrooms, beds, 
         bed_type,
         price, security_deposit, cleaning_fee,
         guests_included, extra_people, minimum_nights, maximum_nights,
         number_of_reviews, last_review, review_scores_rating,
         review_scores_accuracy, review_scores_cleanliness, 
         review_scores_checkin, review_scores_communication, 
         review_scores_location, review_scores_value, 
         instant_bookable, cancellation_policy) %>%
  mutate_all(na_if, "")

str(data_cleaning)

# Missing Values ----

data_cleaning[data_cleaning == 'N/A'] <- NA
sapply(data_cleaning, function(x) sum(is.na(x)))
skim(data_cleaning)


# id ----
# using for when we need to use scoringData

# host_response_rate ----

unique(data_cleaning$host_response_rate)
sum(is.na(data_cleaning$host_response_rate))

data_cleaning$host_response_rate <- as.numeric(gsub("%", "", data_cleaning$host_response_rate))/100
sort(unique(data_cleaning$host_response_rate))
hist(data_cleaning$host_response_rate, breaks = seq(0, 1, 0.1))

data_cleaning$host_response_rate[is.na(data_cleaning$host_response_rate)] <- median(data_cleaning$host_response_rate,na.rm = TRUE)

sort(unique(data_cleaning$host_response_rate))
sum(is.na(data_cleaning$host_response_rate))


# host_acceptance_rate ----

unique(data_cleaning$host_acceptance_rate)
sum(is.na(data_cleaning$host_acceptance_rate))

data_cleaning$host_acceptance_rate = as.numeric(gsub("%", "", data_cleaning$host_acceptance_rate))/100
sort(unique(data_cleaning$host_acceptance_rate))
hist(data_cleaning$host_response_rate, breaks = seq(0, 1, 0.1))

data_cleaning$host_acceptance_rate[is.na(data_cleaning$host_acceptance_rate)] <- median(data_cleaning$host_acceptance_rate,na.rm = TRUE)

sort(unique(data_cleaning$host_acceptance_rate))
sum(is.na(data_cleaning$host_acceptance_rate))


# host_is_superhost ----

unique(data_cleaning$host_is_superhost)
sum(is.na(data_cleaning$host_is_superhost))

data_cleaning$host_is_superhost <- ifelse(data_cleaning$host_is_superhost == "t", 1, 0)
hist(data_cleaning$host_is_superhost)
summary(data_cleaning$host_is_superhost)
data_cleaning$host_is_superhost[is.na(data_cleaning$host_is_superhost)] <- median(data_cleaning$host_is_superhost,na.rm = TRUE)
data_cleaning$host_is_superhost <- as.factor(data_cleaning$host_is_superhost)

unique(data_cleaning$host_is_superhost)
sum(is.na(data_cleaning$host_is_superhost))
hist(as.numeric(data_cleaning$host_is_superhost))


# host_has_profile_pic ----

unique(data_cleaning$host_has_profile_pic)
sum(is.na(data_cleaning$host_has_profile_pic))

data_cleaning$host_has_profile_pic <- ifelse(data_cleaning$host_has_profile_pic == "t", 1, 0)
hist(data_cleaning$host_has_profile_pic)
summary(data_cleaning$host_has_profile_pic)
data_cleaning$host_has_profile_pic[is.na(data_cleaning$host_has_profile_pic)] <- median(data_cleaning$host_has_profile_pic,na.rm = TRUE)
data$host_has_profile_pic <- as.factor(data$host_has_profile_pic)

unique(data_cleaning$host_has_profile_pic)
sum(is.na(data_cleaning$host_has_profile_pic))


# host_identity verified ----

unique(data_cleaning$host_identity_verified)
sum(is.na(data_cleaning$host_identity_verified))

data_cleaning$host_identity_verified <- ifelse(data_cleaning$host_identity_verified == "t", 1, 0)
hist(data_cleaning$host_identity_verified)
table(data_cleaning$host_identity_verified)
summary(data_cleaning$host_identity_verified)

# for (i in 1:nrow(data_cleaning)) {
#if(is.na(data_cleaning$host_identity_verified[i]) == TRUE){
#  data_cleaning$host_identity_verified[i] <- round(runif(1, min = 0, max = 1))
#}
#}

data_cleaning$host_identity_verified[is.na(data_cleaning$host_identity_verified)] <- median(data_cleaning$host_identity_verified, na.rm = TRUE)
data_cleaning$host_identity_verified <- as.factor(data_cleaning$host_identity_verified)
hist(data_cleaning$host_identity_verified)

unique(data_cleaning$host_identity_verified)
sum(is.na(data_cleaning$host_identity_verified))
table(data_cleaning$host_identity_verified)


# zipcode - REMOVE ----

#unique(data_cleaning$zipcode)
#sort(unique(data_cleaning$zipcode))

#data_cleaning <- data_cleaning %>%
#  mutate(zipcode = parse_number(zipcode)) %>% 
#  mutate_at("zipcode", str_replace, "1009", "10009")

#data_cleaning$zipcode

#sum(is.na(data_cleaning$zipcode))
#data_cleaning[, c("zipcode", "neighbourhood_cleansed")]
#data_cleaning[data_cleaning$zipcode == "10003", "neighbourhood_cleansed"]
#unique(data_cleaning[data_cleaning$zipcode == "10003", "neighbourhood_cleansed"])

# zipcodes are the same in different neighbourhood_cleansed - need to remove!
data_cleaning <- data_cleaning %>%
  
  select(-zipcode)

# neighbourhood_cleansed - USE INSTEAD OF ZIPCODE ----

unique(data_cleaning$neighbourhood_cleansed)

data_cleaning$neighbourhood_cleansed <- as.numeric(factor(data_cleaning$neighbourhood_cleansed))

unique(data_cleaning$neighbourhood_cleansed)
data_cleaning$neighbourhood_cleansed


# neighbourhood_group_cleansed ----

unique(data_cleaning$neighbourhood_group_cleansed)

data_cleaning$neighbourhood_group_cleansed <- as.numeric(factor(data_cleaning$neighbourhood_group_cleansed))

unique(data_cleaning$neighbourhood_group_cleansed)
data_cleaning$neighbourhood_group_cleansed


# is_location_exact ----

unique(data_cleaning$is_location_exact)

data_cleaning$is_location_exact <- ifelse(data_cleaning$is_location_exact == "t", 1, 0)
data_cleaning$is_location_exact <- as.factor(data_cleaning$is_location_exact)

unique(data_cleaning$is_location_exact)
data_cleaning$is_location_exact


# property_type - REMOVE ----

#unique(data_cleaning$property_type)
data_cleaning <- data_cleaning %>%
  select(-property_type)


# room_type ----

unique(data_cleaning$room_type)


# accommodates ----

unique(data_cleaning$accommodates)
sort(unique(data_cleaning$accommodates))
hist(data_cleaning$accommodates)
summary(data_cleaning$accommodates)
table(data_cleaning$accommodates)


# bathrooms ----

unique(data_cleaning$bathrooms)
sort(unique(data_cleaning$bathrooms))
hist(data_cleaning$bathrooms)
summary(data_cleaning$bathrooms)
table(data_cleaning$bathrooms)


# bedrooms ----

unique(data_cleaning$bedrooms)
sort(unique(data_cleaning$bedrooms))
hist(data_cleaning$bedrooms)
summary(data_cleaning$bedrooms)
table(data_cleaning$bedrooms)

# beds ----

unique(data_cleaning$beds)
summary(data_cleaning$beds)
table(data_cleaning$beds)
hist(data_cleaning$beds)

#for (i in 1:nrow(data_cleaning)) {
#if(is.na(data_cleaning$beds[i]) == TRUE){
#  data_cleaning$beds[i] <- round(runif(1, min = 0, max = 26))
#}
#}

data_cleaning$beds[is.na(data_cleaning$beds)] <- median(data_cleaning$beds, na.rm = T)
hist(data_cleaning$beds)
unique(data_cleaning$beds)
table(data_cleaning$beds)


# bed_type ----

unique(data_cleaning$bed_type)


# price ----

unique(data_cleaning$price)
summary(data_cleaning$price)


# security_deposit ----

unique(data_cleaning$security_deposit)
sum(is.na(data_cleaning$security_deposit))
hist(data_cleaning$security_deposit)
summary(data_cleaning$security_deposit)

#library(mice)
#data_cleaning_mice <- mice::complete(mice(data_cleaning))

# for (i in 1:nrow(data_cleaning)) {
#if(is.na(data_cleaning$security_deposit[i]) == TRUE){
#  data_cleaning$security_deposit[i] <- round(runif(1, min = 0, max = 999))
#}
#}

hist(data_cleaning$security_deposit)
data_cleaning$security_deposit[is.na(data_cleaning$security_deposit)] <- median(data$security_deposit,na.rm = T)

unique(data_cleaning$security_deposit)
sum(is.na(data_cleaning$security_deposit))
summary(data_cleaning$security_deposit)


# cleaning_fee ----

unique(data_cleaning$cleaning_fee)
sort(unique(data_cleaning$cleaning_fee))
sum(is.na(data_cleaning$cleaning_fee))
boxplot(data_cleaning$cleaning_fee, horizontal = T)

#for (i in 1:nrow(data_cleaning)) {
#if(is.na(data_cleaning$cleaning_fee[i]) == TRUE){
#  data_cleaning$cleaning_fee[i] <- round(runif(1, min = 0, max = 600))
#}
#}
boxplot(data_cleaning$cleaning_fee, horizontal = T)
data_cleaning$cleaning_fee[is.na(data_cleaning$cleaning_fee)] <- median(data_cleaning$cleaning_fee, na.rm = T)

unique(data_cleaning$cleaning_fee)
sum(is.na(data_cleaning$cleaning_fee))
hist(data_cleaning$cleaning_fee)

# guests_included ----

sort(unique(data_cleaning$guests_included))
table(data_cleaning$guests_included)


# extra_people ----

unique(data_cleaning$extra_people)
sort(unique(data_cleaning$extra_people))
table(data_cleaning$extra_people)
summary(data_cleaning$extra_people)
boxplot(data_cleaning$extra_people, horizontal = T)
quantile(data_cleaning$extra_people, 0.975)


# minimum_nights ----

unique(data_cleaning$minimum_nights)
sort(unique(data_cleaning$minimum_nights))
boxplot(data_cleaning$minimum_nights, horizontal = T)
summary(data_cleaning$minimum_nights)
table(data_cleaning$minimum_nights)
boxplot.stats(data_cleaning$minimum_nights)$out
length(boxplot.stats(data_cleaning$minimum_nights)$out)
#minimum_nights_outlier <- data_cleaning$minimum_nights>30

#data_cleaning[minimum_nights_outlier, "minimum_nights"] <- median(data_cleaning$minimum_nights, na.rm = T)

unique(data_cleaning$minimum_nights)
sort(unique(data_cleaning$minimum_nights))


# maximum_nights ----

unique(data_cleaning$maximum_nights)
sort(unique(data_cleaning$maximum_nights))
boxplot(data_cleaning$maximum_nights, horizontal = T)
summary(data_cleaning$maximum_nights)
boxplot.stats(data_cleaning$maximum_nights)$out
length(boxplot.stats(data_cleaning$maximum_nights)$out)

quantile(data_cleaning$maximum_nights, 0.975)
maximum_nights_outlier <- data_cleaning$maximum_nights>1125
sum(maximum_nights_outlier)

data_cleaning[maximum_nights_outlier, "maximum_nights"] <- median(data_cleaning$maximum_nights, na.rm = T)

unique(data_cleaning$maximum_nights)
sort(unique(data_cleaning$maximum_nights))


# number_of_reviews ----

unique(data_cleaning$number_of_reviews)
sort(unique(data_cleaning$number_of_reviews))


# last_review / days_since_last_review ----

unique(data_cleaning$last_review)
sort(unique(data_cleaning$last_review))

library(lubridate)

is.Date(data_cleaning$last_review)
data_cleaning$last_review <- as.Date(data$last_review)
is.Date(data_cleaning$last_review)
sum(is.na(data_cleaning$last_review))

data_cleaning$last_review[is.na(data_cleaning$last_review)] <- median(data_cleaning$last_review, na.rm = T)
data_cleaning$days_since_last_review <- as.integer(Sys.Date() - data_cleaning$last_review)

unique(data_cleaning$days_since_last_review)
sort(unique(data_cleaning$days_since_last_review))

# review_scores_rating ----

unique(data_cleaning$review_scores_rating)
sort(unique(data_cleaning$review_scores_rating))


# review_scores_accuracy ----

unique(data_cleaning$review_scores_accuracy)
sort(unique(data_cleaning$review_scores_accuracy))


# review_scores_cleanliness ----

unique(data_cleaning$review_scores_cleanliness)
sort(unique(data_cleaning$review_scores_cleanliness))


# review_scores_checkin ----

unique(data_cleaning$review_scores_checkin)
sort(unique(data_cleaning$review_scores_checkin))


# review_scores_communication ----

unique(data_cleaning$review_scores_communication)
sort(unique(data_cleaning$review_scores_communication))


# review_scores_location ----

unique(data_cleaning$review_scores_location)
sort(unique(data_cleaning$review_scores_location))


# review_scores_value ----

unique(data_cleaning$review_scores_value)
sort(unique(data_cleaning$review_scores_value))


# instant_bookable ----

unique(data_cleaning$instant_bookable)

data_cleaning$instant_bookable <- ifelse(data_cleaning$instant_bookable == "t", 1, 0)
hist(data_cleaning$instant_bookable)
summary(data_cleaning$instant_bookable)
data_cleaning$instant_bookable <- as.factor(data_cleaning$instant_bookable)

unique(data_cleaning$instant_bookable)


# cancellation_policy ----

unique(data_cleaning$cancellation_policy)

data_cleaning$cancellation_policy <- ifelse(
  data_cleaning$cancellation_policy %in% c("strict_14_with_grace_period",
                                           "super_strict_60",
                                           "super_strict_30",
                                           "strict"),
  "strict", data_cleaning$cancellation_policy)

unique(data_cleaning$cancellation_policy)
table(data_cleaning$cancellation_policy)


sapply(data_cleaning, function(x) sum(is.na(x))) # ----



################################################################################
# SCORING DATA CLEANING
#######################


# STORING scoring_scoring_data ----------------------------------------------------------------
scoring_data <- read.csv("scoringData.csv")
str(scoring_data)

skim(scoring_data)


# VARIABLES -------------------------------------------------------------------
names(scoring_data)

# PRICE IS MISSING - everything else is the same ----


# CLEANING --------------------------------------------------------------------

# Select variables to clean and use in model ----

library(dplyr)
library(tidyr)
library(tidyverse)

scoring_data_cleaning <- scoring_data %>%
  
  select(id, host_response_rate, host_acceptance_rate, host_is_superhost,
         host_has_profile_pic, host_identity_verified, 
         neighbourhood_cleansed, neighbourhood_group_cleansed, zipcode, is_location_exact,    
         property_type, room_type, accommodates, bathrooms, bedrooms, beds, 
         bed_type,
         security_deposit, cleaning_fee,
         guests_included, extra_people, minimum_nights, maximum_nights,
         number_of_reviews, last_review, review_scores_rating,
         review_scores_accuracy, review_scores_cleanliness, 
         review_scores_checkin, review_scores_communication, 
         review_scores_location, review_scores_value, 
         instant_bookable, cancellation_policy) %>%
  mutate_all(na_if, "")

str(scoring_data_cleaning)

# Missing Values ----

scoring_data_cleaning[scoring_data_cleaning == 'N/A'] <- NA
sapply(scoring_data_cleaning, function(x) sum(is.na(x)))
skim(scoring_data_cleaning)


# id ----
# using for when we need to use scoringData


# host_response_rate ----

unique(scoring_data_cleaning$host_response_rate)
sum(is.na(scoring_data_cleaning$host_response_rate))

scoring_data_cleaning$host_response_rate <- as.numeric(gsub("%", "", scoring_data_cleaning$host_response_rate))/100
sort(unique(scoring_data_cleaning$host_response_rate))
hist(scoring_data_cleaning$host_response_rate, breaks = seq(0, 1, 0.1))

scoring_data_cleaning$host_response_rate[is.na(scoring_data_cleaning$host_response_rate)] <- median(scoring_data_cleaning$host_response_rate,na.rm = TRUE)

unique(scoring_data_cleaning$host_response_rate)
sum(is.na(scoring_data_cleaning$host_response_rate))


# host_acceptance_rate ----

unique(scoring_data_cleaning$host_acceptance_rate)
sum(is.na(scoring_data_cleaning$host_acceptance_rate))

scoring_data_cleaning$host_acceptance_rate = as.numeric(gsub("%", "",scoring_data_cleaning$host_acceptance_rate))/100
sort(unique(scoring_data_cleaning$host_acceptance_rate))
hist(scoring_data_cleaning$host_response_rate, breaks = seq(0, 1, 0.1))
scoring_data_cleaning$host_acceptance_rate[is.na(scoring_data_cleaning$host_acceptance_rate)] <- median(scoring_data_cleaning$host_acceptance_rate,na.rm = TRUE)

unique(scoring_data_cleaning$host_acceptance_rate)
sum(is.na(scoring_data_cleaning$host_acceptance_rate))


# host_is_superhost ----

unique(scoring_data_cleaning$host_is_superhost)
sum(is.na(scoring_data_cleaning$host_is_superhost))

scoring_data_cleaning$host_is_superhost <- ifelse(scoring_data_cleaning$host_is_superhost == "t", 1, 0)
hist(scoring_data_cleaning$host_is_superhost)
summary(scoring_data_cleaning$host_is_superhost)
scoring_data_cleaning$host_is_superhost[is.na(scoring_data_cleaning$host_is_superhost)] <- median(scoring_data_cleaning$host_is_superhost,na.rm = TRUE)
scoring_data_cleaning$host_is_superhost <- as.factor(scoring_data_cleaning$host_is_superhost)

unique(scoring_data_cleaning$host_is_superhost)
sum(is.na(scoring_data_cleaning$host_is_superhost))
hist(scoring_data_cleaning$host_is_superhost)


# host_has_profile_pic ----

unique(scoring_data_cleaning$host_has_profile_pic)
sum(is.na(scoring_data_cleaning$host_has_profile_pic))

scoring_data_cleaning$host_has_profile_pic <- ifelse(scoring_data_cleaning$host_has_profile_pic == "t", 1, 0)
hist(scoring_data_cleaning$host_has_profile_pic)
summary(scoring_data_cleaning$host_has_profile_pic)
scoring_data_cleaning$host_has_profile_pic[is.na(scoring_data_cleaning$host_has_profile_pic)] <- median(scoring_data_cleaning$host_has_profile_pic,na.rm = TRUE)
#scoring_data_cleaning$host_has_profile_pic <- as.factor(scoring_data_cleaning$host_has_profile_pic)

unique(scoring_data_cleaning$host_has_profile_pic)
sum(is.na(scoring_data_cleaning$host_has_profile_pic))


# host_identity verified ----

unique(scoring_data_cleaning$host_identity_verified)
sum(is.na(scoring_data_cleaning$host_identity_verified))

scoring_data_cleaning$host_identity_verified <- ifelse(scoring_data_cleaning$host_identity_verified == "t", 1, 0)
hist(scoring_data_cleaning$host_identity_verified)
table(scoring_data_cleaning$host_identity_verified)
summary(scoring_data_cleaning$host_identity_verified)

#for (i in 1:nrow(scoring_data_cleaning)) {
#if(is.na(scoring_data_cleaning$host_identity_verified[i]) == TRUE){
#  scoring_data_cleaning$host_identity_verified[i] <- round(runif(1, min = 0, max = 1))
#}
#}

scoring_data_cleaning$host_identity_verified[is.na(scoring_data_cleaning$host_identity_verified)] <- median(scoring_data_cleaning$host_identity_verified, na.rm = T)
scoring_data_cleaning$host_identity_verified <- as.factor(scoring_data_cleaning$host_identity_verified)

unique(scoring_data_cleaning$host_identity_verified)
sum(is.na(scoring_data_cleaning$host_identity_verified))
table(scoring_data_cleaning$host_identity_verified)


# zipcode - REMOVE ----

#unique(data_cleaning$zipcode)
#sort(unique(data_cleaning$zipcode))

#data_cleaning <- data_cleaning %>%
#  mutate(zipcode = parse_number(zipcode)) %>% 
#  mutate_at("zipcode", str_replace, "1009", "10009")

#data_cleaning$zipcode

#sum(is.na(data_cleaning$zipcode))
#data_cleaning[, c("zipcode", "neighbourhood_cleansed")]
#data_cleaning[data_cleaning$zipcode == "10003", "neighbourhood_cleansed"]
#unique(data_cleaning[data_cleaning$zipcode == "10003", "neighbourhood_cleansed"])

# zipcodes are the same in different neighbourhood_cleansed - need to remove!
scoring_data_cleaning <- scoring_data_cleaning %>%
  
  select(-zipcode)


# neighbourhood_cleansed - USE INSTEAD OF ZIPCODE ----

unique(scoring_data_cleaning$neighbourhood_cleansed)

scoring_data_cleaning$neighbourhood_cleansed <- as.numeric(factor(scoring_data_cleaning$neighbourhood_cleansed))

unique(scoring_data_cleaning$neighbourhood_cleansed)
scoring_data_cleaning$neighbourhood_cleansed


# neighbourhood_group_cleansed ----

unique(scoring_data_cleaning$neighbourhood_group_cleansed)

scoring_data_cleaning$neighbourhood_group_cleansed <- as.numeric(factor(scoring_data_cleaning$neighbourhood_group_cleansed))

unique(scoring_data_cleaning$neighbourhood_group_cleansed)
scoring_data_cleaning$neighbourhood_group_cleansed


# is_location_exact ----
unique(scoring_data_cleaning$is_location_exact)

scoring_data_cleaning$is_location_exact <- ifelse(scoring_data_cleaning$is_location_exact == "t", 1, 0)
scoring_data_cleaning$is_location_exact <- as.factor(scoring_data_cleaning$is_location_exact)

unique(scoring_data_cleaning$is_location_exact)
scoring_data_cleaning$is_location_exact


# property_type ---- REMOVE

#unique(scoring_data_cleaning$property_type)
scoring_data_cleaning <- scoring_data_cleaning %>%
  select(-property_type)


# room_type ----

unique(scoring_data_cleaning$room_type)


# accommodates ----

unique(scoring_data_cleaning$accommodates)
sort(unique(scoring_data_cleaning$accommodates))
hist(scoring_data_cleaning$accommodates)
summary(scoring_data_cleaning$accommodates)
table(scoring_data_cleaning$accommodates)


# bathrooms ----

unique(scoring_data_cleaning$bathrooms)
sort(unique(scoring_data_cleaning$bathrooms))
hist(scoring_data_cleaning$bathrooms)
summary(scoring_data_cleaning$bathrooms)
table(scoring_data_cleaning$bathrooms)


# bedrooms ----

unique(scoring_data_cleaning$bedrooms)
sort(unique(scoring_data_cleaning$bedrooms))
hist(scoring_data_cleaning$bedrooms)
summary(scoring_data_cleaning$bedrooms)
table(scoring_data_cleaning$bedrooms)

# beds ----

unique(scoring_data_cleaning$beds)
summary(scoring_data_cleaning$beds)
table(scoring_data_cleaning$beds)
hist(scoring_data_cleaning$beds)

#for (i in 1:nrow(scoring_data_cleaning)) {
#if(is.na(scoring_data_cleaning$beds[i]) == TRUE){
#  scoring_data_cleaning$beds[i] <- round(runif(1, min = 0, max = 26))
#}
#}

scoring_data_cleaning$beds[is.na(scoring_data_cleaning$beds)] <- median(scoring_data_cleaning$beds, na.rm = T)

hist(scoring_data_cleaning)
unique(scoring_data_cleaning$beds)
table(scoring_data_cleaning$beds)


# bed_type ----

unique(scoring_data_cleaning$bed_type)


# price - OUTCOME ----

unique(data_cleaning$price)
summary(data_cleaning$price)


# security_deposit ----

#library(mice)
#data_cleaning_mice <- mice::complete(mice(data_cleaning))

unique(scoring_data_cleaning$security_deposit)
sum(is.na(scoring_data_cleaning$security_deposit))
hist(scoring_data_cleaning$security_deposit)
summary(scoring_data_cleaning$security_deposit)

#for (i in 1:nrow(scoring_data_cleaning)) {
#if(is.na(scoring_data_cleaning$security_deposit[i]) == TRUE){
#  scoring_data_cleaning$security_deposit[i] <- round(runif(1, min = 0, max = 999))
#}
#}

scoring_data_cleaning$security_deposit[is.na(scoring_data_cleaning$security_deposit)] <- mean(scoring_data_cleaning$security_deposit, na.rm = T)

unique(scoring_data_cleaning$security_deposit)
sum(is.na(scoring_data_cleaning$security_deposit))
summary(scoring_data_cleaning$security_deposit)


# cleaning_fee ----

unique(scoring_data_cleaning$cleaning_fee)
sort(unique(scoring_data_cleaning$cleaning_fee))
sum(is.na(scoring_data_cleaning$cleaning_fee))
boxplot(scoring_data_cleaning$cleaning_fee, horizontal = T)

# for (i in 1:nrow(scoring_data_cleaning)) {
#if(is.na(scoring_data_cleaning$cleaning_fee[i]) == TRUE){
#  scoring_data_cleaning$cleaning_fee[i] <- round(runif(1, min = 0, max = 600))
#}
#}
boxplot(scoring_data_cleaning$cleaning_fee, horizontal = T)
scoring_data_cleaning$cleaning_fee[is.na(scoring_data_cleaning$cleaning_fee)] <- median(scoring_data_cleaning$cleaning_fee, na.rm = T)

unique(scoring_data_cleaning$cleaning_fee)
sum(is.na(scoring_data_cleaning$cleaning_fee))
hist(scoring_data_cleaning$cleaning_fee)


# guests_included ----

sort(unique(scoring_data_cleaning$guests_included))
table(scoring_data_cleaning$guests_included)


# extra_people ----

unique(scoring_data_cleaning$extra_people)
sort(unique(scoring_data_cleaning$extra_people))
table(scoring_data_cleaning$extra_people)
summary(scoring_data_cleaning$extra_people)
boxplot(scoring_data_cleaning$extra_people, horizontal = T)
quantile(scoring_data_cleaning$extra_people, 0.975)


# minimum_nights ----

unique(scoring_data_cleaning$minimum_nights)
sort(unique(scoring_data_cleaning$minimum_nights))
boxplot(scoring_data_cleaning$minimum_nights, horizontal = T)
summary(scoring_data_cleaning$minimum_nights)
table(scoring_data_cleaning$minimum_nights)
boxplot.stats(scoring_data_cleaning$minimum_nights)$out
length(boxplot.stats(scoring_data_cleaning$minimum_nights)$out)
#scoring_minimum_nights_outlier <- scoring_data_cleaning$minimum_nights>30

#scoring_data_cleaning[scoring_minimum_nights_outlier, "minimum_nights"] <- median(scoring_data_cleaning$minimum_nights, na.rm = T)

unique(scoring_data_cleaning$minimum_nights)
sort(unique(scoring_data_cleaning$minimum_nights))


# maximum_nights ----

unique(scoring_data_cleaning$maximum_nights)
sort(unique(scoring_data_cleaning$maximum_nights))
sum(is.na(scoring_data_cleaning$maximum_nights))
boxplot(scoring_data_cleaning$maximum_nights, horizontal = T)
summary(scoring_data_cleaning$maximum_nights)
boxplot.stats(scoring_data_cleaning$maximum_nights)$out
length(boxplot.stats(scoring_data_cleaning$maximum_nights)$out)

quantile(scoring_data_cleaning$maximum_nights, 0.975)
scoring_maximum_nights_outlier <- scoring_data_cleaning$maximum_nights>1125
sum(scoring_maximum_nights_outlier)

scoring_data_cleaning[scoring_maximum_nights_outlier, "maximum_nights"] <- median(scoring_data_cleaning$maximum_nights, na.rm = T)

unique(scoring_data_cleaning$maximum_nights)
sort(unique(scoring_data_cleaning$maximum_nights))


# number_of_reviews ----

unique(scoring_data_cleaning$number_of_reviews)
sort(unique(scoring_data_cleaning$number_of_reviews))


# last_review / days_since_last_review ----

unique(scoring_data_cleaning$last_review)
sort(unique(scoring_data_cleaning$last_review))
is.character(scoring_data_cleaning$last_review)

library(lubridate)

is.Date(scoring_data_cleaning$last_review)
scoring_data_cleaning$last_review <- mdy(scoring_data_cleaning$last_review)
is.Date(scoring_data_cleaning$last_review)

scoring_data_cleaning$last_review

scoring_data_cleaning$last_review[is.na(scoring_data_cleaning$last_review)] <- median(scoring_data_cleaning$last_review, na.rm = T)
scoring_data_cleaning$days_since_last_review <- as.integer(Sys.Date() - scoring_data_cleaning$last_review)

unique(scoring_data_cleaning$days_since_last_review)
sort(unique(scoring_data_cleaning$days_since_last_review))


# review_scores_rating ----

unique(scoring_data_cleaning$review_scores_rating)
sort(unique(scoring_data_cleaning$review_scores_rating))


# review_scores_accuracy ----

unique(scoring_data_cleaning$review_scores_accuracy)
sort(unique(scoring_data_cleaning$review_scores_accuracy))


# review_scores_cleanliness ----

unique(scoring_data_cleaning$review_scores_cleanliness)
sort(unique(scoring_data_cleaning$review_scores_cleanliness))


# review_scores_checkin ----

unique(scoring_data_cleaning$review_scores_checkin)
sort(unique(scoring_data_cleaning$review_scores_checkin))


# review_scores_communication ----

unique(scoring_data_cleaning$review_scores_communication)
sort(unique(scoring_data_cleaning$review_scores_communication))


# review_scores_location ----

unique(scoring_data_cleaning$review_scores_location)
sort(unique(scoring_data_cleaning$review_scores_location))


# review_scores_value ----

unique(scoring_data_cleaning$review_scores_value)
sort(unique(scoring_data_cleaning$review_scores_value))


# instant_bookable ----

unique(scoring_data_cleaning$instant_bookable)

scoring_data_cleaning$instant_bookable <- ifelse(scoring_data_cleaning$instant_bookable == "t", 1, 0)
hist(scoring_data_cleaning$instant_bookable)
summary(scoring_data_cleaning$instant_bookable)
scoring_data_cleaning$instant_bookable <- as.factor(scoring_data_cleaning$instant_bookable)

unique(scoring_data_cleaning$instant_bookable)


# cancellation_policy ----

unique(scoring_data_cleaning$cancellation_policy)

scoring_data_cleaning$cancellation_policy <- ifelse(
  scoring_data_cleaning$cancellation_policy %in% c("strict_14_with_grace_period",
                                                   "super_strict_60",
                                                   "super_strict_30",
                                                   "strict"),
  "strict", scoring_data_cleaning$cancellation_policy)

unique(scoring_data_cleaning$cancellation_policy)
table(scoring_data_cleaning$cancellation_policy)


sapply(scoring_data_cleaning, function(x) sum(is.na(x))) # ----


###############################################################################
# CORRELATION IDENTIFYING
#########################

names(data_cleaning)

# Downloading package
library(ggplot2) # not working
library(ggcorrplot)
library(caret)


# Extract numeric variables
num_variables <- which(sapply(data_cleaning, is.numeric))
length(num_variables)


# Design a new data frame with correlation coefficients
df_with_num_variables <- data_cleaning[, num_variables[c(-1, -16)]]
ggcorrplot(cor(df_with_num_variables), method = "square", type = "lower",
           show.diag = F, colors = c("dark red", "light yellow", "dark green"))


# Correlation matrix color coded
corMatrix <- as.data.frame(cor(df_with_num_variables[, -c(1, 16)]))
corMatrix$var1 <- rownames(corMatrix)

corMatrix %>%
  gather(key = var2, value = r, 1:22)%>%
  arrange(var1, desc(var2))%>%
  ggplot(aes(x = var1, y = reorder(var2, order(var2,decreasing = F)), fill = r))+
  geom_tile()+
  geom_text(aes(label = round(r,2)), size = 3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x = element_text(angle = 75,hjust = 1))+xlab('')+ylab('')


###############################################################################
# TRAINING MODELS
#################

library(caret)

split_data_cleaning <- createDataPartition(y = data_cleaning$price,
                                           p = 0.8, list = F, groups = 100)
train_data_cleaning <- data_cleaning[split_data_cleaning, ]
test_data_cleaning <- data_cleaning[-split_data_cleaning, ]


###############################################################################
# MODEL 5
# Tree with Tuning - tuning CV
##############################

library(caret); library(rpart)

trControl <- trainControl(method='cv', number = 5)
tuneGrid <- expand.grid(.cp = seq(from = 0.001,to = 0.1,by = 0.001))
cvModel5 <- train(price ~ host_acceptance_rate + host_is_superhost + 
                   neighbourhood_cleansed + neighbourhood_group_cleansed + 
                   is_location_exact + room_type + accommodates + bathrooms +
                   bedrooms + beds + security_deposit + cleaning_fee + 
                   number_of_reviews + days_since_last_review + review_scores_rating +
                   review_scores_accuracy + review_scores_cleanliness + 
                   review_scores_checkin + review_scores_location + 
                   review_scores_value + instant_bookable + cancellation_policy,
                 data = train_data_cleaning,
                 method = "rpart",
                 trControl = trControl,
                 tuneGrid = tuneGrid)

library(ggplot2)
ggplot(data=cvModel5$results, aes(x=cp, y=RMSE))+
  geom_line(size=0.5,alpha=0.2)+
  geom_point(color='brown')+
  theme_bw()+
  ggtitle(label=paste('Lowest RMSE is at a cp of ',cvModel5$bestTune$cp))

cvTree <- rpart(price ~ host_acceptance_rate + host_is_superhost + 
                  neighbourhood_cleansed + neighbourhood_group_cleansed +
                  is_location_exact + room_type + accommodates + bathrooms +
                  bedrooms + beds + security_deposit + cleaning_fee +
                  number_of_reviews + days_since_last_review + review_scores_rating +
                  review_scores_accuracy + review_scores_cleanliness +
                  review_scores_checkin + review_scores_location +
                  review_scores_value + instant_bookable + cancellation_policy, 
                data = train_data_cleaning, cp = cvModel5$bestTune$cp)

pred_price5_test <- predict(cvTree, newdata = test_data_cleaning)
RMSE(pred_price5_test, test_data_cleaning$price)
#74.12983 with 10 fold
#76.28357 with 5 fold and data updates


###############################################################################
# MODEL 5 TO SCORING DATA
#########################

pred_price5_scoring <- predict(cvModel5, newdata = scoring_data_cleaning)


###################################################################

# Construct submission from predictions

submissionDataFrame = data.frame(id = scoring_data_cleaning$id, price = pred_price5_scoring)
write.csv(submissionDataFrame, 'my_third_submission.csv', row.names = F)
getwd()
