rm(list=ls()) 
library(leaflet)
library(tidyverse)
library(patchwork)
library(MASS)
library(caret)
library(stringi)
library(dplyr)
library(ggplot2)
setwd("~/Desktop/CDS 492/CDS-492-aah")
data  = read.csv("houses_Madrid.csv")

subtitle_data = read.csv("subtitles.csv", sep = ";", header = F)
colnames(subtitle_data) = c("subtitle", "lat", "long")
subtitle_data = subtitle_data[-c(1,2),]


data$X = NULL
data$latitude = NULL
data$longitude = NULL
data$portal = NULL
data$door = NULL
data$rent_price = NULL
data$rent_price_by_area = NULL
data$is_rent_price_known = NULL
data$are_pets_allowed = NULL
data$is_furnished = NULL
data$is_kitchen_equipped = NULL
data$has_private_parking = NULL
data$has_public_parking = NULL
data$operation = NULL
data$raw_address = NULL
data$sq_mt_useful = NULL
data$is_exact_address_hidden = NULL
data$is_buy_price_known = NULL
data$buy_price_by_area = NULL #nuevo
data = subset(data, data$built_year <= 2022)


#If a value in one of this variables is empty we assume is False or give it a name and translate variables if needed
data$is_floor_under[data$is_floor_under == ""] = "False"
data$is_new_development[data$is_new_development == ""] = "False"
data$has_central_heating[data$has_central_heating == ""] = "False"
data$has_individual_heating[data$has_individual_heating == ""] = "False"
data$has_ac[data$has_ac == ""] = "False"
data$has_fitted_wardrobes[data$has_fitted_wardrobes == ""] = "False"
data$has_lift[data$has_lift == ""] = "False"
data$is_exterior[data$is_exterior == ""] = "False"
data$has_garden[data$has_garden == ""] = "False"
data$has_pool[data$has_pool == ""] = "False"
data$has_terrace[data$has_terrace == ""] = "False"
data$has_balcony[data$has_balcony == ""] = "False"
data$has_storage_room[data$has_storage_room == ""] = "False"
data$is_accessible[data$is_accessible == ""] = "False"
data$is_parking_included_in_price[data$is_parking_included_in_price == ""] = "False"
data$has_green_zones[data$has_green_zones == ""] = "False"
data$is_orientation_east[data$is_orientation_east == ""] = "False"
data$is_orientation_west[data$is_orientation_west == ""] = "False"
data$is_orientation_north[data$is_orientation_north == ""] = "False"
data$is_orientation_south[data$is_orientation_south == ""] = "False"
data$house_type_id[data$house_type_id == "HouseType 1: Pisos"] = "HouseType 1: Flat"
data$house_type_id[data$house_type_id == "HouseType 2: Casa o chalet"] = "HouseType 2: Detached house"
data$house_type_id[data$house_type_id == "HouseType 4: Dúplex"] = "HouseType 4: Duplex"
data$house_type_id[data$house_type_id == "HouseType 5: Áticos"] = "HouseType 5: Attics"
data$house_type_id[data$house_type_id == ""] = "HouseType 3: Studio"

#We convert needed variables to factor
data$subtitle = as.factor(data$subtitle)
data$is_floor_under = as.factor(data$is_floor_under)
data$house_type_id = as.factor(data$house_type_id)
data$is_renewal_needed = as.factor(data$is_renewal_needed)
data$is_new_development = as.factor(data$is_new_development) #as it only has one factor we have to delete it 

data$has_central_heating = as.factor(data$has_central_heating)
data$has_individual_heating = as.factor(data$has_individual_heating)
data$has_ac = as.factor(data$has_ac)
data$has_fitted_wardrobes = as.factor(data$has_fitted_wardrobes)
data$house_type_id = as.factor(data$house_type_id)
data$has_lift = as.factor(data$has_lift)
data$is_exterior = as.factor(data$is_exterior)
data$has_garden = as.factor(data$has_garden)
data$has_pool = as.factor(data$has_pool)
data$has_terrace = as.factor(data$has_terrace)
data$has_balcony = as.factor(data$has_balcony)
data$has_storage_room = as.factor(data$has_storage_room)
data$is_accessible = as.factor(data$is_accessible)
data$has_green_zones = as.factor(data$has_green_zones)
data$energy_certificate = as.factor(data$energy_certificate)
data$has_parking = as.factor(data$has_parking)
data$is_parking_included_in_price = as.factor(data$is_parking_included_in_price)
data$is_orientation_north = as.factor(data$is_orientation_north)
data$is_orientation_south = as.factor(data$is_orientation_south)
data$is_orientation_east = as.factor(data$is_orientation_east)
data$is_orientation_west = as.factor(data$is_orientation_west)

#Recreate some variables
data$subtitle = stri_sub(data$subtitle, 1 ,-9)

north = data$is_orientation_north == "True"
south = data$is_orientation_south == "True"
east = data$is_orientation_east == "True"
west = data$is_orientation_west == "True"
data$orientation[north] = "north"
data$orientation[south] = "south"
data$orientation[east] = "east"
data$orientation[west] = "west"

data$is_orientation_north = NULL
data$is_orientation_south = NULL
data$is_orientation_east = NULL
data$is_orientation_west = NULL

#shuffle rows and merge both datasets
data = merge(data, subtitle_data, by = "subtitle")
n = nrow(data)
shuffled_indices = sample(1:n)
data = data[shuffled_indices, ]
data$lat = as.numeric(sub(",", ".", data$lat, fixed = TRUE))
data$long = as.numeric(sub(",", ".", data$long, fixed = TRUE))

idx=createDataPartition(data$buy_price, p = 0.75, list = FALSE)  
#data$built_year = as.character(data$built_year) #nuevo
training = data[idx,]
testing = data[-idx,]
nrow(training) #75%
nrow(testing) 

colSums(is.na(training))
colSums(is.na(testing))
training$is_new_development = NULL
training$orientation = as.factor(training$orientation)
training$neighborhood_id = as.factor(training$neighborhood_id)
testing$is_new_development = NULL
testing$orientation = as.factor(testing$orientation)
testing$neighborhood_id = as.factor(testing$neighborhood_id)
training  = subset(training, select = -c(n_floors, sq_mt_allotment,parking_price, id , title , street_name, street_number, lat, long ))
testing  = subset(testing, select = -c(n_floors, sq_mt_allotment,parking_price, id , title , street_name, street_number, lat, long ))


training$neighborhood_id = factor(training$neighborhood_id, levels = unique(training$neighborhood_id))
testing$neighborhood_id = factor(testing$neighborhood_id, levels = unique(training$neighborhood_id))

training$floor <- factor(training$floor, levels = unique(training$floor))
testing$floor <- factor(testing$floor, levels = unique(training$floor))

training$subtitle <- factor(training$subtitle, levels = unique(training$subtitle))
testing$subtitle <- factor(testing$subtitle, levels = unique(training$subtitle))

training = na.omit(training)
testing = na.omit(testing)

library(glmnet)

x = model.matrix(buy_price ~ ., training)[,-training$buy_price]
y = training$buy_price
set.seed(123)
model = train(
  buy_price ~., data = training, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)
model$bestTune

en.fit = cv.glmnet(x, y, alpha = model$bestTune$alpha)

plot(en.fit)
en.fit

plot(en.fit$glmnet.fit, 
     "lambda", label=FALSE)

opt.lambda = en.fit$lambda.min


ctrl = trainControl(method = "cv", number = 5, verboseIter = TRUE)

elasticnet = train(buy_price ~ ., data = training, method = "glmnet", trControl = ctrl, tuneGrid = expand.grid(alpha = model$bestTune$alpha, lambda =opt.lambda ))

y.pred = predict(elasticnet, newdata = testing)


# Calculate R-squared
rsq <- cor(y.pred, testing$buy_price)^2
rsq

elasticnet_model = glmnet(x, y, alpha = model$bestTune$alpha, lambda = opt.lambda)
enet_coefs = coef(elasticnet_model)
