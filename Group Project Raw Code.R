# Group Project: Dream Home: Your House Price Calculator

# Raw Code

# Load packages that are required to process and visualize the data later
library(openxlsx)
library(leaflet)
library(leaflet.extras)

# Import the data
kc_data <- read.xlsx("kc_house_data.xlsx")

# Adjust the output format
options(scipen = 6)

# Explore the data
dim(kc_data)
head(kc_data)

# We do not need the House ID and the transaction date so they are removed
kc_data <- kc_data[,3:21]

# Create new variables that are important for the estimation of the property value

# Variable 1: Age
max(kc_data$yr_built)
kc_data$age <- 2015 - kc_data$yr_built

# Variable 2: Number of years since last renovation
for(i in 1:length(kc_data$yr_renovated)) {
  if(kc_data$yr_renovated[i]==0) {
  kc_data$age_r[i] <- kc_data$age[i]
  } else {
    kc_data$age_r[i] <- 2015 - kc_data$yr_renovated[i]
    }
}

# Examine the new variable age
summary(kc_data$age)
par(mfrow = c(1,2))
hist(kc_data$age, col="dodgerblue", main="Property Age Distribution", xlab="Property Age")
plot(price ~ age, data=kc_data, cex=0.5, col="dodgerblue", main="Price-Age Relationship", xlab="Property Age", ylab="Property Price")

# Create a function that converts square foot to square meters
sqft_sqm <- function(sqft) {
  sqm <- round(sqft/10.76391)
  return(sqm)
}

# Apply the function to all variables that are measured in square foot
kc_data$sqm_living <- sqft_sqm(kc_data$sqft_living)
kc_data$sqm_lot <- sqft_sqm(kc_data$sqft_lot)
kc_data$sqm_above <- sqft_sqm(kc_data$sqft_above)
kc_data$sqm_basement <- sqft_sqm(kc_data$sqft_basement)
kc_data$sqm_living15 <- sqft_sqm(kc_data$sqft_living15)
kc_data$sqm_lot15 <- sqft_sqm(kc_data$sqft_lot15)

# Examine one of the new converted variables
summary(kc_data$sqm_living)
par(mfrow = c(1,2))
hist(kc_data$sqm_living, col="red", main="Property Size Distribution", xlab="Square Meters")
plot(price ~ sqm_living, data=kc_data, cex=0.5, col="red", main="Price-Size Relationship", xlab="Property Size", ylab="Property Price")

# Show all variables we now have in our dataset
ls(kc_data)

# Visualize Data

# Create an interactive map that shows the location of all properties
m1 <- leaflet()
m1 <- setView(m1, lng = mean(kc_data$long), lat = mean(kc_data$lat), zoom = 10)
m1 <- addTiles(m1)
m1 <- addCircleMarkers(m1, lng = kc_data$long, lat = kc_data$lat,
                       clusterOptions = markerClusterOptions())
m1

# Create an interactive map that shows the house price in every location (purple=expensive, yellow=cheap)
m2 <- leaflet()
m2 <- setView(m2, lng = mean(kc_data$long), lat = mean(kc_data$lat), zoom = 10)
m2 <- addTiles(m2)

pal.quantile <- colorQuantile("viridis", 
                              domain = kc_data$price, reverse = TRUE, n = 10)
kc_data$price.colors.quant <- pal.quantile(kc_data$price)

m2 <- addCircleMarkers(m2, lng = kc_data$long,
                       lat = kc_data$lat, radius = log(kc_data$price/1000),
                       stroke = FALSE, fillOpacity = 0.1, fill = T, 
                       fillColor =  kc_data$price.colors.quant) 
m2 <- addLegend(m2, pal = pal.quantile, values = kc_data$price, opacity = 1, title = "House Prices")
m2

# Regression models
r1 <- lm(price ~ age, data=kc_data)
summary(r1)

r2 <- lm(price ~ age + I(age^2), data=kc_data)
summary(r2)

r3 <- lm(price ~ age + I(age^2) + age_r + I(age_r^2) + bedrooms + bathrooms + floors + waterfront + view + condition + grade + sqm_living + sqm_lot + sqm_above + sqm_basement + sqm_living15 + sqm_lot15, data=kc_data)
summary(r3)

r4 <- lm(price ~ age + I(age^2) + age_r + I(age_r^2) + bedrooms + bathrooms + floors + waterfront + view + condition + grade + sqm_living + sqm_lot, data=kc_data)
summary(r4)

# House Price Calculator

# Ask for the required user inputs
user_age <- as.numeric(readline(prompt="Please enter the age of your desired property in years: "))
user_age_r <- as.numeric(readline(prompt="Please enter the years since the last renovation: "))
user_bedrooms <- as.numeric(readline(prompt="Please enter the number of bedrooms: "))
user_bathrooms <- as.numeric(readline(prompt="Please enter the number of bathrooms: "))
user_floors <- as.numeric(readline(prompt="Please enter the number of floors: "))
user_waterfront <- as.numeric(readline(prompt="Please indicate if your desired property is located at the waterfront (1 for yes, 0 for no): "))
user_view <- as.numeric(readline(prompt="Please indicate on a scale from 0 to 4 how good the view from your property is (0 = no view, 4 = perfect view): "))
user_condition <- as.numeric(readline(prompt="Please indicate the condition of the house on a scale from 1 to 5 (1 = bad, 5 = perfect): "))
user_grade <- as.numeric(readline(prompt="Please indicate on a scale from 1 to 13 how good the building construction and design are (1 = bad, 7 = average, 13 = perfect): "))
user_sqm_living <- as.numeric(readline(prompt="Please enter the size of the interior living space in square meters: "))
user_sqm_lot <- as.numeric(readline(prompt="Please enter the size of the overall land space in square meters: "))

# Store the input in a data frame and estimate a property value
userhouse <- data.frame(age=user_age, age_r=user_age_r, bedrooms=user_bedrooms, bathrooms=user_bathrooms, floors=user_floors, waterfront=user_waterfront, view=user_view, condition=user_condition, grade=user_grade, sqm_living=user_sqm_living, sqm_lot=user_sqm_lot)
predict(r4, newdat = userhouse, interval = "prediction", level = 0.1)

# House price estimation for an example property
examplehouse <- data.frame(age=50, age_r=10, bedrooms=3, bathrooms=2, floors=2, waterfront=0, view=2, condition=4, grade=7, sqm_living=120, sqm_lot=300)
predict(r4, newdat = examplehouse, interval = "prediction", level = 0.25)


