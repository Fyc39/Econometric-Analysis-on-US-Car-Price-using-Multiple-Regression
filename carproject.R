library(MASS)
install.packages('e1071')

library(e1071)
car_data<-read.csv("/Users/mustaviislam/Desktop/Econometric of Data Science/Project/USA_cars_datasets.csv",header=TRUE)
class(cardata)
nrow(cardata)
View(cardata)
summary(cardata)
library(dplyr)
install.packages('plyr')

# Adding a new Column REGION

getStateRegion <- function(state) {
  region_mapping <- list(
    "alabama" = "South",
    "alaska" = "West",
    "arizona" = "West",
    "arkansas" = "South",
    "california" = "West",
    "colorado" = "West",
    "connecticut" = "Northeast",
    "delaware" = "South",
    "florida" = "South",
    "georgia" = "South",
    "hawaii" = "West",
    "idaho" = "West",
    "illinois" = "Midwest",
    "indiana" = "Midwest",
    "iowa" = "Midwest",
    "kansas" = "Midwest",
    "kentucky" = "South",
    "louisiana" = "South",
    "maine" = "Northeast",
    "maryland" = "South",
    "massachusetts" = "Northeast",
    "michigan" = "Midwest",
    "minnesota" = "Midwest",
    "mississippi" = "South",
    "missouri" = "Midwest",
    "montana" = "West",
    "nebraska" = "Midwest",
    "nevada" = "West",
    "new hampshire" = "Northeast",
    "new jersey" = "Northeast",
    "new mexico" = "West",
    "new york" = "Northeast",
    "north carolina" = "South",
    "north dakota" = "Midwest",
    "ohio" = "Midwest",
    "oklahoma" = "South",
    "oregon" = "West",
    "pennsylvania" = "Northeast",
    "rhode island" = "Northeast",
    "south carolina" = "South",
    "south dakota" = "Midwest",
    "tennessee" = "South",
    "texas" = "South",
    "utah" = "West",  
    "vermont" = "Northeast",
    "virginia" = "South",
    "washington" = "West",
    "west virginia" = "South",
    "wisconsin" = "South",
    "wyoming" = "West"
  
  )
 

  lowercase_state <- tolower(state)
  
  
  if (lowercase_state %in% names(region_mapping)) {
    return(region_mapping[[lowercase_state]])
  } else {
    return(NA)
  }
}


car_data_with_region <- car_data %>%
  mutate(Region = sapply(state, getStateRegion))
View(car_data_with_region)


print(car_data_with_region)
car_data_with_region<-subset(car_data_with_region, Region!="NA")
View(car_data_with_region)
install.packages("ggplot2")
library(ggplot2)





getBrand <- function(brand) {
  brand_mapping <- list(
    "toyota"="Japanese",
     "honda"="Japanese",  
    "hyundai"="Korean", 
    "kia"="Korean" ,
    "ford"="American",
    "chevrolet"="American",
    "volkswagen"="German"  ,
    "bmw"="German"     ,
    "mercedes-benz"="German",
   " audi"="German",
   "acura"="American",
   "cadillac"="American",
  "gmc" ="American",
  "infiniti" ="American",
  "dodge" ="American",
   "chrysler"="American",
   "jeep"="American",
   "lincoln"="American",
   "nissan"="Japanese",
   "lexus"="Japanese",
   "maserati"="German",
   "heartland"="American",
   "land"="Japanese",
   "peterbilt"="American",
   "buick"="American",
   "jaguar"="German",
   "mazda"="Japanese",
  "chevrolet"="American"
  )
  

  
  
  lowercase_brand <- tolower(brand)
  
  
  if (lowercase_brand %in% names(brand_mapping)) {
    return(brand_mapping[[lowercase_brand]])
  } else {
    return(NA)
  }
}


car_data_with_brand <- car_data_with_region %>%
  mutate(Manufacture = sapply(brand, getBrand))

car_data_with_brand

View(car_data_with_brand)

car_data_with_brand <- car_data_with_brand %>%
  mutate(
    region_northeast = ifelse(Region == "Northeast", 1, 0),
    region_west = ifelse(Region == "West", 1, 0),
    region_midwest = ifelse(Region == "Midwest", 1, 0),
    region_south = ifelse(Region == "South", 1, 0)
  )
View(car_data_with_brand)




View(car_data_with_brand)
car_data_with_brand <- na.omit(car_data_with_brand)

nrow(car_data_with_brand)
View(car_data_with_brand)

install.packages('car')
install.packages('alr4')
install.packages('faraway')
library(car)
library(alr4)
library(faraway)
library(MASS)

selected_brand <- "American"  # Change this to the brand you want to select

# Select rows based on the condition (select rows with a certain brand)
car_data_with_brand <- car_data_with_brand %>%
  filter(Manufacture == selected_brand)



View(car_data_with_brand)

table(car_data_with_brand$Manufacture)
model1 = lm(formula = sqrt(price) ~sqrt(mileage) + title_status+model+year,
              data = car_data_with_brand)
summary(model1)

vif(model1)
#region_northeast+ region_west+region_south


df<- car_data_with_brand


convert_to_hours <- function(time_string) {
  if (grepl("days", time_string)) {
    # Extract days and convert to hours
    days <- as.numeric(gsub("[^0-9]", "", time_string))
    return(days * 24)  # Convert days to hours
  } else if (grepl("hours", time_string)) {
    # Extract hours
    hours <- as.numeric(gsub("[^0-9]", "", time_string))
    return(hours)  # Already in hours
  } else {
    return(NA)  # Return NA for other formats
  }
}

df$Time_In_Hours <- sapply(df$condition, convert_to_hours)

median(df$mileage)
sd(df$Time_In_Hours)
median()

df <- df %>%
  select(-lot, -vin,-state,color)


modelX = lm(formula = sqrt(price) ~mileage + title_status+model+Time_In_Hours+Region+year,
            data = df)
summary(modelX)



vif(modelX)

plot(modelX$residuals ~ modelX$fitted.values)
#add horizental line from 0
abline(h = 0, lty = 2)


residualPlots(modelX)
avPlots(modelX)
residualPlots(modelX)
qqnorm(modelX$residuals)  #Normal Quantile to Quantile plot
qqline(modelX$residuals)

summary(df[,c("price", "mileage", "model", "title_status", "region_midwest","region_south","region_northeast","region_west", "Time_In_Hours")])
summary(api.data[, c("api00", "enroll", "meals", "full", "hsg", "acs_k3", "acs_46")])
#----------------





filtered_incomes1 <- subset(df, price >= 0 & price <= 5000)
percentage1 <- (nrow(filtered_incomes1) / nrow(df)) * 100
percentage1

model1 = lm(formula = sqrt(price) ~mileage + title_status+model+Time_In_Hours+Region,
            data = filtered_mileage1)
summary(model1)

filtered_incomes2 <- subset(df, price >= 5000 & price <= 10000)
percentage2 <- (nrow(filtered_incomes2) / nrow(df)) * 100
percentage2

filtered_incomes3 <- subset(df, price >= 10000 & price <= 15000)
percentage3 <- (nrow(filtered_incomes3) / nrow(df)) * 100
percentage3

filtered_incomes4 <- subset(df, price >= 15000 & price <= 20000)
percentage4 <- (nrow(filtered_incomes4) / nrow(df)) * 100
percentage4

filtered_incomes5 <- subset(df, price >= 20000 & price <= 30000)
percentage5 <- (nrow(filtered_incomes5) / nrow(df)) * 100
percentage5


filtered_incomes6 <- subset(df, price >= 30000 & price <= 40000)
percentage6 <- (nrow(filtered_incomes6) / nrow(df)) * 100
percentage6

filtered_incomes7 <- subset(df, price >= 40000 & price <= 50000)
percentage7 <- (nrow(filtered_incomes7) / nrow(df)) * 100
percentage7

filtered_incomes8 <- subset(df, price >= 50000 & price <= 60000)
percentage8 <- (nrow(filtered_incomes8) / nrow(df)) * 100
percentage8

filtered_incomes9 <- subset(df, price >= 60000 & price <= 74000)
percentage9 <- (nrow(filtered_incomes9) / nrow(df)) * 100
percentage9
#--------------------------------------------------Mileage------

filtered_mileage0 <- subset(df, mileage >= 0 & mileage <= 20000)
mileage0 <- (nrow(filtered_mileage0) / nrow(df)) * 100
mileage0

filtered_mileage1 <- subset(df, mileage >= 20000 & mileage <= 50000)
mileage1 <- (nrow(filtered_mileage1) / nrow(df)) * 100
mileage1

filtered_mileage2 <- subset(df, mileage >= 50000 & mileage <= 100000)
mileage2 <- (nrow(filtered_mileage2) / nrow(df)) * 100
mileage2

filtered_mileage3 <- subset(df, mileage >= 100000 & mileage <= 200000)
mileage3 <- (nrow(filtered_mileage3) / nrow(df)) * 100
mileage3

filtered_mileage4 <- subset(df, mileage >= 300000 & mileage <= 400000)
mileage4 <- (nrow(filtered_mileage4) / nrow(df)) * 100
mileage4


filtered_mileage5 <- subset(df, mileage >= 500000 & mileage <= 600000)
mileage5 <- (nrow(filtered_mileage5) / nrow(df)) * 100
mileage5


filtered_mileage6 <- subset(df, mileage >= 700000 & mileage <= 800000)
mileage6 <- (nrow(filtered_mileage6) / nrow(df)) * 100
mileage6


filtered_mileage7 <- subset(df, mileage >= 900000 & mileage <= 1000000)
mileage7 <- (nrow(filtered_mileage7) / nrow(df)) * 100
mileage7

filtered_mileage8 <- subset(df, mileage >= 1000000 & mileage <= 1100000)
mileage8 <- (nrow(filtered_mileage8) / nrow(df)) * 100
mileage8

#-------------------------------_Condition------------------
df <- na.omit(df)
max(df$Time_In_Hours)

filtered_time1 <- subset(df, Time_In_Hours >= 0 & Time_In_Hours <= 60)
time1 <- (nrow(filtered_time1) / nrow(df)) * 100
time1


filtered_time2 <- subset(df, Time_In_Hours >= 60 & Time_In_Hours <= 120)
time2 <- (nrow(filtered_time2) / nrow(df)) * 100
time2

filtered_time3 <- subset(df, Time_In_Hours >= 120 & Time_In_Hours <= 180)
time3 <- (nrow(filtered_time3) / nrow(df)) * 100
time3

filtered_time4 <- subset(df, Time_In_Hours >= 180 & Time_In_Hours <= 240)
time4 <- (nrow(filtered_time4) / nrow(df)) * 100
time4

filtered_time5 <- subset(df, Time_In_Hours >= 240 & Time_In_Hours <= 300)
time5 <- (nrow(filtered_time5) / nrow(df)) * 100
time5

filtered_time6 <- subset(df, Time_In_Hours >= 300 & Time_In_Hours <= 360)
time6 <- (nrow(filtered_time6) / nrow(df)) * 100
time6

#----------------_Title-------------------------------------------

filtered_clean <- subset(df, title_status == "salvage insurance" )
clean <- (nrow(filtered_clean) / nrow(df)) * 100
clean

filtered_sal <- subset(df, title_status == "clean vehicle" )
sal <- (nrow(filtered_sal) / nrow(df)) * 100
sal

#-------------------------------Year---------------------------

filtered_year1 <- subset(df, year >= 1976 & year<=1990 )
year1 <- (nrow(filtered_year1) / nrow(df)) * 100
year1




filtered_year2 <- subset(df, year >= 1990 & year<=2000)
year2 <- (nrow(filtered_year2) / nrow(df)) * 100
year2


filtered_year3 <- subset(df, year >= 2000 & year<=2010)
nrow(filtered_year3)
nrow(df)
year3 <- (nrow(filtered_year3) / nrow(df)) * 100
year3

filtered_year4 <- subset(df, year >= 2010 & year<=2020)
year4 <- (nrow(filtered_year4) / nrow(df)) * 100
year4
sd(df$region_west)



#h0= mpg are equal
#ha= mpg are not equal

ford.yes = df[df$title_status == "clean vehicle", 'price'] 
ford.no = df[df$title_status == "salvage insurance", 'price'] 

boxplot(foreign.yes, foreign.no, horizontal = TRUE, names = c("Foreign Cars", "American Cars"), xlab  = 'Miles per Gallon of Car')
result = t.test(ford.yes, ford.no, conf.level = 0.95, alternative = 'two.sided')
result
