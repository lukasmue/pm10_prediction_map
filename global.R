library(dplyr)

#allzips <- readRDS("data/superzip.rds")
#allzips$latitude <- jitter(allzips$latitude)
#allzips$longitude <- jitter(allzips$longitude)
#allzips$college <- allzips$college * 100
#allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

allsensors <- read.csv("data/forecast.csv",header = TRUE, sep = ",", dec=".")

countries <- allsensors %>%
  distinct(
    country = country,
    city = city
  )

print (countries$country)

cleansensors <- allsensors %>% 
  select(
    SensorID = sensor_id,
    PM10_Forecast = PM10,
    Temperature = temp,
    Humidity = humidity,
    Rain = rain_3h,
    Wind_Speed = wind_speed,
    Wind_Degreee = wind_deg,
    PM10_Floating_Average = PM10_average,
    City = city,
    Country = country,
    Lat = latitude,
    Long = longitude
  )

#cleantable <- allzips %>%
#  select(
#    City = city.x,
#    State = state.x,
#    Zipcode = zipcode,
#    Rank = rank,
#    Score = centile,
#    Superzip = superzip,
#    Population = adultpop,
#    College = college,
#    Income = income,
#    Lat = latitude,
#    Long = longitude
#  )#