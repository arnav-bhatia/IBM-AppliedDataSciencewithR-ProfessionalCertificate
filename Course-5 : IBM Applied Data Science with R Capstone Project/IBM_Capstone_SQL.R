rm(list=ls())
# Load the RSQLite library
library(RSQLite)

# Establish the SQLite connection
conn <- dbConnect(SQLite(), dbname = "eda.db")

seoul_bike_sharing <- read.csv("C:/Users/iamar/Downloads/SEOUL_BIKE_SHARING.csv")
cities_weather_forecast <- read.csv("C:/Users/iamar/Downloads/CITIES_WEATHER_FORECAST.csv")
bike_sharing_systems <- read.csv("C:/Users/iamar/Downloads/BIKE_SHARING_SYSTEMS.csv")
world_cities <- read.csv("C:/Users/iamar/Downloads/WORLD_CITIES.csv")

# Write the data frames into tables in the SQLite database
dbWriteTable(conn, "SEOUL_BIKE_SHARING", seoul_bike_sharing, overwrite = TRUE)
dbWriteTable(conn, "CITIES_WEATHER_FORECAST", cities_weather_forecast, overwrite = TRUE)
dbWriteTable(conn, "BIKE_SHARING_SYSTEMS", bike_sharing_systems, overwrite = TRUE)
dbWriteTable(conn, "WORLD_CITIES", world_cities, overwrite = TRUE)

# Task 1 - Record Count
record_count <- dbGetQuery(conn, "SELECT COUNT(*) FROM SEOUL_BIKE_SHARING")

# Task 2 - Operational Hours
operational_hours <- dbGetQuery(conn, "SELECT COUNT(*) FROM SEOUL_BIKE_SHARING WHERE RENTED_BIKE_COUNT > 0")

# Task 3 - Weather Outlook
weather_outlook <- dbGetQuery(conn, "SELECT * FROM CITIES_WEATHER_FORECAST WHERE CITY = 'Seoul' LIMIT 1")

# Task 4 - Seasons
seasons <- dbGetQuery(conn, "SELECT DISTINCT SEASONS FROM SEOUL_BIKE_SHARING")

# Task 5 - Date Range
date_range <- dbGetQuery(conn, "SELECT MIN(DATE) AS first_date, MAX(DATE) AS last_date FROM SEOUL_BIKE_SHARING")

# Task 6 - Subquery - 'all-time high'
all_time_high <- dbGetQuery(conn, "SELECT DATE, HOUR, MAX(RENTED_BIKE_COUNT) AS max_rented_bike_count
                                   FROM SEOUL_BIKE_SHARING 
                                   GROUP BY DATE, HOUR 
                                   ORDER BY max_rented_bike_count DESC 
                                   LIMIT 1")

# Task 7 - Hourly popularity and temperature by season
hourly_popularity_temp_season <- dbGetQuery(conn, "SELECT SEASONS, HOUR, AVG(RENTED_BIKE_COUNT) AS avg_rented_bike_count, AVG(TEMPERATURE) AS avg_temperature
                                                  FROM SEOUL_BIKE_SHARING
                                                  GROUP BY SEASONS, HOUR
                                                  ORDER BY avg_rented_bike_count DESC, avg_temperature DESC
                                                  LIMIT 10")

# Task 8 - Rental Seasonality
rental_seasonality <- dbGetQuery(conn, "SELECT SEASONS, HOUR,
                                        AVG(RENTED_BIKE_COUNT) AS avg_rented_bike_count,
                                        MIN(RENTED_BIKE_COUNT) AS min_rented_bike_count,
                                        MAX(RENTED_BIKE_COUNT) AS max_rented_bike_count,
                                        SQRT(AVG(RENTED_BIKE_COUNT*RENTED_BIKE_COUNT) - AVG(RENTED_BIKE_COUNT)*AVG(RENTED_BIKE_COUNT)) AS std_dev_rented_bike_count
                                        FROM SEOUL_BIKE_SHARING
                                        GROUP BY SEASONS, HOUR")

# Task 9 - Weather Seasonality
weather_seasonality <- dbGetQuery(conn, "SELECT SEASONS,
                                         AVG(TEMPERATURE) AS avg_temperature,
                                         AVG(HUMIDITY) AS avg_humidity,
                                         AVG(WIND_SPEED) AS avg_wind_speed,
                                         AVG(VISIBILITY) AS avg_visibility,
                                         AVG(DEW_POINT_TEMPERATURE) AS avg_dew_point_temperature,
                                         AVG(SOLAR_RADIATION) AS avg_solar_radiation,
                                         AVG(RAINFALL) AS avg_rainfall,
                                         AVG(SNOWFALL) AS avg_snowfall,
                                         AVG(RENTED_BIKE_COUNT) AS avg_rented_bike_count
                                         FROM SEOUL_BIKE_SHARING
                                         GROUP BY SEASONS")

# Task 10 - Total Bike Count and City Info for Seoul
total_bike_count_seoul <- dbGetQuery(conn, "SELECT SUM(BIKE_SHARING_SYSTEMS.BICYCLES) AS total_bike_count,
                                            WORLD_CITIES.CITY,
                                            WORLD_CITIES.COUNTRY,
                                            WORLD_CITIES.LAT,
                                            WORLD_CITIES.LNG,
                                            WORLD_CITIES.POPULATION
                                            FROM WORLD_CITIES
                                            INNER JOIN BIKE_SHARING_SYSTEMS 
                                            ON WORLD_CITIES.CITY = BIKE_SHARING_SYSTEMS.CITY
                                            WHERE WORLD_CITIES.CITY = 'Seoul'")


# Task 11 - Find all city names and coordinates with comparable bike scale to Seoul's bike sharing system
similar_cities <- dbGetQuery(conn, "SELECT WORLD_CITIES.CITY, WORLD_CITIES.COUNTRY, WORLD_CITIES.LAT, WORLD_CITIES.LNG, WORLD_CITIES.POPULATION,
                                    BIKE_SHARING_SYSTEMS.BICYCLES AS total_bike_count
                                    FROM WORLD_CITIES
                                    INNER JOIN BIKE_SHARING_SYSTEMS 
                                    ON WORLD_CITIES.CITY = BIKE_SHARING_SYSTEMS.CITY
                                    WHERE BIKE_SHARING_SYSTEMS.BICYCLES BETWEEN 15000 AND 20000")
dbDisconnect(conn)


record_count
operational_hours
weather_outlook
seasons
date_range
View(all_time_high)
View(hourly_popularity_temp_season)
View(rental_seasonality)
View(weather_seasonality)
View(total_bike_count_seoul)
View(similar_cities)