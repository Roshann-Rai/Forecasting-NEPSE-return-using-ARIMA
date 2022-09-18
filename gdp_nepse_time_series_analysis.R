options(warn = -1)
options(scipen = 10000)
options(repr.plot.width = 10.8, repr.plot.height = 7.2)
#Importing Libraries
pacman::p_load(dplyr, tidyverse, ggplot2, readr, xts, forecast,TSstudio, tseries)

#Importing datasets
nepse <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Time-series-analysis-of-GDP-and-NEPSE-data/master/nepse.csv")
gdp_growth_rate <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Time-series-analysis-of-GDP-and-NEPSE-data/master/gdp_growth_rate_np.csv")
nepse_2022_2014_daily_returns <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Time-series-analysis-of-GDP-and-NEPSE-data/master/nepse_index_2022_2014.csv")
gdp <- read_csv("https://raw.githubusercontent.com/Roshann-Rai/Time-series-analysis-of-GDP-and-NEPSE-data/master/gdp.csv", show_col_types = F)

#Lets look the datasets
head(nepse, 5)
head(gdp_growth_rate, 5)
head(nepse_2022_2014_daily_returns)
head(gdp, 5)

#look the structure of datasets
glimpse(nepse)
glimpse(gdp)
glimpse(gdp_growth_rate)
glimpse(nepse_2022_2014_daily_returns)

#Lets clean the gdp_growth_rate dataset
gdp_cleaned <- gdp_growth_rate %>%
  rename(growth_rate = gdp) %>%             #renaming the gdp column in growth_rate
  mutate(growth_rate = as.numeric(gsub("%", '', growth_rate)))            #removing the % character and converting the growth rate column into numeric

#Lets add the gdp growth rate of 2022 in gdp_cleaned
year = c(2022)
growth_rate = c(4.1)
gdp_2022 <- data.frame(year, growth_rate)
gdp_1997_2022 <- rbind(gdp_cleaned, gdp_2022)
str(gdp_1997_2022)

#Cleaning the nepse_2022_2014_daily_returns datasets
nepse_2022_2014 <- nepse_2022_2014_daily_returns %>%
  mutate(date = as.character(date, format = "%d/%m/%Y"),
         nepse_index = as.numeric(gsub(",", "", nepse_index))) %>%     #removing , from the nepse_index column and converting into numeric format
  filter(date < "30/02/2014")      #filtering indexes before 2020/01/01

str(nepse_2022_2014)

#Wide to long
gdp_reshaped <- gather(gdp, key = "year", value = "gdp", 2:63)

#gdp of nepal
gdp_nepal <- gdp_reshaped %>%
  mutate(date = year) %>%
  select(date, gdp)

#Format date column as date
gdp_nepal$date <- as.Date(paste(gdp_nepal$date, 12, 31, sep = "/"))

#Into xts object
gdp_nepal_ts <- xts(gdp_nepal$gdp, order.by = gdp_nepal$date)
head(gdp_nepal_ts)

#Converting the nepse_2022_2019 dataframe in xts 
nepse_2022_2014_ts <- xts(nepse_2022_2014$nepse_index, order.by = as.POSIXct(nepse_2022_2014$date, format = "%d/%m/%Y"))
str(nepse_2022_2014_ts)
head(nepse_2022_2014_ts)

#Monthly return
nepse_2014_2022_monthly <- apply.monthly(nepse_2022_2014_ts, FUN = mean)
head(nepse_2014_2022_monthly)

#Extract nepse index for 2015
nepse_2015_ts <- nepse_2022_2014_ts["2015",]
nepse_2015 <- round(mean(apply.yearly(nepse_2015_ts, FUN = mean)), digit = 2)
head(nepse_2015)

#Extract nepse index for 2016
nepse_2016_ts <- nepse_2022_2014_ts["2016",]
nepse_2016 <- round(mean(apply.yearly(nepse_2016_ts, FUN = mean)), digit = 2)
head(nepse_2016)

#Extract nepse index for 2017
nepse_2017_ts <- nepse_2022_2014_ts["2017",]
nepse_2017 <- round(mean(apply.yearly(nepse_2022_2014_ts, FUN = mean)), digit = 2)
head(nepse_2017)

#Extract nepse index for 2018
nepse_2018_ts <- nepse_2022_2014_ts["2018",]
nepse_2018 <- round(mean(apply.yearly(nepse_2018_ts, FUN = mean)), digit = 2)
head(nepse_2018)

#Extract nepse index for 2019
nepse_2019_ts <- nepse_2022_2014_ts["2019",]
nepse_2019 <- round(mean(apply.yearly(nepse_2019_ts, FUN = mean)), digit = 2)
head(nepse_2019)

#Extract nepse index for 2020
nepse_2020_ts <- nepse_2022_2014_ts["2020",]
nepse_2020 <- round(mean(apply.yearly(nepse_2020_ts, FUN = mean)), digit = 2)
head(nepse_2020)

#Extract nepse index for 2021
nepse_2021_ts <- nepse_2022_2014_ts["2021",]
nepse_2021 <- round(mean(apply.yearly(nepse_2021_ts, FUN = mean)), digit = 2)
head(nepse_2021)

#Extract nepse index for 2022
nepse_2022_ts <- nepse_2022_2014_ts["2022",]
nepse_2022 <- round(mean(apply.yearly(nepse_2022_ts, FUN = mean)), digit = 2)
head(nepse_2022)

#Creating a new dataframe
year <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
nepse_index <- c("1028.88", "1539.43", "1553.74", "1261.82", "1192.81", "1523.69", "2700.05", "2314.07")
nepse_2015_2022 <- data.frame(year, nepse_index)

#Format year column as date
nepse_2015_2022$year <- as.Date(paste(nepse_2015_2022$year, 12, 31, sep = "/"))

#Creating time series object
nepse_2015_2022_ts <- xts(as.numeric(nepse_2015_2022$nepse_index), order.by = nepse_2015_2022$year)
nepse_2015_2022_ts

#Converting year column of nepse dataframe into date format
nepse$year <- as.Date(paste(nepse$year, 12, 31, sep = "/"))

#converting nepse dataframe into xts object
nepse_1997_2019_ts <- xts(nepse$nepse_index, order.by = nepse$year)
nepse_1997_2014_ts <- nepse_1997_2019_ts["1997/2014",]
nepse_1997_2014_ts

#Merging the xts objects
nepse_ts <- rbind(nepse_1997_2014_ts, nepse_2015_2022_ts)
nepse_ts

#Plotting the nepse index
plot(nepse_ts,
     main = "Nepse Index",
     type = "b",
     col = "black",
     grid.col = NA,
     cex.axis = 1.3, pch = 20)

#Yearly return on nepse 
return_annual <- (round(diff(log(nepse_ts))[-1,], digit = 4))*100
return_annual

#Formatting year column of gdp_1997_2022 as character
gdp_1997_2022$year <- as.character(gdp_1997_2022$year)

#Formatting year column of gdp_1997_2022 as date
gdp_1997_2022$year <- as.Date(paste(gdp_1997_2022$year, 12, 31, sep = "/"))

#Creating a time series object of gdp_cleaned
gdp_ts <- xts (gdp_1997_2022$growth_rate, order.by = gdp_1997_2022$year)
head(gdp_ts, 5)

#merge gdp and nepse xts objects
nepse_gdp_ts <- merge.xts(gdp_ts, return_annual)
plot(nepse_gdp_ts, cex.axis = 1.2, type = "b", pch = 20, ylab = "Percentage", main = "Nepse Annual return and GDP Growth Rate")
legend("topleft", legend = c("GDP Growth Rate", "NEPSE Annual Return"), col = c("black", "red"))

#The plot of Nepse Annual Return and GDP Growth Rate of Nepal shows no relationships.

#Creating gdp_nepse_df
gdp_nepse_df <- as.data.frame(nepse_gdp_ts)

#Naming the column date
gdp_nepse_df$date <- rownames(gdp_nepse_df)    # Convert row names to column
rownames(gdp_nepse_df) <- NULL           # Reset row names
gdp_nepse_df <- gdp_nepse_df[-1,]

#Correlation coeff. between gdp growth rate and NEPSE annual return
gdp_nepse_selected <- gdp_nepse_df %>%
  select(return_annual, gdp_ts)
cor <- cor(gdp_nepse_selected)
cor

#Test of Stationarity
adf.test(gdp_nepal_ts, k = 15)

#Forecasting GDP
#Plotting acf and pacf
par(mfrow = c(1,2))
acf(gdp_nepal_ts)
pacf(gdp_nepal_ts)

#Creating the model using auto.arima()
model <- auto.arima(gdp_nepal_ts, ic="aic", trace = T)
model

#Has the model been stationary?
par(mfrow = c(1,2))
acf(ts(model$residuals))
pacf(ts(model$residuals))

#Lets forecast the gdp for next 10 years at 95% confidence interval
gdp_forecast <- forecast(model, level = c(95), h = 10)
gdp_forecast
plot(gdp_forecast)

##Forecasting NEPSE
#Testing stationarity
adf.test(nepse_2014_2022_monthly, k = 15)

#Plotting acf and pacf
par(mfrow = c(1,2))
acf(nepse_2014_2022_monthly)
pacf(nepse_2014_2022_monthly)

#Creating the model using auto.arima()
model_nepse <- auto.arima(nepse_2014_2022_monthly, ic="aic", trace = T)
model_nepse

#Has the model been stationary?
par(mfrow = c(1,2))
acf(ts(model_nepse$residuals))
pacf(ts(model_nepse$residuals))

#Lets forecast the gdp for next 10 years at 95% confidence interval
nepse_forecast <- forecast(model_nepse, level = c(95), h = 12)
nepse_forecast
plot(nepse_forecast)

