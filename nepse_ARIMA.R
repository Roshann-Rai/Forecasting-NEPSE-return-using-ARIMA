options(warn = -1)
options(scipen = 10000)
options(repr.plot.width = 10.8, repr.plot.height = 7.2)
#Importing Libraries
pacman::p_load(dplyr, tidyverse, ggplot2, readr, xts, forecast,TSstudio, tseries)

#Importing datasets
nepse <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/Time-series-analysis-of-GDP-and-NEPSE-data/master/nepse.csv")


#Lets look the datasets
head(nepse, 5)
str(nepse)


# #Lets clean the gdp_growth_rate dataset
# gdp_cleaned <- gdp_growth_rate %>%
#   rename(growth_rate = gdp) %>%             #renaming the gdp column in growth_rate
#   mutate(growth_rate = as.numeric(gsub("%", '', growth_rate)))            #removing the % character and converting the growth rate column into numeric


# #Wide to long
# gdp_reshaped <- gather(gdp, key = "year", value = "gdp", 2:63)

# Converting date into date format
nepse$date <- as.Date(nepse$date, format = "%m/%d/%Y")


#Converting the nepse df into xts 
nepse_ts <- xts(nepse$close, order.by = nepse$date)

#Daily return
nepse_daily_ts <- diff(log(nepse_ts))
nepse_daily_ts <- nepse_daily_ts[-1,]
nepse_daily_avg_ts <- apply.monthly(nepse_daily_ts, FUN = mean)  #calculates the average daily return for every month

#converting nepse_daily_avg_ts into dataframe
nepse_daily_avg_df <- as.data.frame(nepse_daily_avg_ts)
nepse_daily_avg_df$date <- rownames(nepse_daily_avg_df)    
rownames(nepse_daily_avg_df) <- NULL           
head(nepse_daily_avg_df,5)

#calculating monthly return
nepse_monthly_return <- nepse_daily_avg_df %>%
  mutate(monthly_return = ((1+V1)^22)-1,
         date = as.Date(date, format = "%Y-%m-%d")) %>%
  select(date, monthly_return)
str(nepse_monthly_return)

#converting monthly return into xts
nepse_monthly_return_ts <- xts(nepse_monthly_return$monthly_return, order.by = as.yearmon(nepse_monthly_return$date))
plot(nepse_monthly_return_ts)

#Plotting the nepse index
# plot(nepse_ts,
#      main = "Nepse Index",
#      type = "b",
#      col = "black",
#      grid.col = NA,
#      cex.axis = 1.3, pch = 20)


##Forecasting NEPSE
#Testing stationarity
adf.test(nepse_monthly_return_ts)

#Plotting acf and pacf
par(mfrow = c(1,2))
acf(nepse_monthly_return_ts)
pacf(nepse_monthly_return_ts)

#Creating the model using auto.arima()
model_nepse <- auto.arima(nepse_monthly_return_ts, ic="aic", trace = T)
model_nepse


#Has the model been stationary?
par(mfrow = c(1,2))
acf(ts(model_nepse$residuals))
pacf(ts(model_nepse$residuals))

#Lets forecast the gdp for next 10 years at 95% confidence interval
nepse_forecast <- forecast(model_nepse, level = c(95), h = 12)
nepse_forecast
plot(nepse_forecast)


