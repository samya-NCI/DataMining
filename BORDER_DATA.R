border_Data = read.csv("Border_Crossing_Entry_Data.csv")
border_data2 = read.csv("BORDER_DATA.csv")
summary(border_Data)

border_backup1 = border_Data
border_backup2 = border_Data
border_Data = border_backup2
border_backup3 = border_data2

border_data2 = select(border_data2, -c(10,11,12,13))

##### ARIMA
set.seed(123)
sample_data = sample(1:nrow(border_data2), 1000)
border_data2 = border_data2[sample_data, -c(10,11,12,13)]

border_Data$Date = as.character.Date(border_Data$Date)

library("anytime")
install.packages("anytime")
border_Data$Date = anytime(border_Data$Date)
border_Data$Date = strptime(border_Data$Date, "%m/%d/%y %H:%M:%S")
class(border_Data$Date)
border_Data$Date = format(border_Data$Date, "%m/%d/%y")
border_data2$date = as.Date(border_data2$Date,"%m/%d/%y")

install.packages('ggplot2')
install.packages('forecast')
install.packages('tseries')
library('ggplot2')
library('forecast')
library('tseries')

ggplot(border_data2, aes(date,Value)) + geom_line() + scale_x_date('year')+ylab("No. of Vehicles") + xlab("")
write.csv(border_Data,"BORDER_DATA.csv")

count_ts = ts(border_data2[,c('Value')])
border_data2$clean_Value = tsclean(count_ts)

ggplot() +geom_line(data = border_data2, aes(x = date, y = clean_Value)) + ylab('Clean Value')

border_data2$count_monthly = ma(border_data2$clean_Value, order = 1)
border_data2$count_ma30 = ma(border_data2$clean_Value, order = 30)

ggplot() + geom_line(data = border_data2, aes(x = date, y = clean_Value, colour = "Counts")) + geom_line(data = border_data2, aes(x = date, y = count_monthly, colour = 'Monthly Moving Average')) + ylab("Vehicle numbers")

count_ma = ts(na.omit(border_data2$count_monthly), frequency = 30)
decomp = stl(count_ma, s.window = "periodic")
deseasonal_count = seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")
Acf(count_ma, main = '')
Pacf(count_ma, main = '')

auto.arima(deseasonal_count, seasonal = FALSE)

fit = auto.arima(deseasonal_count, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = '(1,1,1) Model Residuals')

fit2 = arima(deseasonal_count, order = c(1,1,7))
tsdisplay(residuals(fit2), lag.max = 15, main = 'Seasonal Model Residuals')
fit
fcast = forecast(fit2,h = 30)
fcast

count_d1 = diff(deseasonal_count, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')
