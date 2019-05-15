data <- read_csv("Documents/Course materials/Winter/442-Advanced Stats/Class 6/data_week.csv") 	# weekly data
names(data)
head(data,5)
temp <- data[,11]
n <- nrow(temp)				# centigrade

##### Representing Data as Time Series Objects #####

yy = ts(temp, frequency = 52, start = c(2015,1))
plot.ts(yy)



##### Moving Average #####

library("TTR")		# install TTR package first and then load the library
yy4 = SMA(yy,4)			# 4-wekk moving average
yy13 = SMA(yy,13)		# 13 week (quarterly) moving average
yy52 = SMA(yy,52)		# annual moving average

plot.ts(cbind(yy,yy4,yy13,yy52))		# understand the "smoothing" concept due to the averaging operation

# Sales: What's the growth rate? How do we obtain seasonally adjusted sales?
yd = decompose(yy) 
yd.trend = yd$trend
yd.seasonal = yd$seasonal
yd.random = yd$random
yy.season.adj = yy - yd.seasonal									# seasonally adjusted sales
plot.ts(cbind(yy,yd.trend, yd.seasonal, yd.random, yy.season.adj))


# default Holt Winters function
# HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
#             seasonal = c("additive", "multiplicative"),
#             start.periods = 2, l.start = NULL, b.start = NULL,
#             s.start = NULL,
#             optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
#             optim.control = list())


##### Simple Exponential Smoothing #####
least_sse <- 100000
possible_values <- c(FALSE, NULL)
model <- NULL
# for (beta in c(FALSE, NULL)) {
#   for (gamma in c(FALSE, NULL)) {
#     out1 <- HoltWinters(xx, beta=beta, gamma=gamma) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
#     sse <- out1$SSE
#     print(sse)
#     if (sse < least_sse) {
#       least_sse <- sse
#       model <- out1
#     }
#     #need to calculate AIC, BIC, AICc
# }}		

# tried using for loop, but failed utterly
p <- 1
info_criteria_calculation <- function(n, p, sse){
  sigma2 <- sse/(n-p) 
  AIC = n*log(sigma2) + 2*p
  AICC = AIC + 2*(p+1)*(p+2)/(n-p-2)
  BIC = n*log(sigma2) + n*log(p)
  return(list(AIC, AICC, BIC))
}


out1 <- HoltWinters(xx, beta=FALSE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
sse <- out1$SSE
p <- 53
info_criteria <- info_criteria_calculation(n, p, sse)
output_data1 <- cbind(sse, info_criteria[1], info_criteria[2], info_criteria[3])

  
out2 <- HoltWinters(xx, gamma=FALSE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
sse <- out2$SSE
p <- 2 
info_criteria <- info_criteria_calculation(n, p, sse)
output_data2 <- cbind(sse, info_criteria[1], info_criteria[2], info_criteria[3])

  
out3 <- HoltWinters(xx, beta=FALSE, gamma=FALSE) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
sse <- out3$SSE
p <- 1
info_criteria <- info_criteria_calculation(n, p, sse)
output_data3 <- cbind(sse, info_criteria[1], info_criteria[2], info_criteria[3])

  
out4 <- HoltWinters(xx) 				# Holt-Winters Filtering - only level updating b/c beta and gamma are zeros
sse <- out4$SSE
p <- 54
info_criteria <- info_criteria_calculation(n, p, sse)
output_data4 <- cbind(sse, info_criteria[1], info_criteria[2], info_criteria[3])

model_names <- c("HW: beta=FALSE", "HW: gamma= FALSE", "HW: beta=FALSE, gamma=FALSE", "HW-Exponential smoothing")
output_data <- cbind(model_names, rbind(output_data1, output_data2, output_data3, output_data4))
colnames(output_data) <- c("Model", "SSE", "AIC", "AICc", "BIC")
print(output_data)

# As we observe, both the models out1 and out4 have pretty close SSE.

plot(out3)	# graph of actual (black) vs fitted (red)

yhat=out3$fitted
yd = decompose(yhat) 
yd.random = yd$random							
plot(yhat)
plot(yd.random)

##### Out of Sample Forecasts

library("forecast")								# install "forecast" package and load the library
out3_pred = forecast:::forecast.HoltWinters(out3, h = 26, level = c(68, 95, 99))	
# forecast horizon 26 weeks. CI 50%, 80%, 95% 

# something wierd as should directly call forecast.HoltWinters() function, but does not. Hence, to force it, I used the prefix forecast::: 
plot(out3_pred)
