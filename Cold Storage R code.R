# Setup Working Directory
setwd('/Users/vaibhavverdhan/Documents/Self 2/Start/Training/Great Learning/Session 8/')

getwd()

#upload your dataset
cold_temp = read.csv("Cold_Storage_Temp_Data.csv", header = TRUE)
dim(cold_temp)
str(cold_temp)
summary(cold_temp)

attach(cold_temp)

### Graphical Analysis


par(mfrow=c(1,2))

hist(Temperature,main='Cold Storage Temperature Histogram',xlab = "Temperature", 
ylab = "Frequency",col = "turquoise")

boxplot(Temperature,main='Cold Storage Temperature Box Plot',xlab = "Temperature", 
ylab = "Frequency",col = "turquoise1",horizontal = TRUE)

dev.off() 

boxplot(Temperature ~ Season,horizontal=TRUE,col=c("Red","Blue","Green"), main='Cold Storage Temperature across Season')

#########################################################################
#1. Find mean cold storage temperature for Summer, Winter and Rainy
#Season.
#########################################################################

View(cold_temp)
detach(cold_temp)
attach(cold_temp)
by(data=Temperature,INDICES = Season,FUN = mean)


#########################################################################

#2. Find overall mean for the full year
#########################################################################


##Mean Temp (Full Year)
mean_full_year_temp <- mean(cold_temp$Temperature)
mean_full_year_temp 



#########################################################################
#3. Find Standard Deviation for the full year
#########################################################################

##Standard deviation for entire year
sd_temp <- sd(cold_temp$Temperature)
sd_temp

#########################################################################
#4. Assume Normal Distribution, what is the probability of temperature
#having fallen below 2 Degree C?
#########################################################################

# calculate probability of temp <2 
min_temp <-2 

#
prob_less_2 <- pnorm(q = min_temp,
                     mean = mean_full_year_temp,
                     sd = sd_temp, lower.tail = TRUE )
prob_less_2


#########################################################################
#5. Assume Normal Distribution, what is the probability of temperature
#having gone above 4 Degree C?
#########################################################################
# calculate probability of temp >4
max_temp <-4



prob_greater_4 <- pnorm(q = max_temp,
                        mean = mean_full_year_temp,
                        sd = sd_temp, lower.tail = FALSE)
prob_greater_4


########################################################################
#6. What will be the penalty for the AMC Company?
########################################################################

## Combined Probability for <2 and >4

Total_Probability <- prob_greater_4+prob_less_2
Total_Probability

## Calculate Penalty
if(Total_Probability>0.025 && Total_Probability <= 0.05) {
  penalty <- '10%'
}else if(Total_Probability >0.5){
  penalty <- '25%'
}else
  penalty <- '0%'

penalty

## [1] "10%"
########################################################################
#7.	Perform a one-way ANOVA test to determine if there is significant difference in Cold Storage temperature between rainy, summer and winter seasons and comment on the findings.
########################################################################
model<-aov(Temperature~Season,data = cold_temp)
summary(model)


###########################################################################
#Problem 2: Cold_Storage_Mar2018
############################################################################

#Upload data set
cold_mar_data = read.csv("Cold_Storage_Mar2018.csv", header = TRUE)

dim(cold_mar_data)

str(cold_mar_data)
summary(cold_mar_data)

attach(cold_mar_data)

par(mfrow=c(1,2))

hist(Temperature,main='Cold Storage March Temperature Histogram',xlab = "Temperature", 
ylab = "Frequency",col = "turquoise")

boxplot(Temperature,main='Cold Storage March Temperature Box Plot',xlab = "Temperature", 
ylab = "Frequency",col = "turquoise1",horizontal = TRUE)

dev.off() 

mean <- mean(cold_mar_data$Temperature)
mean
mu = 3.9
n=35

# Using Z Test:
# ? Null Hypothesis: Temperature is maintained below 3.90c and
# hence no corrective action is required.
# H0: Temperature <= 3.90c
# ? Alternate Hypothesis: Temperature is above 3.90c and hence
# corrective action in the Cold Storage Plant is required.
# Ha: Temperature > 3.90c

sd_temp <-0.508589
#Z Score
z <- (mean - mu)/(sd_temp/(sqrt(n)))
z


pValue = pnorm(z,lower.tail = FALSE)
pValue


#############################################################################
# t -test
#############################################################################

t.test(cold_mar_data$Temperature, mu
       = mu,
       alternative = "greater",
       conf.level = .9)
