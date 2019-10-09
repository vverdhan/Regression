

library(car)

library(ggplot2) # for some nice looking graphs!

library(MASS) # Another library for our box-cox transform down the end. # Inspect and summarize the data.

head(Prestige,5)
str(Prestige)
summary(Prestige)
# Subset the data to capture only 
newdata = Prestige[,c(1:2)] 
summary(newdata)

# Histogram of Education
qplot(education, data = newdata, geom="histogram", binwidth=1) + labs(title = "Historgram of Average Years of Education") + labs(x ="Average Years of Education") +
  labs(y = "Frequency") +
  scale_y_continuous(breaks = c(1:20), minor_breaks = NULL) + scale_x_continuous(breaks = c(6:16), minor_breaks = NULL) +
  geom_vline(xintercept = mean(newdata$education), show.legend=TRUE, color="red", labels="Average") +
  geom_vline(xintercept = median(newdata$education), show.legend=TRUE, color=" blue", labels="Median")

# Histogram of Income

?geom_vline

qplot(income, data = newdata, geom="histogram", binwidth=1000) + labs(title = "Historgram of Average Income") +
  labs(x ="Average Income") + labs(y = "Frequency") + scale_y_continuous(breaks = c(1:20), minor_breaks = NULL) +scale_x_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000, 20000, 22000, 24000, 26000), minor_breaks = NULL) +geom_vline(xintercept = mean(newdata$income), show_guide=TRUE, color="red", labels="Average") +geom_vline(xintercept = median(newdata$income), show_guide=TRUE, color="blu e", labels="Median")
  
  
set.seed(1)
education.c = scale(newdata$education, center=TRUE, scale=FALSE)
mod = lm(income ~ education.c, data = newdata)
summary(mod)

Income = 898.8*Education + 6797.9

# visualize the model results.
qplot(education.c, income, data = newdata, main = "Relationship between Income and Education") + stat_smooth(method="lm", col="red") + scale_y_continuous(breaks = c(1000, 2000, 4000, 6000, 8000, 10000, 12000,14000, 16000, 18000, 20000, 25000), minor_breaks = NULL)

