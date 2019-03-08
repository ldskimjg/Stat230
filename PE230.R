library(tidyverse)

#Question 1


1 - pf(3.728798,1,6)
1 - pf(1.624,6,6)
1 - pf(3.177,1,6)
1 - pf(40.796,1,6)
#Question 8
schizo <- c(-2,-1,-3,2,2,7,0,0,3,-2,-4,-1,3,1,0,2,-3,-2,-2,0)
norm <- c(7,4,5,2,-1,2,2,6,3,0,0,-2,-2,3,1,1,2,6,2,2)
1 - pf(6.901,1,38)
#Question 10
compact <- c(643,655,702)
midsize <- c(469,427,525)
fullsize <- c(484,456,402)
cars <- tibble(compact,midsize,fullsize) %>% 
gather() %>% 
rename(type = key, claim = value)

anova(lm(value ~ key, data = cars))

anova(lm(claim ~ type, data = cars))

mean(compact) + qt(0.975,length(compact)-1)*(sd(compact)/sqrt(length(compact)))
mean(compact) - qt(0.975,length(compact)-1)*(sd(compact)/sqrt(length(compact)))
mean(midsize) + qt(0.975,length(midsize)-1)*(sd(midsize)/sqrt(length(midsize)))
mean(midsize) - qt(0.975,length(midsize)-1)*(sd(midsize)/sqrt(length(midsize)))
mean(fullsize) + qt(0.975,length(fullsize)-1)*(sd(fullsize)/sqrt(length(fullsize)))
mean(fullsize) - qt(0.975,length(fullsize)-1)*(sd(fullsize)/sqrt(length(fullsize)))

#Question 14
ethanol <- c("10%", "15%","15%","10%","20%","20%","15%","15%","10%","15%","20%","10%","10%")
amount <- c(5,8,9,5,7,9,10,10,9,8,7,5,10)
temp <- c(80,76,77,90,78,74,73,73,78,80,81,82,77)
carb <- c(20,17,26,26,38,15,25,54,27,11,23,13,120)

emission <- tibble(ethanol, amount, temp, carb)

anova(lm(carb ~ ethanol, data = emission))

#Question 16
(74 - 38.25) - qt(0.95,df=4)*sqrt(((1/4)+(1/4))*84.625)
(74 - 38.25) + qt(0.95,df=4)*sqrt(((1/4)+(1/4))*84.625)
#Question 17
small <- c(12,20,14,23)
medium <- c(15,23,13,28)
large <- c(14,30,17,25)
saws <- tibble(small,medium,large) %>% gather() %>% rename(size = key, trees = value)
anova(lm(trees ~ size, data = saws))

#SS Grandmean
12*mean(saws$trees)^2
#SS Total
sum(saws$trees^2)
#P value
1 - pf(0.425,2,9)

mean(small) - mean(saws$trees)
mean(medium) - mean(saws$trees)
mean(large) - mean(saws$trees)
sum((small - mean(small))^2)
sum((medium - mean(medium))^2)
sum((large - mean(large))^2)
