# Read in Data

# Popcorn Example ~

# Typing in
scores <- c(36,63,47,67,28,81,42,85)
salt <- c(0,1,0,1,0,1,0,1)
oil <- c(0,0,0,0,1,1,1,1)
brand <- c(1,1,0,0,1,1,0,0)

pop.lm <- lm(scores ~ salt + oil + brand)
anova(pop.lm)

# read.table
popcorn <- read.table(file = "popcorn.txt", 
            col.names=c("salt2","oil2","brand2","score2"))

popcorn
# Aware of Capital letter
View(popcorn)

# Build linear model and use of anova function
pop.lm2 <- lm(score2 ~ salt2 + oil2 + brand2, data = popcorn)
pop.lm2.1 <- lm(popcorn$score2 ~ popcorn$salt2 + popcorn$oil2 + popcorn$brand2)

summary(pop.lm2)
summary(pop.lm2.1)

anova(pop.lm2)
anova(pop.lm2.1)


# What does cbind do? + matrix
smallfish <- cbind( c(5.4, 5.2, 6.1, 4.8, 5.0, 4.8, 3.9, 4.0, 4.8, 5.4, 4.9, 5.7),
                    c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
smallfish
smallfish.1 <- c(5.4, 5.2, 6.1, 4.8, 5.0, 4.8, 3.9, 4.0, 4.8, 5.4, 4.9, 5.7,1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
smallfish.1

smallfish[1,1]
tapply(smallfish[,1], smallfish[,2], mean)


# Master HW question #58
schi <- c(-2,-1,-3,2,2,7,0,0,3,-2,-4,-1,3,1,0,2,-3,-2,-2,0)
norm <- c(7,4,5,2,-1,2,2,6,3,0,0,-2,-2,3,1,1,2,6,2,2)
mean(normal)
mean(schizo)
length(schi)
length(norm)

mydata <- data.frame(condition = c(rep("schizo",20),rep("normal",20)), response = c(schi,norm))
mydata

library(tidyverse)
mydata <- tibble(schi, norm) %>% gather() %>% rename(condition = key, response = value)
mydata
# If you didn't do gather or rename
mydata.1 <- tibble(schi,norm)
mydata.1
mydata.2 <- tibble(schi,norm) %>% gather()
mydata.2

mylm <- lm(response ~ condition, data = mydata)
mylm.1 <- lm(mydata$response ~ mydata$condition)

anova(mylm)

# Master HW question # 67
singer <- read.csv(file = "singerheights.csv", header = TRUE)
singer

singer.lm <- lm(height ~ gender + part + gender*part, data = singer)
anova(singer.lm)

library(car)
singer.lm2 <- lm(height ~ gender + part + gender*part, data = singer, 
                 contrasts = list(gender = contr.sum, part = contr.sum))
Anova(singer.lm2, type = "III")

# power.anova.test, HW question 60
nint <- c(2:20)

#(a)
powerA <- power.anova.test(groups = 4, between.var = var(c(21,23,25,27)),within.var=4.7^2, n = nint)
plot(nint,powerA$power)
plot(nint,powerA$power, type ="l")
plot(nint,powerA$power, type ="l", col = "purple", lwd=4, ylim = c(0,1))
plot(nint,powerA$power, type ="l", col = "purple", lwd=4, ylim = c(0,1),
     main = "power graph!!!!", xlab = "Sample size", ylab = "Power!!!")

#(b)
power.anova.test(groups = 4, between.var = var(c(21,23,25,27)),within.var=4.7^2, sig.level = 0.05, power = 0.85)

#(c)
powerC <- power.anova.test(groups = 4, between.var = var(c(27,25,23,21)),within.var=4.7^2, n = nint)
plot(nint,powerA$power, type ="l", col = "purple", lwd=4, ylim = c(0,1),
     main = "power graph!!!!", xlab = "Sample size", ylab = "Power!!!")
lines(powerC$n, powerC$power, lwd = 2, col = "red")

#(d)
powerD <- power.anova.test(groups = 4, between.var = var(c(21,21,21,27)),within.var=4.7^2, n = nint)
lines(powerD$n, powerD$power, lwd = 2, col = "blue")
#(e) - I will let you try it

# Interaction plot
wear <- read.csv(file = "wear.csv", header = TRUE)
wear

as.factor(wear$prop)
as.factor(wear$filler)

interaction.plot(wear$prop, wear$filler, wear$wear1)
interaction.plot(wear$filler,wear$prop,wear$wear1)

wear.lm <- lm(wear1 ~ prop + filler + prop*filler, data = wear)
anova(wear.lm)
