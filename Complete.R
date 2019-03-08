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



smallfish <- cbind( c(5.4, 5.2, 6.1, 4.8, 5.0, 4.8, 3.9, 4.0, 4.8, 5.4, 4.9, 5.7),
                    c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
smallfish.1 <- c(5.4, 5.2, 6.1, 4.8, 5.0, 4.8, 3.9, 4.0, 4.8, 5.4, 4.9, 5.7,1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)


# Master HW question #58
schi <- c(-2,-1,-3,2,2,7,0,0,3,-2,-4,-1,3,1,0,2,-3,-2,-2,0)
norm <- c(7,4,5,2,-1,2,2,6,3,0,0,-2,-2,3,1,1,2,6,2,2)
mean(normal)
mean(schizo)
length(schi)
length(norm)

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