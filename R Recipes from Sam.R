# STAT 230 Recipes - general R
# ANYTHING AFTER A # IS A COMMENT AND DOESN'T RUN ANY CODE - IT'S JUST THERE TO EXPLAIN THE CODE TO YOU
  # CREATING A VARIABLE:
    answerToTheUniverse <- 42
  
  # CREATING A LIST OF DATA
    observations <- c(23,23.4,45,67)
  
  # GETTING LENGTH OF A LIST
    numObservations <- length(observations)
  
  # ARTITHMETIC
    sumOfNums <- 2 + 3 # = 5
    prodOfNums <- 2 * 3 # = 6
    quotientOfNums <- 2 / 3 # = 2/3
    differenceOfNums <- 2 - 3 # = -1
    numSquared <- 2^2 # = 4
    squareRoot <- sqrt(3) # the square root of 3
    absValOfDiff <- abs(differenceOfNums) # calculates the absolute value of differenceOfNums (= 1)
    # artithmetic can be used with variables and numbers in any combination
    someNum <- 3.7
    otherNum <- 2
    sumOfNums <- someNum + otherNum
    prodOfNums <- someNum * 3
  
  # BASIC STATS FUNCTIONS ON LISTS
  # you can run some functions on lists to get basic data from them
    meanOfObs <- mean(observations) # gets the mean of our observations
    sdOfObs <- sd(observations) # gets the standard deviation of our observations
    minObs <- min(observations) # gets the smallest value in our observations
    maxObs <- max(observations) # gets the biggest value in our observations
    medianObs <- median(observations) # gets the median of our observations
    varObs <- var(observations) # gets the variance of our observations

# STAT 230 RECIPES:
  # RECIPE 1: One-sided Z-test
    # 1) Find the z-score (compare to equation)
      observations1 <- c(11.176, 7.089, 8.097, 11.739, 11.291, 10.759, 6.467, 8.315)
      observations2 <- c(5.263, 6.748, 7.461, 7.015, 8.133, 7.418, 3.772, 8.963)
      sd1 <- sd(observations1)
      sd2 <- sd(observations2)
      zScore <- (mean(observations1) - mean(observations2)) / (sqrt(sd1^2/length(observations1) + sd2^2/length(observations2)))
    # 2) Find the ONE-SIDED p-value
      p <- pnorm(-abs(zscore))
  
  # RECIPE 2: Two-sided Z-Test
    # 1) Find the z-score (compare to equation)
      observations1 <- c(11.176, 7.089, 8.097, 11.739, 11.291, 10.759, 6.467, 8.315)
      observations2 <- c(5.263, 6.748, 7.461, 7.015, 8.133, 7.418, 3.772, 8.963)
      sd1 <- sd(observations1)
      sd2 <- sd(observations2)
      zScore <- (mean(observations1) - mean(observations2)) / (sqrt(sd1^2/length(observations1) + sd2^2/length(observations2)))
    # 2) Find the TWO-SIDED p-value
      p <- 2 * pnorm(-abs(zScore)) # the only difference from the one-sided is that we multiply p by 2

  # RECIPE 3: Calculate Z Standard Error, Margin of Error, and Confidence Interval
    # We already have p from above or from the question
    # 1) Find the Z Critical Value (z*) from p
      zCrit <- qnorm(p)
    # 2) Calculate Standard Error
      stdErr <- sqrt(sd1^2/length(observations1) + sd2^2/length(observations2))
    # 3) Calculate Margin of Error
      marginErr <- zCrit * stdErr
    # 4) Calculate Bound for Confidence Interval
      meanDiff <- mean(observations1) - mean(observations2)
      lowerBound <- meanDiff - marginErr
      upperBound <- meanDiff + marginErr

  # RECIPE 4: Calculate Sample Size for Z
    sigma <- 3 # random value given by problem
    ME <- .2 # random margin of error provided by problem
    # 1) Calculate sample size
      n <- (zCrit * sigma / ME)^2
        
  # RECIPE 5: Calculate t-test vs theoretical pop mean (mu) (1-sided - Ha is that mu is really greater)
    mu <- 18 # arbitrary mu provided by question
    t.test(observations, mu=mu, alternative="greater")
    # t.test(data, mu=mu, alternative="less")
    
  # RECIPE 6: Calculate t-test vs theoretical pop mean (mu) (2-sided)
    t.test(observations, mu=mu, alternative="two.sided")
    
  # RECIPE 7: Calculate paired t-test (1 sided)
    t.test(observations1, observations2, paired=TRUE, alternative = "greater")
    # t.test(observations1, observations2, paired = TRUE, alternative = "less")
  
  # RECIPE 8: Calculate paired t-test (2 sided)
    t.test(observations1, observations2, paired=TRUE, alternative = "two.sided")
    
  # RECIPE 9: Calculate two-sample t-test
    # 1) Detect if can pool
      canPool = max(sd(observations1), sd(observations2)) / min(sd(observations1), sd(observations2)) < 2
    # 2) If you can pool, calc pooled t-test
      t.test(observations1, observations2, var.equal=TRUE, alternative="two.sided") # two-sided
      # t.test(observations1, observations2, var.equal=TRUE, alternative="greater")
      # t.test(observations1, observations2, var.equal=TRUE, alternative="less")
    # 3) If you can't pool, calc non-pooled t-test
      t.test(observations1, observations2, alternative="two.sided") # two-sided
      # t.test(observations1, observations2, alternative="greater")
      # t.test(observations1, observations2, alternative="less")
  
  # RECIPE 10: Calculate t-test for a given confidence level (default is .95; remember that 1-confidence = alpha)
    t.test(observations, mu=mu, alternative="greater", conf.level=.99)
    t.test(observations1, observations2, var.equal=TRUE, alternative="two.sided", conf.level=.90)
    t.test(observations1, observations2, alternative="less", conf.level=.90)
    t.test(observations1, observations2, paired = TRUE, alternative = "greater", conf.level=.93)
    
  # RECIPE 11: Calculate Margin of Error & Standard Error for t-test
    # 1) Save results of t-test in a variable
      tTest <- t.test(observations1, observations2, var.equal=TRUE, alternative="two.sided")
    # 2) Calculate Margin of Error off of tTest results
      ME <- tTest$conf.int[2] - tTest$conf.int[1]
    # 3) Calculate Standard Error by subtracting Margin of Error from upper bound 
      SE <- tTest$conf.int[2] - ME

  # RECIPE 12: Calculate Sample Size(s) for t-test(s)
    # 1) Prepare inputs for calculation of sample size
      delta <- .8
      sigma <- 3.6
      d <- delta / sigma
      alpha <- .05
      desiredPower <- .80
    # 2) Calculate required sample size (rounding-up to next integer via ceiling())
      n <- ceiling(power.t.test(d = d, sig.level = alpha, power = desiredPower, type="two.sample")$n)
      # power.t.test(d = d, sig.level = alpha, power = desiredPower, type = "one.sample")
      # power.t.test(d = d, sig.level = alpha, power = desiredPower, type = "paired")

  # RECIPE 13: Read-in a table
    # 1) Set working directory to where your file is (mine is at ~, which is the home directory)
      setwd("~")
    # 2) Read-in table to a variable
      cancer <- read.table("cancer.txt", header=TRUE)
      
  # RECIPE 14: Grab column off table
    days <- cancer$days # this is a list of all the values in the "days" column
    
  # RECIPE 15: Add column to table
    # this adds a column called "dblDays" whose value is double the value of whatever's in days
    cancer$dblDays <- cancer$days * 2 
    
  # RECIPE 16: Get grouped results
    # this groups days by cancer type and reports the standard deviation of days for each cancer type
    aggregate(days ~ type, data = cancer, FUN = sd)
    # this groups days by cancer type and reports the mean of days for each cancer type
    aggregate(days ~ type, data = cancer, FUN = mean)
    
  # RECIPE 17: Create box plots
    # creates a box plot of days for each cancer type and specifies x and y labels
    boxplot(cancer$days ~ cancer$type, ylab="Days Survived", xlab="Cancer Type", plot=TRUE)
    
  # RECIPE 18: Find number of outliers without printing boxplot
    numOutliers <- length(boxplot(cancer$days, plot=FALSE)$out)
    
  # RECIPE 19: Determine if ANOVA can be applied (also check factors?)
    # 1) Response variable must be continuous
    # 2) Factors must be categorical (categories may be represented by a small set of meaningful numbers)
    # 3) Check for equal variance
      sds <- aggregate(days ~ type, data = cancer, FUN = sd)$days
      hasEqualVariance <- max(sds) / min(sds) < 2
      # if hasEqualVariance is false, try log'ing & check again
        # create new column with log'd version of old column
        cancer$logDays <- log(cancer$days)
        sds <- aggregate(logDays ~ type, data = cancer, FUN = sd)$logDays
        hasEqualVariance <- max(sds) / min(sds) < 2
        # if hasEqualVariance is false, all hope is lost, and ANOVA can't be used
    # 4) Check for normality with plot &/or outliers - if not normal, too bad, we're in Stat 230
      daysOutliers <- length(boxplot(cancer$days, plot=FALSE)$out)
      # a few outliers are okay - check to see if the number of outliers is bearable
      # if the log has not yet been taken, and you have normality issues, take the log & check again
        cancer$logDays <- log(cancer$days)
        sds <- aggregate(logDays ~ type, data = cancer, FUN = sd)$logDays
        # check to see if the number of outliers is bearable
          daysOutliers <- length(boxplot(cancer$days, plot=FALSE)$out)
    # 5) Determine that one response is independent from other responses (one observation shouldn't 
      #   affect another)
    
  # RECIPE 20: One-way ANOVA
    cancer.lm <- lm(days ~ type, data=cancer)
    anova(cancer.lm)
    
  # RECIPE 21: Two-way ANOVA
    # since we have a numeric column that we want to treat as a category, we define it as a factor
    cancer$gender <- as.factor(cancer$gender)
    # check the effect on days of gender, type, and the interaction between gender and type
    cancer.lm <- lm(days ~ gender + type + gender:type, data=cancer)
    
    anova(cancer.lm)
    # after checking these effects, we see that the interaction between gender:type is insignificant,
    #   so we can re-run the anova without the interaction in order to free-up more DF for other analysis
    cancer.lm <- lm(days ~ gender + type, data=cancer)
    anova(cancer.lm)
    
    # a more detailed breakdown of the results
    summary(cancer.lm)
    
  # RECIPE 22: Three-way ANOVA with intervals (with some techniques not necessary for this class)
    # since we have a numeric column that we want to treat as a category, we define it as a factor
    cancer$gender <- as.factor(cancer$gender)
    
    #since we want to use a continuous column as a category, we need to break it into named ranges
    maxAge <- max(cancer$age)
    minAge <- min(cancer$age)
    intervals <- 4 # arbitrarily to divide into 4 age ranges
    intervalLength <- (maxAge - minAge) / intervals
    
    # create empty breaks and labels lists
    breaks <- c(minAge)
    labels <- c()
    
    # build breaks and labels lists
    i <- minAge
    for (i in 1:4) {
      breaks[i + 1] <- breaks[i] + intervalLength
      labels[i] <- paste(breaks[i], '-', breaks[i] + intervalLength, sep='')
    }
  
    # create new column for the age range
    cancer$ageRange <- cut(cancer$age, breaks=breaks, labels=labels)
    
    # check the effect on days of gender, type, ageRange, the interactions between gender and type,
    #   gender and ageRange, type and ageRange, gender and type and ageRange
    cancer.lm <- lm(days ~ gender + type + ageRange + gender:type + gender:ageRange + type:ageRange + type:ageRange:gender, data=cancer)
    
    anova(cancer.lm)
    
    # after checking these effects, we see that all interactions are insignificant so, remove them
    cancer.lm <- lm(days ~ gender + type + ageRange, data=cancer)
    anova(cancer.lm)
    
    # a more detailed breakdown of the results
    summary(cancer.lm)

  # RECIPE 23: Calculate Power of ANOVA test (relies on educated guesses about variance)
    # 1) Calculate power anticipating small variance (for 2-way anova of type and gender on days)
      alpha <- .05
      groupMeans <- aggregate(days ~ type + gender, data = cancer, FUN = mean)
      numGroups <- length(groupMeans$days)
      groupSampleSize <- 4 # b/c whatever - just arbitrarily assign n within groups to be 4
      power <- power.anova.test(groups=numGroups, n=groupSampleSize, between.var=2, within.var=1, sig.level=alpha)$power
    # 2) Calculate power anticipating high variance
      power <- power.anova.test(groups=numGroups, n=groupSampleSize, between.var=2, within.var=10, sig.level=alpha)$power
      
  # RECIPE 24: Calculate Group Sample Sizes of ANOVA test
    groupSampleSize <- ceiling(power.anova.test(groups=numGroups, power=.80, between.var=2, within.var=5, sig.level=alpha)$n)

  # RECIPE 25: Interaction Plot
    # plot for interaction of type and gender on days
    interaction.plot(cancer$type, cancer$gender, cancer$days, las=2)
    # there's an intersection for the "bronchus" type, which may indicate that the effect of cancer
    # type on different genders' survival (days) may differ, but this difference is small and after 
    # running an ANOVA, we know that it's not singificant and could be due to chance
      
    