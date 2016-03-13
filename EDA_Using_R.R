
#---------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Exploratory Data Analysis Using R
#----------------------------------------------------------------------------
# college.csv contains educational expenditures over three consecutive years(2013,2014 and 2015).
# These fifty colleges are randomly selected the United States.The United States is divided into four regions. 
# The definition of the variables is:
# Expenditure Per capita annual expenditure on college education 
# Income Per capita monthly personal income 
# region A: East , B: North, C: South, D: West 
# year "2013", "2014", "2015" factor

require(moments)
# More information of moments package : https://cran.r-project.org/web/packages/moments/moments.pdf
require(ggplot2)
# More information of ggplot2 package : https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
require(rockchalk) 
# If you want to learn more about rockchalk: https://cran.r-project.org/web/packages/rockchalk/rockchalk.pdf

# Spend a lot of time to understand the structure of the data

college <- read.csv("college.csv",sep=",")


str(college)

# Convert college into a factor 
college$year <- as.factor(college$year)

# Check summary statistics.
summary(college)

# Form an overview table with average expenditure and income by region so that we can compare
# are there any statistically significant differences between them.

mexp <- aggregate(college$Expenditure~college$region, data = college, mean)
mexp
minc <- aggregate(college$Income~college$region, data = college, mean)
minc
minc <- minc[,2]
overview <- cbind(mexp,minc)
colnames(overview) <- c("region","expenditures", "income")
overview

# Evaluate distributions.
par(mfrow = c(2,2))
# this command splits the frame of the graph into four parts

boxplot(Expenditure~year, data = college, col = "red", main = "Expenditures by Year")
boxplot(Expenditure~region, data = college, col = "red", main = "Expenditures by Region")
boxplot(Income~year, data = college, col = "blue", main = "Monthly Income by Year")
boxplot(Income~region, data = college, col = "blue", main = "Monthly Income by Region")
par(mfrow = c(1,1))

# Perform initial one-way analyses of variance.
# Analysis of Variance - We can see from the above boxplot that expenditure and monthly income will often
# from item to item or observation to observation.As we explore each of the experimental designs and 
# their associated analysis. This  statistical technique is attempting to "break down" 
# the total variance among the objects being studied into possible causes.
# In the case of the valve openings, this variance of measurements might 
# be due to the variation between the regions as well as an unexplained error variation.

aov.region <- aov(Expenditure~region, college)
summary(aov.region)

# Statistically significant F-test results.  Perform TukeyHSD.  Compare to the boxplots.

TukeyHSD(aov.region)

#-------------------------------------------------------------------------------------
# Two-way layout is more efficient and includes an interaction term.

result <- aov(Expenditure~region+year+region*year,college)
summary(result)

# Now the error residuals will be compared and the fitted values will be examined.

r <- residuals(result)
fitt <- fitted(result)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)


skewness(r)
kurtosis(r)

plot(fitt,r, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red")
abline(h = 0, lty = 2, col = "green")
abline(h = 101.96, lty = 2, col = "blue")
abline(h = -101.96, lty = 2, col = "blue")

# This evaluation suggests another factor is needed to explain the variability.
#-------------------------------------------------------------------------------------

# A bivariate plot is a useful way to visualize data.

plot(college$Income, college$Expenditure,main = "Expenditures versus Personal Income", 
     xlab = "Per capita monthly personal income", ylab = "Per capita annual 
     expenditure", col = "red", pch = 16)
abline(v = median(college$Income), col = "green", lty = 2, lwd = 2)
abline(h = median(college$Expenditure), col = "green", lty = 2, lwd = 2)

# I can create a 2*2 contingency table and then use chi square test as a test of independence at this point of time to see if these two variables are independent of each other


# Evaluate Pearson Product Moment Correlation between Y and X.

cor(college[,1],college[,2], method = c("p"))

# This suggests a simple linear regression analysis may be used.

result <- lm(Expenditure~Income,data=college)
summary(result)

# Note that the correlation coefficient from cor() when squared equals
# the multiple R-squared value of 0.3435 in the simple linear regression.  
#-------------------------------------------------------------------------------------

# The AOV results point to region as an important factor for a multiple regresson model.
# Using ggplot2 it is possible to visualize the role played by region.

p <- ggplot(college, aes(x = Income, y = Expenditure))+geom_point(aes(color = region), size = 3)+
  ggtitle("Plot of Expenditures versus Income Colored by Region")
p


result <- lm(Expenditure~Income+region,college)

# we have one continuous variable income and one categorical variable called region here, we  call it 
# parallel lines regression analysis
summary(result)

section <- combineLevels(college$region, levs=c("A","B","C"),newLabel = "S")
college <- cbind(college,section)
str(college)
result <- lm(Expenditure~Income+section,college)
summary(result)

# Lets construct the line using the results
p + geom_abline(intercept=92.29, slope=0.0305)+geom_abline(intercept=35.38,slope=0.0305)

# A multiple regression model needs to be evaluated.  Using the residuals is one way.
# It is highly desirable for the residuals to conform to a normal distribution with 
# few to no outliers.  Other examinations are also useful.

r <- residuals(result)
fitt <- fitted(result)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)

skewness(r)
kurtosis(r)

plot(fitt,r, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red")
abline(h = 0, lty = 2, col = "green")
abline(h = 87.65, lty = 2, col = "blue")
abline(h = -87.65, lty = 2, col = "blue")

# Note that the sum of residuals in ordinary least squares must be zero.  There 
# should be no trend departing from the horizontal line shown.The residuals indicate 
# the regression model is a reasonable fit to the data despite a few outliers.  


# All models are wrong, some are useful.  This model can be improved with more 
# data in the form of a larger sample size and additional predictive variables.










 

