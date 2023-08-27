## Linear Regression

data(trees)

trees
#    Girth Height Volume
# 1    8.3     70   10.3
# 2    8.6     65   10.3
# 3    8.8     63   10.2
# 4   10.5     72   16.4
# 5   10.7     81   18.8
# 6   10.8     83   19.7
# 7   11.0     66   15.6
# 8   11.0     75   18.2
# 9   11.1     80   22.6
# 10  11.2     75   19.9
# 11  11.3     79   24.2
# 12  11.4     76   21.0
# 13  11.4     76   21.4
# 14  11.7     69   21.3
# 15  12.0     75   19.1
# 16  12.9     74   22.2
# 17  12.9     85   33.8
# 18  13.3     86   27.4
# 19  13.7     71   25.7
# 20  13.8     64   24.9
# 21  14.0     78   34.5
# 22  14.2     80   31.7
# 23  14.5     74   36.3
# 24  16.0     72   38.3
# 25  16.3     77   42.6
# 26  17.3     81   55.4
# 27  17.5     82   55.7
# 28  17.9     80   58.3
# 29  18.0     80   51.5
# 30  18.0     80   51.0
# 31  20.6     87   77.0

head(trees)
#   Girth Height Volume
# 1   8.3     70   10.3
# 2   8.6     65   10.3
# 3   8.8     63   10.2
# 4  10.5     72   16.4
# 5  10.7     81   18.8
# 6  10.8     83   19.7


##  1
##  In black cherry trees, what is the relationship between height and girth?

## The dataset need to be eyeballed

install.packages("scatterplot3d")

library(scatterplot3d)

plot.colors <- c("gold", "blue")
color.vector <- rep(x=plot.colors, each=1)
color.vector
s3d <- with(trees,plot(Height ~ Girth, pch=20,cex=3,col=color.vector,
                       xlab="Girth (inches)", ylab="Height(feet)"))

### A relationship exists between the Girth and Height but a test of confirmation needs to be done 
## As height increases, the girth would also increase. This shows a positive linear relationship between Height and Girth


## A Linear regression would also be applied to confirm the test


## Linear model finds a straight line through the data that minimises the error in the Y direction

# Predictor = Girth(x) ; Response = Height (Y)

abline(a=70, b = 0.2, lwd =2)
  ## abline draws a straight line
  ## with intercept a (i.e. where it crosses Y axis at X=0)
  ## and slope b


## Fit in the linear model and it would be named lm0

lmd = lm(Height ~ Girth, data = trees)
lmd

Call:
  lm(formula = Height ~ Girth, data = trees)

Coefficients:
(Intercept)        Girth  
#    62.031        1.054 

## Parameters = lmd = Intercept(62.031) and Slope(1.054)
 
## An abline would be used to plot a slope referenced line

s3d <- with(trees,plot(Height ~ Girth, pch=20,cex=3,col=color.vector,
                      xlab="Girth (inches)", ylab="Height(feet)"))

a <- coefficients(lmd)[1] #intercept
b <- coefficients(lmd)[2] #slope

abline(a=a, b=b, col="black")

## Check assumptions to confirm if the model fits

par(mfrow=c(2,2)) ## this was used to create multiple plots

par(mar=c(4.5, 0.5, 0.5, 1.5)) ## the margin set of the data had to be arranged properly to get a better view of the plot

hist(resid(lmd)) # this was used to visualize the data set

plot(lmd)  ## i was able to generate 4 different plots and this actually siginifies that the linear model regression fits properly

## Plot 1; this plot shows no correlation
## Plot 2; this shows a positive correlation between Height and Girth
## Plot 3; this plot shows no correlation
## Plot 4; this plot shows no correlation

## A test of significance needs to be done

s3d <- with(trees,plot(Height ~ Girth, pch=20,cex=3,col=color.vector,
                       xlab="Girth (inches)", ylab="Height(feet)"))

a <- coefficients(lmd)[1] #intercept
b <- coefficients(lmd)[2] #slope

abline(a=a, b=b, col="black")

abline(a = mean(trees$Height), b = 0,lwd = 5, col = "brown") ##Thick brown line on the scattered plot indicates the average mean of the plot which is 75

anova(lmd)
# Analysis of Variance Table

#   Response: Height
#   Df        Sum Sq Mean Sq F value   Pr(>F)   
#   Girth      1 328.44  328.44  10.707 0.002758 **
#   Residuals 29 889.56   30.67                    
---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## P value = p < 0.0023, the p value is statistically significant and the null hypothesis would be accepted and the alternative hypothesis would be rejected

## Due to the p value (0.0023) gotten, it shows that their is a relationship between Height and Girth
## It also explains that the mean value between Height and Girth are not equal
  
  
  
  
## 2
## How much variation in height is explained by girth?

## Determining the R2 of the model and the variation
  
summary(lmd)

Call:
  lm(formula = Height ~ Girth, data = trees)

Residuals:
#      Min       1Q   Median       3Q      Max 
# -12.5816  -2.7686   0.3163   2.4728   9.9456 

Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  62.0313     4.3833  14.152 1.49e-14 ***
#  Girth        1.0544     0.3222   3.272  0.00276 ** 
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 5.538 on 29 degrees of freedom
# Multiple R-squared:  0.2697,	Adjusted R-squared:  0.2445 
# F-statistic: 10.71 on 1 and 29 DF,  p-value: 0.002758 (the mean value between Height and Girth are not equal)

## Adjusted R-squared value = 0.24 - 24% of the variation in height as explained by Girth. Variability in height (linear model F= (1, 29) = 10.707, P<0.002758), Equation is Y = 62.0313 + 1.0544(x) (R2 = 0.24)

## It was also observed that the mean value between Height and Girth are not equal


  

##  3
##  How much does height increase for each additional inch of girth?

summary(lmd)

Call:
  lm(formula = Height ~ Girth, data = trees)

Residuals:
#    Min       1Q   Median       3Q      Max 
# -12.5816  -2.7686   0.3163   2.4728   9.9456 

Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  62.0313    4.3833  14.152 1.49e-14 ***
#  Girth         1.0544     0.3222   3.272  0.00276 ** 
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 5.538 on 29 degrees of freedom
# Multiple R-squared:  0.2697,	Adjusted R-squared:  0.2445 
# F-statistic: 10.71 on 1 and 29 DF,  p-value: 0.002758

  # "Estimate" is R's best guess at the value
  # "(Intercept)" is (naturally) the Y intercept -  62.0313 with a standard error of 4.3833 (62.03 ± 4.383 SE)
  # "Girth" is the slope of the line  1.0544 with a standard error of 0.3222 (1.054 ± 0.322 SE)

## Equation Y = 62.0313 + 1.0544(x)
## The tree height increases by 1.0544ft for each additional inch of Girth


## A GGplot would also be used to make the analysis look more publishable

install.packages("ggplot2")

library(ggplot2)

ggplot(trees, aes(x=Girth, y=Height)) + geom_point(color="red", size=2) + labs() +
  scale_x_continuous(labels = function(x) paste0(x, " Girth")) +
  scale_y_continuous(labels = function(y) paste0(y, " Height")) +
  geom_smooth(formula = y ~ x, method = "lm", col = "red",lwd=2) + xlab("Girth(inches)") + ylab("Height(feet)")

## A scattered plot was produced showing different levels of confidence intervals

## A best fit line would be added using GGplot

ggplot(trees, aes(x=Girth, y=Height)) + geom_point(color="red", size=2) + labs() +
  scale_x_continuous(labels = function(x) paste0(x, " Girth")) +
  scale_y_continuous(labels = function(y) paste0(y, " Height")) +
  geom_smooth(formula = y ~ x, method = "lm", col = "red",lwd=2) + xlab("Girth(inches)") + ylab("Height(feet)") +
  geom_abline(intercept = 62.0313, slope = 1.0544, color = "black", lwd = 2)

## It could be observed that the tree height increase is 1.0544ft for every 1 inch in Girth




##  4
##  Use R to work out the predicted height (in feet) for a tree of girth 30 inches.

range(trees$Height)
# [1] 63 87

range(trees$Girth)
# [1]  8.3 20.6

install.packages("RColorBrewer")

library(RColorBrewer)

with(trees, plot(Height ~  Girth, xlim = c(min(trees$Girth), 30), ylim = c(min(trees$Height), 90)),)         
# Xlim was used to accommodate the 30inches of the Xaxis
# Ylim extended the Y axis that was used to show our predicted point 

abline(v = 30, lwd = 4, col = brewer.pal(7, "Spectral"))  # plot a straight line

abline(h = mean(trees$Height), lwd = 4, col = brewer.pal(9, "BrBG")) # this was used to get the mean value of Y(Height) which was 75

abline(h = mean(trees$Height) + sd(trees$Height), lwd = 5, lty = 4, col = brewer.pal(6, "Dark2")) # this shows the standard deviation above the mean value (80)
abline(h = mean(trees$Height) - sd(trees$Height), lwd = 5, lty = 4, col = brewer.pal(6, "Dark2")) # this shows the standard deviation below the mean value (70)

with(trees, plot(Height ~  Girth, xlim = c(min(trees$Girth), 30), ylim = c(min(trees$Height), 90)),)

pred.Y <- predict(lmd, newdata = data.frame(Girth = 30))

pred.Y            
#         1 
#  93.66238 

# The point function was added in other to show our the predicted height when Girth is 30

points(x = 30, y = pred.Y, pch = 20, cex = 4, col = brewer.pal(6, "PRGn"))

text(x = 22, y = 95, font = 2, cex= 1.5, col = brewer.pal(5, "Reds")) # this was used in adding a text to the plot

## The predicted height when Girth is 30 inches would be 93.66238


## END