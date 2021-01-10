rm(list=ls())
################################################################################
#BASICS
seventh_grade_iq = read.table("~/Desktop/ST1131/Data/seventhgrade.csv", header = T, sep=",")
#displays the data in console
seventh_grade_iq
#important step to ensure data is in
attach(seventh_grade_iq)
#gives all the variable names in the data
names(seventh_grade_iq)
##########################################
#five number summary
summary(IQ)#for a specific var 
summary(seventh_grade_iq) #for all the variable in the table
fivenum(IQ) #not as accurate as summary

#find mean,sd,min,max
mean(IQ)
sd(IQ)
min(IQ)
max(IQ)

#1st and 3rd quartile, and IQR
quantile(IQ,0.25)
quantile(IQ,0.75)
IQR(IQ)
##########################################
#different possible plots/graphs
#https://rstudio-pubs-static.s3.amazonaws.com/7953_4e3efd5b9415444ca065b1167862c349.html

#plotting a histogram 
hist(seventh_grade_iq$IQ, #can use IQ instead too
     breaks = 10,
     xlab = 'IQ',
     ylab = 'No. of students',
     main = 'IQ scores of seventh graders',
     xlim = c(70, 140),
     ylim = c(0, 20)
)

#plotting scatterplot
plot(seventh_grade_iq$IQ, #x value
     seventh_grade_iq$GPA,#y value
     xlab = 'IQ',
     ylab = 'GPA',
     main = 'GPA vs IQ',
     pch = 15,
     col = 'blue')


#plotting bestfit line for scatterplot
fit1 = lm(IQ~GPA, data = seventh_grade_iq)
abline(fit1, col = "red", lwd = 3)

#plotting line graph
plot(seventh_grade_iq$ID,
     seventh_grade_iq$GPA,
     main = "Individual GPA scores",
     type = "l",
     xlab = "Person",
     ylab = "GPA")

#plotting residual plot
seventhgrade.lm = lm(GPA ~ IQ, data = seventh_grade_iq)
seventhgrade.res = resid(seventhgrade.lm)
plot(IQ, seventhgrade.res, xlab = "IQ", ylab = "Residual")

# linear regression
lm(IQ ~ GPA)
# correlation
cor(IQ, GPA)
#plot linear regression line 
abline(lm(IQ ~ GPA))

##########################################

#outliers using OutVals = boxplot(var)$out, then print OutVals again
OutVals = boxplot(IQ)$out
OutVals

# removing outliers from the set of data
IQ[ !(IQ %in%OutVals) ]
# using new data w/o outliers to find quartile
quantile(IQ[ !(IQ %in%OutVals) ],0.75)

# minimum IQ when outlier present
min(IQ) 
# minimum IQ when outlier removed
min(IQ[ !(IQ %in%OutVals) ]) 

#Generate random numbers
sample(1:10, 9, replace=FALSE) #numbers do not repeat
sample(1:10, 9, replace=TRUE) #numbers can repeat





#Chapter 3

#library(MASS)
#(a)
pnorm(240, mean=266, sd=16, lower.tail=TRUE)
#(b)
pnorm(270, mean=266, sd=16, lower.tail=TRUE) -
  pnorm(240, mean=266, sd=16, lower.tail=TRUE)

# find 80th normal percentile
qnorm(0.8, mean=266, sd=16, lower.tail=TRUE)

# find normal density at x=270
dnorm(270, mean=266, sd=16)


# generate 1000 random numbers from the normal distribution
rdata <- rnorm(100000, mean=266, sd=16)
hist(rdata)
help(var)

#t test and finding confidence interval
fb = read.table('~/Desktop/ST1131/Data/facebookfriends.csv', header = T, sep = ",")
attach(fb)
hist(Friends, breaks = 15)
summary(fb)
x = fb$Friends
t.test(x, conf.level = 0.95)


#filtering out specific values/ sub variables
drivethru = read.table('~/Desktop/ST1131/Data/speakerclarity.csv', header = T, sep = ',')
names(drivethru)
install.packages("dplyr")
library("dplyr")
malesonly = drivethru %>% filter(Gender != "F")
femalesonly = drivethru %>% filter(Gender != "M")

#Check skewness
install.packages('e1071')
library('e1071')
skewness(malesonly$Rating)

#t.test for paired
mpg = read.table('~/Desktop/ST1131/Data/mpgcomparison.csv', header = T, sep = ",")
attach(mpg)
mpg
comp = mpg$Computer
drv = mpg$Driver
diff = mpg$Computer - mpg$Driver
t.test(diff, mu = 0, conf.level = 0.975, alternative = "two.sided")

#t.test for unpaired(independent)
drivethru = read.table('~/Desktop/ST1131/Data/speakerclarity.csv', header = T, sep = ',')
names(drivethru)
install.packages("dplyr")
library("dplyr")
malesonly = drivethru %>% filter(Gender != "F")
femalesonly = drivethru %>% filter(Gender != "M")
mean(malesonly$Rating)
sd(malesonly$Rating)
t.test(femalesonly$Rating, malesonly$Rating, alternative = "two.sided", var.equal = TRUE)



