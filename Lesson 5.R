#
seg.df<- read.csv("http://goo.gl/qw303p")


seg.df[, c(2, 5:7)] <- lapply(seg.df[, c(2, 5:7)], factor)

#Exploring the data
library(psych)
library(car)
library(corrplot)
library(gplots)

summary(seg.df)
describe(seg.df)
str(seg.df)
pairs(seg.df)
scatterplotMatrix(seg.df)
scatterplotMatrix(formula= ~ age+income+kids, data=seg.df)

#obtaining means
mean(seg.df$income [seg.df$Segment=="Moving up" & seg.df$subscribe=="subNo"])

by(seg.df$income, seg.df$subscribe, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

aggregate(seg.df$income, list(seg.df$Segment), mean)
aggregate(seg.df$income, list(seg.df$Segment, seg.df$subscribe), FUN=function(x)c(mean,sd))

aggregate(cbind(mean = seg.df$income, sd = seg.df$income), 
          by = list(Segment = seg.df$Segment, Subscribe = seg.df$subscribe), 
          FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(cbind(mean = seg.df$income, sd = seg.df$income), 
          by = list(Segment = seg.df$Segment, Subscribe = seg.df$subscribe), 
          FUN = function(x) {c(mean = mean(x), sd = sd(x))})

aggregate(income~Segment, data = seg.df, mean)
aggregate(income~Segment+subscribe, data = seg.df, mean)

# adding a variable with segment means

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.income.mean
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]

seg.income.mean[seg.df$Segment, ]
seg.income.mean[seg.df$Segment,2 ]

###5.2 Descriptives for two groups
aggregate(income~Segment+ownHome, data = seg.df, mean)
aggregate(income~Segment+ownHome+subscribe, data = seg.df, mean)
agg.data <- aggregate(income~Segment+ownHome+subscribe, data = seg.df, mean)


#to see the number of people per subgroup: 
table(seg.df$Segment, seg.df$ownHome)
with(seg.df, table(Segment, ownHome)) #another way, which temporarily attachs the database :)

with(seg.df, table(kids,Segment))

# different ways we can obtain counts (sums of values) across grouping variables

xtabs(kids~Segment, data = seg.df)
aggregate(kids~Segment, data=seg.df, sum)

seg.tab <-with(seg.df, table(kids, Segment))
seg.tab
apply(seg.tab*0:7,2,sum)

colSums (seg.tab*0:7)
