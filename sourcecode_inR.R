#IMPORT DATA
df=read.csv("Computer-NA.csv")
#import data from csv file on the disk to R
#df is dataframe that contain input data
class(df)
#We use classcheck whether object is data frame
#Show the data frame df in table

#DATA CLEANING
ndf=na.omit(df)
#remove observations that contain missing values
View(ndf)
#show the data frame ndf in table
write.csv(ndf,"D:\\download\\Compressed\\archive\\computer-noNA.csv", row.names=FALSE)
#save (no NA values) new dataframe into csv file

#TRANSFORMATION
str(ndf)

ndf$cd = as.factor(ndf$cd)
ndf$multi= as.factor(ndf$multi)
ndf$premium= as.factor(ndf$premium)
str(ndf$cd) ##check the structure of CD after transformation
str(ndf$multi)
str(ndf$premium)

#Descriptive statistics
install.packages(c("explore","tidyverse"))
library(explore); library(tidyverse)
describe(ndf)
summary(ndf)
sd(ndf$price)
sd(ndf$speed)
sd(ndf$hd)
sd(ndf$ram)
sd(ndf$screen)
sd(ndf$ads)
sd(ndf$trend)

#GRAPHS
hist(ndf$price, breaks=32, xlab="Price of computer", ylab="Probability",prob=T,main="Histogram of Price of computer",
     col="blue",border="white")
#we can increase breaks to see the distribution more clearly
lines(density(na.omit(ndf$price)),col="red",lwd=3)
#draw the line of distribution

install.packages(c("explore","tidyverse"))
library(explore); library(tidyverse)
ndf %>% explore(price,target=ram)
ndf %>% explore(price,ram)

qplot(ndf$price,xlab="Price of computer",ylab="Probability",geom="density",fill=ndf$cd,alpha=I(0.5)) + theme(legend.position="top")
boxplot(ndf$price ~ ndf$cd , notch=T,col=c("red","blue"), xlab="Price of computer",ylab="cd",main="Box plot of Price of 
computer with respect to cd",horizontal=T)

ndf %>% explore(price,target=screen)
ndf %>% explore(price,screen)

qplot(ndf$price,xlab="Price of computer",ylab="Probability",      geom="density",fill=ndf$multi,alpha=I(0.5)) 
+ theme(legend.position="top")
boxplot(ndf$price ~ ndf$multi, notch=T,col=c("red","blue"), xlab="Price of computer",ylab="cd",main="Box plot of Price of 
computer with respect to multi",horizontal=T)

qplot(ndf$price,xlab="Price of computer",ylab="Probability",      geom="density",fill=ndf$premium,alpha=I(0.5)) 
+ theme(legend.position="top")
boxplot(ndf$price ~ ndf$premium, notch=T,col=c("red","blue"),           xlab="Price of computer",ylab="cd",main="Box plot of Price of 
computer with respect to premium",horizontal=T)

library(psych)
attch(ndf)
pair=cbind(price,speed,hd,ads,trend)
pairs.panels(pair)

#T-TEST
csv = read.csv("Computer-noNA.csv")
csv = csv[sample(nrow(csv)),]
non=csv[csv$premium=="no",][0:30,]
premium=csv[csv$premium=="yes",][0:30,]

print(t.test(premium$price, non$price,
             var.equal=TRUE, conf.level=0.95, alternative="greater"))

#Z-TEST
data = read.csv("Computer-noNA.csv")
shuffled_data = data[sample(nrow(data)),]
big = shuffled_data[shuffled_data$ram >= 16,][0:300,]
small = shuffled_data[shuffled_data$ram < 16,][0:300,]

prop.test(x = c(nrow(big[big$speed >= 50,]), nrow(small[small$speed >= 50,])), n = c(300, 300), conf.level = 0.95)


#ANOVA
table <- read.csv(file='Computer-noNA.csv')
ndf2=table
ndf2$screenc[ndf2$screen == 14] = "small"
ndf2$screenc[ndf2$screen == 15] = "normal"
ndf2$screenc[ndf2$screen == 17] = "large"
ANOVA_table=data.frame(ndf2[,'X'],ndf2[,'price'],ndf2[,'screenc'])
colnames(ANOVA_table) = c("X","price","screen")
oneway.test(price ~ screen, data=ANOVA_table,var.equal=T)
anova(lm(price ~ screen, data=ANOVA_table))



#LINEAR REGRESSION
csv = read.csv("Computer-noNA.csv")
csv = csv[sample(nrow(csv)),]
training_rows = floor(nrow(csv) * 0.7)
d_train = csv[0:(training_rows-1),]
d_test = csv[training_rows:nrow(csv),]

model = lm(formula = d_train$price ~ d_train$speed + d_train$hd + d_train$ram + d_train$screen)
model = lm(formula = price ~ speed + hd + ram + screen + ads + trend, data = d_train)
summary(model)
#print(predict(model, d_test))
print(cor(d_test$price, predict(model, d_test))^2)





