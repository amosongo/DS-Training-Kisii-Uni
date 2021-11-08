### Introduction to R  ###

#Basic arithmetics and objects
12+24
23*56
56/6

#assigning
x=56
x<-561
5*23/x -> x

#vectors
bfr <-c(67.8,78,96,56.7,66,73.6,71.5)
afr <-c(56,62.3,84.8,55,62.4,64.6,72.6)
wght_loss <- c(bfr-afr)  #; mean(wght_loss); mean(bfr-afr)

x<-c(2:56)
y<-seq(from=2, to=6, by=0.4)
y<-rep(5,3)
j<-rep(2:5,each=3)
k<-rep(-1:3, length.out=10)
l<-2^(0:10)
o<-1:3 + rep(seq(from=0,by=10,to=30), each=3)
p<-1:10 * c (-1,1) ## recycling; applying a shoter seq over longer seq 
q<-rep (seq(0:49)*c(1,0))

#matrices
job_sats<-matrix(c(2,4,13,3,2,6,22,4,0,1,15,8,0,3,13,8), nrow=4, ncol = 4, byrow = TRUE)
dimnames(job_sats)<-list(c("<5000","5000-15000","15000-25000",">25000"),c("vd","ls","ms","vs"))
job_sats
##alternatively
income<-c("<5000","5000-15000","15000-25000",">25000")
vd<-c(2,2,0,0)
ls<-c(4,6,1,3)
ms<-c(13,22,15,13)
vs<-c(3,4,8,8)
matrix<-cbind(income,vd,ls,ms,vs)
matrix

#summarizing and presentation Continuous/discrete data
mean(x)
xy<-c(45,6,7,NA,67,86,46)
mean(xy,na.rm=T)
# mean(na.rm=TRUE, xy)
# mean(xy, 0, TRUE)
##  mean(xy) #gives NA


median(xy,na.rm = T)
##  median(xy,na.rm = F)

var(xy,na.rm = T)
## var(xy)
sd(xy,na.rm = T)

summary(xy)

#graphical presentation
boxplot(bfr, afr) # checking for outliers in a variable
plot(wght_loss) 
hist(wght_loss)

# Descriptive stats
weight <- c(56, 67, 65, 78, 49, 87, 55, 63, 70, 72, 79, 52, 60, 78, 90) 
sex <- c(1,1,1,2,1,2,1,1,1,2,1,1,1,2,2) 
sex <-factor(sex, labels = c('Male','Female'))
tapply(weight,sex,mean)

## relative % element divided by sum of column
apply(mtcars,2,mean)# find mean of every column
apply(mtcars,1,summary)# finds summary of each row

# by command summarises one or more continuos var from a dataset in term of one or more grouping variables
#mean avrg of cars relative to sepcific var
by(mtcars$mpg,list(am=mtcars$am,cyl=mtcars$cyl),mean)
tapply(mtcars$mpg,mtcars$cyl,mean)
aggregate(mtcars['mpg'],list(transmission_type=mtcars$am),mean)
aggregate(mtcars[c('mpg','hp')],list(transmisson_type=mtcars$am),mean)
aggregate(mtcars[c('mpg','hp')],list(transmisson_type=mtcars$am,cylinders=mtcars$cyl),mean)


eyecol<-c(1,2,1,2,2,2,3,3,1,4,2,2,2,3,1) 
eyecol<-factor(eyecol, labels=c("blue","grey","brown","green"))
table(sex,eyecol)
prop.table(table(sex,eyecol),1) # row percentages
prop.table(table(sex,eyecol),2) # column percentages   

# creating a dataset/ frame
eyecol<-c(1,2,1,2,2,2,3,3,1,4,2,2,2,3,1,4,3,2,1,1,1) 
eyecol<-factor(eyecol, labels=c("blue","grey","brown","green"))
eyecol
table(eyecol)
prop.table(table(eyecol))
round(100*prop.table(table(eyecol)),1)
barplot(table(eyecol))

weight <- c(60,72,57,90,95,72)
height <- c(1.75,1.80,1.65,1.90,1.74,1.91)
sex <- rep(c("female", "male"), each = 3)
smoking <- rep(c("TRUE","FALSE"), 3)
try_data <- data.frame(weight, height, sex, smoking); try_data

### DATA ANALYSIS

ls() #lists all obects in your environment
rm(list=ls()) # removes all objects

setwd("C:/Users/pc/Documents") # setting a working directory
getwd()

# read in data from Excel
# install.packages("readxl")
library(readxl)
#importing a dataset into R
#Sample_data<-read.csv("C:/Users/pc/Desktop/Gadafi/Sample data.csv",header = TRUE)

Sample_data <- read_excel("C:/Users/pc/Desktop/Gadafi/Sample data.xlsx") # importing data
View(Sample_data) # Viewing the dataset

str(Sample_data)# structure of the dataset

##Changing characters into factors
Sample_data$Gender<-as.factor(Sample_data$Gender)
Sample_data$`Favorite social networking site`<-as.factor(Sample_data$`Favorite social networking site`)
Sample_data$`Which is your primary means of communication`<-as.factor((Sample_data$`Which is your primary means of communication`))
Sample_data$`How often do you log into your social account`<-as.factor(Sample_data$`How often do you log into your social account`)
Sample_data$`Time spent on your social network`<-as.factor(Sample_data$`Time spent on your social network`)
Sample_data$`What is your grade point average(G.P.A) currently`<-as.factor(Sample_data$`What is your grade point average(G.P.A) currently`)

str(Sample_data)# structure of the dataset
attach(Sample_data)# attaching dataset to the working envirnoment
summary(Sample_data)

#Missing data
# is.na(x) - finding missing values
summary(is.na(Sample_data)) #Shows missing data as per variable
filter(Sample_data, is.na(`Grade_point_average(G.P.A)_currently`))

#tet<-na.omit(Sample_data) #alternatively
#View(tet)

Refined<-drop_na(Sample_data) #dropping missing values
View(Refined)

detach(Sample_data)
attach(Refined)

#Visualization
demo(graphics) # visualization/ Graphics types in R

colnames(Refined) #column names

hist(`Grade_point_average(G.P.A)_currently`)
boxplot(`Grade_point_average(G.P.A)_currently`,Gender)
barplot(table(Gender,`What is your grade point average(G.P.A) currently`))

#Tables/ descriptive stats
table(Gender,`What is your grade point average(G.P.A) currently`) # categorical variables
#mean GPA per gender
tapply(`Grade_point_average(G.P.A)_currently`,Gender, mean)
tapply(`Grade_point_average(G.P.A)_currently`,Gender, mean, na.rm =T) #if youve missing data

#others: sapply, mapply, apply etc.

library(ggplot2)

##Data wrangling

#Data wrangling, sometimes referred to as data munging, is the process of transforming 
#and mapping data from one "raw" data form into another format with the intent of making 
#it more appropriate and valuable for a variety of downstream purposes such as analytics.

library(tidyverse) #package containing packages designed for data science
library(dplyr)

#using pipes in R %>%
# filter used in selecting observations/ rows
Male_G<- Refined %>% 
  filter(Gender=="Male") # used to filter only male respondents in the dataset
View(Male_G)

Female_G<- Refined %>% 
  filter(Gender=="Female") # used to filter only female respondents in the dataset
View(Female_G)

#partitioning datasets
# select used in selecting variables/ columns
data1<-Male_G %>%
  select(Gender:`Grade_point_average(G.P.A)_currently`)
View(data1)

data2<-Male_G %>%
  select(Gender, `Grade_point_average(G.P.A)_currently`,`Age (Years)`,`How often do you log into your social account`)
View(data2)

#Conditional statements
weight <- c(56, 67, 65, 78, 49, 87, 55, 63, 70, 72, 79, 52, 60, 78, 90)

#for statement

###for (variable in Vector){
###  commands 
###}

for (i in 1:5) {
  print(i^2)
}
for (i in weight){
  print(c(i,i^2))
}
for (i in 1:5) {
  tey[i]<-i^2
}
tey

xy<-c(-3,2,5,7)
Tor<-numeric(4)#Creates or coerces objects of type "numeric".
Tor
for (i in xy){
  Tor[i]<-i^2
}
Tor

xo<-c(-3,2,6,7)
Torr<-numeric(4)#Creates or coerces objects of type "numeric".
for (i in 1:4){
  Torr[i]<-(xo[i])^2
}
Torr

# Changing degree C to degree F
temperature<-c(24.3,22.1,23.7,26.3,27.4,18.9)
for (degr_C in temperature){
  degr_F <- degr_C*(9/5)+32
  print(c(degr_C,degr_F))
}

# loops
for (temp_e in c(24.3,-22.1,23.7,-26.3,27.4,18.9))
if (temp_e>0){
  print('above 0')
} else if (temp_e>20 & temp_e<26){
  print('room temp')
} else {
  print('others')
}

#Nested loops
#for (variable1 in vector1){
#  for (variable2 in vector2){
#    for (variable_n in vector_n){
#      commands 
#    }
#  }
#}

for (i in 1:3){
  for (j in 1:2){
    print(i+j)
  }
}


# Regression modelling
summary(faithful)
attach(faithful)
plot(eruptions,waiting, xlab = 'Eruptions', ylab = 'Time')
abline(lm(waiting~eruptions)) # linear regression line

test_data<- faithful %>% 
  filter(eruptions>=2.0) # eruptions equal or greater than 2
View(Faithful_a)

train_data<- faithful %>% 
  filter(eruptions<2.0) # eruptions less than 2
View(Faithful_b)

#prediction
fait_t<-lm(waiting~eruptions, data = test_data)
summary(fait_t)

predicted_lm<-predict(fait_t, newdata = train_data)
predicted_lm

detach(faithful)