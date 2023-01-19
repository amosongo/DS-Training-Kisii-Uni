#importing data
#importing data
data <-read.csv('Desktop/Busara/dat.csv')

#identifying missing values
data<-is.na(data)
data
#summary statistics of the data.
summary(data)

head(data)

#load ggplot2 library
library(ggplot2)

# Convert the date column to a date format
data$date<-as.Date(data$InitialVisitDate, format = '%Y-%m-%d')

#extract the month and year from the date column and create a new column
data$month <- format(data$date, '%m')
data$year <- format(data$date, '%Y')


#use ggplot to create a line graph to show data on a monthly basis
ggplot(data, aes(x = month, y = Refill1month, fill=year)) +
  geom_bar(stat = "identity") +
  ggtitle("Data for Refill1month in a Years 2017-2018") +
  xlab("Refill1month") +
  ylab("Received") +
  scale_fill_discrete(name = "Year")
facet_wrap(~year, ncol = 2)

#Refill2months
ggplot(data, aes(x = month, y = Refill2months, fill=year)) +
  geom_bar(stat = "identity") +
  ggtitle("Data for Refill2months in a Years 2017-2018") +
  xlab("Refill2months") +
  ylab("Received") +
  scale_fill_discrete(name = "Year")
facet_wrap(~year, ncol = 2)

#Refill3months
ggplot(data, aes(x = month, y = Refill3months, fill=year)) +
  geom_bar(stat = "identity") +
  ggtitle("Data for Refill3months in a Years 2017-2018") +
  xlab("Refill3months") +
  ylab("Received") +
  scale_fill_discrete(name = "Year")
facet_wrap(~year, ncol = 2)

#Refill6months
ggplot(data, aes(x = month, y = Refill6months, fill=year)) +
  geom_bar(stat = "identity") +
  ggtitle("Data for Refill6months in a Years 2017-2018") +
  xlab("Refill6months") +
  ylab("Received") +
  scale_fill_discrete(name = "Year")
