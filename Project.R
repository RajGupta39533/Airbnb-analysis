#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("na.tools")

library(na.tools)
library(ggplot2)
library(dplyr)
library(openintro)
library(readr)

#import data from a csv file
c="C:/Users/rajgu/Desktop/Talend Assignment/AB_NYC_2019.csv"
a=read.csv(c,header = TRUE)

#simple Operatioins on table to analyse the data
head(a)
str(a)
glimpse(a)
tail(a)
table(a$host_name)
unique(a)
desc(a)
unique(a$name)
unique(a$host_name)
unique(a$host_id)
table(a$room_type)
table(a$neighbourhood)
unique(a$neighbourhood)
unique(a$host_name)


#First we produce the frequency of data to see how it will cost us to rent an accomadation in NYC.
a$price %>% hist(xlab="Price",main="frequency of prices", col = "dodgerblue3", breaks = 78)

boxplot(a$price, main = "overal view of property prices")
#Becasue of the existing outliners depicted in figure below, it is not very clear that how much the prices are varying.

#Hence by applying Log10 on prices we would be able to weaken the outliers and have a quick guess about the price range.
hist(log10(a$price),xlab="price", main = "Transformed frequency of the price data Airbnb NYC", col = "dodgerblue3", breaks = 78)
#Graph above depicts that most of the properties are around 10^2 = 100 USD

#Room type analysis
#We would like to see how much prices are varying dependant on whether it is a private room or it is the enitire house or apartment.
Privaterooms = a %>% filter(room_type == "Private room")
Privaterooms$price %>% summary()

Entirehouse = a %>% filter(room_type == "Entire home/apt")
Entirehouse$price %>% summary()

sharedroom = a %>% filter(room_type == "Shared room")
sharedroom$price %>% summary()
#Since our data is extremely skewed because of the outliners, we would prefer to use median price as a more reliable parameter rather that mean value.
#Based on median values and our budget we are able to choose a suitable flat style that we wish to live in.

#Proportion of room type in each area
ggplot(a,aes(x=neighbourhood_group,fill=room_type))+geom_bar()

#Cumulative distribution of Property price
median(l$price)
ggplot(l, aes(price)) + 
  stat_ecdf(geom = "step", color = '#fd5c63', lwd = 1.2) + 
  ylab("Proportion") + xlab("Price") + theme_minimal(base_size = 13) + 
  ggtitle("The Cumulative Distrubition of Property Price")
#Shows around 48% houses cost approximately 100 dollars

#Neighbourhood area is, also , an important factor in finding a nice place to accomodate.
table(a$room_type,a$neighbourhood_group)
#Table above depicts the number of entire apartment in each each neighbourhood based on the property type.

a %>% boxplot(price ~ neighbourhood_group,data = ., main="Box Plot of neighbourhood vs price", 
              ylab="Price",xlab="neighbourhood",horizontal=FALSE, col = "violet")
#Box plot above shows that in Manhattan and Broklyn there are the most expensive properties comparing to other neighbourhoods.
#But yet we are not able to decide what are the median prices in each area by this plot.
#So we categorize each area versus each type of room to realize their median value.

Entirehouse %>% group_by(neighbourhood_group) %>% summarise(Min = min(price,na.rm = TRUE),
                                                            Q1 = quantile(price,probs = .25,na.rm = TRUE),
                                                            Median = median(price, na.rm = TRUE),
                                                            Q3 = quantile(price,probs = .75,na.rm = TRUE),
                                                            Max = max(price,na.rm = TRUE),
                                                            Mean = mean(price, na.rm = TRUE),
                                                            SD = sd(price, na.rm = TRUE),
                                                            n = n(),
                                                            Missing = sum(is.na(price)))

Privaterooms %>% group_by(neighbourhood_group) %>% summarise(Min = min(price,na.rm = TRUE),
                                                             Q1 = quantile(price,probs = .25,na.rm = TRUE),
                                                             Median = median(price, na.rm = TRUE),
                                                             Q3 = quantile(price,probs = .75,na.rm = TRUE),
                                                             Max = max(price,na.rm = TRUE),
                                                             Mean = mean(price, na.rm = TRUE),
                                                             SD = sd(price, na.rm = TRUE),
                                                             n = n(),
                                                             Missing = sum(is.na(price)))

sharedroom %>% group_by(neighbourhood_group) %>% summarise(Min = min(price,na.rm = TRUE),
                                                           Q1 = quantile(price,probs = .25,na.rm = TRUE),
                                                           Median = median(price, na.rm = TRUE),
                                                           Q3 = quantile(price,probs = .75,na.rm = TRUE),
                                                           Max = max(price,na.rm = TRUE),
                                                           Mean = mean(price, na.rm = TRUE),
                                                           SD = sd(price, na.rm = TRUE),
                                                           n = n(),
                                                           Missing = sum(is.na(price)))

#Number of property in each area
ggplot(a) + geom_histogram(aes(neighbourhood_group, fill = neighbourhood_group), stat = "count",alpha = 0.85) + 
  theme_minimal(base_size=13) + xlab("") + ylab("") +theme(legend.position="none") + 
  ggtitle("The Number of Property in Each Area")
#Among roughly 45,000 properties, a majority of them are located in Manhattan and in Brooklyn. Manhattan with more than 20,000 properties has 50% of the properties in New York City, and Brooklyn has nearly 40%


#price less than 500$
l=a %>%
  filter(price<500)
glimpse(l)

ggplot(l,aes(x=longitude,y=latitude,col=price))+geom_point(size=1)

#Shows density graph of neighbourhood_group with no.of reviews
ggplot(a,aes(neighbourhood_group,fill=number_of_reviews))+geom_density(alpha=0.3)

#Shows room availability for private room
ggplot(Privaterooms,aes(availability_365,0))+geom_jitter(col=my_color)+scale_y_continuous(limits = c(-0.5,0.5))

#Shows room availability for Entire house
ggplot(Entirehouse,aes(availability_365,0))+geom_jitter(col="green")+scale_y_continuous(limits = c(-0.5,0.5))

#Shows room availability for Shared room
ggplot(sharedroom,aes(availability_365,0))+geom_jitter(col="violet")+scale_y_continuous(limits = c(-0.5,0.5))

#Plotting for minimum_nights a customer has stayed
x=a %>% 
  filter(minimum_nights<400)
ggplot(x,aes(minimum_nights,0))+geom_jitter()+scale_y_continuous(limits = c(-2,2))


#Shows which area is popular by number of reviews
count(a, neighbourhood_group, sort=TRUE)
ggplot(a,aes(x=factor(neighbourhood_group)))+geom_bar(fill="green")
