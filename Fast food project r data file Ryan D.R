#Ryan Downing 
#Data intelligence
#Fast Food Industry Analysis 


#part 0: ideology/concept/Starting statistics...
#Using a large data set containing 10,000 fast food locations can give a lot of insight about the industry.


#I wanted to combine this location data with other demographic and industry statistics...
#To find correlations between them, so we can successfully estimate how many locations should exist in a given area


#This knowledge can be incredibly useful to a plethora of people, for example..
#franchisers,
#Fast Food companies
#Someone looking to make a new chain and open their first store
#city/government officials, who approve/disapprove of new additions to their town
#citizens who may feel that there are too many/too little restaurants around them


#after doing some research, I came across a few important statistical concepts i will be consistently implementing into my analysis



#**as of 2021
#fast food places in US: 197,653 
#data represents 
10000/197653
#5.06% of total locations   (.05059372)
#Us fast food total revenue: $296.6 billion 
#average revenue per year per restaurant:
296600000000 / 197653 
#= 1,500,609
#37% or 50 million Americans consume fast food every day.
#The average American Adult spends $1,200 for a single individual within a year.
#with EVERY citizen in mind
#US population = 331.9 million
#average citizen actually spends... 
296600000000/331900000 
#= 893.64 dollars per year
#this number is lower than 1200 because it includes every individual, those who can not eat fast food, ex babies
#source: https://www.zippia.com/advice/us-fast-food-industry-statistics/




#Age related fast food consumption:
#Percentage of people who consume fast food daily:
#Age 20-39: 44.9%
#Age 40-59: 37.7%
#Age 60+:   24.1%
#Didn't end up incorporating much



#Daily Fast food consumption by race:
#white: 37.6%
#Black: 42.4%
#Asian: 30.6%
#Hispanic: 35.5%
#INCORPORATED HEAVILY

#Daily fast food consumption by income:
#Lower class: 31.7%
#Middle class: 36.4%
#Upper class: 42%
#Wasn't able to incorporate much 




                                   


#part 1, installing packages/library usage/importing data/explaining data
install.packages("dplyr")
library(tidyr)
library(dplyr)
library("ggplot2")
library(readxl)
#using import data set to import X10_000_fast_food, fast_food_revenue, state_review

#10,000 fast food is a data set from kaggle, and the beginning data set 
View(X10_000_fast_food_)

#cities_initial is a combination of self acquired census data related to population and other demographics. 
#it is roughly a sample of the top 50-60 cities by population
#However some cities were not included due to insufficient/outlier restaurant data as i will display in data management
#hence why i got a sample of 60 to start, I wanted to use a sample of the 50 most accurate out of the top 60 to get accurate data
View(Cities_initial)

#state review is just like cities_initial in how it was created, nut it is an overview of 50 states
#this allowed more precise analysis for some points
View(state_review)


#data sources 
# https://www.kaggle.com/datasets/datafiniti/fast-food-restaurants
# https://res.cloudinary.com/tmxfoc/images/f_auto,q_auto/v1639071913/titlemax/1df559ee-revenue-fast-food-chains-america-5/1df559ee-revenue-fast-food-chains-america-5.png?_i=AA
# https://www.census.gov/

##Part 2: Data Management/data cleaning
#removed Date added, date updated, ID, Country, sourceURL, keys, categories, websites to reduce redundancy in 10,000 fast food data
df <- X10_000_fast_food_
X10_000_fast_food_$dateAdded <- NULL
X10_000_fast_food_$id <- NULL
X10_000_fast_food_$dateUpdated <- NULL
X10_000_fast_food_$categories <- NULL
X10_000_fast_food_$keys <- NULL
X10_000_fast_food_$sourceURLs <- NULL
X10_000_fast_food_$country <- NULL
X10_000_fast_food_$websites <- NULL
head(X10_000_fast_food_)



#Now i want to concatenate city and province in the data 
#this way searching for amount of restaurants in a given location is more precise
#as some cities exist in multiple states, so they would return inaccurate counts
unite(X10_000_fast_food_, city_state, city, province, sep = ",")
X10_000_fast_food_ <- unite(X10_000_fast_food_, city_state, city, province, sep = ",")
View(X10_000_fast_food_)




#Now i want to find out how many restaurants exist in my top 60 cities and all 50 states
sum(X10_000_fast_food_$city_state == "New York,NY") #1 = 28
sum(X10_000_fast_food_$city_state == "Los Angeles,CA") #2 = 64
sum(X10_000_fast_food_$city_state == "Chicago,IL") #3 = 57
sum(X10_000_fast_food_$city_state == "Houston,TX") #4 = 106
sum(X10_000_fast_food_$city_state == "Phoenix,AZ") #5 = 77
sum(X10_000_fast_food_$city_state == "Philadelphia,PA") #6 = 53
sum(X10_000_fast_food_$city_state == "San Antonio,TX") #7 = 54
sum(X10_000_fast_food_$city_state == "San Diego,CA") #8 = 47
sum(X10_000_fast_food_$city_state == "Dallas,TX") #9 = 61
sum(X10_000_fast_food_$city_state == "San Jose,CA") #10 = 34
sum(X10_000_fast_food_$city_state == "Austin,TX") #11 = 36
sum(X10_000_fast_food_$city_state == "Jacksonville,FL") #12 = 35
sum(X10_000_fast_food_$city_state == "Fort Worth,TX") #13 = 13
sum(X10_000_fast_food_$city_state == "Columbus,OH") #14 = 56
sum(X10_000_fast_food_$city_state == "Indianapolis,IN") #15 = 40
sum(X10_000_fast_food_$city_state == "Charlotte,NC") #16 = 30
sum(X10_000_fast_food_$city_state == "San Francisco,CA") #17 = 16
sum(X10_000_fast_food_$city_state == "Seattle,WA") #18 = 31
sum(X10_000_fast_food_$city_state == "Denver,CO") #19 = 42
sum(X10_000_fast_food_$city_state == "Oklahoma City,OK") #20 = 40
sum(X10_000_fast_food_$city_state == "Nashville,TN") #21 = 29
sum(X10_000_fast_food_$city_state == "El Paso,TX") #22 = 26
sum(X10_000_fast_food_$city_state == "Washington,DC") #23 = 0
sum(X10_000_fast_food_$city_state == "Boston,MA") #24 = 16
sum(X10_000_fast_food_$city_state == "Las Vegas,NV") #25 = 81
sum(X10_000_fast_food_$city_state == "Portland,OR") #26 = 27
sum(X10_000_fast_food_$city_state == "Detroit,MI") #27 = 18
sum(X10_000_fast_food_$city_state == "Memphis,TN") #28 = 29
sum(X10_000_fast_food_$city_state == "Baltimore,MD") #29 = 3
sum(X10_000_fast_food_$city_state == "Milwaukee,WI") #30 = 19
sum(X10_000_fast_food_$city_state == "Albuquerque,NM") #31 = 33
sum(X10_000_fast_food_$city_state == "Fresno,CA") #32 = 20
sum(X10_000_fast_food_$city_state == "Tucson,AZ") #33 =41
sum(X10_000_fast_food_$city_state == "Sacramento,CA") #34 = 21
sum(X10_000_fast_food_$city_state == "Mesa,AZ") #35 = 27
sum(X10_000_fast_food_$city_state == "Kansas City,MO") #36 = 10
sum(X10_000_fast_food_$city_state == "Atlanta,GA") #37 = 8
sum(X10_000_fast_food_$city_state == "Omaha,NE") #38 = 29
sum(X10_000_fast_food_$city_state == "Colorado Springs,CO") #39 = 6
sum(X10_000_fast_food_$city_state == "Raleigh,NC") #40 = 20
sum(X10_000_fast_food_$city_state == "Virginia Beach,VA") #41 = 20
sum(X10_000_fast_food_$city_state == "Long Beach,CA") #42 = 21
sum(X10_000_fast_food_$city_state == "Miami,FL") #43 = 60
sum(X10_000_fast_food_$city_state == "Oakland,CA") #44 = 7
sum(X10_000_fast_food_$city_state == "Minneapolis,MN") #45 = 38
sum(X10_000_fast_food_$city_state == "Tulsa,OK") #46 = 20
sum(X10_000_fast_food_$city_state == "Bakersfield,CA") #47 = 19
sum(X10_000_fast_food_$city_state == "Wichita,KS") #48 = 12
sum(X10_000_fast_food_$city_state == "Arlington,TX") #49 = 41
sum(X10_000_fast_food_$city_state == "Aurora,CO") #50 = 16
sum(X10_000_fast_food_$city_state == "Tampa,FL") #51 = 22
sum(X10_000_fast_food_$city_state == "New Orleans,LA") #52 = 14
sum(X10_000_fast_food_$city_state == "Cleveland,OH") #53 = 35
sum(X10_000_fast_food_$city_state == "Anaheim,CA") #54 = 12
sum(X10_000_fast_food_$city_state == "Honolulu,HI") #55 = 14
sum(X10_000_fast_food_$city_state == "Henderson,NV") #56 = 5
sum(X10_000_fast_food_$city_state == "Stockton,CA") #57 = 0
sum(X10_000_fast_food_$city_state == "Lexington,KY") #58 = 14
sum(X10_000_fast_food_$city_state == "Corpus Christi,TX") #59 = 13
sum(X10_000_fast_food_$city_state == "Riverside,CA") #60 = 14
sum(X10_000_fast_food_$province == "AL")
#fast food locations in AL -> 6
sum(X10_000_fast_food_$province == "AK")
#fast food locations in AK -> 16
sum(X10_000_fast_food_$province == "AZ")
#fast food locations in AZ -> 330
sum(X10_000_fast_food_$province == "AR")
#fast food locations in AR -> 102
sum(X10_000_fast_food_$province == "CA")
#fast food locations in CA -> 1201
sum(X10_000_fast_food_$province == "CO")
#fast food locations in CO -> 148
sum(X10_000_fast_food_$province == "CT")
#fast food locations in CT -> 53
sum(X10_000_fast_food_$province == "DE")
#fast food locations in DE -> 44
sum(X10_000_fast_food_$province == "FL")
#fast food locations in FL -> 621
sum(X10_000_fast_food_$province == "GA")
#fast food locations in GA -> 420
sum(X10_000_fast_food_$province == "HI")
#fast food locations in HI -> 32
sum(X10_000_fast_food_$province == "ID")
#fast food locations in ID -> 51
sum(X10_000_fast_food_$province == "IL")
#fast food locations in IL -> 405
sum(X10_000_fast_food_$province == "IN")
#fast food locations in IN -> 254
sum(X10_000_fast_food_$province == "IA")
#fast food locations in IA -> 115
sum(X10_000_fast_food_$province == "KS")
#fast food locations in KS -> 74
sum(X10_000_fast_food_$province == "KY")
#fast food locations in KY -> 166
sum(X10_000_fast_food_$province == "LA")
#fast food locations in LA -> 202
sum(X10_000_fast_food_$province == "ME")
#fast food locations in ME -> 25
sum(X10_000_fast_food_$province == "MD")
#fast food locations in MD -> 172
sum(X10_000_fast_food_$province == "MA")
#fast food locations in MA -> 205
sum(X10_000_fast_food_$province == "MI")
#fast food locations in MI -> 374
sum(X10_000_fast_food_$province == "MN")
#fast food locations in MN -> 199
sum(X10_000_fast_food_$province == "MS")
#fast food locations in MS -> 55
sum(X10_000_fast_food_$province == "MO")
#fast food locations in MO -> 163
sum(X10_000_fast_food_$province == "MT")
#fast food locations in MT -> 38
sum(X10_000_fast_food_$province == "NE")
#fast food locations in NE -> 87
sum(X10_000_fast_food_$province == "NV")
#fast food locations in NV -> 121
sum(X10_000_fast_food_$province == "NH")
#fast food locations in NH -> 34
sum(X10_000_fast_food_$province == "NJ")
#fast food locations in NJ -> 129
sum(X10_000_fast_food_$province == "NM")
#fast food locations in NM -> 78
sum(X10_000_fast_food_$province == "NY")
#fast food locations in NY -> 352
sum(X10_000_fast_food_$province == "NC")
#fast food locations in NC -> 295
sum(X10_000_fast_food_$province == "ND")
#fast food locations in ND -> 35
sum(X10_000_fast_food_$province == "OH")
#fast food locations in OH -> 522
sum(X10_000_fast_food_$province == "OK")
#fast food locations in OK -> 166
sum(X10_000_fast_food_$province == "OR")
#fast food locations in OR -> 154
sum(X10_000_fast_food_$province == "PA")
#fast food locations in PA -> 383
sum(X10_000_fast_food_$province == "RI")
#fast food locations in RI -> 15
sum(X10_000_fast_food_$province == "SC")
#fast food locations in SC -> 188
sum(X10_000_fast_food_$province == "SD")
#fast food locations in SD -> 42
sum(X10_000_fast_food_$province == "TN")
#fast food locations in TN -> 302
sum(X10_000_fast_food_$province == "TX")
#fast food locations in TX -> 811
sum(X10_000_fast_food_$province == "UT")
#fast food locations in UT -> 63
sum(X10_000_fast_food_$province == "VT")
#fast food locations in VT -> 15
sum(X10_000_fast_food_$province == "VA")
#fast food locations in VA -> 253
sum(X10_000_fast_food_$province == "WA")
#fast food locations in WA -> 196
sum(X10_000_fast_food_$province == "WV")
#fast food locations in WV -> 70

count <- data.frame(restaurants = c(28, 64, 57, 106, 77, 53, 54, 47, 61, 34, 36, 35, 13, 56, 40, 30, 16, 31, 42, 40, 29, 26, 0, 16, 81, 27, 18, 29, 3, 19, 33, 20, 41, 21, 27, 10, 8, 29, 6, 20, 20, 21, 60, 7, 38, 20, 19, 12, 41, 16, 22, 14, 35, 12, 14, 5, 0, 14, 13, 14))
View(count)

X10_000_fast_food_1 <- cbind(Cities_initial, count$restaurants)
View(Cities_initial)

#Washington DC and Stockton California removed due to zero occurrences in data set


#adding in new column that is the ratio of population to restaurant locations
cities_ratio <- data.frame(Cities_initial$pop / Cities_initial$restaurants)
View(cities_ratio)

Cities_initial <- Cities_initial1
Cities_initial1 <- cbind(Cities_initial, cities_ratio)
View(Cities_initial1)
#changing Variable name
names(Cities_initial1)[names(Cities_initial1) == "Cities_initial.pop.Cities_initial.restaurants"] <- "ratio"
View(Cities_initial1)


#part 3 Statistical methodology/analysis
#finding mean and standard deviation of ratio column
#I will use this to find z scores to remove outliers 
mean(Cities_initial1$ratio, na.rm = TRUE) #36181.09
sd(Cities_initial1$ratio, na.rm = TRUE) #45,769

#since this is a sample i will actually use sample population and since this represents 5.06% of the population,
#we create a new line called sample_pop that multiplies original population by 5.06%

mean(Cities_initial$ratio, na.rm=TRUE) #1844.715
sd(Cities_initial$ratio, na.rm = TRUE) #2284.457

#to make the sample cleaner and more precise, we will now remove instances with z scores greater than 3

Cities_initial[Cities_initial$number != 1, ] #removing New York due to 6.08 score
Cities_initial[Cities_initial$number != 29, ] #removing baltimore due to 3.48 score
Cities_initial[Cities_initial$number != 23, ] #removing DC due to zero occurrences
Cities_initial[Cities_initial$number != 57, ] #removing Stockton California due to zero occurrences

#new mean and sd
mean(Cities_initial$ratio, na.rm=TRUE) #1428.964
sd(Cities_initial$ratio, na.rm = TRUE)#779.1142
#now remove new outliers
Cities_initial[Cities_initial$number != 36, ]#remove colorado springs,
Cities_initial[Cities_initial$number != 40, ]#remove miami fl
Cities_initial[Cities_initial$number != 12, ]#remove fort worth texas
Cities_initial[Cities_initial$number != 40, ]#remove oakland CA
Cities_initial[Cities_initial$number != 50, ]#remove henderson nv
Cities_initial[Cities_initial$number != 1, ]#remove Los angeles CA
mean(Cities_initial$ratio, na.rm=TRUE) #1,262.5
sd(Cities_initial$ratio, na.rm = TRUE)#474.6
#real mean/stand dev 1,262.5mean,  474.6stdev

#now we have a solid data set of 50 cities in the US with race statistics

#regression
#simple regression to find equation for population vs restaurants 
attach(Cities_initial)
plot(sample_pop, restaurants, main="Scatterplot of restaurants vs sample adjusted city populations")
cor(sample_pop, restuarants)
populationvsrestaurants <- lm(restaurants ~ sample_pop)
summary(populationvsrestaurants)
attributes(populationvsrestaurants)
populationvsrestaurants$coefficients
confint(populationvsrestaurants)
#linear model 9.96 + sample population*.000587 = number of restaurants expected in sample
#however general linear model for guessing number of fast food locations is..
#331.9 million/197,653 = 1,679.21   
# 1/1,679.21 = .000595 
#so this sample model does a very good job of representing whole population 
#in fact, (.000595-.000587)/.000595 = .0134 
#so sample model is providing only 1.34% of error in prediction of restaurants



#plotting relationships between race poverty and age with number of people per one fast food location
attach(Cities_initial)
plot(poverty, ratio, main="Poverty")
plot(asian, ratio, main="Asian")
plot(black, ratio, main="Black")
plot(white, ratio, main="White")
plot(hispanic, ratio, main="Hispanic")
plot(seniors, ratio, main="Seniors")
plot(other, ratio, main="other")


racemodel <- lm(ratio ~ asian + black + white + hispanic)
summary(racemodel)
racemodel$coefficients
#(intercept, -3700) (asian, 8050) (black, 5250.615) (white, 4874.436) (hispanic, 4710.925)
# this is not perfect due to high levels of asian population not being consistent with analysis, but it works for normally distributed cities
#example Chicago:
(8050*.068)+(5250.615*.2920)+(4874.436*.3330)+(4710.925*.286) -3700
# this gives us 1,351 people per restaurant, which is a solid analysis, and shows chicago needs more fast food locations 


#Calculating percentage of people who eat fast food every day exactly, instead of using rough 37%:
296600000000/365 #= 812,602,739 per day
#White (total pop) 57.8%; (percent that eat fast food every day: 37.6%) 
.578*.376  #=21.73% *(percent of 37% daily consumption accounted for by white race)
#Hispanic 18.7%: (percent that eat fast food every day: 35.5%%)
.187*.355  #=6.64%
#black 12.1% (percent that eat fast food every day: 42.4%)
.121*.424  #=5.13%
#Asian 5.9%: (percent that eat fast food every day: 30.6%) 
.059*.306  #=1.81%
#other/mixed 5.5% *assuming average of 37%  
.055*.37  # =2.04%

21.73+6.64+5.13+1.81+2.04 #= 37.35%

#how much does each race spend every day on fast food,?
#and what is the expected amount a person who does eat fast food, will spend during that trip?
#white: 
21.7/37.35 #= 58.1% of total daily revenue 
.581*812602739#=472,122,191
#Hispanic: 
6.64/37.35 #= 17.78% of total daily revenue 
.1778*812602739#=144,480,767
#black: 
5.13/37.35 #= 13.73% of total daily revenue 
.1373*812602739#=111,570,356
#Asian: 
1.81/37.35 #= 4.85% of total daily revenue 
.485*812602739#=39,411,232.8
#other: 
2.04/37.35 #= 5.46% of total daily revenue 
.546*812602739#=44,368,109.5

#average spent on one fast food meal: 
331900000*.3735 
#123,964,650 people spend 812,602,739$ per day
812602739/123964650
#6.56$ is average spent by one customer



#now finding number of top restaurant locations in an area
#looking at big 5, so Mcdonalds, subway, burger king, taco bell, wendys
#found these using countifs function in excel, counting by state for higher precision


#linear models
attach(state_review)
View(state_review)
#building linear models to see relation of each restaurant for population
plot(Sample_pop, `McDonald's`, main="Mcdonalds")
cor(sample_pop, restuarants)
Mcdonaldsmodel <- lm(`McDonald's` ~ Sample_pop)
summary(Mcdonaldsmodel)
attributes(Mcdonaldsmodel)
Mcdonaldsmodel$coefficients
confint(Mcdonaldsmodel)

#Equation: 7.5 + .00009307965*Sample_pop
#obviously at a population of zero there will not be 7 McDonald's
#so equation for mcdonalds is: .00009307965*population

attach(state_review)
plot(Sample_pop, `Wendy's`, main="Wendy's")
Wendysmodel <- lm(`Wendy's` ~ Sample_pop)
summary(Wendysmodel)
Wendysmodel$coefficients

#Wendy's = .00002445815*population


attach(state_review)
plot(Sample_pop, Taco_Bell, main="Taco Bell")
Tacobellmodel <- lm(Taco_Bell ~ Sample_pop)
summary(Tacobellmodel)
Tacobellmodel$coefficients

#Taco Bell: .0000522797*population

attach(state_review)
plot(Sample_pop, Burger_King, main="Burger King")
BurgerKingmodel <- lm(Burger_King ~ Sample_pop)
summary(BurgerKingmodel)
BurgerKingmodel$coefficients

#Burger King: .0000522645*population

attach(state_review)
plot(Sample_pop, SUBWAY, main="Subway")
Subwaymodel <- lm(SUBWAY ~ Sample_pop)
summary(Subwaymodel)
Subwaymodel$coefficients

#Subway = .00003716617*population



#Final model/math used to predict number of restaurants as well as which restaurant will succeed
#Population of city*.000587 = expected restaurants originally
#Expected customers: (percentage Asian population)*.306 + (percent Black population)*.424 + (percent white population)*.376 + (percent hispanic population)*.355 + (percent other)*.37
#Add those percentages from expected customers and multiply by population to get exact number of daily fast food consumers
#multiply exact number of consumers by 6.56 to get daily revenue share
#then multiply that number by 365 to get yearly revenue share
#yearly revenue share divided by number of restaurants after addition will be indicator of expected revenue
#obviously there are many other costs that go into play, but with this i can predict the average aamount of sales to expect in an are


#example: 
#my town, Spring Grove, IL
#population: 5,672
#race distribution 85%white 1.99%asian  (5.7%hispanic) (black 0%) (other/mixed race 7.3%)
#current number of fast food restaurants:2
(.85*.376) + (.057*.355) + (.073*.37) + (.0199*.3) #=.3728 or 37.28%
.3728*5672 #2115 people eat fast food every day here
2115*6.56 #13,874.4$ dollars daily spent
13874.4*365 #5,064,156$ total spent yearly 
5064156/3 #1,688,052 revenue share with addition of new location 


#Other Visualizations
numberofcitizensperlocation <- table(state_review$sample_ratio)
barplot(sample_ratio, main="Number of people per restaurant by state",xlab="States")

attach(state_review)
plot(median_income, revenue_share, main="revenue vs income") #no correlation so median income does not have an effect on expected revenue of restaurants

#More Probability
#percentage chance a location has more expected restaurants than actual restaurants
(state_review$add_or_remove > 0)
# 23 true
# 26 false
(23)/(23+26)
#46.9% chance of randomly selecting a location, and it would be able to support another fast food restaurant
# so this means there are a good amount of options still availabel for expansion for these companies and franchisers












