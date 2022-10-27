## Human Wildlife conflict in Chitwan National Park----
### Saroj Kandel, 10-23-2022, sarojkndl@gmail.com----
###libraries----
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
data1<-read_xlsx("hwc_data.xlsx" )
data1
#making data frame----
data2<-data.frame(data1)
data2
str(data2)

#changing character column into factor column----
data3<-as.data.frame(unclass(data2),stringsAsFactors = TRUE)
str(data3)

#for categorical variables use Table to find statistics----
table(data3$Species)
###finding the proportions or percentage of each species----
table_species<- table(data3$Species)
table_species
percentage_species<- prop.table(table_species)
percentage_species

# for cross tabulation----
#install.packages("gmodels")
library(gmodels)
CrossTable(data3$code_attack, data3$Location)# 1 is death and 0 is injury of humans

# day 2----
#showing frequency of event by different species in bar plot
plot(data3$Species)

# lets use ggplot2


ggplot(data3,aes(x=Species))+
  geom_bar()

# group bar plots of species vs attack category(death and injury)----
library(ggplot2)
ggplot(data3,aes(x=Species,fill=Attack))+
  geom_bar(position = "dodge")+
  labs(y="Frequency of attacks", x="Species")+
  theme(panel.grid = element_blank(), # it removes the background grids
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
          legend.position = c(0.9,0.9)) # position the legend at top right
    
# chi-square test of independent----
# between species and attacks ( fatalities and injury)
# lets make contingency table first

ctable<- table(data3$Species, data3$Attack)
ctable
 test<- chisq.test(ctable)
 
 btable<- table(data3$Year, data3$Species=="Rhino")
 btable
 
 

 
 
 
 
 subset(data3, Species=="Rhino")
# we get error that " chi-squared approximation may be incorrect"
# so we are using Fisher's exact test----
##test_f<- fisher.test(ctable, workspace = 2e9) # error shown that work space is less so we increase work space o 2e9
#test_f
#test_f$p.value

# temporal pattern of human injuries and fatalities----

total_atack<- select(data3,Attack)
total_atack
filter_death<- total_atack %>% 
  filter(Attack=="Death")
filter_death
  
# summarizing categorical data----
# counting number of death and injury in attacks

number<- data3 %>%  # this command is not working
  group_by(Attack) %>% 
  summarise(number.rows=n() )
number
#check the frequency of attacks by several species in data frame----
##install.packages("plyr")
library(plyr)
count_species<-count(data3, "Species")
count

# I am lost----
a<- data3 %>% 
  filter(Attack=="Death") %>% 
  group_by(Year) %>% 
  arrange(desc(Year))
a

ggplot(data3, aes(x=Year))+
  geom_bar()
ggplot(data3,aes(x=))


## its working from here----
twocol<- data3[,c("Year","Species")]
twocol

onlyrhino<- subset(twocol, Species=="Rhino")
onlyrhino

#facet try

facet<- ggplot(data3, aes(x=Year))+
  geom_bar()
facet

facet+facet_wrap(~ Species, ncol=2)

# adding a season column----

data4<-data3 %>% 
  mutate(
    Season= case_when(
      Month %in% 9:11 ~ "Autumn",
      Month %in% c(12,1,2)~ "Winter",
      Month %in% 3:5 ~ "Spring",
      TRUE~ "Summer"
    )
  )
 head(data4)
#plot the data of season into graph
 ggplot(data4, aes(x=Season))+
   geom_bar()
 
 # see the wildlife events species wise-----
 
species_table<- table(data4$Species, data4$Year)
species_table 
 
 rhino_table<- table(data4$Year, data4$Species=="Rhino")   
rhino_table

#making data frame for data4------
data5<- data.frame(data4)
data5
head(data5)
# asking for help with dput-----

my_data_compressed<- data5[1:6,]

# trying tibble for data wrangling----
tdata<-tibble(data5)
tdata

#count the data for factors----
species_freq<- count(tdata$Species)
species_freq 
# changing year that was in numeric to factor and showing in graph
tdata$Year<- as.factor(tdata$Year)
tdata
plot(tdata$Year)

a<-count(tdata$Year)
a

# lets remove 2021 from the data as we dont have complete data for the year.
 b<- data.frame(Year=factor(c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)),
                Attacks=c(11,38,32,38,36,31,27,55,24,94,63,56))
b

ggplot(b, aes(x=Year, y=Attacks, group=1)) +
  geom_path()+
  geom_point()




