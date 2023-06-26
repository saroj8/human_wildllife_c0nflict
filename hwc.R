## Human Wildlife conflict in Chitwan National Park----
### Saroj Kandel, 10-23-2022, sarojkndl@gmail.com----
###libraries----
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
data1<-read_xlsx("hwc_data.xlsx" )
data1
#############******General descriptive statistics and overall result exploring************
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
round<- round((percentage_species*100),2)
round

# for cross tabulation----
#install.packages("gmodels")
library(gmodels)
CrossTable(data3$c_attack, data3$Location)# 1 is death and 0 is injury of humans
#showing frequency of event by different species in bar plot
plot(data3$Species)


ggplot(data3,aes(x=Species))+
  geom_bar()

# group bar plots of species vs attack category(death and injury)----
fig2<-ggplot(data3,aes(x=Species,fill=Attack))+
  geom_bar(position = "dodge")+
  labs(y="Frequency of attacks", x="Species")+
  theme(panel.grid = element_blank(), # it removes the background grids
        panel.background = element_blank(),
        text=element_text(size = 10, family = "Arial"),
        axis.line = element_line(size = 0.7, color = "black"),
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = "top",
        legend.direction = "horizontal")
fig2 +
  scale_fill_manual(values = c("red","blue"))

#saving in high resolution
ggsave("june26.tiff", width = 12, height = 12, units=c("cm"), dpi = 400)


# temporal pattern of human injuries and fatalities----
# adding a season column-------

data4<-data3 %>% 
  mutate(
    Season= case_when(
      Month %in% 9:11 ~ "Autumn",
      Month %in% c(12,1,2)~ "Winter",
      Month %in% 3:5 ~ "Spring",
      TRUE~ "Summer"
    ))
head(data4)
#plot the data of season into graph
ggplot(data4, aes(x=Season))+
  geom_bar()

#see the wildlife events species wise----- (overall table1 in manuscript)

species_table<- table(data4$Species, data4$Year)
species_table 

#making data frame for data4------
data5<- data.frame(data4)
data5
head(data5)

#########################    Manuscript ############################

# Overall conflict pattern
# percentages of events by animals 
percentage_species<- prop.table(table_species)
percentage_species
round_per<- round((percentage_species*100),2)
round_per

#average number of fatalities per year
attack_death<- data5 %>% 
  filter(Attack=="Death")




#check if the injury and death frequencies are significant among species or not?

ctable<- table(data3$Species, data3$Attack)
ctable
chisq.test(ctable)

#figure for species wise attack( fill with death and injury)

ggplot(data5,aes(x=Species,fill=Attack))+
  geom_bar(position = "dodge")+
  labs(y="Frequency of attacks", x="Species")+
  theme(panel.grid = element_blank(), # it removes the background grids
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = c(0.9,0.9)+ # position the legend at top right
          scale_fill_brewer(palette="Set1"))

##### Temporal pattern of wildlife attacks ------

# frequency of attacks in year

time_data<- data.frame(Year=factor(c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)),
                       Attacks=c(11,38,32,38,36,31,27,55,24,94,63,56))
time_data

fig4<-ggplot(time_data, aes(x=Year, y=Attacks)) +
  geom_col() +
  
  theme(panel.grid = element_blank(), # it removes the background grids
        panel.background = element_blank(),
        text=element_text(size = 10, family = "Arial"),
        axis.line = element_line(size = 0.7, color = "black"),
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = "top",
        legend.direction = "horizontal")
fig4


ggsave("fig4.tiff",width=15, height = 15, units = c("cm"), dpi=400)

#***************for line to find regression**************************
ggplot(time_data, aes(x=Year, y=Attacks, group=1)) +
  geom_line(color="black", size=1.5)+
  geom_point(size=2)+
  theme(panel.grid = element_blank(), # it removes the background grids
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = c(0.9,0.9)) # position the legend at top right

time_data1<- data.frame(Year=(c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)),
                        Attacks=c(11,38,32,38,36,31,27,55,24,94,63,56))
str(time_data1)
time_data1
regression<-lm.fit<-lm(Attacks~Year, data=time_data1)
summary(regression)
#plotting the linear regression
Year=(c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))
Attacks=(c(11,38,32,38,36,31,27,55,24,94,63,56))
plot(Year, Attacks, main = "Main Linear regression",
     xlab = "Year of conflict", ylab = "Frequency of conflict",
     pch = 19, frame = FALSE)
line<-abline(lm(Attacks ~ Year, data = time_data1), col = "blue")
summary(regression)



#chatgpt way----
# Plotting the scatter plot
plot(Year, Attacks, pch = 16, col = "blue", xlab = "Year", ylab = "Attacks")
#plotting using ggplot2
ggplot(time_data1, aes(x = Year, y = Attacks)) +
  geom_point(color= "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Year", y = "Attacks") +
  theme(panel.grid = element_blank(), # it removes the background grids
        panel.background = element_blank(),
        text=element_text(size = 10, family = "Arial"),
        axis.line = element_line(size = 0.7, color = "black"),
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = "top",
        legend.direction = "horizontal")
labs(y="Number of incidents", x="Year")
ggsave("june26_trend_line.tiff", width= 15, height=15, units=c("cm"), dpi=400)

# Adding a trend line
trend_line <- lm(Attacks ~ Year)  # Linear regression model
abline(trend_line, col = "red", lwd = 2)
summary(trend_line)

summary(time_data1$Attacks)
attacks<- c(11,38,32,38,36,31,27,55,24,94,63,56)

sd(attacks)
mean(attacks)
21.96/12

year_attack<- table(data5$Year)
year_attack
chisq.test(year_attack)
count(data5$code_attack)

#season wise wildlife attacks
fig5<-ggplot(data5, aes(x= Species, fill= Season))+
  geom_bar(position = position_dodge(0.5), width = 0.4)+
  theme(panel.grid = element_blank(), # it removes the background grids
        panel.background = element_blank(),
        text=element_text(size = 10, family = "Arial"),
        axis.line = element_line(size = 0.7, color = "black"),
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = "top",
        legend.direction = "horizontal")+
  labs(y="Number of incidents", x="wildlife species")
fig5 +
  scale_fill_manual(values = c("red","blue","green","yellow"))
ggsave("june26_seasonVsWildlife.tiff", width=15, height=15, units=c("cm"), dpi=400)

season_table<- table(data5$Species, data5$Season)
season_table
chisq.test(season_table) 
#months********************************************************************************
month_table<-table(data5$Month)
month_table 
chisq.test(month_table) 
barplot(month_table)


month_data<-data.frame(Months=c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec"),
                       Frequency=c(42,39,44,39,55,31,28,28,42,37,42,76))
month_data 

# now plotting in graph
fig6<- ggplot(month_data, aes(x= Months, y= Frequency))+
  geom_col()+
  theme(panel.grid = element_blank(), # it removes the background grids
        panel.background = element_blank(),
        text=element_text(size = 10, family = "Arial"),
        axis.line = element_line(size = 0.7, color = "black"),
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = "top",
        legend.direction = "horizontal")+
  labs(y="Number of incidents", x="Months")

fig6

ggsave("fig6.tiff", width=15, height=15, units=c("cm"), dpi=400)

# Spatial pattern of wildlife attacks on human************************************************
data5
c<-table(data5$Location)
chisq.test(c)

b<-prop.table(c)
b*100

r<-table(data5$Location, data5$Species=="Rhino")
r
chisq.test(r)
#species and location significant**************************************************
f<-table(data5$Species, data5$Location)
f
chisq.test(f)
# there was warning message for chi test and fisher test takes too much time so we use this
chisq.test(f, simulate.p.value = TRUE)
#creating new data by removing all missing values in location
data_l<-read_xlsx("hwc_data1.xlsx" )
data_l

fig7<-ggplot(data_l, aes(x=Species, fill=Location))+
  geom_bar(position = position_dodge(0.5), width = 0.4)+
  theme(panel.grid = element_blank(), # it removes the background grids
        panel.background = element_blank(),
        text=element_text(size = 10, family = "Arial"),
        axis.line = element_line(size = 0.7, color = "black"),
        plot.margin = unit(c(1,1,1,1),units="cm"), # put a margin around the graph
        legend.title = element_blank(), # remove the title of legend
        legend.position = "top",
        legend.direction = "horizontal")+
  labs(y="Number of incidents", x="wildlife species")
fig7
fig7 +
  scale_fill_manual(values = c("red","blue","green","yellow"))
ggsave("june26_locationVsSpecies.tiff", width= 15, height=15, units=c("cm"), dpi=400)
chisq.test(data_l$Species, data_l$Location, simulate.p.value = TRUE)

# Logistic regression*********************************************************************

model_species<- glm(c_attack~c_rhino+ c_elephant+c_tiger+c_sloth_bear+ c_wild_boar ,data=data5,family = binomial)

summary(model_species) 


