# COURSE: Introductory to Case Study Summer 2023
# PROJECT TITLE: Descriptive Analysis of Demographic Data (Project 1)
# DATE CREATED: 28 April 2023 # To be submitted by 12 May 2023 #
# InSTRUCTOR: Prof. Dr. Katja Ickstadt, M.Sc. Zeyu Ding, M.Sc. Yassine Talleb
# GROUP MEMBERS: Opeyemi Ayanwale (236783), Azeezat Mosunmade Mustapha(), Divya Prima Crasta(), Taiwo Ojekunle(224926)
# PROJECT GROUP: Group 10
# Last changes:



#############################
# R SETUP
#############################

## Download relevant R packages with the following command:
#install.packages()
#install.packages(c("ggplot2", "data.table", "dplyr", "tidyverse"))
#install.packages("psych")


# 1.	Load the R package needed and with library()
#library()
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse)
library(psych)
library(cowplot)
library(Amelia)



#############################
# DATA SETUP
#############################

# 2. Download the dataset "Census_Data" and store it locally in a directory.
#    Set your working directory to this directory with setwd() if not set already.
setwd("/Users/user/Desktop/SOSE23/ICS")


# 3. Import the data using the read.csv function and store it in a variable named census_data#
census_data <- read.csv("census2002_2022.csv")

# 4. Check whether the dataset was correctly loaded: # 454 obs of 11 variables
View(census_data)
str(census_data)
names(census_data)
head(census_data)

missmap(census_data)
# 5. Check for missig values
# to know how many missing value in the variable: sum(is.na()) 
sum(is.na(census_data))                                      #44 missing 
sum(is.na(census_data$Country.Area.Name))                    #0 missing    
sum(is.na(census_data$Subregion))                            #4 missing                            
sum(is.na(census_data$Region))                               #4 missing 
sum(is.na(census_data$Year))                                 #0 missing 
sum(is.na(census_data$Life.Expectancy.at.Birth..Both.Sexes)) #6 missing 
sum(is.na(census_data$Life.Expectancy.at.Birth..Males))      #6 missing 
sum(is.na(census_data$Life.Expectancy.at.Birth..Females))    #6 missing 
sum(is.na(census_data$Under.Age.5.Mortality..Both.Sexes))    #6 missing 
sum(is.na(census_data$Under.Age.5.Mortality..Males))         #6 missing 
sum(is.na(census_data$Under.Age.5.Mortality..Females))       #6 missing 


# without having to count d missing value we can also
which(is.na(census_data$Subregion))                            # 101 102 107 108
which(is.na(census_data$Region))
which(is.na(census_data$Life.Expectancy.at.Birth..Both.Sexes)) # 235 325 379 385 393 429
which(is.na(census_data$Life.Expectancy.at.Birth..Males))
which(is.na(census_data$Life.Expectancy.at.Birth..Females))
which(is.na(census_data$Under.Age.5.Mortality..Both.Sexes))
which(is.na(census_data$Under.Age.5.Mortality..Males))
which(is.na(census_data$Under.Age.5.Mortality..Females))


# 6. Data Cleanup: Imput and Remove missing and unnessary column in the data 
# replacing NA in region and subregion with the actual subregion and region
census_data$Subregion[census_data$Country == "Curaçao"] <- "Caribbean"
census_data$Subregion[census_data$Country == "Côte d'Ivoire"] <- "Western Africa"

census_data$Region[census_data$Country == "Curaçao"] <- "Americas"
census_data$Region[census_data$Country == "Côte d'Ivoire"] <- "Africa"

#it is necessary to give exact value for country, it was not getting changed
#it was still na before. I corrected it.

# Re-check for missing values
sum(is.na(census_data$Subregion))           # no more missing value 
sum(is.na(census_data$Region))              # no more missing value 


View(census_data)

#the remaining missing values are in 2002 data which is not needed for first
#three questions, so they are dealt with later.

census_data <- census_data[,-1]
View(census_data)                              # 448 obs of 10 variables


# 7. Filter Data for year 2022 
census_22 <- filter(census_data, census_data$Year == 2022)   # 227 obs of 10 variables
View(census_22)
head(census_22)


#############################
# DESCRIPTIVE STATISTICS
#############################
means <- apply(census_22[,5:10],2,mean)
sds <- apply(census_22[,5:10],2,sd)
quantiles <- sapply(census_22[,5:10],quantile)
table1 <- cbind(data.frame(means,sds),t(quantiles))
table1

# Frequency tables 
table(census_22$Subregion)
table(census_22$Region)

# 9. Data visualization for Descriptive analysis Task 1 using histogram

census_22$Region <- factor(census_22$Region,
                    levels = c("Africa","Americas","Asia","Europe","Oceania"))

#this step was needed because otherwise in plot na was also considered as a factor

hist1 <- ggplot(census_22, aes(Life.Expectancy.at.Birth..Both.Sexes,groups = Region)) + 
  geom_histogram( binwidth = 2, color= 'black',aes(fill=Region),show.legend = F) +
  xlab('LEB') + ggtitle('LEB')+
  scale_x_continuous(breaks = seq(0,90,5))

hist2 <- ggplot(census_22, aes(Life.Expectancy.at.Birth..Males,groups = Region)) + 
  geom_histogram( binwidth = 2, color= 'black',aes(fill=Region),show.legend = F) +
  xlab('LEM') + ggtitle('LEM')+
  scale_x_continuous(breaks = seq(50,90,5))

hist3 <- ggplot(census_22, aes(Life.Expectancy.at.Birth..Females,groups = Region)) + 
  geom_histogram( binwidth = 2, color= 'black',aes(fill=Region),show.legend = F) +
  xlab('LEF') + ggtitle('LEF')+
  scale_x_continuous(breaks = seq(50,90,5))


hist4 <- ggplot(census_22, aes(Under.Age.5.Mortality..Both.Sexes,groups = Region)) + 
  geom_histogram( binwidth = 5, color= 'black',aes(fill=Region),show.legend = F) +
  xlab('MB') + ggtitle('MB')+
  scale_x_continuous(breaks = seq(0,150,15))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

hist5 <- ggplot(census_22, aes(Under.Age.5.Mortality..Males,groups = Region)) + 
  geom_histogram( binwidth = 5, color= 'black',aes(fill=Region),show.legend = F) +
  xlab('MM') + ggtitle('MM')+
  scale_x_continuous(breaks = seq(0,165,15))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

hist6 <- ggplot(census_22, aes(Under.Age.5.Mortality..Females,groups = Region)) + 
  geom_histogram( binwidth = 5, color= 'black',aes(fill=Region),show.legend = F) +
  xlab('MF') + ggtitle('MF')+
  scale_x_continuous(breaks = seq(0,150,15))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_grid(hist1,hist2,hist3,hist4,hist5,hist6)
plot_grid()

LEc <- ggplot(census_22,aes(Life.Expectancy.at.Birth..Males,
                            Life.Expectancy.at.Birth..Females))+
  geom_point()+
  scale_x_continuous(breaks = seq(50,95,5))+
  geom_line(aes(Life.Expectancy.at.Birth..Males,Life.Expectancy.at.Birth..Males))

#mortality comparison btwn sexes
Mc <- ggplot(census_22,aes(Under.Age.5.Mortality..Males,
                        Under.Age.5.Mortality..Females))+geom_point()+
  scale_x_continuous(breaks = seq(0,150,50))+
  geom_line(aes(Under.Age.5.Mortality..Males,Under.Age.5.Mortality..Males))

plot_grid(LEc,Mc)

#this plot is for comparison of sexes

# Barplot for categorical variable
#ggplot(census_22, aes(fct_infreq(Subregion))) +
 # geom_bar(fill = "grey") +
 #  coord_flip() +
 # xlab("Subregion") + ggtitle("Distribution of Subregion")

#ggplot(census_22, aes(fct_infreq(Region))) +
 # geom_bar(fill = "grey") +
#  xlab("Region") + ggtitle("Distribution of Region")

#This is not necessary.


new_data <- data.frame(Life.Expectancy = c(census_22$Life.Expectancy.at.Birth..Males,
              census_22$Life.Expectancy.at.Birth..Females),
              "Mortality" = c(census_22$Under.Age.5.Mortality..Males,
                census_22$Under.Age.5.Mortality..Females),
              "Sex" = factor(c(rep("Male",227),rep("Female",227))))


sexhist1 <- ggplot(new_data,aes(Life.Expectancy,groups = Sex))+
  geom_histogram(aes(fill=Sex),col = 'black',binwidth = 2,show.legend = F)+
  scale_x_continuous(breaks = seq(50,100,5))

sexhist2 <- ggplot(new_data,aes(Mortality,groups = Sex))+
  geom_histogram(aes(fill=Sex),col = 'black', binwidth = 5,show.legend = F)+
  scale_x_continuous(breaks = seq(0,175,15))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_grid(sexhist1,sexhist2)

#############################
# VARIABILITY OF VALUES
#############################

# 10. TASK 2:  Variability of the values in the individual subregions using boxplot 


census_22ar <- arrange(census_22,Region,Subregion)

census_22ar$Subregion <- factor(census_22ar$Subregion,
                              levels=unique(census_22ar$Subr))

census_22ar$Region <- factor(census_22ar$Region,
                             levels=unique(census_22ar$Region))

ggplot(census_22ar,aes(x=Subregion,y=Life.Expectancy.at.Birth..Males))+
  geom_boxplot(aes(fill=Region))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1))



census_22_asia <- census_22[census_22$Region == "Asia",]

box1 <- ggplot(census_22_asia,aes(x= Subregion,y=Life.Expectancy.at.Birth..Both.Sexes))+
  geom_boxplot(alpha=0.3)+scale_y_continuous(breaks = seq(50,90,5))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1),
        axis.title.x=element_blank())+
  scale_x_discrete(labels= c("EA","SCA","SEA","WA"))+ylab("LEB")

box2 <- ggplot(census_22_asia,aes(x= Subregion,y=Life.Expectancy.at.Birth..Males))+
  geom_boxplot(alpha=0.3)+scale_y_continuous(breaks = seq(50,90,5))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1),
        axis.title.x=element_blank())+
  scale_x_discrete(labels = c("EA","SCA","SEA","WA"))+ylab("LEM")

box3 <- ggplot(census_22_asia,aes(x= Subregion,y=Life.Expectancy.at.Birth..Females))+
  geom_boxplot(alpha=0.3)+scale_y_continuous(breaks = seq(50,90,5))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1),
        axis.title.x=element_blank())+
  scale_x_discrete(labels = c("EA","SCA","SEA","WA"))+ylab("LEF")

box4 <- ggplot(census_22_asia,aes(x= Subregion,y=Under.Age.5.Mortality..Both.Sexes))+
  geom_boxplot(alpha=0.3)+scale_y_continuous(breaks = seq(0,160,15))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1),
        axis.title.x=element_blank())+
  scale_x_discrete(labels = c("EA","SCA","SEA","WA"))+ylab("MB")

box5 <- ggplot(census_22_asia,aes(x= Subregion,y=Under.Age.5.Mortality..Males))+
  geom_boxplot(alpha=0.3)+scale_y_continuous(breaks = seq(0,160,15))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1),
        axis.title.x=element_blank())+
  scale_x_discrete(labels = c("EA","SCA","SEA","WA"))+ylab("MM")

box6 <- ggplot(census_22_asia,aes(x= Subregion,y=Under.Age.5.Mortality..Females))+
  geom_boxplot(alpha=0.3)+scale_y_continuous(breaks = seq(0,160,15))+
  theme(axis.text.x =element_text(angle=90,vjust=0.3,hjust=1),
        axis.title.x=element_blank())+
  scale_x_discrete(labels = c("EA","SCA","SEA","WA"))+ylab("MF")


plot_grid(box1,box2,box3,box4,box5,box6)

#############################
# BIVARIATE CORRELATION
#############################

# 11. TASK 3:  Correlation 
#names(census_22)[5:10] <- c("LEB","LEM","LEF","MB","MM","MF")
#you have not considered the case of correlation between all combinations, using below function we can get all possible
cor.table <- round(cor(census_22[,5:10], method = "pearson"),4)
dimnames(cor.table) <- (list(c("LEB","LEM","LEF","MB","MM","MF"),
                             c("LEB","LEM","LEF","MB","MM","MF")))
View(cor.table)
# 12. Data visualization for Correlation Task 3 using scatter plot

#Exploring correlation between Life Expectancy and Mortality Rate for both sexes
plot(census_22$Under.Age.5.Mortality..Both.Sexes, census_22$Life.Expectancy.at.Birth..Both.Sexes, main="Life Expectancy vs Mortality Rate in Both Sexes",
     xlab="Total Mortality Rate", ylab="Life Expectancy - Both Sexes", pch=20)  


#Exploring correlation between Life Expectancy for male and Mortality Rate 
plot(census_22$Under.Age.5.Mortality..Both.Sexes, census_22$Life.Expectancy.at.Birth..Males, main="Life Expectancy vs Mortality Rate in Male",
     xlab="Total Mortality Rate", ylab="Life Expectancy - Male", pch=20)


#Exploring correlation between Life Expectancy for female and Mortality Rate
plot(census_22$Under.Age.5.Mortality..Both.Sexes, census_22$Life.Expectancy.at.Birth..Females, main="Life Expectancy vs Mortality Rate in Female",
     xlab=" Total Mortality Rate", ylab="Life Expectancy - Female", pch=20)



#if we are to do scatter plot for all there will be 15 graphs, in grid it will
#be conjusted, I don't think there's place for these plots in report
#if you do you can include.


#############################
# VARIABLE CHANGE OVERTIME
#############################

# 13. TASK 4:  comparing 2002 with 2022?  
#Boxplot comparing Life Expectancy by Year
#boxplot(Life.Expectancy.at.Birth..Both.Sexes~Year,data=census_data2, main="Life Expectancy by Year",
 #       xlab="Year", ylab="Life Expectancy - Both Sexes")

#boxplot(Life.Expectancy.at.Birth..Males~Year,  data=census_data2, main="Life Expectancy Male by Year",
  #      xlab="Year", ylab="Life Expectancy Male")

#boxplot(Life.Expectancy.at.Birth..Females~Year,  data=census_data2, main="Life Expectancy Female by Year",
    #    xlab="Year", ylab="Life Expectancy Female")

#Boxplot comparing Mortality Rate by Year
#boxplot(Under.Age.5.Mortality..Both.Sexes~Year,data=census_data2, main="Mortality Rate by Year",
 #       xlab="Year", ylab="Mortality Rate")

#before doing this we need to impute the average to missing observations.


census_data <- read.csv("census2002_2022.csv")
r <- which(is.na(census_data$Life.Expectancy.at.Birth..Both.Sexes))

r #235 325 379 385 393 429


census_data[r,'Subregion'] #N.Af, Caribbean, Naf, Naf, West Asia, Nam

census_data2 <- na.omit(census_data)


Naf_means <- sapply(4:9,function(i)
  mean(census_data2[census_data2$Subregion == "Northern Africa"
            & census_data2$Year == 2002, i]))
Caribbean_means <- sapply(4:9,function(i)
  mean(census_data2[census_data2$Subregion == "Caribbean"
                   & census_data2$Year == 2002, i]))
Was_means <- sapply(4:9,function(i)
  mean(census_data2[census_data2$Subregion == "Western Asia"
                   & census_data2$Year == 2002, i]))

Nam_means <- sapply(4:9,function(i)
  mean(census_data2[census_data2$Subregion == "Western Asia"
                   & census_data2$Year == 2002, i]))

census_data[c(235,379,385),4:9] <- Naf_means

census_data[c(325),4:9] <- Caribbean_means

census_data[c(393),4:9] <- Was_means

census_data[c(429),4:9] <- Nam_means

library(Amelia)
missmap(census_data)

#here I imputed the mean values of subregion,
#the doubt is whether to imputed mean of subregion or region and what which year

#after this we can do boxplots like above.

census_data$Year <- factor(census_data$Year, levels = c(2002,2022))
boxplot(Life.Expectancy.at.Birth..Both.Sexes~Year,data=census_data)
levels(census_22$Region)
#Boxplot comparing Life Expectancy by Year
LEB <- boxplot(Life.Expectancy.at.Birth..Both.Sexes~Year,data=census_data2, main="Life Expectancy by Year",
       xlab="Year", ylab="Life Expectancy - Both Sexes")

LEM <- boxplot(Life.Expectancy.at.Birth..Males~Year,  data=census_data2, main="Life Expectancy Male by Year",
      xlab="Year", ylab="Life Expectancy Male")

LEF <- boxplot(Life.Expectancy.at.Birth..Females~Year,  data=census_data2, main="Life Expectancy Female by Year",
     xlab="Year", ylab="Life Expectancy Female")

#Boxplot comparing Mortality Rate by Year

MB <-  boxplot(Under.Age.5.Mortality..Both.Sexes~Year,data=census_data2, main="Mortality Rate by Year",
       xlab="Year", ylab="Mortality Rate")
MM <- boxplot(Under.Age.5.Mortality..Males~Year,data=census_data2, main="Mortality Rate male by Year",
             xlab="Year", ylab="Mortality Rate male")
MF <-  boxplot(Under.Age.5.Mortality..Females~Year,data=census_data2, main="Mortality Rate female by Year",
             xlab="Year", ylab="Mortality Rate female")

par(mfrow = c(3,2))

