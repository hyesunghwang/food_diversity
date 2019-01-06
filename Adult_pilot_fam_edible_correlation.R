# Clear the workspace and console
rm(list = ls(all = TRUE)) 
cat("\014")

# load library
library(readxl)
library(psych)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)


# set working directory
setwd("/Users/hyesunghwang/Dropbox/food_diversity")

# Import dataset
data1<-read.csv("Adult_pilot1.csv", header=TRUE)

# Familiarity and its relation to rating
## change columns names 
colnames(data1)[colnames(data1) == 'Q50_1']<-"Fam_Sago"
colnames(data1)[colnames(data1) == 'Q50_2']<-"Fam_Mi-yeok-gook"
colnames(data1)[colnames(data1) == 'Q50_3']<-"Fam_Nopales"
colnames(data1)[colnames(data1) == 'Q50_4']<-"Fam_Chinchow"
colnames(data1)[colnames(data1) == 'Q50_5']<-"Fam_Fengzhua"
colnames(data1)[colnames(data1) == 'Q50_6']<-"Fam_Yanwo"
colnames(data1)[colnames(data1) == 'Q50_7']<-"Fam_Pidan"
colnames(data1)[colnames(data1) == 'Q50_8']<-"Fam_Yuchi"

colnames(data1)[colnames(data1) == 'Q56_1']<-"Fam_Oreos"
colnames(data1)[colnames(data1) == 'Q56_2']<-"Fam_Mac_cheese"
colnames(data1)[colnames(data1) == 'Q56_3']<-"Fam_Chili"
colnames(data1)[colnames(data1) == 'Q56_4']<-"Fam_Hamburger"
colnames(data1)[colnames(data1) == 'Q56_5']<-"Fam_BLT"
colnames(data1)[colnames(data1) == 'Q56_6']<-"Fam_Milkshakes"
colnames(data1)[colnames(data1) == 'Q56_7']<-"Fam_Caesar_salad"
colnames(data1)[colnames(data1) == 'Q56_8']<-"Fam_Pizza"

## select only relevant columns
### find relevant column number for edible ratings
match("Sago..lab",names(data1))
match("Mac...cheese..no.lab",names(data1))
data2<-select(data1, c(18:33))
data2$p<-c(1:10)
data2<-melt(data2, id = "p")
colnames(data2)<-c("p", "food_type","edible_rating")

### find relevant column number for familiarity ratings
match("Fam_Sago",names(data1))
match("Fam_Yuchi",names(data1))
data3<-select(data1, c(42:49,55,52,53,54,56,57,50,51))
data3$p<-c(1:10)
data3<-melt(data3, id = "p")
colnames(data3)<-c("p", "food_type","fam_rating")

### combine dataset
data4<-cbind(data2,data3)

# correlation between edible rating and fam rating
cor.test(data4$edible_rating, data4$fam_rating, method = "pearson")













