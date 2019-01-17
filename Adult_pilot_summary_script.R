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
library(reshape)


# set working directory
setwd("/Users/hyesunghwang/Dropbox/food_diversity")

# Import dataset
adult_pilot<-read.csv("Adult_pilot1.csv", header=TRUE)
data1<-read.csv("adult_pilot_summary_updated.csv", header=TRUE)


# add summary columns 
## foreign food labeled
adult_pilot<-dplyr::mutate(adult_pilot, foreign_food_label = (Sago..lab+Nopales..lab+Feng.zhua..lab+Pidan..lab)/4) 
## foreign foods non-labeled 
adult_pilot<-dplyr::mutate(adult_pilot, foreign_food_no_label = (Mi.yeok.gook..no.lab+Chin.chow..no.lab+Yan.Wo..no.lab+Yu.chi..no.lab)/4) 
## western food labeled
adult_pilot<-dplyr::mutate(adult_pilot, western_food_label = (Chili..lab+BLT..lab+Caesar.salad..lab+Oreo..lab)/4) 
## western foods non-labeled 
adult_pilot<-dplyr::mutate(adult_pilot, western_food_no_label = (Milkshake..no.lab+Hamburger..no.lab+Pizza..no.lab+Mac...cheese..no.lab)/4) 
## nonfood labeled
adult_pilot<-dplyr::mutate(adult_pilot, nonfood_label = (Soap..lab+Candle..lab+Chalk..lab+Thread..lab)/4) 
## nonfoods non-labeled 
adult_pilot<-dplyr::mutate(adult_pilot, nonfood_no_label = (Cotton.ball..no.lab+Newspaper..no.lab+Dish.sponge..no.lab+Pen..no.lab)/4) 
## labels
adult_pilot<-dplyr::mutate(adult_pilot, label = (foreign_food_label+western_food_label+nonfood_label)/3) 
## no_lables
adult_pilot<-dplyr::mutate(adult_pilot, no_label = (foreign_food_no_label+western_food_no_label+nonfood_no_label)/3) 

adult_pilot<-as.data.frame(adult_pilot)
write.csv(adult_pilot, file = "adult_pilot_summary.csv")

# overall summary
## standard error function
se <- function(x) sd(x)/sqrt(length(x))

## create data frame
adult_pilot_summary<-data.frame(matrix(nrow=2,ncol=3))
colnames(adult_pilot_summary)<- c("mean", "se", "label_type")
adult_pilot_summary$label_type<-c("label", "no label")

## fill data frame
adult_pilot_summary$mean<-c(mean(adult_pilot$label),mean(adult_pilot$no_label))
adult_pilot_summary$se<-c(se(adult_pilot$label), se(adult_pilot$no_label))
                          
## graph
ggplot(data = adult_pilot_summary, aes(x = label_type, y = mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))

# summary by food type
## create data frame
adult_pilot_summary2<-data.frame(matrix(nrow=6,ncol=4))
colnames(adult_pilot_summary2)<- c("mean", "se", "food_type" ,"label_type")
adult_pilot_summary2$label_type<-c("label", "no label",
                                   "label", "no label",
                                   "label", "no label")
adult_pilot_summary2$food_type<-c("foreign_food", "foreign_food", "western_food", "western_food",
                                 "non_food", "non_food")
## fill data frame
adult_pilot_summary2$mean<-c(mean(adult_pilot$foreign_food_label),mean(adult_pilot$foreign_food_no_label),
                             mean(adult_pilot$western_food_label),mean(adult_pilot$western_food_no_label),
                             mean(adult_pilot$nonfood_label),mean(adult_pilot$nonfood_no_label))
adult_pilot_summary2$se<-c(se(adult_pilot$foreign_food_label),se(adult_pilot$foreign_food_no_label),
                           se(adult_pilot$western_food_label),se(adult_pilot$western_food_no_label),
                           se(adult_pilot$nonfood_label),se(adult_pilot$nonfood_no_label))

## graph
tiff("Adult_pilot1_mainresults.tiff", res=800, compression = "lzw", height=5, width=5, units="in")
a<-ggplot(data = adult_pilot_summary2, aes(x = food_type, y = mean, fill = label_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))+
  labs(y = "Acceptability ratings: How okay is it to eat this")+
  ggtitle("Adult MTurk Pilot ver 1 - main results")
a
dev.off()

# linear model to test main effect and interactions
## select only relevant columns
adult_pilot_short<-select(adult_pilot, foreign_food_label, foreign_food_no_label,
                          western_food_label, western_food_no_label,
                          nonfood_label, nonfood_no_label)
rownames(adult_pilot_short)<-c(1:10)
adult_pilot_short$p<-c(1:10)

## transpose
adult_pilot_short_t<-melt(adult_pilot_short, id = "p")

# create columns 
adult_pilot_short_t$food_type<-as.factor(c(rep("foreign_food", length(1:20)), 
                                  rep("western_food", length(1:20)),
                                 rep("non_food", length(1:20))))
adult_pilot_short_t$label_type<-as.factor(c(rep("label", length(1:10)), 
                                  rep("no_label", length(1:10)),
                                  rep("label", length(1:10)),
                                  rep("no_label", length(1:10)),
                                  rep("label", length(1:10)),
                                  rep("no_label", length(1:10))))

## linear model
model1<-lm(value~food_type+label_type, data = adult_pilot_short_t)
summary(model1)

model2<-lm(value~food_type*label_type, data = adult_pilot_short_t)
summary(model2)

