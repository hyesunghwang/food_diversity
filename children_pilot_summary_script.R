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
child_pilot<-read_excel("child_pilot1.xlsx")
child_pilot<-read.csv("child_pilot1.csv")


match("sago..lab",names(child_pilot))
match("Pen..no.lab",names(child_pilot))

child_pilot[,21:44]<-as.numeric(child_pilot[,21:44])

# add summary columns 
## foreign food labeled
child_pilot<-dplyr::mutate(child_pilot, foreign_food_label = (sago..lab+Nopales..lab+Feng.zhua..lab+Pidan..lab)/4) 
## foreign foods non-labeled 
child_pilot<-dplyr::mutate(child_pilot, foreign_food_no_label = (Mi.yeok.gook..no.lab+Chin.chow..no.lab+Yan.Wo..no.lab+Yu.chi..no.lab)/4) 
## western food labeled
child_pilot<-dplyr::mutate(child_pilot, western_food_label = (Chili..lab+BLT..lab+Caesar.salad..lab+Oreo..lab)/4) 
## western foods non-labeled 
child_pilot<-dplyr::mutate(child_pilot, western_food_no_label = (Milkshake..no.lab+Hamburger..no.lab+Pizza..no.lab+Mac...cheese..no.lab)/4) 
## nonfood labeled
child_pilot<-dplyr::mutate(child_pilot, nonfood_label = (Soap..lab+Candle..lab+Chalk..lab+Thread..lab)/4) 
## nonfoods non-labeled 
child_pilot<-dplyr::mutate(child_pilot, nonfood_no_label = (Cotton.ball..no.lab+Newspaper..no.lab+Dish.sponge..no.lab+Pen..no.lab)/4) 
## labels
child_pilot<-dplyr::mutate(child_pilot, label = (foreign_food_label+western_food_label+nonfood_label)/3) 
## no_lables
child_pilot<-dplyr::mutate(child_pilot, no_label = (foreign_food_no_label+western_food_no_label+nonfood_no_label)/3) 

child_pilot<-as.data.frame(child_pilot)
write.csv(child_pilot, file = "child_pilot_summary.csv")

# overall summary
## standard error function
se <- function(x) sd(x)/sqrt(length(x))

## create data frame
child_pilot_summary<-data.frame(matrix(nrow=2,ncol=3))
colnames(child_pilot_summary)<- c("mean", "se", "label_type")
child_pilot_summary$label_type<-c("label", "no label")

## fill data frame
child_pilot_summary$mean<-c(mean(child_pilot$label),mean(child_pilot$no_label))
child_pilot_summary$se<-c(se(child_pilot$label), se(child_pilot$no_label))
                          
## graph
ggplot(data = child_pilot_summary, aes(x = label_type, y = mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))

# summary by food type
## create data frame
child_pilot_summary2<-data.frame(matrix(nrow=6,ncol=4))
colnames(child_pilot_summary2)<- c("mean", "se", "food_type" ,"label_type")
child_pilot_summary2$label_type<-c("label", "no label",
                                   "label", "no label",
                                   "label", "no label")
child_pilot_summary2$food_type<-c("foreign_food", "foreign_food", "western_food", "western_food",
                                 "non_food", "non_food")
## fill data frame
child_pilot_summary2$mean<-c(mean(child_pilot$foreign_food_label),mean(child_pilot$foreign_food_no_label),
                             mean(child_pilot$western_food_label),mean(child_pilot$western_food_no_label),
                             mean(child_pilot$nonfood_label),mean(child_pilot$nonfood_no_label))
child_pilot_summary2$se<-c(se(child_pilot$foreign_food_label),se(child_pilot$foreign_food_no_label),
                           se(child_pilot$western_food_label),se(child_pilot$western_food_no_label),
                           se(child_pilot$nonfood_label),se(child_pilot$nonfood_no_label))

## graph
tiff("child_pilot1_mainresults.tiff", res=800, compression = "lzw", height=5, width=5, units="in")
a<-ggplot(data = child_pilot_summary2, aes(x = food_type, y = mean, fill = label_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))+
  labs(y = "Acceptability ratings: How okay is it to eat this")+
  ggtitle("Child Pilot ver 1 - main results")
a
dev.off()

# linear model to test main effect and interactions
## select only relevant columns
child_pilot_short<-select(child_pilot, foreign_food_label, foreign_food_no_label,
                          western_food_label, western_food_no_label,
                          nonfood_label, nonfood_no_label)
rownames(child_pilot_short)<-c(1:10)
child_pilot_short$p<-c(1:10)

## transpose
child_pilot_short_t<-melt(child_pilot_short, id = "p")

# create columns 
child_pilot_short_t$food_type<-as.factor(c(rep("foreign_food", length(1:20)), 
                                  rep("western_food", length(1:20)),
                                 rep("non_food", length(1:20))))
child_pilot_short_t$label_type<-as.factor(c(rep("label", length(1:10)), 
                                  rep("no_label", length(1:10)),
                                  rep("label", length(1:10)),
                                  rep("no_label", length(1:10)),
                                  rep("label", length(1:10)),
                                  rep("no_label", length(1:10))))

## linear model
model1<-lm(value~food_type+label_type, data = child_pilot_short_t)
summary(model1)

model2<-lm(value~food_type*label_type, data = child_pilot_short_t)
summary(model2)
