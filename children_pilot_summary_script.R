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
child_pilot<-read_excel("child_pilot2.xlsx")
#child_pilot<-read.csv("child_pilot2.csv")

child_pilot<-child_pilot[-c(1:2),]
# add summary columns 
## unconventional combo labeled
child_pilot<-dplyr::mutate(child_pilot, unconventional_food_label = (milk_mustard_lab+potato_cereal_lab+chips_syrup_lab+pancake_salsa_lab)/4) 
## foreign foods non-labeled 
child_pilot<-dplyr::mutate(child_pilot, unconventional_food_no_label = (fries_jelly_no_lab+hotdog_choco_no_lab+obj_ketchup_no_lab+yogurt_gravy_no_lab)/4) 
## western food labeled
child_pilot<-dplyr::mutate(child_pilot, conventional_food_label = (hotdog_mustard_lab+milk_choco_lab+yogurt_granola_lab+pancake_syrup_lab)/4) 
## western foods non-labeled 
child_pilot<-dplyr::mutate(child_pilot, conventional_food_no_label = (pbj_no_lab+potatoe_gravy_no_lab+nacho__no_lab+fries_ketchup_no_lab)/4) 
## nonfood labeled
child_pilot<-dplyr::mutate(child_pilot, nonfood_label = (cotton_lab+newspaper_lab+sponge_lab+pen_lab)/4) 
## nonfoods non-labeled 
child_pilot<-dplyr::mutate(child_pilot, nonfood_no_label = (soap_no_lab+candle_no_lab+chalk_no_lab+thread_no_lab)/4) 
## labels
child_pilot<-dplyr::mutate(child_pilot, label = (unconventional_food_label+conventional_food_label+nonfood_label)/3) 
## no_labels
child_pilot<-dplyr::mutate(child_pilot, no_label = (unconventional_food_no_label+conventional_food_no_label+nonfood_no_label)/3) 

child_pilot<-as.data.frame(child_pilot)
write.csv(child_pilot, file = "child_pilot2_summary.csv")

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
child_pilot_summary2$food_type<-c("unconventional_food", "unconventional_food", "conventional_food", "conventional_food",
                                 "non_food", "non_food")
## fill data frame
child_pilot_summary2$mean<-c(mean(child_pilot$unconventional_food_label),mean(child_pilot$unconventional_food_no_label),
                             mean(child_pilot$conventional_food_label),mean(child_pilot$conventional_food_no_label),
                             mean(child_pilot$nonfood_label),mean(child_pilot$nonfood_no_label))
child_pilot_summary2$se<-c(se(child_pilot$unconventional_food_label),se(child_pilot$unconventional_food_no_label),
                           se(child_pilot$conventional_food_label),se(child_pilot$conventional_food_no_label),
                           se(child_pilot$nonfood_label),se(child_pilot$nonfood_no_label))

## graph
tiff("child_pilot2_unconventional_mainresults.tiff", res=800, compression = "lzw", height=5, width=5, units="in")
a<-ggplot(data = child_pilot_summary2, aes(x = food_type, y = mean, fill = label_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))+
  labs(y = "Acceptability ratings: How okay is it to eat this")+
  ggtitle("Child Pilot Unconventional Foods - main results")
a
dev.off()

# linear model to test main effect and interactions
## select only relevant columns
child_pilot_short<-select(child_pilot, unconventional_food_label, unconventional_food_no_label,
                          conventional_food_label, conventional_food_no_label,
                          nonfood_label, nonfood_no_label)

child_pilot_short$p<-c(1:10)

## transpose
child_pilot_short_t<-melt(child_pilot_short, id = "p")

# create columns 
child_pilot_short_t$food_type<-as.factor(c(rep("unconventional_food", length(1:20)), 
                                  rep("conventional_food", length(1:20)),
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
