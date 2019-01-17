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
library(ggpubr)
library(reshape)

# set working directory
setwd("/Users/hyesunghwang/Dropbox/food_diversity")

# Import dataset
data1<-read.csv("Adult_pilot1.csv", header=TRUE)

##################### Familiarity and its relation to rating ##################### 
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

# change values in the familiarity column from 6 to 5 (accidentally mislabeled)
data1[,c(42:57)][data1[,c(42:57)] == 6]<-5

write.csv(data1, file = "adult_pilot_summary_updated.csv")

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
colnames(data3)<-c("p2", "food_type2","fam_rating")

### combine dataset
data4<-cbind(data2,data3)

# correlation between edible rating and fam rating
cor.test(data4$edible_rating, data4$fam_rating, method = "pearson")
data4$edible_rating<-as.numeric(data4$edible_rating)
# summary by food type
data5 <- ddply(data4, "food_type", summarise,
               N    = length(edible_rating),
               edible_rating_mean = mean(edible_rating),
               edible_rating_sd   = sd(edible_rating),
               edible_rating_se   = edible_rating_sd/sqrt(N)
)

data6<-ddply(data4, "food_type2", summarise,
             N2    = length(fam_rating),
             fam_rating_mean = mean(fam_rating),
             fam_rating_sd   = sd(fam_rating),
             fam_rating_se   = fam_rating_sd/sqrt(N2)
)

data7<-cbind(data5, data6)

# correlation graph
tiff("Adult_pilot1_fam+accept_corr.tiff", res=800, compression = "lzw", height=5, width=5, units="in")
ggscatter(data5, x="edible_rating", y="fam_rating", fille = "food_type", 
          add = "reg.line",
          conf.int= TRUE,
          repel = TRUE, 
          #ylim=c(1,6),
          #xlim=c(1,6),
          ylab = "Familiarity ratings",
          xlab = "Acceptability ratings",
          title = "Adult MTurk Pilot ver 1 \n Correlation between acceptability and familiarity")+
  stat_cor(method = "pearson")
dev.off()

## create data frame
adult_pilot_summary2<-data.frame(matrix(nrow=2,ncol=3))
colnames(adult_pilot_summary2)<- c("mean", "se", "food_type")
adult_pilot_summary2$food_type<-c("foreign_food", "western_food")
## fill data frame
adult_pilot_summary2$mean<-c(mean(data5[c(1:8), 7]), mean((data5[c(9:16), 7])))
adult_pilot_summary2$se<-c(se(data5[c(1:8), 7]), se((data5[c(9:16), 7])))


## graph
tiff("Adult_pilot1_fam_rating_sum.tiff", res=800, compression = "lzw", height=5, width=5, units="in")
ggplot(data = adult_pilot_summary2, aes(x = food_type, y = mean)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9))+
  labs(y = "Familiarity ratings: How familiar (5 point rating)")+
ggtitle("Adult MTurk Pilot ver 1 - Familiarity ratings")
dev.off()

tiff("Adult_pilot1_accept_rating_individual.tiff", res=800, compression = "lzw", height=5, width=10, units="in")
a<-ggplot(data = data7, aes(x = food_type, y = edible_rating_mean)) +          
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=edible_rating_mean-edible_rating_se, ymax=edible_rating_mean+edible_rating_se), width=.2,
                position=position_dodge(.9))+
  labs(y = "Acceptability ratings: How okay is it to eat", xlab = "Food items")+
  ggtitle("Adult MTurk Pilot ver 1 - Acceptability ratings - individual items")+
  theme(axis.text.x = element_text(face = "bold",size=10, angle=45))
a

tiff("Adult_pilot1_familiar_rating_individual.tiff", res=800, compression = "lzw", height=5, width=10, units="in")
b<-ggplot(data = data7, aes(x = food_type, y = fam_rating_mean)) +          
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=fam_rating_mean-fam_rating_se, ymax=fam_rating_mean+fam_rating_se), width=.2,
                position=position_dodge(.9))+
  labs(y = "Familiarity ratings", xlab = "Food items")+
  ggtitle("Adult MTurk Pilot ver 1 -Familiarity ratings - individual items")+
  theme(axis.text.x = element_text(face = "bold",size=10, angle=45))
b









