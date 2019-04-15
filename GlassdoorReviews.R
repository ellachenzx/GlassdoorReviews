library(tidyverse)
library(gridExtra, warn.conflicts = F)
library(corrplot)
reviews <- read.csv("data/employee_reviews.csv")
head(reviews,1)

UpFirst <- function(s){
  paste(toupper(substring(s,1,1)), substring(s,2), sep = "")}

reviews$company <- UpFirst(reviews$company)
reviews$dates <- str_sub(reviews$dates, -4, -1)

names(reviews)
reviews <- reviews[,-c(1,3,5)]

cat("Rows and Columns: ", dim(reviews))

reviews2018 <- filter(reviews, dates == 2018)
cat("Rows and Columns:", dim(reviews2018))

#2018 reviews only take up about 20% of all reviews


tabl_comp <- as.data.frame(table(reviews$company))
names(tabl_comp)[1] <- "Company Name"
names(tabl_comp)[2] <- "Number_of_Reviews"
tabl_comp[order(tabl_comp$Number_of_Reviews, decreasing = T),]


#boxplots of ratings

c1 <- ggplot(reviews2018, aes(company, overall.ratings))+
  geom_boxplot(fill = "red3")+
  coord_flip()+
  labs(x = "", y = "", title = "Overall Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

reviews2018$work.balance.stars[reviews2018$work.balance.stars == "none"] <- NA
reviews2018$work.balance.stars <- as.numeric(as.character(reviews2018$work.balance.stars))
wb_full <- reviews2018[complete.cases(reviews2018$work.balance.stars), ]
c2 <- ggplot(wb_full, aes(company, work.balance.stars))+
  geom_boxplot(fill = "Royalblue2")+
  coord_flip()+
  labs(x = "", y = "", title = "Work Balance Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

reviews2018$culture.values.stars[reviews2018$culture.values.stars == "none"] <- NA
reviews2018$culture.values.stars <- as.numeric(as.character(reviews2018$culture.values.stars))
cv_full <- reviews2018[complete.cases(reviews2018$culture.values.stars), ]
c3 <- ggplot(cv_full, aes(company, culture.values.stars))+
  geom_boxplot(fill = "springgreen2")+
  coord_flip()+
  labs(x = "", y = "", title = "Culture Values Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

reviews2018$carrer.opportunities.stars[reviews2018$carrer.opportunities.stars == "none"] <- NA
reviews2018$carrer.opportunities.stars <- as.numeric(as.character(reviews2018$carrer.opportunities.stars))
copp_full <- reviews2018[complete.cases(reviews2018$carrer.opportunities.stars), ]
c4 <- ggplot(copp_full, aes(company, carrer.opportunities.stars))+
  geom_boxplot(fill = "yellow")+
  coord_flip()+
  labs(x = "", y = "", title = "Career Opportunities Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

reviews2018$comp.benefit.stars[reviews2018$comp.benefit.stars == "none"] <- NA
reviews2018$comp.benefit.stars <- as.numeric(as.character(reviews2018$comp.benefit.stars))
cb_full <- reviews2018[complete.cases(reviews2018$comp.benefit.stars), ]
c5 <- ggplot(cb_full, aes(company, comp.benefit.stars))+
  geom_boxplot(fill = "violet")+
  coord_flip()+
  labs(x = "", y = "", title = "Company Benefit Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

reviews2018$senior.mangemnet.stars[reviews2018$senior.mangemnet.stars == "none"] <- NA
reviews2018$senior.mangemnet.stars <- as.numeric(as.character(reviews2018$senior.mangemnet.stars))
sem_full <- reviews2018[complete.cases(reviews2018$senior.mangemnet.stars), ]
c6 <- ggplot(sem_full, aes(company, senior.mangemnet.stars))+
  geom_boxplot(fill = "sienna1")+
  coord_flip()+
  labs(x = "", y = "", title = "Senior Management Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

grid.arrange(c1, c2, c3, c4, c5, c6, ncol = 3)


#mean
mean_1 <- reviews2018 %>%
  group_by(company) %>%
  summarise(mean = mean(overall.ratings))

c7 <- ggplot(mean_1, aes(reorder(company, + mean), mean))+
  geom_bar(stat = "identity", colour = "black", fill = "red3")+
  coord_flip()+
  geom_text(aes(label = round(mean,2), y = mean/2), size = 3.5)+
  labs(x = "", y = "", title = "Overall Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))
  

mean_2 <- wb_full %>%
  group_by(company) %>%
  summarise(mean = mean(work.balance.stars))

C8 <- ggplot(mean_2, aes(reorder(company, + mean), mean))+
  geom_bar(stat = "identity", colour = "black", fill = "royalblue2")+
  coord_flip()+
  geom_text(aes(label = round(mean,2), y = mean/2), size = 3.5)+
  labs(x = "", y = "", title = "Work Balance Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))


mean_3 <- cv_full %>%
  group_by(company) %>%
  summarise(mean = mean(culture.values.stars))

C9 <- ggplot(mean_3, aes(reorder(company, + mean), mean))+
  geom_bar(stat = "identity", colour = "black", fill = "springgreen2")+
  coord_flip()+
  geom_text(aes(label = round(mean,2), y = mean/2), size = 3.5)+
  labs(x = "", y = "", title = "Culture Values Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))


mean_4 <- copp_full %>%
  group_by(company) %>%
  summarise(mean = mean(carrer.opportunities.stars))

C10 <- ggplot(mean_4, aes(reorder(company, + mean), mean))+
  geom_bar(stat = "identity", colour = "black", fill = "yellow")+
  coord_flip()+
  geom_text(aes(label = round(mean,2), y = mean/2), size = 3.5)+
  labs(x = "", y = "", title = "Career Opportunities Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))


mean_5 <- cb_full %>%
  group_by(company) %>%
  summarise(mean = mean(comp.benefit.stars))

C11 <- ggplot(mean_5, aes(reorder(company, + mean), mean))+
  geom_bar(stat = "identity", colour = "black", fill = "violet")+
  coord_flip()+
  geom_text(aes(label = round(mean,2), y = mean/2), size = 3.5)+
  labs(x = "", y = "", title = "Company Benefits Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

mean_6 <- sem_full %>%
  group_by(company) %>%
  summarise(mean = mean(senior.mangemnet.stars))

C12 <- ggplot(mean_6, aes(reorder(company, + mean), mean))+
  geom_bar(stat = "identity", colour = "black", fill = "sienna1")+
  coord_flip()+
  geom_text(aes(label = round(mean,2), y = mean/2), size = 3.5)+
  labs(x = "", y = "", title = "Senior Management Rating")+
  theme(axis.text.y = element_text(size=8), plot.title = element_text(size=8))

grid.arrange(c7, C8, C9, C10, C11, C12, ncol = 3)


#correlation 

reviews2018_full_ratings <- reviews2018[,c(7:12)]
reviews2018_full_ratings$overall.ratings[reviews2018_full_ratings$overall.ratings == "none"] <- NA
reviews2018_full_ratings$work.balance.stars[reviews2018_full_ratings$work.balance.stars == "none"] <- NA
reviews2018_full_ratings$culture.values.stars[reviews2018_full_ratings$culture.values.stars == "none"] <- NA
reviews2018_full_ratings$carrer.opportunities.stars[reviews2018_full_ratings$carrer.opportunities.stars == "none"] <- NA
reviews2018_full_ratings$comp.benefit.stars[reviews2018_full_ratings$comp.benefit.stars == "none"] <- NA
reviews2018_full_ratings$senior.mangemnet.stars[reviews2018_full_ratings$senior.mangemnet.stars == "none"] <- NA

reviews2018_full_ratings <- na.omit(reviews2018_full_ratings)

reviews2018_full_ratings$overall.ratings <- as.numeric(as.character(reviews2018_full_ratings$overall.ratings))
reviews2018_full_ratings$work.balance.stars <- as.numeric(as.character(reviews2018_full_ratings$work.balance.stars))
reviews2018_full_ratings$culture.values.stars <- as.numeric(as.character(reviews2018_full_ratings$culture.values.stars))
reviews2018_full_ratings$carrer.opportunities.stars <- as.numeric(as.character(reviews2018_full_ratings$carrer.opportunities.stars))
reviews2018_full_ratings$comp.benefit.stars <- as.numeric(as.character(reviews2018_full_ratings$comp.benefit.stars))
reviews2018_full_ratings$senior.mangemnet.stars <- as.numeric(as.character(reviews2018_full_ratings$senior.mangemnet.stars))

corrplot(cor(reviews2018_full_ratings), method="color", type = "upper", tl.col="black",
         tl.srt=45, addCoef.col = "gray8", diag = T)


#count characters in comments
nchar_pros <- nchar(as.vector(reviews2018$pros))
nchar_cons <- nchar(as.vector(reviews2018$cons))
nchar_reviews2018 <- data.frame(company = reviews2018$company, pros = nchar_pros, cons = nchar_cons)

c13 <- nchar_reviews2018 %>%
  group_by(company) %>%
  summarise(mean = mean(pros)) %>%
ggplot(., aes(reorder(company, + mean), mean)) + 
  geom_bar(stat = "identity", colour = "black", fill = "yellowgreen")+
  coord_flip()+
  labs(x="", y="Mean number of characters", title = "Pros")+
  geom_text(aes(y = mean/2, label = round(mean,0)))


c14 <- nchar_reviews2018 %>%
  group_by(company) %>%
  summarise(mean = mean(cons)) %>%
ggplot(., aes(reorder(company, + mean), mean)) + 
  geom_bar(stat = "identity", colour = "black", fill = "orangered")+
  coord_flip()+
  labs(x="", y="Mean number of characters", title = "Cons")+
  geom_text(aes(y = mean/2, label = round(mean,0)))

grid.arrange(c13, c14, ncol = 2)


#part2 what you might perveive from the sight
reviews_helpful <- filter(reviews2018, reviews2018$helpful.count > 10)
dim(reviews_helpful)

tabl_reviews_helpful <- as.data.frame(table(reviews_helpful$company))
names(tabl_reviews_helpful)[1] <- "Company_Name"
names(tabl_reviews_helpful)[2] <- "Reviews_with_More_Than_Ten_Helpful_Count"
tabl_reviews_helpful[order(tabl_reviews_helpful$Reviews_with_More_Than_Ten_Helpful_Count, decreasing = T),]

#let's count their words!
nchar_pros_helpful <- nchar(as.vector(reviews_helpful$pros))
nchar_cons_helpful <- nchar(as.vector(reviews_helpful$cons))
nchar_reviews_helpful <- data.frame(company = reviews_helpful$company, pros = nchar_pros_helpful, cons = nchar_cons_helpful)

c15 <- nchar_reviews_helpful %>%
  group_by(company) %>%
  summarise(mean = mean(pros)) %>%
  ggplot(., aes(reorder(company, + mean), mean)) + 
  geom_bar(stat = "identity", colour = "black", fill = "limegreen")+
  coord_flip()+
  labs(x="", y="Mean number of characters with >10 helpful count", title = "Pros")+
  geom_text(aes(y = mean/2, label = round(mean,0)))


c16 <- nchar_reviews_helpful %>%
  group_by(company) %>%
  summarise(mean = mean(cons)) %>%
  ggplot(., aes(reorder(company, + mean), mean)) + 
  geom_bar(stat = "identity", colour = "black", fill = "orangered3")+
  coord_flip()+
  labs(x="", y="Mean number of characters with >10 helpful count", title = "Cons")+
  geom_text(aes(y = mean/2, label = round(mean,0)))

grid.arrange(c15, c16, ncol = 2)








