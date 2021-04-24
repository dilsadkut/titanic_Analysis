train = read.csv("https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/train.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1520254832&Signature=jiVDVY5V2jdb644%2FFjlgZX7KNJcAkeIa3Axnv%2FHUGgwlZsIG%2FqwatHvTzqRTVRtMiOvANQUpme9ukaujF6inxLFenQCwN2FwFn29HdTR7ZdKD6uAdxH3V1w66XWgVKA3HNzRrIzWLWyqDvgUbVNqjb2OewZ9hpKsBXXBIePWazVJLNpqZBcLn%2FZYT%2FFukxFtHf9w43rJVtODmk%2Fd2omdsPPAd1JnIa4ZKOO%2BEJOwQ7aZhZ0DxmWdEj5kAHwkbrbIbKtWqrKgo1Tl1ZNPqrR%2B69f1zd%2FIP9Je5BY1TH7mfau2Q5PVuBj7z8fSNs7rAtAfRVRY5WlUASrTT8v2c8qjtw%3D%3D")

test = read.csv("https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/test.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1520254854&Signature=jrZSGaEIdQeShknLDECtmAvV3ordq52JOLDcSHox7ILMQGmBIYzJM9IEmjzG2iw6SxeWLheRJTz%2FAGLr6ipHYmiOMpibAd3GInKcgtY%2BtkRHYKkXoiEcxGawmi1lSs0SKmPz6qGGMlYwMrdvkmI%2FCJfcBjOk15aoH84bgccjUB9rIlhhtVtK%2FxKKTgq5cjpLiJ32fwnUshGvnhCYy03h8Pa7rg4sooA4CpIDAt7urzvTT%2BBEzJ%2BhpyxZcvjHeK1NLs0be9KZsG4Gpz%2BqjmPzHVmgIfIswsldRyDZKl1kEoDKG8iSSkkr766skdCPrJM6wQhYu6oOUNKI79X8LNjKEQ%3D%3D")

test$Survived = NA

tam.veri = rbind(train, test)

library(dplyr)
library(ggplot2)
tam.veri = tam.veri %>% mutate(Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex), Embarked = factor(Embarked))

str(tam.veri)

library(ggplot2)

gorsel.veri = tam.veri[1:891,]
rm(gorsel.veri)

p1 = ggplot(tam.veri[1:891,]) + geom_bar(mapping = aes(x = Pclass, fill = Survived),position = "fill")

p2 = ggplot(tam.veri[1:891,]) + geom_freqpoly(mapping = aes(x = Age, color = Survived), bins = 50) + theme(legend.position = "none")
p2

p3 = ggplot(tam.veri[1:891,]) + geom_freqpoly(mapping = aes(x = Fare, color = Survived), bins = 30) + theme(legend.position = "none")
p3

p4 = ggplot(tam.veri[1:891,]) + geom_bar(mapping = aes(x = SibSp + Parch, fill = Survived), position = "fill")+ theme(legend.position = "none")
p4

library(easyGgplot2)

ggplot2.multiplot(p1,p2,p3,p4)

##################

sum(is.na(tam.veri$Sex))

?apply

apply(tam.veri, 2, function(x) sum(is.na(x)))
tam.veri$Age
median(tam.veri[!is.na(tam.veri$Age),]$Age)


#Regular Expressions
#regex

tam.veri$Unvan = Unvan

str(tam.veri)

library(dplyr)

tam.veri = tam.veri %>% mutate(Unvan = factor(Unvan))

levels(tam.veri$Unvan)

install.packages("forcats")
library(forcats)
?fct_collapse

tam.veri = tam.veri %>% mutate(Unvan = fct_collapse(Unvan, "Miss" = c("Mlle", "Ms"), "Mrs" = "Mme", "Ranked" = c("Major", "Dr","Capt", "Col", "Rev"), "Royalty" = c("Lady", "Dona", "the Countess", "Don","Sir", "Jonkheer")))

levels(tam.veri$Unvan)


ggplot(tam.veri[1:891,]) + geom_bar(mapping = aes(x = Unvan, fill = Survived), position = "fill")

tam.veri = tam.veri %>% group_by(Unvan) %>%
  mutate(Age = ifelse(is.na(Age), round(median(Age, na.rm = T), 1), Age))


mean(tam.veri$Age, na.rm = T)

sum(is.na(tam.veri$Age))

?round

?ifelse

kabinBiliniyor = ifelse(tam.veri$Cabin == "", FALSE, TRUE)

tam.veri$kabinBiliniyor = kabinBiliniyor


ggplot(tam.veri[1:891,]) + geom_bar(mapping = aes(x = kabinBiliniyor, fill = Survived), position = "fill")

tam.veri %>% filter(is.na(Fare))

Fare = ifelse(is.na(tam.veri$Fare), round(median(tam.veri$Fare, na.rm = T),1), tam.veri$Fare)

tam.veri$Fare = Fare

train = tam.veri[1:891,]
test = tam.veri[892:1309,]

##### validation



891*0.8

train.1 = train[1:710,]
test.1 = train[711:891,]

## randomForest

install.packages("randomForest")
library(randomForest)

randomForest::randomForest()

?randomForest



rf = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + kabinBiliniyor , data = train.1, mtry = 3, ntree = 1000)

tahminler = predict(rf, test.1[, c(3, 5, 6,7,8,10,14)])
tahminler

table(tahminler, test.1$Survived)
sum(test.1$Survived == 0)

(101 + 54) / 181


tahminler.1 = predict(rf, test[, c(3, 5, 6,7,8,10,14)])

length(tahminler.1)

sonuc = test$PassengerId
sonuc = as.data.frame(sonuc)
colnames(sonuc) = c("PassengerId")
sonuc$Survived = tahminler.1

getwd()
setwd("/Users/lenovo/Documents")
write.csv(sonuc, "sonuc.csv", row.names = F)
