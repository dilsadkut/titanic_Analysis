#R ile Titanik Analizi


#Veri setimizde bulunan train.csv ve test.csv dosyalarinin okunmasi
train <- read_csv("https://storage.googleapis.com/kagglesdsdata/competitions/3136/26502/train.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1619506864&Signature=fiRqIo%2BUyElD1yuQ34Vqi6wRNWuRgDxLprHUBdV7bFT7QxYSzvv95qd7CU7xPTZCvTDTRnMwp1kUaVloXSTgfLQTYrnfaQmfqh4np5xQfl6%2BSZdnMK%2Fy5UueBmGOz0CLVbB%2FSj0vrz7bZAXtRY5nw12mMK0cF2JvmiVTjX%2FfdZAZM0k0iH7B%2BuM22nZF1TjCPlRWr591GXhtKfD0JUyo13j3exIyAC%2Fc0%2BqTd2FXk9VOI%2FVudLpYlpj5JKfj6U8Vtk0RzrMlPql11rsCQFmkvxxXqxIaDfvnyjbwTe7uUavdIO0UlGGmcAEKdM7Py%2FEEUlZGWLuGehZQb29kD6QLvw%3D%3D&response-content-disposition=attachment%3B+filename%3Dtrain.csv")

test <- read_csv("https://storage.googleapis.com/kagglesdsdata/competitions/3136/26502/test.csv?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=1619507013&Signature=MnhkU4rTx7zPCJcrFxbC8%2Bspyn6ixJZ2cVsjDGJ1sVcXvhWbjHlXWs7i%2BbqqYK43hlZAJEAIabNr3FLgy%2Fv8qSKIo3k2BQL6uTkF7hnYW%2FCD9%2B7PjyEfmIYVu4o2xV%2BOJt0rgsJ1fuBcfpu3qqefyQ%2FeERwE6czEi%2Bc4Ycz2tpjY%2FXrfoCvPIapn3H07ig94qqOhtD6WkK%2BFUJ2r9ukanW5Ew4UgBYaBWRtav2unu3Q3T1EfMYR9p1XkIMQxn%2Fb4C9IpjPsXGGJV7YoUw7bhvtvtASNlJLsPyKFa%2Bhzawj9%2Fq6YlKW22KpYMNszGs8XAgW2e0OLfZBo6i9tC%2Bje0nQ%3D%3D&response-content-disposition=attachment%3B+filename%3Dtest.csv")



test$Survived = NA

#train ve test veri setlerinin birleþimi
tam.veri = rbind(train,test)

#verileri inceleme
str(tam.veri)

#Survived - Factor olarak degistirilmeli (Sayisal deger karaktere dönüþtürülecek) 

#PClass - Factor olarak degistirilmeli (Sayisal deger karaktere dönüþtürülecek) 

#Sex - Factor olarak degistirilmeli (Sayisal deger karaktere dönüþtürülecek)  

#Embarked - Factor olarak degistirilmeli (Karakter deger sayisala dönüþtürülecek) 

#S - Southampton, C- Cherbourg and Q- Queenstown

#dplyr paketinin yüklenmesi
install.packages("dplyr")
library(dplyr)

mutate

?mutate 

dplyr::

str(tam.veri)

tam.veri = tam.veri %>% mutate(Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex),Embarked = factor(Embarked))

str(tam.veri)



#Gorsellestirme

library(ggplot2)

gorsel.veri =tam.veri[1:891,]
rm(gorsel.veri)

p1 = ggplot(tam.veri[1:891,]) + geom_bar(mapping = aes(x = Pclass, fill = Survived),position = "fill")

p1
p2 = ggplot(tam.veri[1:891,]) +  geom_freqpoly(mapping = aes(x= Age, color= Survived), bins = 50) + theme(legend.position = "none")

p2

summary(tam.veri)

p3 = ggplot(tam.veri[1:891,]) +  geom_freqpoly(mapping = aes(x= Fare, color= Survived), bins = 30) + theme(legend.position = "none")
p3

p4 = ggplot(tam.veri[1:891,]) +  geom_bar(mapping = aes(x= SibSp + Parch, fill= Survived), position = "fill") + theme(legend.position = "none")

p4

library(easyGgplot2)

ggplot2.multiplot(p1,p2,p3,p4)


#Na veriler

sum(is.na(tam.veri$Sex))

?apply

apply(tam.veri, 2, function(x) sum(is.na(x)))

mean(tam.veri$Age)

tam.veri$Age

mean(tam.veri[!is.na(tam.veri$Age),]$Age)

median(tam.veri[!is.na(tam.veri$Age),]$Age)

!FALSE

!TRUE

#isim degerleri icin karakterlere erisim
#Regular Expressions

#regex

?sub

Unvan = sub(".*,.([^.]*)\\..*","\\1",tam.veri$Name)

Unvan

tam.veri$Unvan = Unvan
##################

str(tam.veri)

library(dplyr)

tam.veri = tam.veri %>% mutate(Unvan=factor(Unvan))

str(tam.veri)

levels(tam.veri$Unvan)

install.packages("forcats")
library(forcats)
?fct_collapse

tam.veri = tam.veri %>% mutate(Unvan = fct_collapse(Unvan, "Miss" = c("Mlle","Ms"), "Mrs" = "Mme", "Ranked"=c("Major","Dr","Capt","Col","Rev"),"Royalty" = c("Lady","Dona","the Countess","Don","Sir","Jonkheer")))

levels(tam.veri$Unvan)

ggplot(tam.veri[1:891,]) + geom_bar(mapping = aes(x=Unvan, fill = Survived),position = "fill")

############################

#Eksik degerleri doldurma

tam.veri = tam.veri %>% group_by(Unvan) %>% 
  mutate(Age = ifelse(is.na(Age),round(median(Age, na.rm = T), 1), Age))

mean(tam.veri$Age,na.rm = T) #eksik degerleri gorme

sum(is.na(tam.veri$Age))

?round

?ifelse

###############################

kabinBiliniyor = ifelse(is.na(tam.veri$Cabin)==TRUE, FALSE, TRUE)

tam.veri$kabinBiliniyor = kabinBiliniyor

ggplot(tam.veri[1:891,]) + geom_bar(mapping =aes(x=kabinBiliniyor, fill= Survived),position = "fill")

####################################

tam.veri %>% filter(is.na(Fare))

Fare = ifelse(is.na(tam.veri$Fare), round(median(tam.veri$Fare,na.rm=T),1), tam.veri$Fare)

tam.veri$Fare = Fare

tam.veri %>% filter(is.na(Fare))

#Veriyi train ve test olarak ayirmak

train = tam.veri[1:891,]

test = tam.veri[892:1309,]

#Dogrulama
#Train veri setini belli bir oranda ayirma
#Yuzde 80'e yuzde 20 olarak ayirdik

891*0.8

train.1 = train[1:710,]
test.1 = train[711:891,]

#  randomForest (Rastgele orman)
## randomForest algoritmasýnýn uygulanmasý

install.packages("randomForest")
library(randomForest)

randomForest::randomForest()

?randomForest



rf = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + kabinBiliniyor, data= train.1, mtry=3, ntree = 1000)

tahminler = predict(rf, test.1[,c(3,5,6,7,8,10,14)])
tahminler

table(tahminler, test.1$Survived)
sum(test.1$Survived == 1)
sum(test.1$Survived == 0)

# Dogru bilinen degerler / test verisindeki tum veriler
(101+54) / 181

## test verisinde hayatta kalanlarýn tahmin edilmesi

tahminler.1 = predict(rf, test[, c(3,5,6,7,8,10,14)])

length(tahminler.1)

sonuc = test$PassengerId
sonuc = as.data.frame(sonuc)
colnames(sonuc) = c("PassengerId")
sonuc$Survived = tahminler.1

getwd()
setwd("/Users/lenovo/Documents")
?write.csv

write.csv(sonuc, "sonuc.csv", row.names = F)

####################################