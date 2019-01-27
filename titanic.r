# study the titanic training set 
train = read.csv(file = "train.csv", sep=",", header = TRUE)
median(train$Age, na.rm= TRUE)
summary(train)
#need to change column to factor
train$Survived = factor(train$Survived)
train$Pclass = factor(train$Pclass)

##############analyse the count variable va visualized with pie chart
count = table (train$Survived)
count
# visualized pie chart
labels=c("Survived","Not Survived")
pct = round(count/sum(count)*100)
labels = paste(labels, pct)
labels = paste (labels,"%",sep = "") 
pie(count,labels, main = "pie charts of Survivers")
png(file = "titanic Survivers chart.png")
dev.off()   

###########analyse the Pclass and visualized bar chart (Survived + Pclass)
countPclass = table(train$Pclass)
countPclass
lables = c("Class 1", "Class 2", "Class 3")
pct = round(countPclass/sum(countPclass)*100,1)
pie(countPclass, lables = paste(labels,pct), main = "pie chart of Pclass", col = rainbow(length(countPclass)))
legend("topright", lables, cex =0.8, fill = rainbow(length(countPclass)))
#analyse the PClass with Survivers
countPclasswSurvivers = table (train$Survived, train$Pclass)
countPclasswSurvivers
#bar chart
axis_y_length =  (ceiling(max(countPclasswSurvivers)/100) +1)*100
bar1 = barplot(countPclasswSurvivers, names.arg = c("Class 1", "Class 2", "Class 3"), beside = TRUE, 
        legend.text = c("Survived","Not Survived"),
        main = "Barchart of Survivers and Pclass",
        ylim = c(0,axis_y_length),
        yaxp = c(0,axis_y_length,axis_y_length/50), ylab = "number of person")
text(bar1, countPclasswSurvivers + 20* sign(countPclasswSurvivers), labels = countPclasswSurvivers, xpd= TRUE)


#########analyse the Survivers and its sex and visualized bar chart
counts = table (train$Survived, train$Sex)
counts
axis_y_length2 =  (ceiling(max(counts)/100) +1)*100
axis_y_length2
bar2 = barplot(counts, names.arg = c("Female","Male"), beside = TRUE, 
        legend.text = c("Survived","Not Survived"),
        main = "Barchart of Survivers and Sex",
        ylim = c(0,axis_y_length2),
        yaxp = c(0,axis_y_length2,axis_y_length2/50), ylab = "number of person")
text(bar2, labels= counts, counts + 20*sign(counts), xpd = TRUE)



############# analyse the name and visualized survived and name
##1. define Extract title 2. Use function to define function title 3. barplot title + survivers
extractTitle = function(sex, name){
  name = as.character(name)
  if (sex == "male"){
    if (length(grep("Mr.", name)>0)){
      return ("Mr")
    }else return ("Other_M")
  }else if (sex == "female"){
    if(length(grep("Mrs.", name)>0)){
    return ("Mrs")
  }else if (length(grep("Miss.", name)>0)){
    return ("Miss")
  }else return("Other_F")
  }
}

Titles = NULL
for( i in 1:nrow(train)) {
  Titles = c(Titles,extractTitle(train [i,"Sex"],train[i,"Name"]))
}
train$Titles = as.factor(Titles)
summary(train$Titles)

countTitle = table (train$Survived, train$Titles)
countTitle
View(countTitle)
axis_y_length2 =  (ceiling(max(countTitle)/100) +1)*100
axis_y_length2
bar3 = barplot(countTitle, names.arg = c("Miss","Mr","Mrs","Other_F","Other_M"), beside = TRUE, 
        legend.text = c("Survived","Not Survived"),
        main = "Barchart of Survivers and Titles",
        ylim = c(0,axis_y_length2),
        yaxp = c(0,axis_y_length2,axis_y_length2/50), ylab = "number of person")
text(bar3, countTitle + 20*sign(countTitle), labels= countTitle, xpd=TRUE) 




######## Analyse the age and visualized survived + age
extractAge = function(age){
  if (is.na(age)){
    age2 = 30
  }
  else age2 = age
  if (age2 <= 15) return ("Child")
  if (age2 <= 30) return ("Young")
  if (age2 <= 60) return ("Adult")
  else return ("Old")
}
AgeGroup = NULL
for (i in 1:nrow(train)){
  AgeGroup = c(AgeGroup, extractAge(train[i,"Age"]))
}
train$AgeGroup = as.factor(AgeGroup)

summary (train$AgeGroup)
countAge = table (train$Survived, train$AgeGroup)
countAge
axis_y_length2 =  (ceiling(max(countAge)/100) +1)*100
axis_y_length2
bar4 = barplot(countAge, names.arg = c("Adult","Child","Old","Young"), beside = TRUE, 
        legend.text = c("Survived","Not Survived"),
        main = "Barchart of Survivers and Age",
        ylim = c(0,axis_y_length2),
        yaxp = c(0,axis_y_length2,axis_y_length2/50), ylab = "number of person")
text (bar4, countAge + 20*sign(countAge), labels= countAge, xpd = TRUE)


##########ANALYSE FAMILY################
train$Family = train$SibSp + train$Parch
str(train$Family)
extractFamily =  function(sib, parch){
  if (sib + parch == 0) return ("Single")
  else if (sib + parch >= 3 ) return("Big Family")
  else return ("Family")
}
Family = NULL

for (i in 1:nrow(train)){
  Family = c(Family, extractFamily(train[i,"SibSp"], train[i,"Parch"]))
}
train$Family = as.factor(train$Family)

########### ANALYSE FARE ###################
summary(train$Fare)
train$FareGroup = cut(train$Fare, breaks = c(-1,8,15,31,75,600),
                      labels = c("Very Cheap", "Cheap", "Median", "Rich", "Very Rich"))

countFare = table (train$Survived, train$FareGroup)
countFare
axis_y_length2 =  (ceiling(max(countFare)/100) +1)*100
axis_y_length2
bar4 = barplot(countFare, names.arg = c("Very Cheap", "Cheap", "Median", "Rich", "Very Rich"), beside = TRUE, 
               legend.text = c("Survived","Not Survived"),
               main = "Barchart of Survivers and Fare",
               ylim = c(0,axis_y_length2),
               yaxp = c(0,axis_y_length2,axis_y_length2/50), ylab = "number of person")
text (bar4, countFare + 20*sign(countFare), labels= countFare, xpd = TRUE)


#########ANALYSE CABIN#############
train$Cabin2 = substr(train$Cabin, 1,1)

for( i in 1:nrow(train)){
  if(train[i,"Cabin2"] == "") train [i,"Cabin2"] = "X"
}
train$Cabin2 = as.factor(train$Cabin2)
countCabin2 = table (train$Pclass, train$Cabin2)
countCabin2
axis_y_length2 =  (ceiling(max(countCabin2)/100) +1)*100
axis_y_length2
bar6 = barplot(countCabin2, names.arg = c("A", "B", "C", "D", "E", "F","G","T","X"), beside = TRUE, 
               legend.text = c("Class 1","Class 2","Class 3"),
               main = "Barchart of Pclass and Cabin",
               ylim = c(0,axis_y_length2),
               yaxp = c(0,axis_y_length2,axis_y_length2/50), ylab = "number of person")
text (bar6, countCabin2 + 20*sign(countCabin2), labels= countCabin2, xpd = TRUE)


#####APPLY RANDOM FOREST TO THE DATA SET###############
#1. split train into 2 subset train and set (80-20)
#2. Formula
#4. Random Forest
#5. Test

ind = sample(2,nrow(train), replace = T, prob = c(0.8,0.2))
data.train = train[ind ==1,]
data.test = train[ind == 2,]

library(randomForest)
Formula = Survived ~ Pclass+Sex+Titles+Family+FareGroup+Cabin2+AgeGroup
rf = randomForest(Formula, data = data.train)
print(rf)

pred = predict(rf, newdata= data.test)
table(pred, data.test$Survived)
rf$confusion
