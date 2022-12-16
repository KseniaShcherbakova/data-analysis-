#install.packages("gplots")
library(gplots) #библиотека устанавлевается с помощью install.packages

#Дисперсионный анализ. Пример

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
setwd('C:/Users/Днс/Documents/Arithmetics')
data = read.csv("diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#data/Diet_data_description.docx
#data/diet.csv
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])

data$gender <- as.factor(data$gender)
par(mfrow=c(1,2)) 
data$gender <- factor(c("male", "female")[data$gender])
summary(data)
#Диеты привелись к нормальному типу

#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
summary(data)

#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#Добавляем проверку на зависисмость потери веса от пола 
boxplot(weight.loss~gender,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#проверим сбалансированные ли данные для диеты
table(data$diet.type)

#проверим сбалансированные ли данные для гендера
table(data$gender)

#График групповых средних для диеты
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

#График групповых средних для гендера
plotmeans(weight.loss ~ gender, data=data)
aggregate(data$weight.loss, by = list(data$gender), FUN=sd)

#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm

#тест на межгрупповые различия для диеты
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)
#тест на межгрупповые различия для гендера
fit <- aov(weight.loss ~ gender, data=data)
summary(fit)


#попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test)
install.packages("multcomp")
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
