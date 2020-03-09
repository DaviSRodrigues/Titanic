#setting the environment
setwd('D:/OneDrive/Jupyter Notebooks/Estudos/Titanic')

#loading data
dataset <- read.csv(file = 'train.csv')

head(dataset, n = 5)
tail(dataset, n = 5)
dim(dataset)

#selecting only the passengers that survived
sobreviventes <- dataset[dataset$Survived == 1,]

head(sobreviventes)

cat('Passengers that survived: ', nrow(sobreviventes))

sobrevNA <- sobreviventes[complete.cases(sobreviventes$Age), ]

aggregate(sobrevNA$Age, by = list(sobrevNA$Sex), FUN = mean)

first_survivors <- sobreviventes[sobreviventes$Pclass == 1, ]

firstNA <- first_survivors[complete.cases(first_survivors$Age), ]

aggregate(firstNA$Age, by = list(firstNA$Sex), FUN = mean)

female_survivors <- sobreviventes[sobreviventes$Sex == 'female' & complete.cases(sobreviventes$Age), ]

aggregate(female_survivors$Age, by = list(female_survivors$Sex), FUN = mean)

# Teste 5
# Naive Bayes
# 
# Utilizando todos os atributos, exceto ID e Nome:
#   
# • sexo (convertido para números: 1 - feminino e 2 - masculino)
# 
# • classe (1ª, 2ª, 3ª)
# 
# • cônjuges, irmãos (SibSp)
# 
# • pais, filhos (Parch)

library(data.table)

dados <- copy(dataset)

dados$Sex <- sapply(dados[, 'Sex'], function(x) if(x == 'female') 1 else 2)

# moda <- function(x) {
#   ux <- unique(x)
#   tab <- tabulate(match(x, ux))
#   ux[tab == max(tab)]
# }
# 
# moda_classe = c()
# 
# #age mode by Pclass
# for (i in 1:3)
#   moda_classe[i] <- moda(dataset[dataset$Pclass == i & complete.cases(dataset$Age), 'Age'])

colunas <- c('Sex', 'SibSp', 'Parch', 'Pclass', 'Survived')

#####################

library(e1071)

model <- naiveBayes(as.factor(Survived)~., data = dados[1:500, colunas])

print(model)

#Prediction on the dataset
predictions <- predict(model, dados[501:714, colunas])

#Confusion matrix to check accuracy
table(predictions, dados[501:714, 'Survived'])

#####################

library(caret)

train_control <- trainControl(method = 'cv', number = 5)
model <- train(as.factor(Survived)~., data = dados[, colunas], trControl = train_control, method = "nb")

print(model)

predictions <- predict(model, dados[, colunas])

table(predictions, dados[, 'Survived'])

#####################

testData <- read.csv('test.csv')

testData$Sex <- sapply(testData[, 'Sex'], function(x) if(x == 'female') 1 else 2)

predictions <- predict(model, testData)

testData$Survived <- sapply(predictions, function(x) as.numeric(x) - 1)

write.csv(testData[, c('PassengerId', 'Survived')], 'testeR.csv', row.names = FALSE, quote = FALSE)

# Your Best Entry 
# You advanced 1,330 places on the leaderboard!
# Your submission scored 0.77033, which is an improvement of your previous score of 0.76555. Great job!