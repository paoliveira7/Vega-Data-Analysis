library(mlbench) 
library(caret) 

# Importar a base de dados do UCL

data("USArrests")
head(USArrests)

names(USArrests) 

# Verificar se h� NA na base de dados 
sum(is.na(USArrests))

# Para atingir um modelo reprodut�vel.
set.seed(100)

#Executar a divis�o aleat�rio da base de dados

trainingindex <- createDataPartition(USArrests$Murder, p = 0.6, list = FALSE)
trainingset <- USArrests[trainingindex,]
testset <- USArrests[-trainingindex,]

# Construindo modelo de treinamento

model <- train(Murder ~ ., data = trainingset,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none")
)


#Aplicar modelo para a predi��o na duas bases
model.training <- predict(model, trainingset)
model.test <- predict(model, testset)


# Plotar gr�ficos 

plot(trainingset$Murder, col = "Blue", xlab = "Indicadores de Viol�ncia ", ylab = "Assasinatos")
plot(testset$Murder, col= "red", xlab = "Indicadores de Viol�ncia ", ylab = "Assasinatos")


# Verificar perfomance do modelo

summary(model)

#Calculando a correla��o de pearson
rtraining <- cor(trainingset, model.training)
rtest <- cor(testset, model.test)
print(rtraining)
print(rtest)


R2.training <- rtraining^2
R2.test <- rtest^2

print(R2.training)
print(R2.test)






