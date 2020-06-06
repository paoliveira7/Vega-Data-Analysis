
library(tidyverse)

# Muda o diretorio de trabalho

# Importando arquivos do tipo CSV - funcao read.table

bank_data <- read.table("bank-full.csv", 
                 header = TRUE, sep = ";")

# Conhecendo a base de dados
head(bank_data)

dim(bank_data)

str(bank_data)

bank_data %>% count(y)

round(prop.table(table(bank_data$job,bank_data$y, dnn = c("Job","Target")),1)*100,2)
round(prop.table(table(bank_data$marital,bank_data$y, dnn = c("Marital","Target")),1)*100,2)
round(prop.table(table(bank_data$education,bank_data$y, dnn = c("Education","Target")),1)*100,2)
round(prop.table(table(bank_data$default,bank_data$y, dnn = c("Default","Target")),1)*100,2)
round(prop.table(table(bank_data$housing,bank_data$y, dnn = c("Housing","Target")),1)*100,2)
round(prop.table(table(bank_data$loan,bank_data$y, dnn = c("Loan","Target")),1)*100,2)
round(prop.table(table(bank_data$contact,bank_data$y, dnn = c("Contact","Target")),1)*100,2)
round(prop.table(table(bank_data$month,bank_data$y, dnn = c("Month","Target")),1)*100,2)
round(prop.table(table(bank_data$poutcome ,bank_data$y, dnn = c("Poutcome","Target")),1)*100,2)

bp_age <- ggplot(bank_data, aes(x = y, y = age)) +
        geom_boxplot(fill = "#228B22", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "Age") +
        scale_x_discrete(name = "Target") +
        ggtitle("Age") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))

bp_balance <- ggplot(bank_data, aes(x = y, y = balance )) +
        geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "Balance") +
        scale_x_discrete(name = "Target") +
        ggtitle("Balance") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))

bp_day <- ggplot(bank_data, aes(x = y, y = day )) +
        geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "Day") +
        scale_x_discrete(name = "Target") +
        ggtitle("Day") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))

bp_duration <- ggplot(bank_data, aes(x = y, y = duration )) +
        geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "Duration") +
        scale_x_discrete(name = "Target") +
        ggtitle("Duration") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))

bp_campaign <- ggplot(bank_data, aes(x = y, y = campaign )) +
        geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "Campaign") +
        scale_x_discrete(name = "Target") +
        ggtitle("Campaign") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))

bp_pdays <- ggplot(bank_data, aes(x = y, y = pdays )) +
        geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "pdays") +
        scale_x_discrete(name = "Target") +
        ggtitle("Pdays") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))

bp_previous <- ggplot(bank_data, aes(x = y, y = previous )) +
        geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.6) +
        scale_y_continuous(name = "Previous") +
        scale_x_discrete(name = "Target") +
        ggtitle("Previous") +
        theme_gray() + 
        theme(plot.title = element_text(hjust = 0.5))


#install.packages("gridExtra")
#install.packages("ggpubr")
library(ggpubr)
library(gridExtra)

grid.arrange(bp_age, bp_balance, bp_day, bp_duration
             ,bp_campaign, bp_pdays, bp_previous
             , nrow = 2, ncol = 4)

str(bank_data)

job <- bank_data %>% count(job, y)

marital <- bank_data %>% count(marital, y)

education <- bank_data %>% count(education, y)

default <- bank_data %>% count(default, y)

housing <- bank_data %>% count(housing, y)

loan <- bank_data %>% count(loan, y)

contact <- bank_data %>% count(contact, y)

month <- bank_data %>% count(month, y)

poutcome <- bank_data %>% count(poutcome, y)

bp_job <- ggplot(job, aes(x = job, y = n )) +
        geom_bar(aes(fill = job), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "Job") +
        ggtitle("Job") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_blank(), axis.ticks = element_blank())

bp_job

ggballoonplot(job, x = "job", y = "n", size = "n",
              fill = "n", facet.by = "y",
              ggtheme = theme_gray()) +
  scale_fill_viridis_c(option = "C")

bp_marital <- ggplot(marital, aes(x = marital, y = n )) +
        geom_bar(aes(fill = marital), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "Marital") +
        ggtitle("Marital") +
        facet_wrap(~y) + 
        theme_gray() +
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_education <- ggplot(education, aes(x = education, y = n )) +
        geom_bar(aes(fill = education), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "education") +
        ggtitle("Education") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_default <- ggplot(default, aes(x = default, y = n )) +
        geom_bar(aes(fill = default), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "default") +
        ggtitle("Default") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_housing <- ggplot(housing, aes(x = housing, y = n )) +
        geom_bar(aes(fill = housing), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "housing") +
        ggtitle("Housing") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_loan <- ggplot(loan, aes(x = loan, y = n )) +
        geom_bar(aes(fill = loan), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "loan") +
        ggtitle("Loan") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_contact <- ggplot(contact, aes(x = contact, y = n )) +
        geom_bar(aes(fill = contact), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "contact") +
        ggtitle("Contact") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_month <- ggplot(month, aes(x = month, y = n )) +
        geom_bar(aes(fill = month), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "month") +
        ggtitle("month") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

bp_poutcome <- ggplot(poutcome, aes(x = poutcome, y = n )) +
        geom_bar(aes(fill = poutcome), stat = "identity", color = "white") +
        scale_y_discrete(name = "Frequencia") +
        scale_x_discrete(name = "poutcome") +
        ggtitle("Poutcome") +
        facet_wrap(~y) + 
        theme_gray() + 
        theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5))

grid.arrange(bp_marital, bp_education, bp_default, bp_housing
             , nrow = 2, ncol = 2)

grid.arrange(bp_loan, bp_contact, bp_month, bp_poutcome
             , nrow = 2, ncol = 2)

library(caret)

set.seed(42)

inTrain1 <- createDataPartition(bank_data$y, p = 0.7, list = FALSE)

train1 <- bank_data[inTrain1, ]

test1 <- bank_data[-inTrain1, ]

dim(train1)
dim(test1)

library(e1071)

?naiveBayes

# Treinamento do modelo 1

nb1 <- naiveBayes(y ~ ., data = train1 , laplace = 1)

print(nb1)

# Previsao na base de treinamento

nb_train_pred1 <- predict(nb1, train1, type = "class")

confusionMatrix(nb_train_pred1, train1$y, positive = 'yes')

# Previsao na base de teste

nb_test_pred1 <- predict(nb1, test1, type = "class")

confusionMatrix(nb_test_pred1, test1$y, positive = 'yes')

bank_oversample <- upSample(bank_data, bank_data$y)

str(bank_oversample)

bank_oversample %>% count(Class)

set.seed(42)

inTrain2 <- createDataPartition(bank_oversample$Class, p = 0.7, list = FALSE)

train2 <- bank_oversample[inTrain2, ]

test2 <- bank_oversample[-inTrain2, ]

dim(train2)
dim(test2)

# Treinamento do modelo 2

nb2 <- naiveBayes(Class ~ .
                  , data = train2)

# Previsao na base de treinamento 2

nb_train_pred2 <- predict(nb2, train2, type = "class")

confusionMatrix(nb_train_pred2, train2$Class, positive = 'yes')

# Previsao na base de teste 2
 
nb_test_pred2 <- predict(nb2, test2, type = "class")

confusionMatrix(nb_test_pred2, test2$Class, positive = 'yes')

# Previsao na base de teste 1 - dados sem oversampling
 
nb_test_pred_no_over <- predict(nb2, test1, type = "class")

confusionMatrix(nb_test_pred_no_over, test1$y, positive = 'yes')

confusionMatrix(nb_test_pred1, test1$y, positive = 'yes')
