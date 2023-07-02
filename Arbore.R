#In cadrul acestui fisier va fi realizata metoda Arbore
library(rpart)
library(rpart.plot)
library(tidyverse)
library(ISLR)
library(rsample)
library(caret)
library(partykit)

#------------Pregatirea datelor
airline_data <- read.csv("dateCompanieAeriana.csv")
airline_data <- select(airline_data, -X:-id)
airline_data <- na.omit(airline_data) 


airline_data <- airline_data %>%
  mutate(
    Gender = factor(Gender),
    Customer.Type = factor(Customer.Type),
    Type.of.Travel = factor(Type.of.Travel),
    Class = factor(Class),
    satisfaction = factor(satisfaction)
  )



airline_data %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")



airline_data <- airline_data %>%
  mutate( Baggage.handling = factor(Baggage.handling),
          Checkin.service = factor(Checkin.service),
          Cleanliness = factor(Cleanliness),
          Departure.Arrival.time.convenient = factor(Departure.Arrival.time.convenient),
          Ease.of.Online.booking = factor(Ease.of.Online.booking),
          Food.and.drink = factor(Food.and.drink),
          Gate.location = factor(Gate.location),
          Inflight.wifi.service = factor(Inflight.wifi.service),
          Inflight.entertainment = factor(Inflight.entertainment),
          Inflight.service = factor(Inflight.service),
          Leg.room.service = factor(Leg.room.service),
          On.board.service = factor(On.board.service),
          Online.boarding = factor(Online.boarding),
          Seat.comfort = factor(Seat.comfort)
  )



airline_data <- select(airline_data, -Arrival.Delay.in.Minutes)
airline_data <- select(airline_data, -Leg.room.service)
airline_data <- select(airline_data, -Inflight.wifi.service)
airline_data <- select(airline_data, -Gate.location)
airline_data <- select(airline_data, -Ease.of.Online.booking)
airline_data <- select(airline_data, -Inflight.entertainment)
airline_data <- select(airline_data, -On.board.service)



airline_data %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")


#------------Incepere proces de analiza-----------
data <- airline_data
str(data)


data <- data %>%
  mutate(Age_Group = case_when(
    Age <= 18 ~ "Copil",
    Age <= 35 ~ "Adult",
    Age <= 60 ~ "Matur",
    TRUE ~ "Senior"
  ))

data$Age_Group <- factor(data$Age_Group)

data <- data %>%
  mutate(Distance_Category = case_when(
    Flight.Distance <= 500 ~ "Scurt",
    Flight.Distance <= 1500 ~ "Mediu",
    Flight.Distance <= 2500 ~ "Lung",
    TRUE ~ "Foarte lung"
  ))

data$Distance_Category <- factor(data$Distance_Category)

data <- select(data, -Age, -Flight.Distance)
str(data)

set.seed(200) 


split <- initial_split(data, prop = 0.7, strata = "satisfaction")
train <- training(split)
test <- testing(split)


table(train$ satisfaction)
table(test$ satisfaction)



set.seed(200)
m1 = rpart(
  formula = satisfaction ~. ,
  data = train,
  method = "class" 
)

m1 
summary(m1)  
rpart.plot(m1) 


pred_m1 <- predict(m1, newdata = test, target ="class")

pred_m1 <- as_tibble(pred_m1) %>% 
  mutate(class = ifelse(`neutral or dissatisfied` >= satisfied, "neutral or dissatisfied", "satisfied"))
table(pred_m1$class, test$satisfaction)
confusionMatrix(factor(pred_m1$class), factor(test$satisfaction))


m2 <- rpart(satisfaction ~., 
            data = train,
            method = "class",
            control = list(cp=0))

summary(m2)
rpart.plot(m2)

pred_m2 <- predict(m2, newdata = test, target ="class")
pred_m2 <- as_tibble(pred_m2) %>% 
  mutate(class = ifelse(`neutral or dissatisfied` >= satisfied, "neutral or dissatisfied", "satisfied"))
table(pred_m2$class, test$satisfaction)
confusionMatrix(factor(pred_m2$class), factor(test$satisfaction))

m2_pruned <- prune(m2, cp = 0.02)
summary(m2_pruned)
rpart.plot(m2_pruned)
pred_m2_pruned <- predict(m2_pruned, newdata = test, target ="class")
pred_m2_pruned <- as_tibble(pred_m2_pruned) %>% 
  mutate(class = ifelse(`neutral or dissatisfied` >= satisfied, "neutral or dissatisfied", "satisfied"))
table(pred_m2_pruned$class, test$satisfaction)
confusionMatrix(factor(pred_m2_pruned$class), factor(test$satisfaction))


library(pROC)
dataset_m1 <- data.frame(
  actual.satisfaction = ifelse(test$satisfaction == "satisfied", 1, 0),
  probability = pred_m1$satisfied)

roc_m1 <- roc(actual.satisfaction ~probability, dataset_m1)
adf_m1 <- data.frame(x = roc_m1$specificities, y =  roc_m1$sensitivities)
ggplot(adf_m1, aes(x, y)) + geom_line() + scale_x_reverse()


library(tree)
set.seed(200)
m1_tree <- tree(satisfaction ~., data = train) 
m1_tree
summary(m1_tree)


pred_m1_tree <- predict(m1_tree, newdata = test, target = "class")
pred_m1_tree <- as_tibble(pred_m1_tree) %>% mutate(class = ifelse(`neutral or dissatisfied` >= satisfied, "neutral or dissatisfied", "satisfied"))
confusionMatrix(factor(pred_m1_tree$class), factor(test$satisfaction))


library(pROC)
dataset_entropy <- data.frame(
  actual.satisfaction = ifelse(test$satisfaction == "satisfied", 1, 0),
  probability = pred_m1_tree$satisfied)

roc_entropy <- roc(actual.satisfaction ~probability, dataset_entropy)
adf_entropy <- data.frame(x = roc_entropy$specificities, y =  roc_entropy$sensitivities)
ggplot(adf_entropy, aes(x, y)) + geom_line() + scale_x_reverse()


set.seed(200)
m1_tree_gini <- tree(satisfaction ~., data = train, split="gini") # works with Gini index
m1_tree_gini
summary(m1_tree_gini)

pred_m1_tree_gini <- predict(m1_tree_gini, newdata = test, target = "class")
pred_m1_tree_gini <- as_tibble(pred_m1_tree_gini) %>% mutate(class = ifelse(`neutral or dissatisfied` >= satisfied, "neutral or dissatisfied", "satisfied"))
confusionMatrix(factor(pred_m1_tree_gini$class), factor(test$satisfaction))


library(pROC)
dataset_gini <- data.frame(
  actual.satisfaction = ifelse(test$satisfaction == "satisfied", 1, 0),
  probability = pred_m1_tree_gini$satisfied)

roc_gini <- roc(actual.satisfaction ~probability, dataset_gini)
adf_gini <- data.frame(x = roc_gini$specificities, y = roc_gini$sensitivities)
ggplot(adf_gini, aes(x, y)) + geom_line() + scale_x_reverse()



ggplot() +
  geom_line(data = adf_m1, aes(x = x, y = y, color = "m1"), size = 1) +
  geom_line(data = adf_entropy, aes(x = x, y = y, color = "entropy"), size = 1) +
  geom_line(data = adf_gini, aes(x = x, y = y, color = "gini"), size = 1) +
  scale_x_reverse() +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Method") +
  scale_color_manual(values = c("m1" = "black", "entropy" = "red", "gini" = "blue")) +
  theme(legend.position = "top")

