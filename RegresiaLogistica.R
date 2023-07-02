#In cadrul acestui fisier va fi realizata metoda Regresie Logistica
library(caret)
library(tidyverse)
library(modelr)
library(ggplot2)
library(rsample)

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

mod_all <- glm(data = airline_data, satisfaction ~ ., family = binomial)

summary(mod_all) 

mod_all2 <- glm(data = airline_data, satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + Class + Flight.Distance + Departure.Arrival.time.convenient + Food.and.drink + Online.boarding + Baggage.handling + Checkin.service + Departure.Delay.in.Minutes, family = binomial)

summary(mod_all2)




set.seed(200) 
split <- initial_split(airline_data, prop = 0.7, strata = "satisfaction")
train <- training(split)
test <- testing(split)


table(train$ satisfaction)
table(test$ satisfaction)


mod_all_train <- glm(data = train, satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + Class + Departure.Arrival.time.convenient + Food.and.drink + Online.boarding + Baggage.handling + Departure.Delay.in.Minutes, family = binomial)

summary(mod_all_train)


pred_test <- predict(mod_all_train, newdata = test, type = "response")
table(pred_test > 0.5, test$satisfaction) #0.5 valoarea optima (0.4 scade true negatives, 0.6 scade mult true positives)





set.seed(200) 


split <- initial_split(airline_data, prop = 0.7, strata = "satisfaction")
train <- training(split)
test <- testing(split)



features <- setdiff(names(train), "satisfaction")
x <- train[,features]
y <- train$satisfaction


train_control <- trainControl(
  method = "cv",
  number = 5 )


mod_glmALL <- train(
  x = x,
  y = y,
  method = "glm",
  family = "binomial",
  trControl = train_control )


mod_glmALL

confusionMatrix(mod_glmALL)


pred_all = predict(mod_glmALL, newdata = test, type = "raw")


test$satisfaction <- factor(test$satisfaction, levels = c("neutral or dissatisfied", "satisfied"))


confusionMatrix(pred_all, test$satisfaction)

summary(mod_glmALL)




xNoATR <- x %>% select(-Seat.comfort, -Inflight.service, -Cleanliness, -Checkin.service, -Flight.Distance)


train_control <- trainControl(
  method = "cv",
  number = 5 )

mod_glm_NoATR <- train(
  x = xNoATR,
  y = y,
  method = "glm",
  family = "binomial",
  trControl = train_control )


mod_glm_NoATR

confusionMatrix(mod_glm_NoATR)

pred_NoATR = predict(mod_glm_NoATR, newdata = test, type = "raw")

confusionMatrix(pred_NoATR, test$satisfaction) #mai slab 



predProb <- predict(mod_glmALL, test, type="prob")

library(pROC)

dataset <- data.frame(
  actual.satisfaction <- test$satisfaction,
  probability <- predProb[,1]
)


roc.val <- roc(actual.satisfaction ~probability, dataset)

adf_rl <- data.frame(x <- roc.val$specificities, y <- roc.val$sensitivities)
colnames(adf_rl) <- c("specificity", "sensitivity")

ggplot(adf_rl, aes(specificity, sensitivity)) + geom_line() + scale_x_reverse()

