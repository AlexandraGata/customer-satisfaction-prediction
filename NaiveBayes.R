#In cadrul acestui fisier va fi realizata metoda Naive Bayes
library(modeldata)
library(tidyverse)
library(ggplot2)
library(caret)
library(corrplot)
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


set.seed(200) 


split <- initial_split(airline_data, prop = 0.7, strata = "satisfaction")
train <- training(split)
test <- testing(split)


table(train$ satisfaction)
table(test$ satisfaction)


features <- setdiff(names(train), "satisfaction")
x <- train[,features]
y <- train$satisfaction


train_control <- trainControl(
  method = "cv",
  number = 5 )


mod_nb1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control )

mod_nb1

confusionMatrix(mod_nb1)


search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0.5,
  adjust = seq(0, 5, by = 1) )


mod_nb2 = train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
)

mod_nb2
confusionMatrix(mod_nb2)


mod_nb2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))


pred <- predict(mod_nb2, test)
pred 


predProb <- predict(mod_nb2, test, type="prob") 
predProb


test$satisfaction <- factor(test$satisfaction, levels = c("neutral or dissatisfied", "satisfied"))


confusionMatrix(pred, test$satisfaction)



library(pROC)


dataset <- data.frame(
  actual.satisfaction <- test$satisfaction,
  probability <- predProb[,1]
)


roc.val <- roc(actual.satisfaction ~probability, dataset)

adf_nb <- data.frame(x <- roc.val$specificities, y <- roc.val$sensitivities)
colnames(adf_nb) <- c("specificity", "sensitivity")

ggplot(adf_nb, aes(specificity, sensitivity)) + geom_line() + scale_x_reverse()




