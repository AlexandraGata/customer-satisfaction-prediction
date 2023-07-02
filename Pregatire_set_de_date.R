#In acest fisier sunt comenzile utilizate in curatarea datelor
#Deoarece atributele erau numeroase si corelarea acestora era mare, am decis sa eliminam o parte din ele
#Procesul de pregatire al datelor este detaliat mai jos
#Datele sunt pregatite si in fiecare fisier cu metode, inainte de realizarea efectiva a metodei
library(tidyverse)
library(ggplot2)



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


airline_data %>%
  filter(satisfaction == "satisfied") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

airline_data %>%
  filter(satisfaction == "neutral or dissatisfied") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()


airline_data <- select(airline_data, -Arrival.Delay.in.Minutes)
airline_data <- select(airline_data, -Leg.room.service)
airline_data <- select(airline_data, -Inflight.wifi.service)
airline_data <- select(airline_data, -Gate.location)
airline_data <- select(airline_data, -Ease.of.Online.booking)
airline_data <- select(airline_data, -Inflight.entertainment)
airline_data <- select(airline_data, -On.board.service)



airline_data %>%
  filter(satisfaction == "satisfied") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

airline_data %>%
  filter(satisfaction == "neutral or dissatisfied") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()




airline_data %>%
  filter(satisfaction == "satisfied") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

airline_data %>%
  filter(satisfaction == "neutral or dissatisfied") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()



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
          Food.and.drink = factor(Food.and.drink),
          Inflight.service = factor(Inflight.service),
          Online.boarding = factor(Online.boarding),
          Seat.comfort = factor(Seat.comfort)
  )



airline_data %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")


