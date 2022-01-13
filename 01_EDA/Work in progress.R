library(caret)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(batman)
library(knitr)
library(tidyr)
library(PASWR2)
library(scales)
library(nortest)
library(kableExtra) # sudo apt install libfontconfig1-dev
library(cowplot)
library(mice)
library(VIM)
library(ggcorrplot)
library(psych)
library(ipred)
library(car)
library(geojsonio)
library(gridExtra)
library(shiny)
library(GGally)
library(RColorBrewer)
library(ggExtra)
library(DMwR) # install.packages(c("zoo","xts","quantmod")), luego instala ROCR, luego install.packages( "Path/To/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
library(geosphere)


RawData <- read.csv("/Users/ekaterinazajceva/Documents/airbnb/00_DatosOriginales/listings.csv")

data <- RawData %>%
  select(id, host_id, host_since, host_is_superhost, description, neighbourhood_group_cleansed, latitude, longitude, property_type, room_type, accommodates, bedrooms, beds, price, minimum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm, availability_365, last_review, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, reviews_per_month)


data$host_since <- as.Date(data$host_since)
data$last_review <- as.Date(data$last_review)

data$host_is_superhost <- as.character(data$host_is_superhost)
data$host_is_superhost[which(data$host_is_superhost == "")] <- NA
data$host_is_superhost[which(data$host_is_superhost == "t")] <- "true"
data$host_is_superhost[which(data$host_is_superhost == "f")] <- "false"
data$host_is_superhost <- to_logical(data$host_is_superhost)

data$price <- as.character(data$price)
data$price<-parse_number(data$price)

data$description <- as.character(data$description)
data$description[which(data$description == "")] <- NA


# Eliminación de observaciones inverosímiles

data <- data[data$price>0 & data$accommodates>0,]


set.seed(12345)

# почему берут здесь сырые данные???

inTraining <- createDataPartition(pull(data, neighbourhood_group_cleansed),
                                  p = .7, list = FALSE, times = 1)


data_train <- slice(data, inTraining)
data_test <- slice(data, -inTraining)


data_train$host_exp_days <- as.integer(max(data_train$host_since, na.rm = TRUE)-data_train$host_since)
data_train$host_since <- NULL

# смотрим ковидные и доковидные времена!!! добавим пеерменную

ggplot(data_train, aes(x=last_review)) + 
  geom_histogram(binwidth = 7, col='darkslategray') + 
  theme(legend.position = "none")

state_of_alarm <- as.Date("2020-03-15")
state_of_alarm_off <- as.Date("2021-05-09")  # last day

days_since_soa <- as.integer(max(data_train$last_review, na.rm = TRUE)-state_of_alarm)

reviewedBeforeCovid <- count(na.omit(data_train[data_train$last_review<state_of_alarm,]))

# тут ошибка, нужно довабить сам день....и может быть посмотреть отмену режима
reviewedAfterCovid <- count(na.omit(data_train[data_train$last_review>=state_of_alarm,]))

# добавим переменную....может добавим окончание estado de alarma

data_train <- data_train %>%
  mutate(reviewedAlarm = case_when(last_review>=state_of_alarm & last_review <= state_of_alarm_off ~ 'On', last_review < state_of_alarm | last_review > state_of_alarm_off ~ 'Off'))

data_train$reviewedAlarm <- as.factor(data_train$reviewedAlarm)

data_train <- data_train %>%
  mutate(reviewedPrecovid = case_when(last_review < state_of_alarm ~ 'On', last_review >= state_of_alarm ~ 'Off'))
data_train$reviewedPrecovid <- as.factor(data_train$reviewedPrecovid)

data_train$last_review_days <- as.integer(max(data_train$last_review, na.rm = TRUE)-data_train$last_review)
data_train$last_review <- NULL


# дополнительные новые переменные

data_train <- data_train %>%
  add_count(host_id)
colnames(data_train)[colnames(data_train) == 'n'] <- 'host_listings_count'

data_train$price_per_person <- data_train$price/data_train$accommodates

distance <- function(origen, row){
  return(as.numeric(distm(origen, cbind(row$longitude, row$latitude),
                          fun = distHaversine))
  )
}

sol <- c(-3.7035799616333795, 40.417114256598694)

data_train$dist_sol <- distance(sol, data_train)


# Удаляем NA, работаем с выбросами






# Wanda - добавим район

dataCan <- data_train[data_train$neighbourhood_group_cleansed=="San Blas - Canillejas",]
cercaWanda <- dataCan[grep("wanda|estadio", tolower(dataCan$description)),]
data_train$neighbourhood_group_cleansed <- as.character(data_train$neighbourhood_group_cleansed)

data_train[rownames(cercaWanda), 'neighbourhood_group_cleansed'] <- "Wanda Metropolitano"
data_train$neighbourhood_group_cleansed <- as.factor(data_train$neighbourhood_group_cleansed)


summary(data_train)

data_train %>% 
  group_by(neighbourhood_group_cleansed) %>%
  count()

data_train %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  summarize(mean_price_per_person = mean(price_per_person)) %>% 
  arrange((mean_price_per_person)) %>% 
  kable()

# add у меня не сходится средняя цена по дистриктам???

data_train %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  summarize(mean_price = mean(price)) %>% 
  arrange((mean_price))

# сделаем группы по дистриктам
# Отдельная группа -  Puente de Vallecas, Villa de Vallecas, Carabanchel, Usera 29-34
# -  Moncloa - Aravaca, Centro, Moratalaz, Chamberí  40-42
# - Barajas, Fuencarral - El Pardo, Salamanca 47-47
# - Hortaleza, Tetuán,Arganzuela, Retiro, Ciudad Lineal, Chamartín - 52-56
# - Latina, Vicálvaro, Villaverde 65-73
# - San Blas - Canillejas - 92
# - Wanda Metropolitano - 173

# удалить выбросы 
data_train <- data_train[data_train$price_per_person<quantile(data_train$price_per_person, 0.955),]

data_train %>%
  ggplot(aes(x=(reorder(neighbourhood_group_cleansed, price_per_person, FUN=mean)), y=price_per_person, fill=neighbourhood_group_cleansed)) + geom_boxplot() + scale_y_continuous(trans='log') + coord_flip() + theme(legend.position="none") + xlab("Distritos") + ylab("Precio per persona")


# сделаем группы по дистриктам
# Отдельная группа -  Puente de Vallecas, Villaverde 19
# -  Latina, Usera, Moratalaz, Vicálvaro - 22-23
# - Arganzuela, Ciudad Lineal,Carabanchel,Villa de Vallecas,Barajas - 24
# - Hortaleza, Centro, Tetuán, Moncloa - Aravaca, Retiro, Fuencarral - El Pardo 28-26
# - Chamartín, Salamanca, San Blas - Canillejas, Chamberí - 30-33
# - Wanda Metropolitano - 41


# кодим группы

data_train <- data_train %>%
  mutate(district_price_group = case_when(neighbourhood_group_cleansed == 'Wanda Metropolitano' ~ 'A',
                                          neighbourhood_group_cleansed == 'Chamartín' | neighbourhood_group_cleansed == 'Salamanca' | neighbourhood_group_cleansed == 'San Blas - Canillejas' | neighbourhood_group_cleansed == 'Chamberí' ~ 'B',
                                          neighbourhood_group_cleansed == 'Hortaleza' | neighbourhood_group_cleansed == 'Centro' | neighbourhood_group_cleansed == 'Tetuán' | neighbourhood_group_cleansed == 'Moncloa - Aravaca' | neighbourhood_group_cleansed == 'Retiro' | neighbourhood_group_cleansed == 'Fuencarral - El Pardo' ~ 'C',
                                          neighbourhood_group_cleansed == 'Arganzuela' | neighbourhood_group_cleansed == 'Ciudad Lineal' | neighbourhood_group_cleansed == 'Carabanchel' | neighbourhood_group_cleansed == 'Villa de Vallecas' | neighbourhood_group_cleansed == 'Barajas' ~ 'D',
                                          neighbourhood_group_cleansed == 'Latina' | neighbourhood_group_cleansed == 'Usera' | neighbourhood_group_cleansed == 'Moratalaz' | neighbourhood_group_cleansed == 'Vicálvaro' ~ 'E', 
                                          neighbourhood_group_cleansed == 'Puente de Vallecas' | neighbourhood_group_cleansed == 'Villaverde' ~ 'F'))

data_train$district_price_group <- as.factor(data_train$district_price_group)

summary(data_train)

data_train %>% 
  group_by(district_price_group) %>% 
  summarize(mean_price_per_person = mean(price_per_person)) %>% 
  arrange((mean_price_per_person))

data_train %>% 
  group_by(reviewedAlarm) %>% 
  summarize(mean_price = mean(price)) %>% 
  arrange((mean_price))


```{r last_review_days}
data_train$last_review_days <- as.integer(max(data_train$last_review, na.rm = TRUE)-data_train$last_review)
data_train$last_review <- NULL
```

Con base en los valores calculados para la variable *last_review_days*, calculamos los valores para la variable *reviewedAlarm*.

```{r}
data_train_del_na <- data_train_del_na %>%
  mutate(reviewedAlarm = case_when(last_review_days>=97 & last_review_days <= 545 ~ 'On', last_review_days < 97 | last_review_days > 545 ~ 'Off'))
data_train_del_na$reviewedAlarm <- as.factor(data_train_del_na$reviewedAlarm)
```

:district_price_group host_listings_count

fit_m3 <- lm(log(price) ~ log(accommodates):district_price_group + log(dist_sol):district_price_group+log(reviews_per_month) + host_listings_count, data = data_train_sel) ### 0.6253
summary(gvlma(fit_m3))

fit_m3 <- lm(log(price) ~ accommodates: district_price_group + dist_sol +reviews_per_month: district_price_group + host_listings_count, data = data_train_sel) ### 0.6253
summary(gvlma(fit_m3))

# если удалить решение по дистриктам, то тоже работает


```{r model3, echo=FALSE}
fit_m3 <- lm(log(price) ~ accommodates:district_price_group+ dist_sol +reviews_per_month:district_price_group + host_listings_count+review_scores_rating, data = data_train_sel) ### 0.6253
summary(gvlma(fit_m3))
```
