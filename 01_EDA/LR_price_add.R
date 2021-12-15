library(tidyr)
library(ggplot2)
library(PASWR2)
library(scales)
library(nortest)
library(cowplot)
library(dplyr)
library(stringr)
library(caret)
library(batman)
library(readr)
library(mice)
library(VIM)
library(corrplot)
library(psych)
library(ipred)
library(DMwR)
library(car)
library(ggfortify)
library("stringi")
library(gvlma)
library(nortest)
library(nortest)
library(leaps)
library('glmnet')
library(MASS)


data_train <- read_csv("data_train_clean.csv")
data_train$description <- NULL
data_train$host_id <- NULL


# transformamos property_type
levels(as.factor(data_train$property_type))

data_train$property_type[grep("Shared room", data_train$property_type)] <- "Shared room"
data_train$property_type[grep("Private room", data_train$property_type)] <- "Private room"
data_train$property_type[grep("Room", data_train$property_type)] <- "Private room"
data_train$property_type[grep("Entire", data_train$property_type)] <- "Entire place"
data_train$property_type[grep("house", data_train$property_type)] <- "Entire place"
data_train$property_type[grep("Casa", data_train$property_type)] <- "Entire place"
data_train$property_type[grep("Hut", data_train$property_type)] <- "Other"
data_train$property_type[grep("Cave", data_train$property_type)] <- "Other"
data_train$property_type[grep("Camper/RV", data_train$property_type)] <- "Other"
data_train$property_type[grep("Floor", data_train$property_type)] <- "Other"

# en resultado tenemos 4 grupos
levels(as.factor(data_train$property_type))

data_train %>%
  group_by(property_type) %>%
  count()

data_train %>%
  group_by(room_type) %>%
  count()

# en resultado salen datos muy precidos a room_type, pues vamos eleminar property_type
data_train$property_type <- NULL

# transformamos datos room_tupe para mejorar usado de dummy de estos variables

levels(as.factor(data_train$room_type))
data_train$room_type[grep("Hotel room", data_train$room_type)] <- "Hotel"
data_train$room_type[grep("Private room", data_train$room_type)] <- "Private"
data_train$room_type[grep("Shared room", data_train$room_type)] <- "Shared"
data_train$neighbourhood_group_cleansed[grep("Puente de Vallecas", data_train$neighbourhood_group_cleansed)] <- "Puente_de_Vallecas"


levels(as.factor(data_train$room_type))

# probamos regresion lineal con todos variables que tenemos

fit_1 <- lm(price ~. -price_per_person, data = data_train)
summary(fit_1)

# resultado no aceptable, como no hay dependencia lineal, se ve bien en plot

plot(x= data_train$accommodates, y = data_train$price)

# los vemos correlaciones
pairs(~., data_train[sapply(data_train,is.numeric)])

cor.test(~ price + accommodates, data_train) # +
cor.test(~ price + bedrooms, data_train) # +
cor.test(~ price + review_scores_rating, data_train) # + (-) - no aceptable
cor.test(~ price + minimum_nights, data_train) # + (-)
cor.test(~ price + availability_365, data_train) # + (min)
cor.test(~ price + reviews_per_month, data_train) # + (-) - no aceptable
cor.test(~ price + host_exp_days, data_train) # - ?
cor.test(~ price + last_review_days, data_train) # + (-)
cor.test(~ price + host_listings_count, data_train) # + (min)
cor.test(~ price + latitude, data_train)  # + 
cor.test(~ price + longitude, data_train) # -

# problema de multicoliniarity

vif(fit_1)

# hay que eleminar longitude y -neighbourhood_group_cleansed como tiene VIF > 10.

# distribucion de variable price tampoco normal
# hacemos transformacion Box-Cox

BoxCoxTrans(data_train$price)
# en Resumen mejor transformacion para variable es log(), como Lambda = 0

ggplot(data=data_train, aes(x=log(price))) + 
  geom_histogram(binwidth=0.4,fill="blue",alpha = 0.9) +theme_minimal()

# ahora distribucion se ve mas normal

ggplot(data_train, aes(accommodates, log(price))) + 
  geom_point() + 
  geom_smooth(col = 'blue', method = lm)

ggplot(data_train, aes(accommodates, log(price))) + 
  geom_point() + 
  geom_smooth(aes(color=room_type), method = lm)


# hay que eleminar longitude y -neighbourhood_group_cleansed como tiene VIF > 10.
fit_2 <- lm(price ~. -price_per_person -longitude, data = data_train)
summary(fit_2)

vif(fit_2)

# eleminamos variables que no estan significante

fit_3 <- lm(log(price) ~. -price_per_person -longitude -neighbourhood_group_cleansed -host_is_superhost -host_listings_count -bedrooms -last_review_days -review_scores_rating, data = data_train)
summary(fit_3) # Adjusted R-squared:  0.5629
anova(fit_3)

plot(fit_3$residuals)

# testos
ad.test(fit_3$residuals) # normalidad
durbinWatsonTest((fit_3)) # autocorrelacion
ncvTest((fit_3))  # gomoskedacity
spreadLevelPlot(fit_3)

# si vale de acuerdo de reglas
summary(gvlma(fit_3))
autoplot(fit_3)

# no lo vale....

# Autoelecion de variables

regfit_full <- leaps::regsubsets(log(price) ~ accommodates*room_type + neighbourhood_group_cleansed+minimum_nights + latitude + last_review_days+host_is_superhost+host_exp_days+review_scores_rating, data_train)

reg_sum <- summary(regfit_full)
reg_sum$adjr2
reg_sum$bic
reg_sum$cp

for (metric in c("r2", "adjr2", "Cp", "bic")){plot(regfit_full, scale=metric)}
coef(regfit_full, 8) # mejor 8 opcion


regfit_bwd <- leaps::regsubsets(log(price) ~ accommodates*room_type + neighbourhood_group_cleansed+minimum_nights + latitude + last_review_days+host_is_superhost+host_exp_days+review_scores_rating, data_train, method="backward")

reg_sum_bwd <-summary(regfit_bwd)
for (metric in c("r2", "adjr2", "Cp", "bic")){plot(regfit_fwd, scale=metric)}
reg_sum_bwd$bic
reg_sum_bwd$adjr2
reg_sum_bwd$cp
coef(regfit_bwd, 8) # mejor 8 opcion



regfit_fwd <- leaps::regsubsets(log(price) ~ accommodates*room_type + neighbourhood_group_cleansed+minimum_nights + latitude + last_review_days+host_is_superhost+host_exp_days+review_scores_rating, data_train, method="forward")

reg_sum_fwd <-summary(regfit_fwd)
for (metric in c("r2", "adjr2", "Cp", "bic")){plot(regfit_fwd, scale=metric)}
reg_sum_fwd$bic
reg_sum_fwd$adjr2
reg_sum_fwd$cp
coef(regfit_fwd, 8)  # mejor 8 opcion


# Lasso filter

x <- model.matrix(price~ accommodates*room_type + neighbourhood_group_cleansed+minimum_nights + latitude + last_review_days+host_is_superhost+host_exp_days+review_scores_rating, data_train)[,-1]
y <- data_train$price

grid <- 10^seq(10,-2,length=100)

lasso_reg <- glmnet(x, y, alpha = 1, lambda=grid)
plot(lasso_reg)

set.seed(1234)
cv_result <- cv.glmnet(x,y,alpha=1)
plot(cv_result)

best_lam <- cv_result$lambda.min
out <- glmnet(x,y,alpha=1,lambda = best_lam)
lasso_coef <- predict(out,type="coefficients",s=best_lam)[1:20,]
lasso_coef[lasso_coef!=0]

first <- max(which(cv_result$nzero == 8))
my_lam <- cv_result$lambda[first]
out_six <- glmnet(x,y,alpha=1,lambda = my_lam)
lasso_coef_six <- predict(out_six,type="coefficients")[1:20,]
lasso_coef_six[lasso_coef_six!=0]
out_six$lambda


data_new <- data.frame(x)
data_new$price <- y


fit_lasso <- lm(price ~ accommodates + room_typeHotel+ room_typePrivate+ neighbourhood_group_cleansedPuente_de_Vallecas + neighbourhood_group_cleansedSalamanca, data = data_new)
summary(fit_lasso)




# otro variante - sale ugual
set.seed(1234)
cv_error <- cv.glmnet(
  x      = x,
  y      = y,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)



modelo <- glmnet(
  x           = x,
  y           = y,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newx = x)

# MSE de entrenamiento
# ==============================================================================
training_mse <- mean((y-predicciones_train)^2)
paste("Error (mse) de entrenamiento:", training_mse)


# MAPE de entrenamiento
training_mape <- mean((predicciones_train-y)/y)



