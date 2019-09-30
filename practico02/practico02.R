## Práctico 2. Modelos Binomiales

#############
### Caso1 ###
#############

ven <- read.table("veneno.txt", header = TRUE)

# construcción de la variable respuesta
rta <- cbind(ven$muertos, ven$tot-ven$muertos)

#modelo
vfit <- glm(rta ~ veneno + dosis, data = ven, family = binomial(logit))

#significancia según el estadístico de Wald
summary(vfit)

#análisis de la devianza 
anova(vfit, test = "Chisq")

#examen gráfico de los residuos
layout(matrix(1:4, 2, 2))
plot(vfit)
layout(1)

#examen sobre sobredispersión
vfit2 <- glm(rta ~ veneno + dosis, data = ven, family = quasibinomial(logit))
summary(vfit2)

#examen sobre linealidad
LP <- vfit$linear.predictors^2
vfit3 <- glm(rta ~ veneno + dosis + LP, data = ven, family = binomial(logit))
summary(vfit3)

#INTERPRETACIÓN DE PARÁMETROS
exp(vfit$coeff)

# parámetros faltantes:

ven$veneno2 <- relevel(ven$veneno, "R")
reor.vfit <- glm(rta ~ veneno2 + dosis, family = binomial(logit), data = ven)
summary(reor.vfit)
exp(reor.vfit$coeff)
1/exp(reor.vfit$coeff)

############
## Caso 2 ##
############

datos <- read.table("uta.txt", header = TRUE)

# modelo
fit <- glm(Uta ~ PA.ratio, data = datos, family = binomial(logit))

# parámetros y su significancia según el estadístico de Wald
summary(fit)

# interpretación de parámetros.
exp(fit$coefficients)

# notar cómo se vuelve más interpretable el intercepto al 
# centrar la variable.
datos$PA.ratio.2 <- datos$PA.ratio - mean(datos$PA.ratio, na.rm = T)

fit2 <- glm(Uta ~ PA.ratio.2, data = datos, family = binomial(logit))
summary(fit2)
exp(fit2$coefficients)

# análisis de la devianza 
anova(fit2, test = "Chisq")

# examen gráfico de los residuos
layout(matrix(1:4, 2, 2))
plot(fit2)
layout(1)

# examen sobre linealidad
LP <- fit2$linear.predictors^2
fit3 <- glm(Uta ~ PA.ratio.2 + LP, data = datos, family = binomial(logit))
summary(fit3)

#gráfico con predict
X <- seq(-20, 64, 0.5)
Y <- predict(fit2, data.frame(PA.ratio.2 = X), type = "response")
plot(Uta ~ PA.ratio.2, data = datos, xlab = "perímetro/área", 
     ylab = "presencia de Uta")
points(X, Y, type = "l")

#matriz de confusión
obs <- datos$Uta
esp <- as.numeric(predict(fit2, type="response") > 0.5)
table(esp, obs)

library(caret)
confusionMatrix(table(esp, obs))

# curva ROC sobre los datos de entrenamiento (!)
# (pocos datos para que sea informativo, sólo como ejemplo)
library(ROCR)
pr <- prediction(predict(fit2, type="response"), datos$Uta)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

### END ###