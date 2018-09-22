## Práctico 2. Modelos Binomiales

#############
### Caso1 ###
#############

ants <- read.table("hormigas.txt", header = TRUE)

# modelo
gfit1 <- glm(Srich ~ Habitat + Latitude + Elevation, data = ants, family = poisson)

# Tabla "tipo regresión" valores de los parámetros y prueba de Wald
summary(gfit1)

# Tabla de análisis de la devianza
anova(gfit1, test = "Chisq")

#significancia del modelo completo (cociente de verosimilitud)
gfit0 <- glm(Srich ~ 1, data = ants, family = poisson (link = log))
anova(gfit0, gfit1, test = "Chisq")

#cociente de veerosimilitud (con lrtest)
library(lmtest)
lrtest(gfit0, gfit1)

#DIAGNÓSTICOS COMUNES
layout(matrix(1:4, 2, 2))
plot(gfit1)
layout(1)

library(car)
vif(gfit1)

#DIAGNÓSTICOS PARA MLG
# 1¿Es adecuada la relación media-varianza? (¿es el parámetro de   
# dispersión = a 1?)
gfit2 <- glm(Srich ~ Habitat + Latitude + Elevation, data = ants, 
             family = quasipoisson(link=log))

summary(gfit2)
anova(gfit2, test = "F")

# 2 ¿Es adecuado el enlace? (¿es lineal la relación?)
PL <- gfit1$linear.predictors^2
gfit3 <- glm(Srich ~ Habitat + Latitude + Elevation + PL, data=ants,
             family = poisson(link=log))
summary(gfit3)

# INTERPRETACIÓN DE LOS PARÁMETROS
be <- gfit1$coefficients
exp(be)

IC <- confint(gfit1)
exp(IC)

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

# examen sobre el enlace
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
pr <- prediction(esp, datos$Uta)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

### END ###