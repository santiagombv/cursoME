## Práctico 3. Modelos Aditivos Generalizados

###############
### Caso 1. ###
###############

library(mgcv) 
RIKZ <- read.table("RIKZ.txt", header = TRUE)

#para calcular la riqueza sumamos las filas 2 a la 76
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)

#convertir "exposure" en factor
RIKZ$exposure <- as.factor(RIKZ$exposure)

#exploración
layout(matrix(1:2, 1, 2))
plot(Richness ~ NAP, data=RIKZ)
plot(Richness ~ exposure, data=RIKZ)
layout(1)

# GAM
fit1 <- gam(Richness ~ exposure + s(NAP), data = RIKZ, 
            family = poisson, method = "REML")
summary(fit1)
anova(fit1)

# prueba de supuestos del modelo
# el ajuste con family = quasipoisson arroja scale = 1.14
layout(matrix(1:4,2,2))
gam.check(fit1)
layout(1)

# examen de las variables
plot(fit1)

# ¿Interacción con un factor?
fit2 <- gam(Richness ~ exposure +  s(NAP, by = exposure),
            data = RIKZ, family = poisson, method = "REML")
summary(fit2)

layout(matrix(1:4,2,2))
gam.check(fit2)
layout(1)

layout(matrix(1:3, 1, 3)) # se esperan 3 splines
plot(fit2, seWithMean = TRUE)
layout(1)

# comparación de modelos
AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2, test = "Chisq")

# Representación gráfica del modelo 
# vis.gam habilita gráficos tipo persp o contour
vis.gam(fit1, plot.type = "persp", theta = 55, type = "response")

# Utilizando predict
layout(matrix(1:3,1,3)) # se esperan 3 splines

#1 para exposure = 8
A<-seq(min(RIKZ$NAP), max(RIKZ$NAP), by =0.01)  
E<-rep("8", length(A))				
nr8<-data.frame(exposure = E, NAP = A)
Y8<-predict(fit2, nr8, se.fit = TRUE, type = "response")
plot(nr8$NAP, Y8$fit, type = "l", xlab = "NAP", ylab = "riqueza", ylim = c(0,22))
points(nr8$NAP, Y8$fit + Y8$se.fit, type = "l", lty = 3)
points(nr8$NAP, Y8$fit - Y8$se.fit, type = "l", lty = 3)
title("exposure 8")

#para exposure=10
A <- seq(min(RIKZ$NAP), max(RIKZ$NAP), by = 0.01)  
E <- rep("10", length(A))				
nr10 <- data.frame(exposure=E, NAP=A)
Y10 <- predict(fit2, nr10, se.fit = TRUE, type = "response")	
plot(nr10$NAP, Y10$fit, type = "l", xlab = "NAP", ylab = "riqueza", ylim = c(0,22))			
points(nr10$NAP, Y10$fit + Y10$se.fit, type = "l", lty = 3)
points(nr10$NAP, Y10$fit - Y10$se.fit, type = "l", lty = 3)
title("exposure 10")

#para exposure= 11
A <- seq(min(RIKZ$NAP), max(RIKZ$NAP), by = 0.01)  
E <- rep("11", length(A))				
nr11 <- data.frame(exposure=E, NAP=A)
Y11 <- predict(fit2, nr11, se.fit=T, type="response")			
plot(nr11$NAP, Y11$fit, type = "l", xlab = "NAP", ylab = "riqueza", ylim = c(0,22))			
points(nr10$NAP, Y11$fit + Y10$se.fit, type = "l", lty = 3)
points(nr10$NAP, Y11$fit - Y10$se.fit, type = "l", lty = 3)
title("exposure 11")

##############
### Caso 2 ###
##############

cyc <- read.table("cyclop.txt", header = TRUE)
cyc<-na.omit(cyc) # mgcv no admite datos faltantes
colnames(cyc)<-c("nec", "flo", "lar", "fru", "PF", "pol", "PP")

# modelo isotrópico puramente aditivo
# la base por defecto de s() es "tp" thin plate spline regression
m1 <- gam(pol ~ s(nec, k = 10, bs = "tp") + s(flo, k = 10, bs = "tp"), 
          data=cyc, family = poisson, method = "REML")
summary(m1)

# examen del modelo
concurvity(m1)
layout(matrix(1:4,2,2))
gam.check(m1)
layout(1)

# gráfico
layout(matrix(1:2,1,2))
plot(m1)
layout(1)

vis.gam(m1, view = c("nec", "flo"), theta = -45, ticktype = "detailed",
        type = "response")

vis.gam(m1, view = c("nec", "flo"), type = "response", plot.type = "contour")

# modelos isotrópico con interacción
m2 <- gam(pol ~ s(nec, flo, k = 100, bs = "tp"), data=cyc, 
          family = poisson, method = "REML")
summary(m2)

vis.gam(m2, view = c("nec", "flo"), type = "response", 
        plot.type = "contour", color = "cm")

# comparación
BIC(m1, m2)

# Modelo invariantes a la escala
# Modelo aditivo, cambiamos la base a "cr" cubic spline
# ti() puede usarse en vez de s()
m3 <- gam(pol ~ s(nec, bs = "cr", k = 5) + s(flo, bs = "cr", k = 5),
          data = cyc, family = poisson, method = "REML")
summary(m3)

# Modelo con full tensor product smooth
m4 <- gam(pol ~ te(nec, flo, k = 5), data = cyc, 
          family = poisson, method = "REML")
summary(m4)

# Modelo con tensor product interaction + additive effects
m5 <- gam(pol ~ s(nec, bs = "cr", k = 5) + s(flo, bs = "cr", k = 5) + ti(nec, flo, k = 5), 
          data = cyc, family = poisson, method = "REML")
summary(m5)

# recordar hacer los gam.check y examinar concurvity

# comparación de modelos
BIC(m3, m4, m5)


#gráficos
layout(matrix(1:3,1,3))
vis.gam(m3, plot.type = "contour", color = "cm", type = "response", 
        n.grid = 100, main = NULL)
points(cyc$nec, cyc$flo, pch = 20)

vis.gam(m4, plot.type = "contour", color = "cm", type = "response", 
        n.grid = 100, main = NULL)
points(cyc$nec, cyc$flo, pch = 20)

vis.gam(m5, plot.type = "contour", color = "cm", type = "response", 
        n.grid = 100, main = NULL)
points(cyc$nec, cyc$flo, pch = 20)

layout(1)

### END ###