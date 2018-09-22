## Práctico 4. Modelos Mixtos


#############################
### Análisis en nlme::lme ###
#############################

```{r, eval=FALSE}
library(nlme)
rikz <- read.table("RIKZ.txt", header = TRUE)
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$exposure)
rikz$Richness <- rowSums(rikz[, 2:76] > 0)

# 1) Elección de la mejor parte random.
# La parte fija debe ser lo más compleja posible.
# No se estima random para la "pendiente" (contraste) de Exposure
# porque esta variable no tiene variabilidad dentro de 
# cada playa.

r1 <- lme(Richness ~ NAP*Exposure, random = ~ 1 | Beach, 
          data = rikz, method = "REML")
r2 <- lme(Richness ~ NAP*Exposure, random = ~ 1 + NAP | Beach, 
          data = rikz, method = "REML")

# usando AIC (idem para BIC)
AIC(r1)
AIC(r2)

# usando LRT
anova(r1, r2) # dividir el P sobre 2

# 2) Eleccion de la parte fija.
# Conservamos la estrutura random elegida en el paso anterior y 
# Probamos diferentes estructuras fijas.
# Reajustamos el modelo por ML.
f1 <- lme(Richness ~ NAP*Exposure, random = ~ 1 | Beach, 
          data = rikz, method = "ML")
f2 <- lme(Richness ~ NAP+Exposure, random = ~ 1 | Beach, 
          data = rikz, method = "ML")

anova(f1, f2) #se puede usar AIC, BIC o L.ratios test

# 3) Presentamos el modelo (ajustado por REML)
fit <- lme(Richness ~ NAP+Exposure, random = ~ 1 | Beach, 
           data = rikz, method = "REML")
summary(fit)

# 4) Diagnósticos gráficos.
res <- resid(fit, type = "normalized")
pre <- fitted(fit)
qqnorm(res)
plot(pre, res)

# Puede examinarse la relación entre los residuos y las variables x
plot(res ~ rikz$Exposure)
plot(res ~ rikz$NAP)


##############################
### Análisis en lme4::lmer ###
##############################

library(lme4)
rikz <- read.table("RIKZ.txt", header = TRUE)
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$exposure)
rikz$Richness <- rowSums(rikz[, 2:76] > 0)

# 1) Elección de la mejor parte random.
r0 <- lm(Richness ~ NAP*Exposure, data = rikz)
r1 <- lmer(Richness ~ NAP*Exposure + (1|Beach), 
           data = rikz, REML = TRUE)
r2 <- lmer(Richness ~ NAP*Exposure + (0 + NAP|Beach), 
           data = rikz, REML = TRUE) 
r3 <- lmer(Richness ~ NAP*Exposure + (1|Beach) + (0 + NAP|Beach), 
           data = rikz, REML = TRUE)
AIC(r1)
AIC(r2)
AIC(r3)

anova(r3, r1, r0) # se reajusta a ML

library(RLRsim) # fast simulation
exactRLRT(m=r1)
exactRLRT(m=r1, mA=r3, m0=r2)
exactRLRT(m=r2, mA=r3, m0=r1)

library(pbkrtest) # parametric bootstrap (modelo pequeño)
pb <- PBmodcomp(r3, r1)
pb

# 2) Eleccion de la parte fija.
f1 <- lmer(Richness ~ NAP*Exposure + (1|Beach), data = rikz, REML=F)
f2 <- lmer(Richness ~ NAP+Exposure + (1|Beach), data = rikz, REML=F)

anova(f1, f2) #se puede usar AIC, BIC o L.ratios test

kr <- KRmodcomp(f2, f1) # de pbkrtest. Kenward-Roger approx.
kr

# 3) Presentamos el modelo.
fit <- lmer(Richness ~ NAP*Exposure + (NAP|Beach), data = rikz, REML = TRUE)
summary(fit) # sin P en fijos

library(lmerTest) # aprox. Satterthwaite (modifica a lmer)
fit <- lmer(Richness ~ NAP*Exposure + (NAP|Beach), data = rikz, REML = TRUE)
summary(fit)

# 4) Diagnósticos gráficos.
res <- resid(fit, type = "pearson")
pre <- fitted(fit)
qqnorm(res)
plot(pre, res)

# Puede examinarse la relación entre los residuos y las variables x
plot(res ~ rikz$Exposure)
plot(res ~ rikz$NAP)


##############################################
### Análisis en lme4::glmer Generalizados! ###
##############################################

library(lme4)
rikz <- read.table("RIKZ.txt", header = TRUE)
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$exposure)
rikz$Richness <- rowSums(rikz[, 2:76] > 0)

# 1) Elección de la mejor parte random. (sin REML)
r0 <- glm(Richness ~ NAP*Exposure, data = rikz, family=poisson)
r1 <- glmer(Richness ~ NAP*Exposure + (1|Beach), 
            data = rikz, family=poisson)
r2 <- glmer(Richness ~ NAP*Exposure + (0 + NAP|Beach), 
            data = rikz, family=poisson) 
r3 <- glmer(Richness ~ NAP*Exposure + (1|Beach) + (0 + NAP|Beach),
            data = rikz, family=poisson)

AIC(r0); AIC(r1); AIC(r2); AIC(r3)

anova(r3, r1, r0)

# 2) Eleccion de la parte fija.
f1 <- glmer(Richness ~ NAP*Exposure + (1|Beach) + (0 + NAP|Beach), 
            data = rikz, family = poisson)
f2 <- glmer(Richness ~ NAP+Exposure + (1|Beach) + (0 + NAP|Beach), 
            data = rikz, family = poisson)

anova(f1, f2) #se puede usar AIC, BIC o L.ratios test

# 3) Presentamos el modelo.
library(lmerTest) # aprox. Satterthwaite (modifica a glmer)
fit <- glmer(Richness ~ NAP+Exposure + (1|Beach)+ (0 + NAP|Beach), 
             data = rikz, family = poisson)
summary(fit) # sin P en fijos

# 4) Diagnósticos gráficos.
res <- resid(fit, type = "pearson")
pre <- fitted(fit)
qqnorm(res)
plot(pre, res)
plot(res ~ rikz$Exposure)
plot(res ~ rikz$NAP)

# 5 sobrediperso???
library(blmeco)
dispersion_glmer(fit)

############################################################
### Análisis en MASS::glmnPQL penalized QUASI likelihood ###
############################################################

library(MASS)
rikz <- read.table("RIKZ.txt", header = TRUE)
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$exposure)
rikz$Richness <- rowSums(rikz[, 2:76] > 0)

# 1) Elección de la mejor parte random. 
q1 <- glmmPQL(Richness ~ NAP*Exposure, random = ~1|Beach, 
              data = rikz, family=poisson)
q2 <- glmmPQL(Richness ~ NAP*Exposure, random = ~NAP|Beach, 
              data = rikz, family=poisson)
#??????

# 2) presentación del modelo
qfit <- glmmPQL(Richness ~ NAP+Exposure, random = ~NAP|Beach, 
                data = rikz, family=poisson)
summary(qfit)


##################################################################################
### Análisis en gamm4::gamm4 Generalized Aditive Mixed Models (basado en mgcv) ###
##################################################################################

library(gamm4)
rikz <- read.table("RIKZ.txt", header = TRUE)
rikz$Beach <- as.factor(rikz$Beach)
rikz$Exposure <- as.factor(rikz$exposure)
rikz$Richness <- rowSums(rikz[, 2:76] > 0)

# 1) Parte random (ver parte mer)
g1 <- gamm4(Richness ~ Exposure + s(NAP, by = Exposure), 
            random =~ (1|Beach), 
            family = poisson, data = rikz)
g2 <- gamm4(Richness ~ Exposure + s(NAP, by = Exposure), 
            random =~ (1|Beach) + (0 + NAP|Beach), 
            family = poisson, data = rikz)

AIC(g1$mer)
AIC(g2$mer)

# 2) parte fija
g3 <- gamm4(Richness ~ Exposure + s(NAP), 
            random =~ (1|Beach), family = poisson, data = rikz)
g4 <- gamm4(Richness ~ Exposure + s(NAP, by = Exposure), 
            random =~ (1|Beach), family = poisson, data = rikz)

summary(g3$gam)
summary(g4$gam) # en este ejemplo son casi iguales

# 3) modelo
summary(g4$gam)

layout(matrix(1:2,1,1))
plot(g4$gam)
layout(1)
vis.gam(g4$gam, theta = 45, scale = "response")

### END ###
