library(tidyverse)
df <- read.csv('data.csv')
df

#We're going to use just the mean columns
df <- df %>% select("id",
              "diagnosis",
              "radius_mean",
              "texture_mean",
              "perimeter_mean",
              "area_mean",
              "smoothness_mean",
              "compactness_mean",
              "concavity_mean",
              "concave_points_mean",
              "symmetry_mean",
              "fractal_dimension_mean")

#Tirando area, e perimetro, já que temos raio
df <- df %>% select("diagnosis",
                    "radius_mean",
                    "texture_mean",
                    "smoothness_mean",
                    "compactness_mean",
                    "concavity_mean",
                    "concave_points_mean",
                    "symmetry_mean",
                    "fractal_dimension_mean")

#Transform target variable into 0 or 1
df$diagnosis[df$diagnosis=="M"] <- 1
df$diagnosis[df$diagnosis=="B"] <- 0
df$diagnosis <- as.numeric(df$diagnosis)
head(df)

##Seleção do modelo
#Queremos prever uma variável categórica, logo, teremos que ter um modelo de regressão logística.

#Modelo 1 (mais simples -> gelmann & hill)
fit.1 <- glm(diagnosis ~ radius_mean, family=binomial(link="logit"), data=df)
summary(fit.1)

plot(fit.1$residuals) #Não parece haver tendencia
#Idea: usar ggplot e plotar a smooth, com a curva de incerteza

##Seleção de variáveis

##Avaliação do modelo

#Plot dos resíduos

#AUC

