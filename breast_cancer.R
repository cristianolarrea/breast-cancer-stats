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

hist(df$radius_mean)

##Seleção do modelo
#Queremos prever uma variável categórica, logo, teremos que ter um modelo de regressão logística.

#Modelo 1 (mais simples -> gelmann & hill)
fit.1 <- glm(diagnosis ~ radius_mean, family=binomial(link="logit"), data=df)
summary(fit.1)
#INTERPRETAÇÕES -----------------------------------------
#Vamos definir a função inversa para mapear de invlogit probabilidade -> probabilidiade
invlogit <- function(x) {1/(1+exp(-x))}

#Intercepto -> preciso avaliar em outro ponto. Não faz sentido
#interpretar intercepto em raio=0.
#Vamos interpretar na média dos raios:
central_intercept <- invlogit(coef(fit.1)[1] + coef(fit.1)[2]*mean(df$radius_mean))
show(central_intercept)
#No ponto médio dos raios (ponto central), a probabilidade do diagnóstico
#ser maléfico é de 0.34 = 34%

#Coeficiente do raio: o coeficiente para o raio da célula é 1, isto é,
#a diferença de 1 no raio de uma célula implica em uma diferença de 1
#1 na logit probabilidade do diagnóstico ser malefíco.

#Para um x=10, por exemplo, a diferença é
diff <- invlogit(coef(fit.1)[1] + coef(fit.1)[2]*11) - invlogit(coef(fit.1)[1] + coef(fit.1)[2]*10)
show(diff) #0.01298613
#Isto é, a diferença de 1 no raio da célula aumenta em 1.29%
#a probabilidade do tumor ser maléfico.

#PLOTANDO E COMPARANDO ----------------
plot(df$radius_mean, df$diagnosis)
curve(invlogit(coef(fit.1)[1] + coef(fit.1)[2]*x), add=TRUE)

#Adicionando mais concavidade como preditor
hist(df$concavity_mean) #Não parece ter dist normal, mt valores pequenos

fit.2 <- glm(diagnosis ~ radius_mean + concavity_mean, family=binomial(link="logit"), data=df)
summary(fit.2)

#Interpretação: os outros dois coeficientes cotinuam bem parecidos
#Coeficiente da concavidade: comparando duas celulas de mesmo raio,
#cada aumento de 1 no valor da concavidade aumenta em 27.33 o valor
#da logit probabilidade.
#Entretanto, perceba que a concavity_mean varia entre 0.0 e 0.5, então
#não faz sentido falar em aumento de 1 no valor da concavidade.

#Plotando 1 gráfico para cada variável
#Gráfico 1
plot(df$radius_mean, df$diagnosis)
curve(invlogit(cbind(1,x,0) %*% coef(fit.2)), add=TRUE)
curve(invlogit(cbind(1,x,0.1) %*% coef(fit.2)), add=TRUE)

#Gráfico 2
plot(df$concavity_mean, df$diagnosis)
curve(invlogit(cbind(1, mean(df$radius_mean), x) %*% coef(fit.2)), add=TRUE)
curve(invlogit(cbind(1, mean(df$radius_mean)/2, x) %*% coef(fit.2)), add=TRUE)



#Avaliação dos modelos

plot(fit.1$residuals) #Não parece haver tendencia
#Idea: usar ggplot e plotar a smooth, com a curva de incerteza

plot(fit.2$residuals) #Mesma coisa que o de cima, entretanto
#tem um outlier gigantesco em -200

summary(fit.1) #AUC = 334.01
summary(fit.2) #AUC = 236.85


#PERGUNTAS:
#PERGUNTA 1: escala do concavity_mean de 0.1 a 0.5 - como interpretar
#coeficiente. Reescalar?
#PERGUNTA 2: melhor explicação sobre interações. Quando usar?

#TO DO:
# - Pesquisar e ler mais sobre as variáveis
# - Fazer a avaliação baseado em falso positivo
