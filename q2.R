
#Questão 2
idades <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
#Nacionalidades: italiana, inglesa, belga, espanhola, francesa, alemana
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"
)
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
curriculos <- data.frame(idades, nacionalidade, renda, experiencia)

# Estatísticas descritivas
#item 1
#Informações
Informações <- c("Idade", "Renda", "Experiência")
#Medias
media_Idades <- mean(idades)
media_Renda <- mean(renda)
media_Experiencia <- mean(experiencia)
Media <- c(media_Idades, media_Renda, media_Experiencia)
#Medianas
mediana_Idades <- median(idades)
mediana_Renda <- median(renda)
mediana_Experiencia <- median(experiencia)
Mediana <- c(mediana_Idades, mediana_Renda, mediana_Experiencia)
#Desvio padrão
desvio_Idades <- sd(idades)
desvio_Renda <- sd(renda)
desvio_Experiencia <- sd(experiencia)
Desvio_Padrão <- c(desvio_Idades, desvio_Renda, desvio_Experiencia)
estatisticas = data.frame(Informações,Media, Mediana, Desvio_Padrão)
print(estatisticas)

#item 2
library(dplyr)
library(tidyverse)
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
cat("Tabela completa:\n")
print(grupos_nacionalidade)

maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_media)%>%
  pull(nacionalidade)

cat("\nNacionalidade com maior renda média desejada:\n")
print(maior_renda)

mais_experiente <- grupos_nacionalidade %>%
  slice_max(Experiencia_media) %>%
  pull(nacionalidade)

cat("Nacionalidade que aparenta ser mais experiente:")
print(mais_experiente)

#item3
coeficienteP <- cor(experiencia, renda)
coeficienteP

ggplot(curriculos, aes(x = experiencia, y = renda)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") + # linha de regressão
  labs(title = "Gráfico de Dispersão",
       x = "Experiência",
       y = "Renda desejada") +
  theme_minimal()

#item 4
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)

print("Idade e nacionalidade dos candidatos que atendem aos critérios de prioridade:\n")
print(prioridades)

print("Número de candidatos que atendem aos critérios de prioridade:")
print(nrow(prioridades))
