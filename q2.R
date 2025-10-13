
#Questão 2
idades <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
#Nacionalidades: italiana, inglesa, belga, espanhola, francesa, alemana
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"
)
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
curriculos <- data.frame(idades, nacionalidade, renda, experiencia)
#item 1
media_Idades <- mean(idades)
print(media_Idades)
media_Renda <- mean(renda)
media_Renda
media_Experiencia <- mean(experiencia)
media_Experiencia
#Questão 2
idades <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
#Nacionalidades: italiana, inglesa, belga, espanhola, francesa, alemana
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"
)
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
curriculos <- data.frame(idades, nacionalidade, renda, experiencia)
#item 1
#Medias
media_Idades <- mean(idades)
media_Renda <- mean(renda)
media_Experiencia <- mean(experiencia)
medias <- data.frame(media_Idades, media_Renda, media_Experiencia)
#Medianas
mediana_Idades <- median(idades)
mediana_Renda <- median(renda)
mediana_Experiencia <- median(experiencia)
medianas <- data.frame(mediana_Idades, mediana_Renda, mediana_Experiencia)
#Desvio padrão
desvio_Idades <- sd(idades)
desvio_Renda <- sd(renda)
desvio_Experiencia <- sd(experiencia)
desvio_Padrão <- data.frame(desvio_Idades, desvio_Renda, desvio_Experiencia)
q2_item1<- data.frame(medias, medianas, desvio_Padrão)
q2_item1
# TI0111 - L03 - Data visualization examples
rm(list=ls())
graphics.off()
getwd()
#setwd('your_path_here')
library(ggplot2)
library(RColorBrewer)
display.brewer.all()
clrb <- brewer.pal(9, 'Blues')
##### Data Visualisation ######
?diamonds
#### Univariate analysis ####N
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) +
  theme(text = element_text(size=12))
ggsave('Figures/diamonds_bar.pdf', device='pdf', width=4, height=4)
# Questão 2 ----
library(dplyr)
# Questão 2 ----
install.packages("dplyr")
library(dplyr)
# Dados
curriculos <- data.frame(
  idades = c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46),
  nacionalidade = c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola",
                    "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa",
                    "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa",
                    "Italiana", "Alemana", "Italiana"),
  renda = c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1,
            2.5, 2.0, 1.7, 2.1, 3.2),
  experiencia = c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
)
# Estatísticas descritivas
estatisticas <- curriculos %>%
  summarise(
    media_Idades = mean(idades),
    mediana_Idades = median(idades),
    desvio_Idades = sd(idades),
    media_Renda = mean(renda),
    mediana_Renda = median(renda),
    desvio_Renda = sd(renda),
    media_Experiencia = mean(experiencia),
    mediana_Experiencia = median(experiencia),
    desvio_Experiencia = sd(experiencia)
  )
# Exibir
estatisticas
# Questão 2 ----
# Dados
idades <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola",
                   "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa",
                   "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa",
                   "Italiana", "Alemana", "Italiana")
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1,
           2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
curriculos <- data.frame(idades, nacionalidade, renda, experiencia)
# Estatísticas descritivas
estatisticas <- data.frame(
  Variável = c("Idades", "Renda", "Experiência"),
  Média = c(mean(idades), mean(renda), mean(experiencia)),
  Mediana = c(median(idades), median(renda), median(experiencia)),
  Desvio_Padrão = c(sd(idades), sd(renda), sd(experiencia))
)
# Exibir a tabela
print(estatisticas)
#Questão 2
idades <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
#Nacionalidades: italiana, inglesa, belga, espanhola, francesa, alemana
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"
)
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
curriculos <- data.frame(idades, nacionalidade, renda, experiencia)
#item 1
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
estatisticas = data.frame(Media, Mediana, Desvio_Padrão)
print(estatisticas)
#Questão 2
idades <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46)
#Nacionalidades: italiana, inglesa, belga, espanhola, francesa, alemana
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"
)
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
curriculos <- data.frame(idades, nacionalidade, renda, experiencia)
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
install.packages(tidyverse)
install.packages("tidyverse")
library(dplyr)
curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia),
  )
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia),
  )
write.csv(resposta_q2_item1, "resposta_q2_item1.csv", row.names = FALSE)
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia),
  )
resposta_q2_item1
maior_RendaMédia <- slice(Renda_Media) %>%
  pull(nacionalidade)
maior_RendaMédia <- slice(Renda_media) %>%
  pull(nacionalidade)
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia) %>%
  ) %>%
  resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  ) %>%
  slice(Renda_media) %>%
  pull(nacionalidade)
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  ) %>%
  slice_max(Renda_media) %>%
  pull(nacionalidade)
resposta_q2_item1
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
resposta_q2_item1
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  ) %>%
  slice_max(Renda_media) %>%      # pega a linha com maior renda média
  pull(nacionalidade)
resposta_q2_item1
resposta_q2_item1 <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  ) %>% summarise(
    slice_max(Renda_media) %>%      # pega a linha com maior renda média
      pull(nacionalidade) )
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  ) %>%
  print(grupos_nacionalidade)
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
print(grupos_nacionalidade)
maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_Media)
rlang::last_trace()
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
print(grupos_nacionalidade)
maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_media)
print(maior_renda)
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
grupos_nacionalidade
maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_media)
print(maior_renda)
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
print(grupos_nacionalidade)
maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_media)
cat("\nGrupo com maior renda média:\n")
print(maior_renda)
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%       # agrupa por nacionalidades
  summarise(
    Renda_media = mean(renda),
    Experiencia_media = mean(experiencia)
  )
cat("Tabela completa:\n")
print(grupos_nacionalidade)
maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_media)
cat("\nGrupo com maior renda média:\n")
print(maior_renda)
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
ggplot(df, aes(x = experiencia, y = renda)) +
  geom_point(color = "blue", size = 3) +      # pontos
  geom_smooth(method = "lm", color = "red") + # linha de regressão
  labs(title = "Gráfico de Dispersão",
       x = "Experiência (anos)",
       y = "Renda") +
  theme_minimal()
ggplot(curriculos, aes(x = experiencia, y = renda)) +
  geom_point(color = "blue", size = 3) +      # pontos
  geom_smooth(method = "lm", color = "red") + # linha de regressão
  labs(title = "Gráfico de Dispersão",
       x = "Experiência (anos)",
       y = "Renda") +
  theme_minimal()
cor(experiencia, renda)
cor
coeficienteP <- cor(experiencia, renda)
coeficienteP
ggplot(curriculos, aes(x = experiencia, y = renda)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") + # linha de regressão
  labs(title = "Gráfico de Dispersão",
       x = "Experiência",
       y = "Renda desejada") +
  theme_minimal()
prioridades <- curriculos %>%
  summarise(
    select(idades, nacionalidade),
    filter(experiencia>10, renda<2)
  )
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)
print(prioridades)
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)
print(prioridades)
print("Número de candidatos que atendem aos critérios de prioridade:")
print(nrow(prioridades))
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)
print(prioridades)
print("Número de candidatos que atendem aos critérios de prioridade:", nrow(prioridades))
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)
print(prioridades)
print(paste("Número de candidatos que atendem aos critérios de prioridade:"), nrow(prioridades))
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)
print(prioridades)
print(paste("Número de candidatos que atendem aos critérios de prioridade:", nrow(prioridades)))
prioridades <- curriculos %>%
  select(idades, nacionalidade)%>%
  filter(experiencia>10, renda<2)
print(paste("Número de candidatos que atendem aos critérios de prioridade:", nrow(prioridades)))
cat("Candidatos prioritários\n")
print(prioridades)
