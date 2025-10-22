#Questao 2
#Dados dos candidatos na ordem da tabela
idade <- c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48,
           33, 38, 46)
nacionalidade <- c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana",
                   "Espanhola", "Francesa", "Belga", "Italiana", "Italiana",
                   "Italiana", "Inglesa", "Francesa", "Espanhola", "Italiana",
                   "Alemana", "Francesa", "Italiana", "Alemana", "Italiana"
)
renda <- c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2,
           1.1, 2.5, 2.0, 1.7, 2.1, 3.2)
experiencia <- c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7,
                 12, 23)

curriculos <- data.frame(idade, nacionalidade, renda, experiencia) # 

# Estatisticas descritivas
#item 1
#Informacoes
Informacoes <- c("Idade", "Renda", "Experiencia")
#Medias
media_idade <- mean(idade)
media_Renda <- mean(renda)
media_Experiencia <- mean(experiencia)
Media <- c(media_idade, media_Renda, media_Experiencia)
#Medianas
mediana_idade <- median(idade)
mediana_Renda <- median(renda)
mediana_Experiencia <- median(experiencia)
Mediana <- c(mediana_idade, mediana_Renda, mediana_Experiencia)
#Desvio padrao
desvio_idade <- sd(idade)
desvio_Renda <- sd(renda)
desvio_Experiencia <- sd(experiencia)
Desvio_Padrao <- c(desvio_idade, desvio_Renda, desvio_Experiencia)

#Display dos dados obtidos
estatisticas = data.frame(Informacoes,Media, Mediana, Desvio_Padrao)
print(estatisticas)

#item 2
library(dplyr)
library(tidyverse)
grupos_nacionalidade <- curriculos %>%
  group_by(nacionalidade) %>%# agrupa por nacionalidade
  summarise(
    Renda_media = mean(renda),# media da renda em cada nacionalidade
    Experiencia_media = mean(experiencia) 
    #media de experiencia em cada nacionalidade
  )
cat("Tabela completa:\n")
print(grupos_nacionalidade)

maior_renda <- grupos_nacionalidade %>%
  slice_max(Renda_media)%>% #seleciona a maior renda media
  pull(nacionalidade) #retorna a naciolidade da media obtida

cat("\nNacionalidade com maior renda media desejada:\n")
print(maior_renda)

mais_experiente <- grupos_nacionalidade %>%
  slice_max(Experiencia_media) %>% # seleciona maior experiencia media
  pull(nacionalidade) # retorna a nacionalidade da media obtida

cat("Nacionalidade que aparenta ser mais experiente:")
print(mais_experiente)

#item3
coeficienteP <- cor(experiencia, renda) # Coeficiente de Pearson

ggplot(curriculos, aes(x = experiencia, y = renda)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") + # linha de regressao
  labs(title = "Grafico de Dispersao",
       x = "Experiencia",
       y = "Renda desejada") +
  theme_minimal()

#item 4
prioridade <- curriculos %>%
  select(idade, nacionalidade)%>%
  filter(experiencia>10, renda<2) %>% # filtra as características selecionadas 
  pull(idade)

print("Idade e nacionalidade dos candidatos que atendem aos criterios de
prioridade:\n")
print(prioridade)

print("Numero de candidatos que atendem aos criterios de prioridade:")
print(nrow(prioridade))

# item 5
#idade
par(mfrow = c(2, 3))#divide o quadro em 6 colunas
nacionalidade_unicas <- unique(curriculos$nacionalidade)
#cria um vetor com as nacionalidades
cores <- c("lightgreen", "skyblue", "gold", "red", "violet", "gray")
#vetor de cores para diferenciar as nacionalidades

for (i in 1:length(nacionalidade_unicas)) { #for para cada nacionalidade
  nac <- nacionalidade_unicas[i]#cada loop trata os dados de uma nacionalidade
  
  dados_filtrados <- curriculos %>%
    filter(nacionalidade == nac) %>%
    pull(idade)#cria um vetor de idades da nacionalidade atual do loop
  
  if (i == 1) {#Na primeira iteracao cria o label do grafico
    ylab_text <- "Idade"
    yaxt_setting <- 's'
  }
  #cria o boxplot
  boxplot(dados_filtrados,
          main = nac,
          ylab = ylab_text,
          col = cores[i],
          border = "black",
          ylim = c(20, 55), #define o limite do eixo y
          yaxt = yaxt_setting)
}

#RENDAS
par(mfrow = c(2,3))
for (i in 1:length(nacionalidade_unicas)) { #for para cada nacionalidade
  nac <- nacionalidade_unicas[i] #cada loop trata os dados de uma nacionalidade
  
  dados_filtrados <- curriculos %>%
    filter(nacionalidade == nac) %>%
    pull(renda) #cria um vetor de renda desejada da nacionalidade atual do loop
  
  if (i == 1) { #Na primeira iteracao cria o label do grafico
    ylab_text <- "Renda"
    yaxt_setting <- 's'
  }
  #cria o boxplot
  boxplot(dados_filtrados,
          main = nac,
          ylab = ylab_text,
          col = cores[i],
          border = "black",
          ylim = c(0.5,3.5),
          yaxt = yaxt_setting)
}
