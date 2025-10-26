#===============================================================#
#QUESTÃO 1#
#===============================================================#

emissions <- scan(); # Permite ler todos os dados agrupados
15.8 22.7 26.8 19.1 18.5 14.4 8.3 25.9 26.4 9.8 21.9 10.5
17.3 6.2 18.0 22.9 24.6 19.4 12.3 15.9 20.1 17.0 22.3 27.5
23.9 17.5 11.0 20.4 16.2 20.8 20.9 21.4 18.0 24.3 11.8 17.9
18.7 12.8 15.5 19.2 13.9 28.6 19.4 21.6 13.5 24.6 20.0 24.1
9.0 17.6 25.7 20.1 13.2 23.7 10.7 19.0 14.5 18.1 31.8 28.5
22.7 15.2 23.0 29.6 11.2 14.7 20.5 26.6 13.3 18.1 24.8 26.1
7.7 22.5 19.3 19.4 16.7 16.9 23.5 18.4

# --- ITEM 1 --- #

# Medidas de tendência central:
mean_emissions <- mean(emissions); # Calcula a média das emissões
median_emissions <- median(emissions); # Calcula a mediana
# Função para moda:
get_mode <- function(x){
  ux <- unique(x); # Vetor contendo cada elemento único do vetor original
  count <- tabulate(match(x,ux)); # Conta quantas vezes cada elemento ocorre
  if(all(count == 1)){
    return(NA); # Se nenhum elemento se repete, não há moda
  }else{
    return(ux[which.max(count)]); # Se há repetições, obtém o elemento mais frequente
  }
}
mode_emissions <- get_mode(emissions); # Retorna a moda

# Medidas de dispersão:
amplitude_emissions <- diff(range(emissions));
# Calcula a amplitude (diferença entre maior e menor valor)

variance_emissions <- var(emissions); # Calcula a variância
stDev_emissions <- sqrt(variance_emissions); # Calcula o desvio padrão
coefVar_emissions <- (stDev_emissions/mean_emissions) * 100;
# Calcula o coeficiente de variação

# Tabela para medidas de tendência central
tabela_tendencia <- data.frame(
  Medida = c("Média", "Mediana", "Moda"),
  Valor = c(
    mean_emissions,
    median_emissions,
    mode_emissions
  )
)

# Tabela para medidas de dispersão
tabela_dispersao <- data.frame(
  Medida = c("Amplitude", "Variância", "Desvio Padrão", "Coeficiente de Variação"),
  Valor = c(
    amplitude_emissions,
    variance_emissions,
    stDev_emissions,
    paste(coefVar_emissions, "%")
  )
)

# Exibir as tabelas
cat("Medidas de Tendência Central:")
tabela_tendencia

cat("Medidas de Dispersão:")
tabela_dispersao

# --- ITEM 2 --- #

par(mfrow = c(1,2)); # Divide a área de plotagem em duas colunas

# Cria nosso histograma
hist(emissions,
     freq = FALSE,
     xlab = "Valores de Emissões",
     ylab = "Densidade",
     main = "Histograma de Emissões", # Título ajustado
     col = "skyblue",
     border = "black")

# Cria nosso box plot
boxplot(emissions,
        main = "Boxplot de Emissões", # Título ajustado
        col = "skyblue",
        border = "black")

par(mfrow = c(1, 1)); # Retorna a configuração da área de plotagem ao normal

# --- ITEM 3 --- #

q1_emissions = quantile(emissions, 0.25); # Calcula o Quartil 1
q2_emissions = quantile(emissions, 0.5); # Calcula o Quartil 2
q3_emissions = quantile(emissions, 0.75); # Calcula o Quartil 3
IQR_emissions = IQR(emissions); # Calcula o IQR das emissões

tabela_quartis <- data.frame(
  Q1 = q1_emissions,
  Q2 = q2_emissions,
  Q3 = q3_emissions,
  IQR = IQR_emissions
)
# Remove o nome da linha
rownames(tabela_quartis) <- NULL

cat("Quartis das Emissões:")
tabela_quartis
# --- ITEM 4 --- #
days_exceeded <- sum(emissions > 25); # Contagem de dias que excederam o limite
total_days <- length(emissions); # Número total de registros de emissões
exceed_proportion <- days_exceeded/total_days; # Proporção de dias excedidos

cat("Proporção de dias em que a planta excedeu o limite de 25 unidades:")
exceed_proportion

#===============================================================#
#QUESTÃO 2#
#===============================================================#

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
cat("Coeficiente de Pearson: ")
print(coeficienteP)

ggplot(curriculos, aes(x = experiencia, y = renda)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") + # linha de regressao
  labs(title = "Grafico de Dispersao",
       x = "Experiencia",
       y = "Renda desejada") +
  theme_minimal()

#item 4
prioridade <- curriculos %>%
  filter(experiencia > 10, renda < 2) %>% # filtra as características selecionadas
  select(idade, nacionalidade)

cat("Idade e nacionalidade dos candidatos que atendem aos criterios de prioridade:")
print(prioridade)

cat("Numero de candidatos que atendem aos criterios de prioridade:")
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

#===============================================================#
#QUESTÃO 3#
#===============================================================#

# File read
setwd("C:\\Users\\savli\\Documents\\GitHub\\Homework_01");
dataBikes <- read.csv("HW1_bike_sharing.csv", header = TRUE);

# ---- Item 1 ----#
#função para obter o número de linhas (=número de observações)
observationNumber <- nrow(dataBikes);
#função para obter o 1º elemento do vetor com os dias das observações
startDate <- dataBikes$dteday[1];
#função para obter o último elemento do vetor com os dias das observações
endDate <- dataBikes$dteday[observationNumber];

#Display das informações obtidas
cat("Número de observações:")
print(observationNumber)

cat("Data de início da amostra:")
print(startDate)

cat("Data de fim da amostra:") 
print(endDate)

# ---- Item 2 ----#

#função para obter a média -> mean
#função para obter a mediana -> median
#função para obter os quartis -> quantile

#TEMPERATURA
tempMean <- mean(dataBikes$temp);
tempMedian <- median(dataBikes$temp);
tempQuantile <- quantile(dataBikes$temp,
                         probs = c(0.25, 0.5, 0.75))
#USUÁRIOS CASUAIS
casualMean <- mean(dataBikes$casual);
casualMedian <- median(dataBikes$casual);
casualQuantile <- quantile(dataBikes$casual,
                           probs = c(0.25, 0.5, 0.75));
#USUÁRIOS REGISTRADOS
registeredMean <- mean(dataBikes$registered);
registeredMedian <- median(dataBikes$registered);
registeredQuantile <- quantile(dataBikes$registered,
                               probs = c(0.25, 0.5, 0.75));

# Tabela com os dados
centralTendTable <- data.frame(
  Variáveis = c("Temperatura", "Usuários casuais", "Usuários registrados"),
  Média = c(tempMean, casualMean, registeredMean),
  Mediana_Q2 = c(tempMedian, casualMedian, registeredMedian),
  Q1 = c(tempQuantile[1], casualQuantile[1], registeredQuantile[1]),
  Q3 = c(tempQuantile[3], casualQuantile[3], registeredQuantile[3])
)
cat("Medidas de tendência central e quartios calculadas por funções do R:\n") 
print(centralTendTable)


# Função para calcular quartis usando o método convencional
calcular_quartis_convencional <- function(x) {
  # Ordenar os dados
  x_ordenado <- sort(x)
  n <- length(x_ordenado)
  
  # Função para calcular posição do quartil
  calcular_posicao <- function(percentil, n) {
    pos <- percentil * (n + 1)
    return(pos)
  }
  
  # Calcular posições
  pos_q1 <- calcular_posicao(0.25, n)
  pos_q2 <- calcular_posicao(0.50, n)
  pos_q3 <- calcular_posicao(0.75, n)
  
  # Função para obter valor baseado na posição
  obter_valor <- function(pos, dados) {
    if (pos == floor(pos)) {
      # Posição inteira
      return(dados[pos])
    } else {
      # Posição não inteira
      pos_inf <- floor(pos)
      pos_sup <- ceiling(pos)
      frac <- pos - pos_inf
      valor <- (1 - frac) * dados[pos_inf] + frac * dados[pos_sup]
      return(valor)
    }
  }
  
  # Calcular quartis
  q1 <- obter_valor(pos_q1, x_ordenado)
  q2 <- obter_valor(pos_q2, x_ordenado)
  q3 <- obter_valor(pos_q3, x_ordenado)
  
  # Retornar resultados
  resultados <- c(Q1 = q1, Q2 = q2, Q3 = q3)
  return(resultados)
}
quartio1 <- calcular_quartis_convencional(dataBikes$temp)
quartio2 <- calcular_quartis_convencional(dataBikes$casual)
quartio3 <- calcular_quartis_convencional(dataBikes$registered)

#tabela com os valores resultantes
tabela_quartis_convencional <- data.frame(
  Variáveis = c("Temperatura", "Usuários casuais", "Usuários registrados"),
  Média = c(mean(dataBikes$temp), mean(dataBikes$casual), mean(dataBikes$registered)),
  Mediana_Q2 = c(quartio1["Q2"], quartio2["Q2"], quartio3["Q2"]),
  Q1 = c(quartio1["Q1"], quartio2["Q1"], quartio3["Q1"]),
  Q3 = c(quartio1["Q3"], quartio2["Q3"], quartio3["Q3"])
)
cat("Medidas de tendência central e quartios calculadas convencionalmente:")
tabela_quartis_convencional

# ---- Item 3 ----#
# Substituição dos números pelos seus nomes de representação
dataBikes$season <- factor(dataBikes$season,
                           levels = c(1, 2, 3, 4),
                           labels = c("Primavera", "Verão",
                                      "Outono", "Inverno")); 
dataBikes$weathersit <- factor(dataBikes$weathersit,
                               levels = c(1, 2, 3, 4),
                               labels = c("Céu limpo", "Nublado",
                                          "Chuva fraca", "Chuva forte"));
# Mostra a frequência de cada estação
freqSeason <- table(dataBikes$season); 
cat("Frequência de usuários em cada estção do ano:")
freqSeason
# Mostra a frequência de cada condição climática
freqWeathersit <- table(dataBikes$weathersit);
cat("Frequência de usuários em cada condição climática:")
freqWeathersit

# Retorna a estação mais frequente
maxSeason <- names(freqSeason)[which.max(freqSeason)];
cat("Estação do ano com maior número de usuários:")
maxSeason
# Retorna a condição climática mais frequente
maxWeathersit <- names(freqWeathersit)[which.max(freqWeathersit)];
cat("Condição climática mais favorável para o uso do sistema, ou seja, na qual há o maior número de usuários:")
maxWeathersit

#gráfico de barras- Estações
barplot(freqSeason,
        col = "skyblue", #cor
        main = "Número de registros por estação", #título
        ylab = "Contagem", #eixo y                    
        xlab = "Estação", #eixo x                    
        ylim = c(0, 240) #amplitude eixo y
)

#gráfico de barras - Condições Climáticas  
barplot(freqWeathersit,
        col = "lightgreen",#cor
        main = "Número de registros por condição climática", #título
        ylab = "Contagem", #eixo y                       
        xlab = "Condição Climática", #eixo x                       
        ylim = c(0,550) #amplitude eixo y
)


# ---- Item 4 ----#
# Calcula o número total de usuários por dia
dataBikes$totalUsers <- dataBikes$casual + dataBikes$registered;
# Converte a temperatura normalizada para seu valor real
dataBikes$tempReal <- dataBikes$temp * 41;

par(mfrow = c(2,1))
#Gráfico de usuários
plot(dataBikes$X, dataBikes$totalUsers, type = "l",
     col = "brown", lwd = 2,
     main = "Contagem total de usuários ao longo do tempo", 
     xlab = "Dias (Índice)", ylab = "Número de Usuários" 
)
#Gráfico de temperatura
plot(dataBikes$X, dataBikes$tempReal, type = "l",
     col = "red", lwd = 2,
     main = "Tendência da temperatura ao longo do tempo", 
     xlab = "Dias (Índice)", ylab = "Temperatura"
)
