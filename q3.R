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