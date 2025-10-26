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
