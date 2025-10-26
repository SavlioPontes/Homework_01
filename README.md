## DESCRIÇÃO DO PROJETO (resolução das seguintes questões)
- Questão 1: Análise de emissões diárias de gases poluentes em uma planta industrial
- Questão 2: Estudo de currículos de candidatos para vaga de gerente de relações exteriores
- Questão 3: Análise de dados de compartilhamento de bicicletas

### ESTRUTURA
Homework_01/
- q1.R                        # Análise das emissões
- q2.R                        # Análise dos currículos
- q3.R                        # Análise do bike sharing
- questions.R                 # Arquivo com todas as questões
- HW1_bike_sharing.csv        # Dataset de bicicletas
- relatorio.pdf               # Relatório do trabalho (PDF)
- README.md                   # Este arquivo

### PROCESSO
Nesse projeto, foram trabalhadas as competências em R relacionadas à estatística descritiva por meio de questões contextualizadas, nas quais foram calculados e analisados dados como as medidas de tendência central, de dispersão, os quartis das informações fornecidas e visualizações gráficas. A partir desses valores, foi possível fazer interpretações acerca do comportamento dos dados e como ele se traduziu nos contextos apresentados. Além disso, também foram realizados os cálculos teóricos das questões de modo a possibilitar uma comparação com os resultados obtidos pelos métodos utilizados pelo R.

Para isso, o projeto foi separado em três arquivos de código em R (q1.R, q2.R e q3.R), um para cada questão, os quais foram criados e programados pela plataforma RStudio conectada ao Git. Desse modo, os integrantes faziam suas partes simultaneamente, enviando as alterações feitas para o repositório remoto do gitHub, no qual conseguiam visualizar todo o trabalho.  Ademais, o relatório do projeto foi feito na linguagem LATEX na plataforma Overleaf, que também permite uma edição simultânea dos textos. A seguir, a configuração da divisão de tarefas:

## INSTRUÇÕES PARA EXECUÇÃO
Para rodar o código, basta:
1. Clone o repositório:
- git clone https://github.com/SavlioPontes/Homework_01.git
cd Homework_01

2. Instale as Dependências #Execute no R/RStudio:
rdependencies <- c(
  "dplyr",      # Manipulação de dados (group_by, filter, etc.)
  "tidyverse",  # Inclui ggplot2, readr, tidyr, purrr, etc.
  "ggplot2",    # Gráficos (já incluso no tidyverse, mas para garantir)
  "readr"       # Leitura de arquivos CSV
)

install.packages(dependencies, dependencies = TRUE)

3. Para rodar o código execute no console do R:
 source("questions.R") 


## CONTRIBUIÇÕES
### Savlio Carvalho Pontes (Matrícula: 567715)
- Implementação e análise da Questão 1 (Análise de emissões)
- Implementação e análise da Questão 3 (Análise de bike sharing)
- Preparação do relatório em LaTeX da questão 1
- Implementação de histogramas e boxplots

### Thaís Sousa Barros Leal (Matrícula: 565548)
- Implementação e análise da Questão 2 (Análise de currículos)
- Implementação de gráficos de dispersão e boxplots
- Preparação do relatório em LaTeX das questões 2 e 3
- Padronização dos códigos em português e exposição das saídas

### Contribuições Conjuntas
- Revisão e validação dos códigos
- Documentação e organização do repositório
- Preparação do relatório final em LaTeX


