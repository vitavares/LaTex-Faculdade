rm(list = ls())

library(ggplot2)
library(scales)
library(xtable)
library(dslabs)
data("murders")
?murders

# Citação para o R
citation()

# site dos dados: 
# https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state

# Variáveis: Número de assassinatos por arma de fogo ocorridos nos EUA em 2010;
#            Estados onde foram registrados os assassinatos;
#            Regiões as quais cada Estado pertence;
#            População de cada Estado.


# tipo de variável, respectivamente: Quantitativa discreta;
#                                    Qualitativa Nominal;
#                                    Qualitativa Nominal;
#                                    Quantitativa discreta.

# Como ocorreu a coleta dos dados: As taxas de homicídio foram calculadas 
# com base nos Relatórios Uniformes de Crimes do FBI e na população estimada 
# do censo de cada estado.

colnames(murders) = c('estado', 'sigla', 'regiao', 'populacao', 'assassinatos')

str(murders)
dim(murders)

attach(murders)

# Algumas medidas descritivas
summary(murders[,3:5])
f = function(x){
  c('desvio padrão'= sd(x), 'CV' = sd(x)/mean(x)*100)
}
f(assassinatos); f(populacao)

# Alguns gráficos para a variável assassinatos
g1 = boxplot(assassinatos, main = 'Boxplot: assassinatos por arma de fogo', 
        xlab = 'Número de assassinatos', horizontal = TRUE, col = 'lightgreen')
abline(v = g1$stats, lty = 2)
g1$out # Outliers do boxplot
subset(murders, assassinatos >= 669 & assassinatos <= 1257)

g2 = hist(assassinatos, main = 'Histograma: Assassinatos por arma de fogo', 
          ylab = 'Densidade', xlab = 'Número de assassinatos', freq = FALSE,
          col = 'lightblue', border = 'darkblue')

library(moments) # Coeficientes para a distribuição dos dados
skewness(g2$density) # Assimetria à direita -> coef. de assimetria > 0
kurtosis(g2$density) # Distribuição dos dados platicúrtica -> coef. de curtose > 0,263

# Gráfico que imcorpora as três variáveis 
b = xtabs((assassinatos/populacao)*100~regiao)
barplot(b, ylab = '% percentual de assassinatos', 
        main = 'Gráfico de barras: percentual de assassinatos por região', xlab = 'Região',
        col = rainbow(4))

# Gráficos para assassinatos por região
tab1 = xtabs((assassinatos/sum(assassinatos))*100~regiao)
pie(tab1,round(tab1, 1),
    radius = 1, main = 'Gráfico de setores: assassinatos X região',
    col = rainbow(15))
legend('topleft', legend = c('Nordeste', 'Sul', 'Norte Central', 'Oeste'),
       fill = rainbow(15), cex = 0.6)


a = tapply(assassinatos, INDEX = regiao, c) # Análise sobre correlação
boxplot(a, col = rainbow(4), ylab = 'número de assassinatos', 
        main = 'Boxplot comparativo: assassinatos por região',
        xlab = 'Região')

cores <- c("red", "blue", "green", "orange")
ggplot(murders, aes(y = assassinatos, x = regiao)) +
  geom_boxplot(fill = cores) +
  scale_fill_manual(values = cores) +
  labs(title = "Boxplot comparativo: assassinatos por região.",
       x = "Região",
       y = "Assassinatos", caption = "Fonte: Elaborado pelos autores.") +
  theme_minimal()

if(sum(aggregate(assassinatos~regiao, murders, var)[,2]*aggregate(assassinatos~regiao, murders, length)[,2])/sum(aggregate(assassinatos~regiao, murders, length)[,2]) > var(assassinatos)){
  print('A variável qualitativa não melhora a previsão de assassinatos')
  } else{print('A variável qualitativa melhora a previsão de assassinatos')}
# A Região de procedência não melhora a previsão de assassinatos

# Gráfico de dispersão e coeficiente de correlação para as variáveis
# assassinatos e população
plot(assassinatos, populacao, main = 'Gráfico de disperção: Assassinatos X População',
     xlab = 'Assassinatos', ylab = 'População', pch = 1)
abline(lm(populacao~assassinatos))
cor(assassinatos, populacao, method = 'pearson') # Correlação direta forte


# Gráfico de dispersão e intervalo de confiança
ggplot(murders, aes(y = populacao, x = assassinatos)) +
  geom_point() +
  geom_smooth(method = "lm",color = "red") +
  labs(subtitle = "Gráfico de dispersão: Assassinatos X População",
       y = "População", x = "Assassinatos",
       caption = "Fonte: Elaborado pelos autores") +
    scale_y_continuous(labels = comma_format()) +
    scale_x_continuous(labels = comma_format())
ggplot(murders, aes(y = populacao, x = assassinatos, shape = regiao)) +
  geom_point(aes(col = regiao), shape = 16) +
  labs(subtitle = "Gráfico de dispersão",
       y = "População", x = "Assassinatos",
       caption = "Fonte: https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state") + 
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(labels = comma_format())

View(murders)

# Tabela mortes por região
soma_assassinatos = aggregate(assassinatos ~ regiao, data = murders, FUN = sum)
soma_assassinatos
xtable(soma_assassinatos)

citation(dslabs)





percentual_assassinatos =  (assassinatos / populacao) * 100

# Criação do dataframe com os dados
dados <- data.frame(regiao, percentual_assassinatos)

# Criação do gráfico de barras usando ggplot
grafico <- ggplot(dados, aes(x = regiao, y = percentual_assassinatos, fill = regiao)) +
  geom_bar(stat = "identity") +
  labs(x = "Região", y = "% Percentual de Assassinatos", 
       title = "Gráfico de barras: Percentual de assassinatos por região") +
  scale_fill_manual(values = rainbow(4)) +
  theme_minimal()

