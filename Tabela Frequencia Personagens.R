library(ggplot2)
library(plotly)
dados <- data.frame()
dados <- data.frame(table(db_arvore[8]))

# Criando o gráfico de barras com o ggplot2
grafico <- ggplot(dados, aes(x = Personagem, y = Freq, fill = Personagem)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequência de Escolha - Personagens") +
    xlab("Personagem") +
  ylab("Frequência")+
  theme(plot.title = element_text(hjust = 0.5))

# Tornando o gráfico interativo com o plotly
ggplotly(grafico)
