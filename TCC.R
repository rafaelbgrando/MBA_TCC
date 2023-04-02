#Instalação pacotes:

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "psych", #elaboração da fatorial e estatísticas
             "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
             "Hmisc", # matriz de correlações com p-valor
             "readxl", # importar arquivo Excel+-
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a arvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'randomForest', #Rodar algorítimo Random Forest
             'plotROC',
             'caret',
             'Rmisc',
              'sjPlot') #elaboração de tabelas de contingência

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
#Baixar Banco de Dados:

db_original <- read.csv("matchups.csv",encoding = "UTF-8")
db_corrigido <- db_original[1:13] #retirando ultima coluna

###############################################################################

#Análise Base Dados:

# Observando a base de dados
#db_corrigido %>% 
 # kable() %>%
  # kable_styling(bootstrap_options = "striped", 
    #            full_width = TRUE, 
     #           font_size = 12)

# Dimensional:
dim(db_corrigido)
#2.4 milhoes de observações e 13 variáveis
str(db_corrigido)

#Variáveis/Colunas:
names(db_corrigido)

# "P_MATCH_ID" - KeyID / Número da partida - chr
# "GOLDEARNED" - Quantidade de ouro ganho na partida - int
# "TOTALMINIONSKILLED" - Total de Minions abatidos - int
# "WIN" - resultados da partida - True/False - Será a variável de decisão -chr
# "KILLS" - Quantidade de adversários abatidos - int
# "ASSISTS" - Quantidade de assistências - int
# "DEATHS" - Quantidade de vezes que foi abatido - int
# "CHAMPION" - personagem selecionado para a partida - chr
# "VISIONSCORE" - pontuação de visão do jogo - int
# "PUUID" - KeyId de identifição do jogador - chr
# "TOTALDAMAGEDEALTTOCHAMPIONS" - Total de dano causado aos adversários - num
# "SUMMONERNAME" - Nome/apelido do jogador - chr
# "GAMEVERSION" - versão do jogo analisado - chr

#Alterando nomes das variáveis:

db_corrigido <- dplyr::rename(db_corrigido, 
                            Partida_ID = 1,
                            Ouro = 2,
                            Total_Minios_Farm = 3,
                            Vitoria = 4,
                            Abates = 5,
                            Assistencias = 6,
                            Mortes = 7,
                            Personagem = 8,
                            Placar_visao = 9,
                            Jogador_Id = 10,
                            Total_dano_causado_advisersario = 11,
                            Nome_jogador = 12,
                            Versao_jogo = 13)

#verificando valores não existentes nas variáveis:

miss_value <- is.na(db_corrigido) #se TRUE significa que temos miss values
verificar_valor <- unique(miss_value) #filtrando resultados diferente encontrados
view(verificar_valor) #somente false - logo base completa

#analisando quantidade de personagens da base:

champions <- unique(db_corrigido$Personagem)
length(champions) # 156 personagens. Ou seja, todos representados

#analisando quantidade de jogadores analisados:

players <- unique(db_corrigido$Jogador_Id)
length(players) # 449.001 jogadores analisados


#verificando se somente vitoria ou derrota
vitoria <- unique(db_corrigido$Vitoria)
length(vitoria) #ou seja, só variáveis sim e não

###############################################################################

#criando KeyId única combinado partida e jogador

db_corrigido <- db_corrigido %>% unite('New_iD',c(1,10),remove = FALSE)

#Retirando variáveis não necessárias para análise:
#não irems precisar das seguintes variáveis "P_MATCH_ID" / "PUUID" / "SUMMONERNAME"
#(criada id unico das observações) e "GAMEVERSION" (não necessária para análise)
db_analise <- db_corrigido[c(1,3:10,12)]

#realocando variável de decisão

db_analise <- db_analise %>% relocate (Vitoria, .after = New_iD)

##############################################################################

# Estatísticas descritivas

# Estatísticas descritivas univariadas
summary(db_analise[,2:10])

summary (db_analise[,c(2,8)]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Coeficientes de correlação de Pearson para cada par de variáveis quantitativas

Var_quat <- c(3:7,9,10)
rho <- rcorr(as.matrix(db_analise[,Var_quat]), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis


ggplotly(db_analise[,Var_quat] %>% 
  cor() %>% 
  melt() %>% 
  dplyr::rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 3) +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom"))


################################################################

#Árvores de decisão

#Transformando a variável resposta em inteiro:
db_analise$Vitoria_sim <- as.integer(db_analise$Vitoria == "true")

per_vitoria <- function(var){
  # Sumariza a taxa de vitória por categoria da variável em análise
  teste <- Rmisc::summarySE(db_analise, measurevar="Vitoria_sim", groupvars=c(var))
  
  ggplotly(
  ggplot(teste) + 
    # Plota as médias de cada grupo
    geom_point(aes(x=teste[,var], y=Vitoria_sim, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=teste[,var], y=Vitoria_sim, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de vitória") + 
    # Marcas do eixo secundário
    scale_y_continuous(labels = scales::percent))
}

per_vitoria("Personagem")

#concluímos que todos os personagens possuem uma taxa de vitório próxima a 50%, 
#sendo uma amostra neutra

#verificando se todas as viaráveis qualitativas são fatores para rodar a árvore
db_analise %>% str

#transformar as variaveis qualitativas em fator:

db_arvore <- db_analise
db_arvore$Vitoria <- as.factor(db_arvore$Vitoria)
db_arvore$Personagem <- as.factor(db_arvore$Personagem)

db_arvore %>% str

# Vamos construir a árvore de classificação #
arvore <- rpart(Vitoria ~ Personagem + Ouro + Total_Minios_Farm + Abates + Assistencias + Mortes + Placar_visao,
                data=db_arvore,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
                )

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot(arvore,box.palette = paleta) # Paleta de cores

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de vitoria
prob = predict(arvore, db_arvore)

# Classificação dos vitórias
class = prob[,2]>.5 # se maior q 50% ganhou
#verificando a quantidade de vitórios pelo algoritimo
sum(class) #1.2M de vitórias
#na base original
sum(db_arvore$Vitoria_sim) #1.2M de vitórias
#indica que podemos utilizar arvore de decisão para a base para prever vitórias

# Matriz de confusão
tab <- table(class, db_analise$Vitoria)
tab

tab %>% 
   kable() %>%
   kable_styling(bootstrap_options = "striped", 
               full_width = TRUE, 
               font_size = 12)

#calculando a acuracia
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc
#ou seja, estamos com 77% de acurácia rodando uma simples arvore.


#identificando o grau de importância da variáveis:
arvore$variable.importance #logo a quantide de vezes que o jogador é abatido
#é a variável mais importante da arvore de decisão

arvore$variable.importance %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 25)

################################################################

#Separando entre base de treino e teste:
separador <- runif(dim(db_arvore)[1])>.25
table(separador)

treino <- db_arvore[separador,]
teste <- db_arvore[!separador,]

#rodar base treino

arvore <- rpart(Vitoria ~ Personagem + Ouro + Total_Minios_Farm + Abates + Assistencias + Mortes + Placar_visao,
                data=treino,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class', # Essa opção indica que a resposta é qualitativa
                xval=5,
                )

# Avaliar a árvore na base de treino
p_treino = predict(arvore, treino)
c_treino = factor(ifelse(p_treino[,2]>.5, "Y", "N"))

#Avaliar comparar com real
p_teste = predict(arvore, teste)
c_teste = factor(ifelse(p_teste[,2]>.5, "Y", "N"))
c_teste

#Calculo de acerto base Treino
tab <- table(c_treino, treino$Vitoria)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

#Confirmação acerto com base na base de teste
tab <- table(c_teste, teste$Vitoria)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de teste: %s ', percent(acc))

#identificando o grau de importância da variáveis:
arvore$variable.importance #logo a quantide de vezes que o jogador é abatido
#é a variável mais importante da arvore de decisão

##########################################################################3

##Rodando do Random Forest##


#ajustando para salvar base em CSV
db_random_forest <- db_arvore[c(2:10)]


write.csv(db_random_forest,"db_python.csv", row.names = FALSE, fileEncoding = "UTF-8")

#### verificando grau de associação entre as variaveis categóricas com a 
#    variável respostas



sjt.xtab(var.row = db_random_forest$Vitoria,
         var.col = db_random_forest$Personagem,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#alto grau de associação



##Retirando variável Personagem, por ter mais de 53 categorias
db_random_forest <- db_arvore[c(2:7,9:10)]

db_random_forest %>% str

#dividir base treino e teste
n <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
            size=nrow(db_random_forest), # O tamanho da amostragem é 2.419.192
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.8,0.2)) # A probabilidade de ser 1 é 80%, de ser 2 é 20%

n %>% table

# Amostra de treino: n==1 (80% do total de amostra)
treino <- db_random_forest[n==1,]
# Amostra de teste: n==2 (20% do total da amostra)
teste <- db_random_forest[n==2,]


# Executar Algorítimo

treino_rf <- randomForest(
  Vitoria ~ ., 
  data = treino, 
  ntree = 20,#hiperparametros numero de arvores
  mtry = 2, #hiperparametros pega 2 variaveis aleatorias para teste inicial
  importance = T)

# Criando função para avaliar o modelo

avalia <- function(modelo, nome_modelo="modelo"){
  p_treino <- predict(modelo, treino, type='prob') # Probabilidade predita
  c_treino <- predict(modelo, treino)              # Classificação
  
  #Base de teste
  p_teste <- predict(modelo, teste, type='prob')
  c_teste <- predict(modelo, teste)
  
  # Data frame de avaliação (Treino)
  aval_treino <- data.frame(obs=treino$Vitoria, 
                            pred=c_treino,
                            true = p_treino[,2],
                            false = 1-p_treino[,2]
  )
  
  # Data frame de avaliação (Teste)
  aval_teste <- data.frame(obs=teste$Vitoria, 
                           pred=c_teste,
                           true = p_teste[,2],
                           false = 1-p_teste[,2]
  )
  
  tcs_treino <- twoClassSummary(aval_treino, 
                                       lev=levels(aval_treino$obs))
  tcs_teste <- twoClassSummary(aval_teste, 
                                      lev=levels(aval_teste$obs))
  ##########################
  # Curva ROC              #
  
  CurvaROC <- ggplot2::ggplot(aval_teste, aes(d = obs, m = true, colour='1')) + 
    plotROC::geom_roc(n.cuts = 0, color="blue") +
    plotROC::geom_roc(data=aval_treino,
                      aes(d = obs, m = true, colour='1'),
                      n.cuts = 0, color = "red") +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    theme(legend.position = "none") +
    ggtitle(paste("Curva ROC | ", nome_modelo, " | AUC-treino=",
                  percent(tcs_treino[1]),
                  "| AUC-teste = ",
                  percent(tcs_teste[1]))
    )
  
  print('Avaliação base de treino')
  print(tcs_treino)
  print('Avaliação base de teste')
  print(tcs_teste)
  CurvaROC
}
#Validando Modelo
avalia(treino_rf, nome_modelo="Random Forest")

### Realizando um Cross Validation

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- trainControl(
  method='repeatedcv', # Solicita um K-Fold com repetições
  number=4, # Número de FOLDS (o k do k-fold)
  repeats=2, # Número de repetições
  search='grid', # especifica o grid-search
  summaryFunction = twoClassSummary, # Função de avaliação de performance
  classProbs = TRUE # Necessário para calcular a curva ROC
)

# Especificar o grid
grid <- expand.grid(.mtry=c(1:2)) #testar com 10 combinações

gridsearch_rf <-        caret::train(Vitoria ~ .,         # Fórmula (todas as variáveis)
                              data = treino,       # Base de dados
                              method = 'rf',        # Random-forest
                              metric='ROC',         # Escolhe o melhor por essa métrica
                              trControl = controle, # Parâmetros de controle do algoritmo
                              ntree=20,            # Numero de árvores
                              tuneGrid = grid)      # Percorre o grid especificado aqui

print(gridsearch_rf)
plot(gridsearch_rf)

###################################
# Avaliar o modelo melhorado         #

avalia(gridsearch_rf, nome_modelo='Random Forest com melhora overfitting')
write()
