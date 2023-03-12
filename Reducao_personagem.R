### Reduzindo quantidade de personagens

## gerar aleatoriamente 53 categorias diferente na variável personagem
set.seed(0)

reducao = sample(1:length(champions),53)

champions_reduzido <- as.data.frame(champions[reducao])
champions_reduzido <- dplyr::rename(champions_reduzido, 
                             Personagem = 1)


##### reduzir base de observações


db_arvore_red <- inner_join(db_arvore, champions_reduzido, by = "Personagem")

#####Testas arvore decisao

db_arvore_red %>% str()
db_arvore_red$Personagem <- as.factor(db_arvore_red$Personagem)
db_arvore_red <- db_arvore_red[,2:11]


db_arvore_red %>% str()

arvore_red <- rpart(Vitoria ~ Personagem + Ouro + Total_Minios_Farm + Abates + Assistencias + Mortes + Placar_visao,
                data=db_arvore_red,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot(arvore_red,box.palette = paleta) # Paleta de cores

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de vitoria
prob = predict(arvore_red, db_arvore_red)

# Classificação dos vitórias
class_red = prob[,2]>.5 # se maior q 50% ganhou
#verificando a quantidade de vitórios pelo algoritimo
sum(class_red) #357KM de vitórias
#na base original
sum(db_arvore_red$Vitoria_sim) #386K de vitórias
#indica que podemos utilizar arvore de decisão para a base para prever vitórias

# Matriz de confusão
tab_red <- table(class_red, db_arvore_red$Vitoria)
tab_red

tab_red %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#calculando a acuracia
acc_red <- (tab_red[1,1] + tab_red[2,2])/ sum(tab_red)
acc_red
#ou seja, estamos com 76% de acurácia rodando uma simples arvore.


##Rodando do Random Forest##


#ajustando para salvar base em CSV
db_random_forest <- db_arvore_red[,1:9]

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


