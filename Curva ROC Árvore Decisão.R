library(ROCR)
library(grid)

arvore_teste <- rpart(Vitoria ~ .,
                data=treino,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class', # Essa opção indica que a resposta é qualitativa
                xval=5,
)

# Gerando as predições para a base de treino e teste
y_pred_train <- predict(arvore_teste, newdata = treino, type = "prob")[, 2]
y_pred_test <- predict(arvore_teste, newdata = teste, type = "prob")[, 2]

# Calculando a curva ROC para a base de treino
pred_train <- prediction(y_pred_train, treino$Vitoria)
perf_train <- performance(pred_train, "tpr", "fpr")
auc_train <- performance(pred_train, measure = "auc")@y.values[[1]]

# Calculando a curva ROC para a base de teste
pred_test <- prediction(y_pred_test, teste$Vitoria)
perf_test <- performance(pred_test, "tpr", "fpr")
auc_test <- performance(pred_test, measure = "auc")@y.values[[1]]

# Adicionando as linhas de grade com a função grid()
plot(perf_train, col = "blue", lwd = 3, main = "Curva ROC | Árvore Decisão", 
     xlab = "1 - Especificidade", ylab = "Sensibilidade", frame.plot = FALSE)
plot(perf_test, col = "red", lwd = 3, add = TRUE)
legend("bottomright", c(paste("AUC Treino =", round(auc_train, 2)*100, "%"),
                        paste("AUC Teste =", round(auc_test, 2)*100, "%")), 
       col = c("blue", "red"), lty = 1, bg = "white")
grid(nx = 5, ny = 5, col = "gray80", lty = "dotted")