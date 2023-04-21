
Var_quat_e <- c(3:7,9:11)

rho_e <- rcorr(as.matrix(db_analise[,Var_quat_e]), type="pearson")

corr_coef_e <- rho_e$r # Matriz de correlações
corr_sig_e <- round(rho_e$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis


ggplotly(db_analise[,Var_quat_e] %>% 
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

