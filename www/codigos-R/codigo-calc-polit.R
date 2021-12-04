library(mirt)

modelo = mirt(matriz, model = 1, itemtype = '{MODELO}', SE = TRUE, verbose = FALSE)

#medidas de ajuste
modelo@Fit$logLik
modelo@Fit$AIC
modelo@Fit$AICc
modelo@Fit$BIC

#escore acertos
rowSums(matriz, na.rm = T)
#escore padronizado
scale(rowSums(matriz, na.rm = T))
#escore tri e erro padrao escore tri
fscores(modelo, full.scores.SE = TRUE)

#graficos habilidades
hist(fscores(modelo, full.scores.SE = TRUE)[,1])
boxplot(fscores(modelo, full.scores.SE = TRUE)[,1])

#coeficientes e ajuste dos itens
coef(modelo, IRTpars = T, printSE = TRUE)
itemfit(modelo, na.rm = T)

#curvas informacao e erro padrao do modelo
plot(modelo, type = "infoSE")

#caracteristica dos itens
plot(modelo, type="trace", facet_items = TRUE) #todos os itens mosaico
plot(modelo, type="trace", facet_items = FALSE) #todos os itens sobrepostos
itemplot(modelo, 3) #item especifico, 3

#informacao dos itens
plot(modelo, type="infotrace", facet_items = TRUE) #todos os itens mosaico
plot(modelo, type="infotrace", facet_items = FALSE) #todos os itens sobrepostos
itemplot(modelo, 3, type = 'info') #item especifico, 3

