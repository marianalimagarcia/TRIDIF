######################
###entrada de dados###
######################

DF = read.csv(file   = "abortoparte.csv")
DF = read.csv(file   = "matrizinicialpolit.csv")
DF = read.csv(file   = "matrizinicialdicot.csv")


#se os dados não tiverem a coluna grupo
matriz = DF

#se os dados tiverem a coluna grupo
matriz = DF[, 1:(ncol(DF) - 1)]
grupo = DF[, ncol(DF)]

######################
###  Descritivas   ###
######################

library(ltm)
descript(matriz)

#########################
###Unidimensionalidade###
#########################

library(psych)

# se os dados da matriz forem dicotomicos
correlacao = tetrachoric(matriz)
# se os dados da matriz forem politomicos
correlacao = polychoric(matriz)    

propexp = eigen(correlacao$rho)$values/sum(eigen(correlacao$rho)$values)
propexp[1] = propexp[1]*100

if (propexp[1] >= 20){
  print("Suposição de unidimensionalidade atendida")
}else{
    print("Suposição de unidimensionalidade NÃO atendida")
}

########################################
###Dif Dicotomica Regressao Logistica###
########################################
library(difR)
resultado = difLogistic(matriz, group=grupo, focal.name = 1, type = "both", p.adjust.method = "none")
#ajuste dos itens 
resultado
resultado$logitPar
resultado$logitSe
#curva caracteristica de um item diferenciando os grupos
plot(resultado, plot = "itemCurve", item = 6)  

#########################
###Dif Dicotomica LORD###
#########################
library(difR)
resultado = difLord(matriz, group=grupo, focal.name = 1, model = "2PL", p.adjust.method = "none")
#ajuste dos itens 
resultado
resultado$itemParInit
#curva característica de um item diferenciando os grupos
plot(resultado, plot = "itemCurve", item = 6)

#####################################
###Dif Politomica Logit Cumulativo###
#####################################
library(difNLR)
resultado = difORD(matriz, group=grupo, focal.name = 1, model = "cumulative", type = "both", p.adjust.method = "none")
#ajuste dos itens
resultado
coef(resultado, SE = TRUE, simplify = TRUE)
#curva característica de um item diferenciando os grupos
plot(resultado, item = 6)

########################################
###Dif Politomica Categoria Adjacente###
########################################
library(difNLR)
resultado = difORD(matriz, group=grupo, focal.name = 1, model = "adjacent", type = "both", p.adjust.method = "none")
#ajuste dos itens
resultado
coef(resultado, SE = TRUE, simplify = TRUE)
#curva característica de um item diferenciando os grupos
plot(resultado, item = 6)

#######################
###Modelo Dicotomico###
#######################
library(mirt)

modelo = mirt(matriz, model = 1, itemtype = "2PL", SE = TRUE, verbose = FALSE)

#escore acertos
rowSums(matriz, na.rm = T)
#escore padronizado
scale(escoreacertos)
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


#######################
###Modelo Politomico###
#######################

library(mirt)

modelo = mirt(matriz, model = 1, itemtype = "graded", SE = TRUE, verbose = FALSE)

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

#########################
###Exportacao de Dados###
#########################

#grafico em imagem
jpeg ('nomedoarquivo.jpg')
hist(fscores(modelo, full.scores.SE = TRUE)[,1],
     xlab = "Titulo eixo x",
     ylab = "Titulo eixo y")
dev.off ()

#tabela em planilha
tabela = data.frame(DF,
           escore = rowSums(matriz, na.rm = T),
           escorepadrao = scale(rowSums(matriz, na.rm = T)),
           escoretri = fscores(modelo, full.scores.SE = TRUE)[,1],
           ep_escoretri = fscores(modelo, full.scores.SE = TRUE)[,2])
write.csv(tabela, "planilha.csv")
