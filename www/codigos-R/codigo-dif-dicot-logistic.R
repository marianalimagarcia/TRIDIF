library(difR)

resultado = difLogistic(matriz, group=grupo, focal.name = 1, type = '{TYPE}', p.adjust.method = '{METODO}')

#ajuste dos itens 
resultado
resultado$logitPar
resultado$logitSe

#curva característica de um item diferenciando os grupos
plot(resultado, plot = "itemCurve", item = 6)  
