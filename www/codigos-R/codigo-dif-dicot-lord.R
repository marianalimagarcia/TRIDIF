library(difR)

resultado = difLord(matriz, group=grupo, focal.name = 1, model = "2PL", p.adjust.method = "none")

#ajuste dos itens 
resultado
resultado$itemParInit

#curva característica de um item diferenciando os grupos
plot(resultado, plot = "itemCurve", item = 6)
