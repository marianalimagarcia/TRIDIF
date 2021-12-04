library(difR)

resultado = difLord(matriz, group=grupo, focal.name = 1, model = '{MODELO}', p.adjust.method = '{METODO}')

#ajuste dos itens 
resultado
resultado$itemParInit

#curva característica de um item diferenciando os grupos
plot(resultado, plot = "itemCurve", item = 6)
