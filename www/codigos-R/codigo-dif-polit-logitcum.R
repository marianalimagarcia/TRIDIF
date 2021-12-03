library(difNLR)

resultado = difORD(matriz, group=grupo, focal.name = 1, model = "cumulative", type = '{TYPE}', match = "score", p.adjust.method = '{METODO}', parametrization = "classic")

#ajuste dos itens
resultado
coef(resultado, SE = TRUE, simplify = TRUE)

#curva característica de um item diferenciando os grupos
plot(resultado, item = 6)