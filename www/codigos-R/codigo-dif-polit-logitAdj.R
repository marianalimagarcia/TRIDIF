library(difNLR)

resultado = difORD(matriz, group=grupo, focal.name = 1, model = "adjacent", type = "both", match = "score", p.adjust.method = "none", parametrization = "classic")

#ajuste dos itens
resultado
coef(resultado, SE = TRUE, simplify = TRUE)

#curva característica de um item diferenciando os grupos
plot(resultado, item = 6)