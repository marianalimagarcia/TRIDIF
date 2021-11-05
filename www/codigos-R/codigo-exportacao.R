#tabela
tabela = data.frame(DF,
           escore = rowSums(matriz, na.rm = T),
           escorepadrao = scale(rowSums(matriz, na.rm = T)),
           escoretri = fscores(modelo, full.scores.SE = TRUE)[,1],
           ep_escoretri = fscores(modelo, full.scores.SE = TRUE)[,2])
write.csv(tabela, "tabela.csv")

#grafico
jpeg ('nomedoarquivo.jpg')
hist(fscores(modelo, full.scores.SE = TRUE)[,1],
     xlab = "Titulo eixo x",
     ylab = "Titulo eixo y")
dev.off ()