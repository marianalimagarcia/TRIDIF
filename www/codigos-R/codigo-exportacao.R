#tabela informações gerais
ajuste = data.frame(
  campo = c("Modelo", "log-likehood", "AIC", "AICc","BIC"),
  valor = c(modelo@Call$itemtype, modelo@Fit$logLik, modelo@Fit$AIC, modelo@Fit$AICc, modelo@Fit$BIC)
)
write.xlsx(ajuste,"resultados.xlsx",sheetName="Ajuste do modelo", append=T, row.names = F)

#tabela traços latentes
tracos = data.frame(DF,
                    escore = rowSums(matriz, na.rm = T),
                    escorepadrao = scale(rowSums(matriz, na.rm = T)),
                    escoretri = fscores(modelo, full.scores.SE = TRUE)[,1],
                    ep_escoretri = fscores(modelo, full.scores.SE = TRUE)[,2])
write.xlsx(tracos,"resultados.xlsx",sheetName="Traços latentes", append=T)

#tabela coeficientes dos itens
coeficientes = coef(modelo, IRTpars = TRUE, simplify = TRUE)$items
write.xlsx(coeficientes,"resultados.xlsx",sheetName="Coeficientes", append=T)


#grafico em imagem
jpeg ('nomedoarquivo.jpg')
hist(fscores(modelo, full.scores.SE = TRUE)[,1],
     xlab = "Titulo eixo x",
     ylab = "Titulo eixo y")
dev.off ()