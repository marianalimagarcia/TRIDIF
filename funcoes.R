calculadescritivas = function(banco){
  geral = descript(banco)
  if(is.null(geral$missin)){
    falta = 0
    tabfaltantes = as.data.frame(matrix(0, ncol=ncol(banco), nrow=2), row.names = c("Freq","(%)"))
    names(tabfaltantes) = names(banco)
  }else{
    falta = sum(geral$missin[1,])
    tabfaltantes = as.data.frame(geral$missin)
    tabfaltantes[2,] = round(tabfaltantes[2,], digits = 1)  
  }
  
  paramParaTexto = list(geral$sample[1],geral$sample[2], falta)

  propresp = as.data.frame(geral$perc)
  propresp = round(propresp, digits = 3)

  alfacronbach = as.data.frame(geral$alpha)
  alfacronbach = round(alfacronbach, digits = 3)
  
  saida = list(paramParaTexto,tabfaltantes, propresp, alfacronbach)
  return(saida)
}
 

verificaunidimen = function(banco){

  if (GLOBAL_Dados$tipo == "N") tipo = " indefinido "
  if (GLOBAL_Dados$tipo == "D") tipo = " dicotômicos "
  if (GLOBAL_Dados$tipo == "P") tipo = " politômicos "
  
  if (GLOBAL_Dados$tipo == "D") {   
    correlacao       = tetrachoric(banco)
    matrizcorrelacao = "tetracorica"
    tiporesposta     = "dicotomicas binarias"
  }else{
    correlacao       = polychoric(banco)
    matrizcorrelacao = "policorica"
    tiporesposta     = "politomicas ordinais"
  }
  propexp = round(eigen(correlacao$rho)$values/
                    sum(eigen(correlacao$rho)$values), digits = 3)
  propexp[1] = propexp[1]*100
  if(propexp[1]>=20){
        paramParaTexto = list(matrizcorrelacao, tiporesposta, propexp[1]," ",     "maior") }
  else{ paramParaTexto = list(matrizcorrelacao, tiporesposta, propexp[1]," nao ", "menor") }
  return(paramParaTexto)
  
}


verificadiflog = function(banco, grupo, tipodiflog, correcaodiflog,
                          nomex = VERIFICA_DIF_LOG_X, nomey = VERIFICA_DIF_LOG_Y,
                          focal = "Focal", referencia = "Referencia"){  
  nomesitens = colnames(banco)
  resultado = difLogistic(banco, group=grupo, focal.name = 1, type = tipodiflog, p.adjust.method = correcaodiflog)
  tabeladif = data.frame(resultado$Logistik, valorp = resultado$adjusted.p, resultado$deltaR2,
                         resultado$logitPar[,1], resultado$logitSe[,1], resultado$logitPar[,2], resultado$logitSe[,2],
                         resultado$logitPar[,3], resultado$logitSe[,3], resultado$logitPar[,4], resultado$logitSe[,4],
                         row.names = nomesitens)
  names(tabeladif) = c("Est Qui", "p-valor", "deltaR2", "b0", "EP(b0)","b1", "EP(b1)","b2", "EP(b2)","b3", "EP(b3)")
  tabeladifarred = round(tabeladif, digits = 3)
  itensdif = resultado$DIFitems
  if(itensdif=="No DIF item detected"){
    itensdif = NULL
    curvas   = NULL
    graficodif = NULL
    graficossave = NULL
  }else{
    if(length(itensdif)==1){
      linhas = 1
    }else if(length(itensdif)%%2 == 0){
      linhas = length(itensdif) / 2
    }else{
      linhas = length(itensdif)%/%2 + 1
    }
    curvas = graficossave = list()
    score = seq(0, length(nomesitens), length.out = 500)
    for (i in 1:length(itensdif)) {
      j = itensdif[i]
      
      df1 =  data.frame(x = score,
                        y = exp(tabeladif[j,4] + tabeladif[j,6] * score)/(1+exp(tabeladif[j,4] + tabeladif[j,6] * score)),
                        grupo = rep(referencia, 500))
      df2 =  data.frame(x = score,
                        y = exp(tabeladif[j,4] + tabeladif[j,8] + (tabeladif[j,6] + tabeladif[j,10]) * score)/
                          (1+exp(tabeladif[j,4] + tabeladif[j,8] + (tabeladif[j,6] + tabeladif[j,10]) * score)),
                        grupo = rep(focal, 500))
      df3 = rbind(df1, df2)
      
      graficossave[[i]] = df3 %>% 
        plot_ly(x = ~x, y = ~y, type = "scatter", mode = 'lines', hoverinfo = "none",
                linetype = ~grupo, linetypes = c("dash", "solid"), color = I('dark blue'),
                showlegend = T) %>%
        layout(annotations = tituloGraficoItem(nomesitens[j]),
               xaxis = list(title = nomex, zeroline=F),
               yaxis = list(title = nomey, zeroline=F))
      
      curvas[[i]] = df3 %>% 
        plot_ly(x = ~x, y = ~y, type = "scatter", mode = 'lines',
                linetype = ~grupo, linetypes = c("dash", "solid"), color = I('dark blue'),
                text= ~grupo, height = 400*linhas,
                showlegend = F,
                hovertemplate = paste('<b>Grupo:</b> %{text}',
                                      '<br><b>',nomex,':</b> %{x:.3f}',
                                      '<br><b>',nomey,':</b> %{y:.3f}<extra></extra>')) %>% 
        layout(annotations = tituloGraficoItem(nomesitens[j]),
               xaxis = list(title = nomex, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
               yaxis = list(title = nomey, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T))
      
    }
    
    graficodif = plotly::subplot(curvas, nrows = linhas, margin=0.03)
  }
  #mensagem para a interface  
  Mens <- str_glue(TEXTO_SIM_DIFLOG, ITENS=paste(itensdif, collapse = ","))
  if (is.null(itensdif)) Mens <- TEXTO_NAO_DIFLOG  
  saida = list(itensdif, tabeladifarred, graficodif, Mens, graficossave)
  saida
}


verificadiflord = function(banco, grupo, modelo, correcaodiflord,
                           nomex = VERIFICA_DIF_LORD_X, nomey = VERIFICA_DIF_LORD_Y){
  
  nomesitens = colnames(banco)
  if(modelo == "1PL"){
    resultado = difLord(banco, group=grupo, focal.name = 1, model = modelo, p.adjust.method = correcaodiflord)
    tabeladif = data.frame(resultado$LordChi, resultado$adjusted.p, resultado$itemParInit[1:ncol(banco),],
                           resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),], row.names = nomesitens)
    names(tabeladif) = c("Lord Qui", "p-valor", "bR", "EP(bR)", "bF", "EP(bF)")
  }else if(modelo == "2PL"){
    resultado = difLord(banco, group=grupo, focal.name = 1, model = modelo, p.adjust.method = correcaodiflord)
    tabeladif = data.frame(resultado$LordChi, resultado$p.value,
                           resultado$itemParInit[1:ncol(banco),1], resultado$itemParInit[1:ncol(banco),3],
                           resultado$itemParInit[1:ncol(banco),2], resultado$itemParInit[1:ncol(banco),4],
                           resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),1], resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),3],
                           resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),2], resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),4],
                           row.names = nomesitens)
    names(tabeladif) = c("Lord Qui", "p-valor", "aR", "EP(aR)", "bR", "EP(bR)", "aF", "EP(aF)", "bF", "EP(bF)")
  }else{
    guess = itemParEst(banco, model = modelo)[, 3]
    resultado = difLord(banco, group=grupo, focal.name = 1, model = modelo, c = guess, p.adjust.method = correcaodiflord)
    tabeladif = data.frame(resultado$LordChi, resultado$adjusted.p,
                           resultado$itemParInit[1:ncol(banco),1], resultado$itemParInit[1:ncol(banco),3],
                           resultado$itemParInit[1:ncol(banco),2], resultado$itemParInit[1:ncol(banco),4],
                           resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),1], resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),3],
                           resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),2], resultado$itemParInit[(ncol(banco)+1):(2*ncol(banco)),4],
                           resultado$itemParInit[1:ncol(banco),6], row.names = nomesitens)
    names(tabeladif) = c("Lord Qui", "p-valor", "aR", "EP(aR)", "bR", "EP(bR)", "aF", "EP(aF)", "bF", "EP(bF)", "c")
  }
  
  
  tabeladifarred = round(tabeladif, digits = 3)
  itensdif = resultado$DIFitems
  if(itensdif=="No DIF item detected"){
    itensdif=NULL
    curvas = NULL
    graficodif = NULL
  }else{
    if(length(itensdif)==1){
      linhas = 1
    }else if(length(itensdif)%%2 == 0){
      linhas = length(itensdif) / 2
    }else{
      linhas = length(itensdif)%/%2 + 1
    }
    curvas = list()
    score = seq(-3, 3, length.out = 500)
    for (i in 1:length(itensdif)) {
      j = itensdif[i]
      if(modelo=="1PL"){
        df1 =  data.frame(x = score,
                          y = exp(score-tabeladif[j,3])/(1+exp(score-tabeladif[j,3])),
                          grupo = rep("Referencia", 500))
        df2 =  data.frame(x = score,
                          y = exp(score-tabeladif[j,5])/(1+exp(score-tabeladif[j,5])),
                          grupo = rep("Focal", 500))
      }else if(modelo=="2PL"){
        df1 =  data.frame(x = score,
                          y = exp(tabeladif[j,3]*(score-tabeladif[j,5]))/(1+exp(tabeladif[j,3]*(score-tabeladif[j,5]))),
                          grupo = rep("Referencia", 500))
        df2 =  data.frame(x = score,
                          y = exp(tabeladif[j,7]*(score-tabeladif[j,9]))/(1+exp(tabeladif[j,7]*(score-tabeladif[j,9]))),
                          grupo = rep("Focal", 500))
      }else{
        df1 =  data.frame(x = score,
                          y = tabeladif[j,11] + (1 - tabeladif[j,11])*
                            (exp(tabeladif[j,3]*(score-tabeladif[j,5]))/(1+exp(tabeladif[j,3]*(score-tabeladif[j,5])))),
                          grupo = rep("Referencia", 500))
        df2 =  data.frame(x = score,
                          y = tabeladif[j,11] + (1 - tabeladif[j,11])*
                            (exp(tabeladif[j,7]*(score-tabeladif[j,9]))/(1+exp(tabeladif[j,7]*(score-tabeladif[j,9])))),
                          grupo = rep("Focal", 500))
      }
      df3 = rbind(df1, df2)
      
      curvas[[i]] = df3 %>% 
        plot_ly(x = ~x, y = ~y, linetype = ~grupo, linetypes = c("dash", "solid"),
                color = I('dark blue'), text= ~grupo, height = 400*linhas,
                type = "scatter", mode = 'lines', showlegend = F,
                hovertemplate = paste('<b>Grupo:</b> %{text}',
                                      '<br><b>',nomex,':</b> %{x:.3f}',
                                      '<br><b>',nomey,':</b> %{y:.3f}<extra></extra>')) %>% 
        layout(annotations = tituloGraficoItem(nomesitens[j]),
               xaxis = list(title=nomex, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
               yaxis = list(title=nomey, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T))
      
    }
    
    graficodif = plotly::subplot(curvas, nrows = linhas, margin=0.03)
    
    # mensagem para a interface  
    Mens <- str_glue(TEXTO_SIM_DIFLOG, ITENS=paste(itensdif, collapse = ","))
    if (is.null(itensdif)) Mens <- TEXTO_NAO_DIFLOG  
    
    saida = list(itensdif, tabeladifarred, graficodif, Mens)
    saida
  }  
}


verificadifcumlogit = function(banco, grupo, tipodifcumlogit, correcaodifcumlogit, categ, modelo,
                               nomex = VERIFICA_DIF_CUMLOGIT_X, nomey = VERIFICA_DIF_CUMLOGIT_Y){
  
  if (modelo == "A") tipoModelo = "adjacent"
  if (modelo == "C") tipoModelo = "cumulative"
  
  resultado = difORD(banco, group=grupo, focal.name = 1, model = tipoModelo, type = tipodifcumlogit,
                     match = "score", p.adjust.method = correcaodifcumlogit, parametrization = "classic")
  
  itensdif = resultado$DIFitems
  if(itensdif[1]=="No DIF item detected"){
    itensdif = NULL 
    tabeladifarred = NULL
    graficodif = NULL
    Mens = TEXTO_NAO_DIFLOG
  }else{
    
    nomesitens = colnames(banco)
    Mens = str_glue(TEXTO_SIM_DIFLOG, ITENS=paste(itensdif, collapse = ","))
    coefsdata = coef(resultado, SE = TRUE, simplify = TRUE)
    coefs = coefsdata[c(TRUE, FALSE), ]
    ses = coefsdata[c(FALSE, TRUE), ]
    tabelacoefs =  cbind(coefs, ses)[, order(c(seq(ncol(coefs)), seq(ncol(ses))))]
    tabeladif = cbind(resultado$Sval, resultado$adj.pval,tabelacoefs, row.names = nomesitens)
    
    numcatred = length(categ)
    nomes = c("Est Qui", "p-valor", "b01", "EP(b01)")
    for(i in 2:(numcatred-1)){
      nomes = c(nomes, paste("b0",i, sep = ""), paste("EP(b0",i,")", sep = ""))
    }
    nomes = c(nomes,"b1","EP(b1)","b2","EP(b2)","b3","EP(b3)")
    names(tabeladif) = nomes

    tabeladifarred = round(tabeladif, digits = 3)
    
    curvas = list()
    
    if(length(itensdif)==1){
      linhas = 1
    }else if(length(itensdif)%%2 == 0){
      linhas = length(itensdif) / 2
    }else{
      linhas = length(itensdif)%/%2 + 1
    }
    
    for(i in 1:length(itensdif)){
      
      cores = c("#191970","#CD8500","#FF1493", "#3CB371","#B8860B", "#00FF00","#FF0000", "#483D8B")
      
      #----------------------------------------------------------------
      if (modelo=="A") g2 = plot(resultado, item = itensdif[i])[[1]] + ggtitle("")
      if (modelo=="C") g2 = plot(resultado, item = itensdif[i], plot.type = "category")[[1]] + ggtitle("")
      g2$layers[[1]] = g2$layers[[2]]
      g2$layers[[2]] = NULL
      g2$guides$linetype$title = NULL
      g2$guides$colour$title = NULL
      #----------------------------------------------------------------
      g2 = ggplotly(g2, height=400*linhas) %>% 
        layout(annotations = tituloGraficoItem(itensdif[i]),
               xaxis = list(title=nomex, showgrid=T, zeroline=F, linecolor="gray",
                            linewidth=1.5, mirror=T),
               yaxis = list(title=nomey, showgrid=T, zeroline=F, linecolor="gray",
                            linewidth=1.5, mirror=T),
               showlegend = FALSE)
      
      for (k in 1:length(g2$x$data)) {
        text <- g2$x$data[[k]]$text
        text <- gsub("category:", "Categoria:", text)
        text <- gsub("probability", nomey, text)
        text <- gsub("matching", nomex, text)
        text <- gsub(paste(c("[(]", "[)]"), collapse = "|"), "", text)
        text <- gsub(" PY = ", " ", text)
        text <- lapply(ifelse(g2$x$data[[k]]$line$dash =="dash",
                              "Grupo: Focal<br />",
                              "Grupo: Referencia<br />"), paste, sep = "", text)[[1]]
        text <- lapply(strsplit(text, split = "<br />"), unique)
        text <- lapply(text, strtrim, width = c(17, 12,21,20))
        
        text <- unlist(lapply(text, paste, collapse = "<br />"))
        g2$x$data[[k]]$text <- text
      }
      if(modelo=="C"){
        cont = 1
        for (k in 1:(length(g2$x$data)-1)) {
          if(k %% 2==1){
            g2$x$data[[k]]$line$color = cores[cont]
            g2$x$data[[k+1]]$line$color = cores[cont]
            cont = cont + 1
          }
          g2$x$data[[k]]$line$width = 2
        }
        g2$x$data[[k+1]]$line$width = 2
      }else{
        for (k in 1:(length(g2$x$data)/2)){
          g2$x$data[[k]]$line$color = cores[k]
          g2$x$data[[k]]$line$width = 2
        }
        cont = 1
        for (k in ((length(g2$x$data)/2)+1):(length(g2$x$data))){
          g2$x$data[[k]]$line$color = cores[cont]
          g2$x$data[[k]]$line$width = 2
          cont = cont + 1
        }
      }
      
      curvas[[i]] = g2
    }
    
    graficodif = plotly::subplot(curvas, nrows = linhas, margin=0.02)
  }
  #mensagem para a interface  
  saida = list(itensdif, tabeladifarred, graficodif, Mens)
  saida
}


ajustamodelo = function(banco, tipomodelo){
  if(tipomodelo=="1PL"){
    s = paste("F = 1-", ncol(banco), "\n","CONSTRAIN = (1-", ncol(banco), ", a1)")
    arg = mirt.model(s)
    modelo = mirt(banco, model = arg, itemtype = "2PL", SE = TRUE, verbose = FALSE)
  }else{
    modelo = mirt(banco, model = 1, itemtype = tipomodelo, SE = TRUE, verbose = FALSE)
  }
  modelo
}


tabulacoefitens = function(modelo, nomesitens, tipomatriz, categ, cod_modelo){
    
  numcatred <- length(categ)
  tabparametros = coef(modelo, IRTpars = TRUE, simplify = TRUE)$items
  if (dim(modelo@vcov)[1] > 1) {
    listaerro = coef(modelo, IRTpars = T, printSE = TRUE)
    tabelaerro = do.call(rbind, lapply(1:nrow(tabparametros), function(i) listaerro[[i]]["SE", ]))
  } else {
    tabelaerro = cbind(rep(NA, nrow(tabparametros)), NA, NA, NA)
  }
  tabela = cbind(tabparametros, tabelaerro)[, order(c(seq(ncol(tabparametros)), seq(ncol(tabelaerro))))]

  if(tipomatriz == "dicot"){
    colnames(tabela)[1:8] = paste0(
      c("", "EP("),
      paste0(rep(c("D.a", "b", "c", "d"), each = 2)),
      c("", ")")
    )
    tabela = tabela[,1:6]
  }else{
    
    if (cod_modelo == "rsm") {
      nomes = c("a")
      for(i in 1:(numcatred-1)){ nomes = c(nomes, paste("d",i, sep = "")) }
      nomes = c(nomes,"b")
      
      dimcoef = 2*numcatred + 2
      colnames(tabela)[1:dimcoef] = paste0(
          c("", "EP("),
          paste0(rep(nomes, each = 2)),
          c("", ")")
      )
      tabela = tabela[,-c(1,2)]
    }

    if (cod_modelo == "gpcmIRT") {
      nomes = c("D.a")
      for(i in 1:(numcatred-1)){ nomes = c(nomes, paste("b",i, sep = "")) }
      dimcoef = 2*numcatred
      colnames(tabela)[1:dimcoef] = paste0(
          c("", "EP("),
          paste0(rep(nomes, each = 2)),
          c("", ")")
      )
      tabela = tabela[,-c(dimcoef+1,dimcoef+2)]
    }
    
    if (cod_modelo == "graded") {
      nomes = c("D.a")
      for(i in 1:(numcatred-1)){ nomes = c(nomes, paste("b",i, sep = "")) }
      dimcoef = 2*numcatred
      colnames(tabela)[1:dimcoef] = paste0(
        c("", "EP("),
        paste0(rep(nomes, each = 2)),
        c("", ")")
      )
    }
    
    if (cod_modelo == "Rasch") {
      nomes = c("a")
      for(i in 1:(numcatred-1)){ nomes = c(nomes, paste("b",i, sep = "")) }
      dimcoef = 2*numcatred
      colnames(tabela)[1:dimcoef] = paste0(
        c("", "EP("),
        paste0(rep(nomes, each = 2)),
        c("", ")")
      )
      tabela = tabela[,-c(1,2)]
    }
  }
  rownames(tabela) = nomesitens
  tabela = round(tabela, digits = 3)
  tabela
}


tabulahabilidades = function(modelo, banco){
  escoreacertos = rowSums(banco, na.rm = T)
  escorepadronizado = scale(escoreacertos)
  habilidades =  fscores(modelo, full.scores.SE = TRUE)
  tabela = data.frame(escoreacertos, escorepadronizado, habilidades)
  colnames(tabela) <- c("Escore Total", "Escore Padronizado", "Escore Tri", "EP(Escore Tri)")
  rownames(tabela) <- paste("Respondente", 1:nrow(tabela))
  tabela = round(tabela, digits = 3)
  tabela
}


plotdisthabilidades = function(modelo,
                               nome_hist_x = PLOTDISTHABILIDADADES_HIST_X, nome_hist_y = PLOTDISTHABILIDADADES_HIST_Y,
                               nome_box_x  = PLOTDISTHABILIDADADES_BOX_X) {
  habilidades = as.vector(fscores(modelo))
  
  histosave = plot_ly(x = habilidades, type = "histogram", hoverinfo = "none")
  histo = plot_ly(x = habilidades, type = "histogram") %>%
    layout(xaxis = list(hoverformat = ',.2f',title = nome_hist_x),
           yaxis = list(title = nome_hist_y))
  
  boxpsave = plot_ly(x = habilidades, type = "box", hoverinfo = "none", name = " ")
  boxp = plot_ly(x = habilidades, type = "box", name = nome_box_x) %>%
    layout(xaxis = list(hoverformat = ',.2f',title = nome_box_x))
  
  saida = list(histo, boxp, histosave, boxpsave)
  return(saida)
}

plotcurvasgerais = function(modelo, nomex = PLOTCURVASGERAIS_X,   nomey = PLOTCURVASGERAIS_Y,
                            serie1 = PLOTCURVASGERAIS_S1, serie2 = PLOTCURVASGERAIS_S2){
  
  graficopronto = plot(modelo, type = "infoSE")
  valores       = graficopronto$panel.args
  repet  = length(valores[[1]]$x)
  dfinf  = data.frame(tipo = rep(serie1, repet),
                      x    = valores[[1]]$x, 
                      y    = valores[[1]]$y)
  dferro = data.frame(tipo = rep(serie2, repet),
                      x    = valores[[1]]$x,
                      y    = 1/sqrt(valores[[1]]$y))
  dfciicomp = rbind(dfinf, dferro)
  
  ciimodelosave = dfciicomp %>% 
    plot_ly(x = ~x, y = ~y, color = ~tipo, colors = c("#56B1F7", "#132B43"),
            type = "scatter", mode = 'lines', hoverinfo = "none") %>% 
    layout(xaxis = list(title=nomex, showgrid=T, zeroline=F, mirror=T),
           yaxis = list(title=nomey, showgrid=T, zeroline=F, mirror=T),
           showlegend = TRUE)
  
  ciimodelo = dfciicomp %>% 
    plot_ly(x = ~x, y = ~y, color = ~tipo, colors = c("#56B1F7", "#132B43"),  
            text= ~tipo, type = "scatter", mode = 'lines',
            hovertemplate = paste('<b>',nomex,':</b> %{x:.3f}',
                                  '<br><b>%{text}:</b> %{y:.3f}<extra></extra>')) %>% 
    layout(xaxis = list(title=nomex,  showgrid=T, zeroline=F, mirror=T),
           yaxis = list(title=nomey,  showgrid=T, zeroline=F, mirror=T),
           showlegend = TRUE)
  
  saida = list(NULL,ciimodelo, ciimodelosave)
}


plotcurvasitensdicot = function(modelo, nomesitens,
                                nome_cci_x = PLOTCURVASITENSDICOT_CCI_X, nome_cci_y = PLOTCURVASITENSDICOT_CCI_Y,
                                nome_cii_x = PLOTCURVASITENSDICOT_CII_X, nome_cii_y = PLOTCURVASITENSDICOT_CII_Y){
  habilgraf  = seq(-6, 6, length.out = 500)
  caracitem  = infoitem   = list()
  cci        = ccisave    = cii   = ciisave = list()
  ccisjuntas = ccisjuntassave = ciisjuntas = ciisjuntassave = plot_ly()
  linhas     = length(nomesitens)
  
  for(i in 1:length(nomesitens)){
    
    caracitem[[i]] = probtrace(extract.item(modelo, nomesitens[i]), habilgraf)[, 2]
    infoitem[[i]]  =  iteminfo(extract.item(modelo, nomesitens[i]), habilgraf)
    
    #-----------------------------------
    
    dfcci = data.frame(x = habilgraf,  y = caracitem[[i]])
    dfcii = data.frame(x = habilgraf,  y = infoitem[[i]])
    nome = paste("Item",sub("I", "", nomesitens[i]))
    
    ccisjuntassave =  ccisjuntassave  %>% add_lines(x = dfcci$x, y= dfcci$y, name=nome, hoverinfo = "none")
    ccisjuntas     =  ccisjuntas  %>%
                add_lines(x = dfcci$x, y= dfcci$y, name=nome, text=i,
                          hovertemplate = paste('<b>Item:</b> %{text}</b>',
                                                '<br><b>',nome_cci_x,':</b> %{x:.3f}',
                                                '<br><b>',nome_cci_y,':</b> %{y:.3f}<extra></extra>')) %>%
                layout(xaxis = list(title=nome_cci_x, hoverformat = ',.2f'),
                       yaxis = list(title=nome_cci_y,    hoverformat = ',.2f'))
    
    
    ciisjuntassave =  ciisjuntassave  %>% add_lines(x = dfcii$x, y= dfcii$y, name=nome, hoverinfo = "none")
    ciisjuntas     =  ciisjuntas  %>%
                add_lines(x = dfcii$x, y= dfcii$y, name=nome, text=i,
                          hovertemplate = paste('<b>Item:</b> %{text}</b>',
                                                '<br><b>',nome_cii_x,':</b> %{x:.3f}',
                                                '<br><b>',nome_cii_y,':</b> %{y:.3f}<extra></extra>')) %>%
                layout(xaxis = list(title=nome_cii_x, hoverformat = ',.2f'),
                       yaxis = list(title=nome_cii_y, hoverformat = ',.2f'))
    
    #-----------------------------------
    ccisave[[i]] = plot_ly(dfcci, x = ~x, y=~y,
                           type = "scatter", mode = 'lines',  hoverinfo = "none") %>%
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis = list(title = " "),
             yaxis = list(title = " "))
    
    cci[[i]] = plot_ly(dfcci, x = ~x, y=~y, type = "scatter", mode = 'lines',
                       name = nome, height=300*linhas,
                       hovertemplate = paste('<br><b>',nome_cci_x,':</b> %{x:.3f}',
                                             '<br><b>',nome_cci_y,':</b> %{y:.3f}<extra></extra>')) %>%
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis       = list(title=nome_cci_x, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             yaxis       = list(title=nome_cci_y, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T, range=c(0,1)),
             showlegend  = FALSE)
    
    ciisave[[i]] = plot_ly(dfcii, x = ~x, y=~y,  hoverinfo = "none",
                           type = "scatter", mode = 'lines') %>%
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis = list(title = " "),
             yaxis = list(title = " "))
    
    cii[[i]] = plot_ly(dfcii, x = ~x, y=~y, type = "scatter", mode = 'lines',
                       name = nome, height=300*linhas,
                       hovertemplate = paste('<br><b>',nome_cii_x,':</b> %{x:.3f}',
                                             '<br><b>',nome_cii_y,':</b> %{y:.3f}<extra></extra>')) %>%
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis       = list(title=nome_cii_x, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             yaxis       = list(title=nome_cii_y, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             showlegend  = FALSE)
  }
  
  #-----------------------------------
  ccimosaico = plotly::subplot(cci, nrows=linhas, margin=0.01)
  ciimosaico = plotly::subplot(cii, nrows=linhas, margin=0.01)
  
  saida = list(ccisjuntas,     ciisjuntas,     ccimosaico, ciimosaico,
               ccisjuntassave, ciisjuntassave, ccisave,    ciisave)
  return(saida)
}


plotcurvasitenspolit = function(modelo, nomesitens, categ,
                                nome_cci_x = PLOTCURVASITENSPOLIT_CCI_X, nome_cci_y = PLOTCURVASITENSPOLIT_CCI_Y,
                                nome_cii_x = PLOTCURVASITENSPOLIT_CII_X, nome_cii_y = PLOTCURVASITENSPOLIT_CII_Y){
  cores = c("#191970","#CD8500","#FF1493", "#3CB371","#B8860B", "#00FF00","#FF0000", "#483D8B")
  habilgraf = seq(-6, 6, length.out = 500)
  cci = ccisave = cii = cii_t = ciisave = ciisave_t = list()
  ciisjuntas = ciisjuntassave = plot_ly()
  
  
  linhas = length(nomesitens)
  for(i in 1:length(nomesitens)){
    
    numcatred <- length(categ)
    #--------------------------------------
    dfccicomp = dfciicomp = data.frame()
    for(j in 1:numcatred){
      dfcciparcial = data.frame(ordi = as.factor(categ[j]), 
                                x    = habilgraf,
                                y    = probtrace(extract.item(modelo, nomesitens[i]), habilgraf)[, j])
      dfciiparcial = data.frame(ordi = as.factor(categ[j]), 
                                x    = habilgraf,
                                y    = iteminfo(extract.item(modelo, nomesitens[i]), habilgraf, total.info = FALSE)[, j])
      dfccicomp = rbind(dfccicomp, dfcciparcial)
      dfciicomp = rbind(dfciicomp, dfciiparcial)
    }

    #--------------------------------------------------------------------------------------
    ccisave[[i]] = dfccicomp %>% 
      plot_ly(x = ~x, y = ~y, color = ~ordi, text=~ordi, hoverinfo = "none") %>% 
      add_lines(colors = cores[1:numcatred]) %>% 
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis = list(title = " "),
             yaxis = list(title = " "))
    
    cci[[i]] = dfccicomp %>% 
      plot_ly(x = ~x, y = ~y, color = ~ordi, text=~ordi, height=200*linhas,
              hovertemplate = paste('<b>Categoria:</b> %{text}',
                                    '<br><b>',nome_cci_x,':</b> %{x:.3f}',
                                    '<br><b>',nome_cci_y,':</b> %{y:.3f}<extra></extra>')) %>% 
      add_lines(colors = cores[1:numcatred]) %>% 
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis = list(title=nome_cci_x,   showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             yaxis = list(title=nome_cci_y,   showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             showlegend = FALSE)
    
    #-------------------------------------------------------------------------------------
    ciisave[[i]] = dfciicomp %>% 
      plot_ly(x = ~x, y = ~y, color = ~ordi, text= ~ordi, hoverinfo = "none") %>% 
      add_lines(colors = cores[1:numcatred]) %>% 
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis = list(title = " "),
             yaxis = list(title = " "))
    
    cii[[i]] = dfciicomp %>% 
      plot_ly(x = ~x, y = ~y, color = ~ordi, text= ~ordi, height=200*linhas, 
              hovertemplate = paste('<b>Categoria:</b> %{text}',
                                    '<br><b>',nome_cii_x,':</b> %{x:.3f}',
                                    '<br><b>',nome_cii_y,':</b> %{y:.3f}<extra></extra>')) %>% 
      add_lines(colors = cores[1:numcatred]) %>% 
      layout(annotations = tituloGraficoItem(nomesitens[i]),
             xaxis = list(title=nome_cii_x,  showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             yaxis = list(title=nome_cii_y,  showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
             showlegend = FALSE)
    
    # - - - - - - - - - - TOTAL
    nome = paste("Item",sub("I", "", nomesitens[i]))
    dfcii_t = data.frame(x = habilgraf, 
                         y = iteminfo(extract.item(modelo, nomesitens[i]), habilgraf, total.info = TRUE))
    cii_t[[i]] = plot_ly(dfcii_t, x = ~x, y=~y, type = "scatter", mode = 'lines', name = nome, height=200*linhas,
                       hovertemplate = paste('<br><b>',nome_cii_x,':</b> %{x:.3f}',
                                             '<br><b>',nome_cii_y,':</b> %{y:.3f}<extra></extra>')) %>%
                 layout(annotations = tituloGraficoItem(nomesitens[i]),
                        xaxis       = list(title=nome_cii_x, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
                        yaxis       = list(title=nome_cii_y, showgrid=T, zeroline=F, linecolor="gray", linewidth=1.5, mirror=T),
                        showlegend  = FALSE)
    
    ciisave_t[[i]] = plot_ly(dfcii_t, x = ~x, y=~y, type = "scatter", mode = 'lines',
                             name = nome, hoverinfo = "none") %>%
                     layout(annotations = tituloGraficoItem(nomesitens[i]),
                            xaxis = list(title = " "),
                            yaxis = list(title = " "))

    # - - - - - - - - - - TOTAL todas curvas 
    nome = paste("Item",sub("I", "", nomesitens[i]))
    dfcii_t_juntas = data.frame(x = habilgraf,  
                                y = iteminfo(extract.item(modelo, nomesitens[i]), habilgraf, total.info = TRUE))
    ciisjuntas =  ciisjuntas  %>%
                  add_lines(x = dfcii_t_juntas$x, y= dfcii_t_juntas$y, name=nome, text=i,
                            hovertemplate = paste('<b>Item:</b> %{text}</b>',
                                                  '<br><b>',nome_cii_x,':</b> %{x:.3f}',
                                                  '<br><b>',nome_cii_y,':</b> %{y:.3f}<extra></extra>')) %>%
                  layout(xaxis = list(title=nome_cii_x, hoverformat = ',.2f'),
                         yaxis = list(title=nome_cii_y, hoverformat = ',.2f'))

    ciisjuntassave =  ciisjuntassave  %>% 
                      add_lines(x = dfcii_t_juntas$x, 
                                y = dfcii_t_juntas$y, name=nome, hoverinfo = "none")
  }
  
  #--------------------------------------
  graficocci   = plotly::subplot(cci,   nrows=linhas, margin=0.01)
  graficocii   = plotly::subplot(cii,   nrows=linhas, margin=0.01)
  graficocii_t = plotly::subplot(cii_t, nrows=linhas, margin=0.01)
  
  saida = list(graficocci,   graficocii, 
               ccisave,      ciisave, 
               graficocii_t, ciisave_t,
               ciisjuntas,   ciisjuntassave)
  
  return(saida)
}


#---------------------------------------------------------------------------------------
# FUNCOES AUXILIARES PARA OS GRAFICOS
#---------------------------------------------------------------------------------------

tituloGraficoItem = function(item) {
  titulo = list(
    text      = paste("Item ", sub("I", "", item)),
    xref      = "paper",
    yref      = "paper",
    yanchor   = "bottom", # bottom",  #center",  #bottom",
    xanchor   = "center",
    align     = "center",
    x         = 0.5,
    y         = 1,
    font      = list(size=16, color="black"),
    showarrow = FALSE
  )
  return(titulo)
}

editagrafico = function(grafico, x, y=NULL){
  if (is.null(grafico)) {
    return(NULL)
  }
  else {
    graficonovo = grafico %>%
      layout(xaxis = list(title = x),
             yaxis = list(title = y))
    return(graficonovo)
  }
}


#---------------------------------------------------------------------------------------
# FUNCOES ALEATORIAS
#---------------------------------------------------------------------------------------

# 
semAcentos <- function(argString) {
  iconv(argString, from="UTF-8", to = "ASCII//TRANSLIT")
}

# Retira a coluna "Grupo" do DF
extraiGrupo <- function (DF) {
  banco = DF
  grupo = NULL
  if(GLOBAL_Dados$temGrupo){
    nquest = ncol(DF) - 1
    banco = DF[, 1:nquest]
    grupo = DF[, nquest+1]
  }
  return(list(banco, grupo))
}

# Retorna "N", "D" ou "P" para os dados da matriz
verificaDicotPolit <- function(df) {
  # se trocar essas strings, verificar no server.R o tratamento
  status = ""
  if ( (max(df, na.rm=T) - min(df, na.rm=T)) == 1 ) status = "D" # dicotomico
  else
  if ( (max(df, na.rm=T) - min(df, na.rm=T)) >  1 ) status = "P" # politomico
  return(status)
}

# Cores e textos para as caixinhas ValueBox
parametrosValueBox <- function() {
  req(GLOBAL_Dados$tipo)
  tipo = ""
  if (GLOBAL_Dados$tipo == "N") {
    tipo = " indefinido "
    cor  = "black"
  }
  if (GLOBAL_Dados$tipo == "D") { 
    tipo = " dicotomicos "
    cor  = "yellow"
  }
  if (GLOBAL_Dados$tipo == "P") {
    tipo = " politomicos "
    cor  = "green"
  }
  return(list(tipo,cor))
}

# Retorna o menor e maior dos dados para o Polit
categoriasPolit = function(banco){
    ncols = length(banco)
    fatores = vector()
    maior = -20    # gambiarra mode on!
    menor = 20     # gambiarra mode on!
    for (i in 1:ncols) {
      if(maior<max(banco[i,], na.rm=T)){
        maior = max(banco[i,], na.rm=T)
      }
      if(menor>min(banco[i,], na.rm=T)){
        menor = min(banco[1,], na.rm=T)
      }
    }
    return(menor:maior)
}

# Retorna uma string com os valores separados por ,
listaStr <- function(df) {
  txt = ""
  df <- df
  if (!is.null(df)) {
    df <- sub("I", "", df) # retira a string "I"
    df <- as.numeric(df)   # converte em numeric
    df = sort(df)          # ordena
    txt = paste(df, collapse = ",")
  }
  return(txt)
}

# Gera uma string com \t e \n para colar no aceEditor
DF_Para_Ace_Editor <- function (DF, quemChamou=1) {

  # quando o DF vem do aceEditor, não chega com o colnames. Quando vem o read.csv, vem certinho
  # então a jogada é criar uma linha a mais, como 1ª 
  DF = rbind(DF[,1],DF)

  # gera os nomes certinhos das colunas: I1, I2...    
  colnames(DF) = DF[1,]                           # 1ª linha lida será o colnames
  nquest       = ncol(DF)                         # nro de colunas
  col_grupo    = colnames(DF)[nquest]             # salva o nome da ultima coluna
  colnames(DF) = paste0(rep("I",nquest),1:nquest) # coloca as colunas todas com I...
  if (GLOBAL_Dados$temGrupo == T) 
    colnames(DF)[nquest] = "Grupo"                # restaura nome da ultima coluna
  DF[1,] = colnames(DF)                           # acerta a 1ª linha com o colnames ajustado


  # gerar uma string com \t e \n para colar no aceEditor
  tmp = ""     
  for(lin in 1:nrow(DF)){
    tmpLin = ""
    for(col in 1:ncol(DF)){
      tmpLin <- paste0(tmpLin,DF[lin,col],"\t")
    } 
    tmpLin <- substr(tmpLin,1,nchar(tmpLin)-1) # tira o ultimo "\t" da string
    tmp    <- paste0(tmp,tmpLin,"\n")
  }


  return(tmp)
}

# Validação das colunas da matriz do ace editor
considerarDados <- function(DF) {
  
  # mensagens de erro de retorno
  tipoErros = data.frame(
    erro = c("'{C}': nao tem valor '0'",
             "'{C}': nao tem valor '1'",
             "'{C}': existem valores diferentes de '0' e '1'",
             "'{C}': nao possui todos os valores entre '{MIN}' e '{MAX}'"
    ))
  
  # Data frames de retorno
  DF_cons = DF                               # colunas consideradas
  DFe     = data.frame(col=NULL, desc=NULL)  # com todos os erros
  
  #------------------------------------------------
  # Verifica DICOT: 
  # - apenas com valores 0 e 1
  # - todas colunas devem ter pelo menos um 0 e um 1
  #------------------------------------------------
  if (GLOBAL_Dados$tipo == "D") {
    
    # coluna grupo no Dicot tem o mesmo tratamento que uma coluna normal
    for (col in 1:ncol(DF)) {
      
      cont   = data.frame(table(DF[,col]))  # Retorna 'Var1' e 'Freq'
      ind0   = match(0,cont$Var1) # indice do valor 0
      ind1   = match(1,cont$Var1) # indice do valor 1
      nomeCol = colnames(DF)[col]

      temErro = F
      
      # verifica se tem 0 e 1 na coluna 'col'
      if (is.na(ind0)) {
         DFe= rbind(DFe,data.frame(col=nomeCol,desc=str_glue(tipoErros$erro[1],C=nomeCol)))
         temErro = T
      }
      else
      if (is.na(ind1)) {
         DFe= rbind(DFe,data.frame(col=nomeCol,desc=str_glue(tipoErros$erro[2],C=nomeCol)))
         temErro = T
      }
      else {
      # verifica se tem outros valores diferentes de 0 e 1
      soma = cont$Freq[ind0] + cont$Freq[ind1] + sum(is.na(DF[,col]))
      if (soma != nrow(DF)) {
         DFe= rbind(DFe,data.frame(col=nomeCol,desc=str_glue(tipoErros$erro[3],C=nomeCol)))
         temErro = T
      }
      }
      
      # retira a coluna bugada
      if (temErro) DF_cons = DF_cons[-col]
      
    }#for
  }#if
  
  #------------------------------------------------
  # Verifica POLIT: 
  # - pode ter varios valores, mas consecutivos, iniciando com 0
  # - todas colunas devem ter pelo menos 1 valor
  #------------------------------------------------
  if (GLOBAL_Dados$tipo == "P") {

    maior = max(DF,na.rm=T)  # maior valor do DF
    
    # verifica se deve considerar a coluna 'grupo'
    if (GLOBAL_Dados$temGrupo) nItens=ncol(DF)-1
                          else nItens=ncol(DF)
    for (col in 1:nItens) {
      
      nomeCol = colnames(DF)[col]
      cont = data.frame(table(DF[,col]))  # Retorna 'Var1' e 'Freq'
      
      # verifica se todos os Var1 existem (de 0 até maior)
      temErro=F    
      for (x in 0:maior) {
        if (is.na(match(x,cont$Var1))) temErro=T
      }

      # retira a coluna bugada
      if (temErro) DF_cons = DF_cons[-col] 
      
    } #for
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    # se existe grupo, tratamento igual ao DICOT (deve ter apenas 0 e 1)
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    if (GLOBAL_Dados$temGrupo) {
      
      col     = ncol(DF)           # a do grupo
      cont    = data.frame(table(DF[,col]))  # Retorna 'Var1' e 'Freq'
      ind0    = match(0,cont$Var1) # indice do valor 0
      ind1    = match(1,cont$Var1) # indice do valor 1
      nomeCol = colnames(DF)[col]
      temErro = F
      
      # verifica se tem 0 e 1 na coluna 'col'
      if (is.na(ind0)) {
        DFe= rbind(DFe,data.frame(col=nomeCol,desc=str_glue(tipoErros$erro[1],C=nomeCol)))
        temErro = T
      }
      else
      if (is.na(ind1)) {
        DFe= rbind(DFe,data.frame(col=nomeCol,desc=str_glue(tipoErros$erro[2],C=nomeCol)))
        temErro = T
      }
      else
      # verifica se tem outros valores diferentes de 0 e 1
      if (cont$Freq[ind0]+cont$Freq[ind1] != nrow(DF)) {
        DFe= rbind(DFe,data.frame(col=nomeCol,desc=str_glue(tipoErros$erro[3],C=nomeCol)))
        temErro = T
      }
      
      if (temErro) DF_cons =  DF_cons[-col] 
      
    } #if
  } #if

  
  
  DF_cons= rbind(colnames(DF_cons),DF_cons)
  
  #----------------------------------------
  return(list(DFe, DF_cons))
  #----------------------------------------
}

