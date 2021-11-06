
server <- function(input, output, session) { 

  #----------------------------------------------------
  # ENTRADA DE DADOS: upload UPLOAD de arquivo CSV
  #----------------------------------------------------
  observe({
     # upload do arquivo selecionado
     req(input$uiEntrada_file_dicot)
     DF = read.csv(file   = input$uiEntrada_file_dicot$datapath,
                   header = FALSE,
                   sep    = input$uiEntrada_rb_separador,
                   quote  = "")

     # verifica se tem grupo
     DF_BKP = DF
     colnames(DF_BKP) = DF_BKP[1,]
     GLOBAL_Dados$temGrupo = tolower(colnames(DF_BKP)[ncol(DF_BKP)]) == "grupo"

     # atualiza o aceEditor com a string com \t e \n
     colnames(DF) = DF[1,]                           # 1ª linha lida será o colnames
     nquest       = ncol(DF)                         # nro de colunas
     col_grupo    = colnames(DF)[nquest]             # salva o nome da ultima coluna
     colnames(DF) = paste0(rep("I",nquest),1:nquest) # coloca as colunas todas com I...
     str_remove_all(col_grupo, "(?<=\\\\n)\\s+|\\s+(?=\\\")|\\\"|(?<=\\\"),|\\\\r(?=\\\\n)|(?<=\\\\r)\\\\n")
     if (GLOBAL_Dados$temGrupo == T) colnames(DF)[nquest]=col_grupo # restaura nome da ultima coluna
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
     updateAceEditor(session,"uiEntrada_ace_dicot",value=tmp)
     
     # verifica se os dados sao dicotomicos ou politomicos
     DF2 = read.csv2(file   = input$uiEntrada_file_dicot$datapath,
                     header = TRUE,
                     sep    = input$uiEntrada_rb_separador,
                     quote  = "")
     GLOBAL_Dados$tipo = verificaDicotPolit(DF2)
     
     output$uiEntrada_txt_Texto <- renderUI({ HTML(" ") })  
     
  })
  
  #----------------------------------------------------
  # ENTRADA DE DADOS: Botão LIMPAR MATRIZ
  #----------------------------------------------------
  observeEvent(input$uiEntrada_btn_LimparAceEditor, {  
     shinyalert(title = MENS_ALERTA_LIMPAR, 
                showConfirmButton=TRUE, confirmButtonText="Sim",
                showCancelButton=TRUE,  cancelButtonText="Não", type = "info",
                callbackR = function(x) { 
                  if(x != FALSE) {
                    updateAceEditor(session, "uiEntrada_ace_dicot", value="")
                    output$uiEntrada_txt_Texto <- renderText({ " " })  
                    GLOBAL_Dados$tipo     = "N"
                    GLOBAL_Dados$temGrupo = NULL
                  }
                },
     )
  })

  #----------------------------------------------------
  # ENTRADA DE DADOS: Botão CARREGAR EXEMPLO DICOT
  #----------------------------------------------------
  observeEvent(input$uiEntrada_btn_Carregar_Dicot_AceEditor, {  
    shinyalert(title = MENS_ALERTA_CARREGAR_DICOT, 
               showConfirmButton=TRUE, confirmButtonText="Sim",
               showCancelButton=TRUE,  cancelButtonText="Não", type = "info",
               callbackR = function(x) { 
                 if(x != FALSE) {
                   updateAceEditor(session, "uiEntrada_ace_dicot", value=DICOT_EXEMPLO)
                   output$uiEntrada_txt_Texto <- renderText({ paste0(" ") })  
                   GLOBAL_Dados$tipo     = "D"
                   GLOBAL_Dados$temGrupo = TRUE
                   
                 }
               },
    )
  })
  
  #----------------------------------------------------
  # ENTRADA DE DADOS: Botão CARREGAR EXEMPLO POLIT
  #----------------------------------------------------
  observeEvent(input$uiEntrada_btn_Carregar_Polit_AceEditor, {  
    shinyalert(title = MENS_ALERTA_CARREGAR_POLIT, 
               showConfirmButton=TRUE, confirmButtonText="Sim",
               showCancelButton=TRUE,  cancelButtonText="Não", type = "info",
               callbackR = function(x) { 
                 if(x != FALSE) {
                   updateAceEditor(session, "uiEntrada_ace_dicot", value=POLIT_EXEMPLO)
                   output$uiEntrada_txt_Texto <- renderText({ paste0(" ") })  
                   GLOBAL_Dados$tipo     = "P"
                   GLOBAL_Dados$temGrupo = TRUE
                 }
               },
    )
  })

  #---------------------------------------------------------
  # ENTRADA DE DADOS: Botão VERIFICAR DADOS
  #---------------------------------------------------------
  observeEvent(input$uiEntrada_btn_Verificar_AceEditor, {  

    # 1ª verificacao: se é dicot ou polit
    DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
    GLOBAL_Dados$tipo     = verificaDicotPolit(DF)
    GLOBAL_Dados$temGrupo = tolower(colnames(DF)[ncol(DF)]) == "grupo"

    # 2ª verificacao: itens a considerar e mensagens de erro
    retorno <- considerarDados(DF)
    DF_erros      = retorno[[1]]
    DF_considerar = retorno[[2]]

    # gerar uma string com \t e \n para colar no aceEditor
    tmp <- NULL
    if (nrow(DF_considerar)!=0) {
      tmp = ""     
      for(lin in 1:nrow(DF_considerar)){
        tmpLin = ""
        for(col in 1:ncol(DF_considerar)){
          tmpLin <- paste0(tmpLin,DF_considerar[lin,col],"\t")
        } 
        tmpLin <- substr(tmpLin,1,nchar(tmpLin)-1) # tira o ultimo "\t" da string
        tmp    <- paste0(tmp,tmpLin,"\n")
      }
    }
    updateAceEditor(session, "uiEntrada_ace_dicot", value=tmp)

    # mostra as mensagens de erros.
    txtErro = ""
    if (nrow(DF_erros)==0) {
      txtErro = paste0(txtErro,"Nenhum erro foi encontrado")
    } else {
      for (x in 1:nrow(DF_erros)) {
        txtErro = paste(txtErro,DF_erros$desc[x], sep = '<br/>')
      }
    }
    output$uiEntrada_txt_Texto <- renderUI({ HTML(txtErro) })  

  })
  
  
  #----------------------------------------------------
  # Caixa de informação
  #----------------------------------------------------
  output$uiEntrada_info_ace <- renderValueBox({

    # aproveitando o evento para desabilitar os botoes de download das imagens na Exportação
    disable(id="uiExpGraf_btn_HIST_png")
    disable(id="uiExpGraf_btn_BOX_png")
    disable(id="uiExpGraf_btn_CIT_png")
    disable(id="uiExpGraf_btn_CCIUNICA_png")
    disable(id="uiExpGraf_btn_CCI_png_dicot")
    disable(id="uiExpGraf_btn_CCI_png_polit")
    disable(id="uiExpGraf_btn_CIIUNICA_png")
    disable(id="uiExpGraf_btn_CII_png_dicot")
    disable(id="uiExpGraf_btn_CII_png_polit")
    disable(id="uiExpGraf_btn_CIIUNICA_polit_png")
    disable(id="uiExpGraf_btn_CII_t_png_polit")


    # aproveitando o evento para habilitar/desabilitar abas
    if (GLOBAL_Dados$tipo == "N") {
      hideTab(inputId = "tabsNavBar", target = "Análises descritivas")
      hideTab(inputId = "tabsNavBar", target = "Unidimensionalidade")
      hideTab(inputId = "tabsNavBar", target = "Ajuste do modelo")    # dicotomico
      hideTab(inputId = "tabsNavBar", target = "Presença de DIF")     # dicotomico
      hideTab(inputId = "tabsNavBar", target = "Ajuste do modelo ")   # politomico A DIFERENÇA É UM ESPAÇO NO FINAL
      hideTab(inputId = "tabsNavBar", target = "Presença de DIF ")    # politomico A DIFERENÇA É UM ESPAÇO NO FINAL
      hideTab(inputId = "tabsNavBar", target = "Exportação de dados")
    }
    else {
      showTab(inputId = "tabsNavBar", target = "Análises descritivas")
      showTab(inputId = "tabsNavBar", target = "Unidimensionalidade")
      showTab(inputId = "tabsNavBar", target = "Exportação de dados")
      if (GLOBAL_Dados$tipo == "D") { 
        showTab(inputId = "tabsNavBar", target = "Ajuste do modelo")    # dicotomico
        showTab(inputId = "tabsNavBar", target = "Presença de DIF")     # dicotomico
        hideTab(inputId = "tabsNavBar", target = "Ajuste do modelo ")   # politomico A DIFERENÇA É UM ESPAÇO NO FINAL
        hideTab(inputId = "tabsNavBar", target = "Presença de DIF ")    # politomico A DIFERENÇA É UM ESPAÇO NO FINAL
      }
      if (GLOBAL_Dados$tipo == "P") {
        hideTab(inputId = "tabsNavBar", target = "Ajuste do modelo")    # dicotomico
        hideTab(inputId = "tabsNavBar", target = "Presença de DIF")     # dicotomico
        showTab(inputId = "tabsNavBar", target = "Ajuste do modelo ")   # politomico A DIFERENÇA É UM ESPAÇO NO FINAL
        showTab(inputId = "tabsNavBar", target = "Presença de DIF ")    # politomico A DIFERENÇA É UM ESPAÇO NO FINAL
      }
    }
    
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados", style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  

  output$uiDesc_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiUni_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiDicot_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiPolit_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiDIFLog_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiDIFLord_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiDIFLogit_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiDIFLogitAdj_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  output$uiExp_info_ace <- renderValueBox({
    param <- parametrosValueBox()
    valueBox(value=tags$p("Dados",style=ESTILO_VALUEBOX),color=param[[2]],subtitle=param[[1]])
  })  
  
  
  #----------------------------------------------------
  # ANALISES DESCRITIVAS: 
  #----------------------------------------------------
  descritivas <- reactive({
    DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
    banco = extraiGrupo(DF)[[1]]
    return(calculadescritivas(banco))
  })
  output$uiDesc_txt_Texto    <- renderText ({ str_glue(TEXTO_DESCRITIVAS,
                                                       NQ=descritivas()[[1]][[1]],
                                                       NR=descritivas()[[1]][[2]],
                                                       NF=descritivas()[[1]][[3]])})
  output$uiDesc_txt_Texto2    <- renderText ({ str_glue(TEXTO_DESCRITIVAS,
                                                       NQ=descritivas()[[1]][[1]],
                                                       NR=descritivas()[[1]][[2]],
                                                       NF=descritivas()[[1]][[3]])})
  output$uiDesc_dt_Faltantes <- renderDataTable({ datatable(descritivas()[[2]],option=DT_SEMPAGINACAO, editable=FALSE) })
  output$uiDesc_dt_Respostas <- renderDataTable({ datatable(descritivas()[[3]],option=DT_TRADUZIDOS, editable=FALSE) })
  output$uiDesc_dt_Alfa      <- renderDataTable({ datatable(descritivas()[[4]],option=DT_TRADUZIDOS, editable=FALSE) })
  

  #----------------------------------------------------
  # UNIDIMENSIONALIDADE 
  #----------------------------------------------------
  unidimen <- reactive({
    DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
    banco = extraiGrupo(DF)[[1]]
    return(verificaunidimen(banco))
  })
  output$uiUni_txt_Texto <- renderText ({ str_glue(TEXTO_UNI,
                                                   TM   = unidimen()[[1]],
                                                   TR   = unidimen()[[2]],
                                                   PERC = unidimen()[[3]],
                                                   NAO  = unidimen()[[4]],
                                                   MAIOR= unidimen()[[5]]) })  
  
  #----------------------------------------------------
  # PRESENÇA DE DIF - MÉTODO REGRESSAO LOGISTICA
  #----------------------------------------------------
  difLog <- reactive({
    
    mens = paste("Tipo:",input$uiDIFLog_rb_tipo,"- Método:",input$uiDIFLog_si_correcao,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
    
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      gruposExtraidos <- extraiGrupo(DF)

      # argumentos da funcao
      banco    <- gruposExtraidos[[1]]
      grupo    <- gruposExtraidos[[2]]
      tipo     <- input$uiDIFLog_rb_tipo
      correcao <- input$uiDIFLog_si_correcao

      # verifica se existe grupo.
      if (is.null(grupo)) {
          # não tem grupo na base de dados
          diflog = list(NULL,NULL,NULL,TEXTO_SEM_GRUPO,NULL)      
      } else {
          # chama a funcao porque tem grupo
          diflog = verificadiflog(banco, grupo, tipo, correcao )
      }
      return (diflog)
    })
  })
  
  output$uiDIFLog_dt_Resultados <- renderDataTable({ datatable(difLog()[[2]],option=DT_TRADUZIDOS) })
  output$uiDIFLog_txt_Texto1    <- renderText     ({           difLog()[[4]]  })                                                       
  output$uiDIFLog_txt_Texto2    <- renderText     ({           difLog()[[4]]  })                                                       
  output$uiDIFLog_Graficos      <- renderPlotly   ({           difLog()[[3]]  })                                                       
  
  
  #----------------------------------------------------
  # PRESENÇA DE DIF - MÉTODO DE LORD
  #----------------------------------------------------
  difLord <- reactive({
    
    mens = paste("Modelo:",input$uiDIFLord_rb_modelo,"- Correção:",input$uiDIFLord_si_correcao,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      gruposExtraidos <- extraiGrupo(DF)

      # argumentos da funcao
      banco    <- gruposExtraidos[[1]]
      grupo    <- gruposExtraidos[[2]]
      modelo   <- input$uiDIFLord_rb_modelo
      correcao <- input$uiDIFLord_si_correcao
    
      # verifica se existe grupo.
      if (is.null(grupo)) {
        # não tem grupo na base de dados
        diflord = list(NULL,NULL,NULL,TEXTO_SEM_GRUPO,NULL)      
      } else {
        # chama a funcao porque tem grupo
        diflord = verificadiflord(banco, grupo, modelo, correcao )
      }
      return (diflord)
    })
    
  })
  output$uiDIFLord_dt_Resultados <- renderDataTable({ datatable(difLord()[[2]],option=DT_TRADUZIDOS) })
  output$uiDIFLord_txt_Texto1     <- renderText ({           difLord()[[4]]  })                                                       
  output$uiDIFLord_txt_Texto2     <- renderText ({           difLord()[[4]]  })                                                       
  output$uiDIFLord_Graficos       <- renderPlotly ({         difLord()[[3]]  })                                                       
  
  #----------------------------------------------------
  # PRESENÇA DE DIF - MÉTODO LOGIT CUMULATIVO
  #----------------------------------------------------
  difLogit <- reactive({
    
    mens = paste("Tipo:",input$uiDIFLogit_rb_tipo,"- Método:",input$uiDIFLogit_si_correcao,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      gruposExtraidos <- extraiGrupo(DF)
      
      # argumentos da funcao
      banco      <- gruposExtraidos[[1]]
      grupo      <- gruposExtraidos[[2]]
      tipo       <- input$uiDIFLogit_rb_tipo
      correcao   <- input$uiDIFLogit_si_correcao
      categ      <- categoriasPolit(banco)
      modelo     <- "C"  # "cumulative"
      
      # verifica se existe grupo.
      if (is.null(grupo)) {
        # não tem grupo na base de dados
        diflogit = list(NULL,NULL,NULL,TEXTO_SEM_GRUPO,NULL,NULL)      
      } else {
        # chama a funcao porque tem grupo
        diflogit = verificadifcumlogit(banco, grupo, tipo, correcao, categ, modelo )
      }
      return (diflogit)
    })
  })
  
  output$uiDIFLogit_dt_Resultados <- renderDataTable({ datatable(difLogit()[[2]],option=DT_TRADUZIDOS) })
  output$uiDIFLogit_txt_Texto1    <- renderText     ({           difLogit()[[4]]  })                                                       
  output$uiDIFLogit_txt_Texto2    <- renderText     ({           difLogit()[[4]]  })                                                       
  output$uiDIFLogit_Graficos      <- renderPlotly   ({           difLogit()[[3]]  })                                                       
  
  #----------------------------------------------------
  # PRESENÇA DE DIF - MÉTODO LOGIT ADJACENTE
  #----------------------------------------------------
  difLogitAdj <- reactive({
    
    mens = paste("Tipo:",input$uiDIFLogitAdj_rb_tipo,"- Método:",input$uiDIFLogitAdj_si_correcao,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      gruposExtraidos <- extraiGrupo(DF)
      
      # argumentos da funcao
      banco     <- gruposExtraidos[[1]]
      grupo     <- gruposExtraidos[[2]]
      tipo      <- input$uiDIFLogitAdj_rb_tipo
      correcao  <- input$uiDIFLogitAdj_si_correcao
      categ     <- categoriasPolit(banco)
      modelo    <- "A"  # adjcent
      
      # verifica se existe grupo.
      if (is.null(grupo)) {
        # não tem grupo na base de dados
        diflogitAdj = list(NULL,NULL,NULL,TEXTO_SEM_GRUPO,NULL,NULL)      
      } else {
        # chama a funcao porque tem grupo
        diflogitAdj = verificadifcumlogit(banco, grupo, tipo, correcao, categ, modelo )
      }
      return (diflogitAdj)
    })
  })
  
  output$uiDIFLogitAdj_dt_Resultados <- renderDataTable({ datatable(difLogitAdj()[[2]],option=DT_TRADUZIDOS) })
  output$uiDIFLogitAdj_txt_Texto1    <- renderText     ({           difLogitAdj()[[4]]  })                                                       
  output$uiDIFLogitAdj_txt_Texto2    <- renderText     ({           difLogitAdj()[[4]]  })                                                       
  output$uiDIFLogitAdj_Graficos      <- renderPlotly   ({           difLogitAdj()[[3]]  })                                                       
  
  
  #----------------------------------------------------
  # AJUSTE DO MODELO DICOTOMICO
  #----------------------------------------------------
  resultmodelo_Dicot <- reactive({
    
    mens = paste("Modelo:",input$uiMod_Dicot_rb_modelo,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      # gera dataframe
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      banco      = extraiGrupo(DF)[[1]]
      nomesitens = colnames(banco)
      tipomodelo = input$uiMod_Dicot_rb_modelo

      # atualiza combo dos itens para a selecao da retirada do ajuste do modelo
      updateSelectInput(session, "uiMod_Dicot_si_itens", choices=c(as.character(nomesitens)))

      # calcula e retorna
      modelo         = ajustamodelo(banco, tipomodelo)
      tabhabilidades = tabulahabilidades(modelo, banco)
      # salva para a exportacao
      GLOBAL_CALCULO_MOD$modelo = modelo        
      GLOBAL_CALCULO_TL$dados   = tabhabilidades
      GLOBAL_CALCULO_COEF$itens    = NULL
      GLOBAL_CALCULO_HIST$graf     = NULL
      GLOBAL_CALCULO_HIST_ALT$graf = NULL
      GLOBAL_CALCULO_BOX_ALT$graf  = NULL
      GLOBAL_CALCULO_CIT_ALT$graf  = NULL
      GLOBAL_CALCULO_CI_DICOT$graf = NULL    
      GLOBAL_CALCULO_CI_POLIT$graf = NULL    
      GLOBAL_CALCULO_CI$nomeitens  = NULL     
      GLOBAL_CALCULO_CCIU_ALT$graf = NULL     
      GLOBAL_CALCULO_CCI_ALT$graf  = NULL     
      GLOBAL_CALCULO_CIIU_ALT$graf = NULL     
      GLOBAL_CALCULO_CII_ALT$graf  = NULL     
      GLOBAL_CALCULO_CII_T_ALT$graf  = NULL     
      GLOBAL_CALCULO_CII_TU_ALT$graf = NULL     
      
      return(list(tabhabilidades))
    })
    
  })
  
  observeEvent(input$uiMod_Dicot_btn_Ajustar, {
    
    mens = paste("Ajustando modelo:",input$uiMod_Dicot_rb_modelo,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      # converte o vetor de itens excluidos I4, I6, I7 (exemplo) em 4, 6, 7
      vetCol <- input$uiMod_Dicot_si_itens
      if (!is.null(vetCol)) {
        vetCol <- sub("I", "", vetCol) # retira a string "I"
        vetCol <- as.numeric(vetCol)   # converte em numeric
      }
            
      # gera dataframe
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      banco = extraiGrupo(DF)[[1]]
      if (!is.null(vetCol)) banco = banco[,-vetCol] # retira as colunas dos elementos selecionados
      nomesitens = colnames(banco)
      tipomodelo = input$uiMod_Dicot_rb_modelo

      # Calcula tudo
      modelo         = ajustamodelo(banco, tipomodelo)
      tabitens       = tabulacoefitens(modelo, nomesitens,"dicot",1,NULL)
      tabhabilidades = tabulahabilidades(modelo, banco)
      histbox        = plotdisthabilidades(modelo)
      curvasmodelo   = plotcurvasgerais(modelo)
      curvasitens    = plotcurvasitensdicot(modelo, nomesitens)
      # salva para a exportacao
      GLOBAL_CALCULO_MOD$modelo = modelo          
      GLOBAL_CALCULO_TL$dados   = tabhabilidades  
      GLOBAL_CALCULO_COEF$itens = tabitens        
      GLOBAL_CALCULO_HIST$graf     = histbox      
      GLOBAL_CALCULO_HIST_ALT$graf = histbox[[1+2]] 
      GLOBAL_CALCULO_BOX_ALT$graf  = histbox[[2+2]] 
      GLOBAL_CALCULO_CIT_ALT$graf  = curvasmodelo[[2+1]]
      GLOBAL_CALCULO_CI_DICOT$graf = curvasitens     
      GLOBAL_CALCULO_CI_POLIT$graf = NULL     
      GLOBAL_CALCULO_CCIU_ALT$graf = curvasitens[[1+4]]     
      GLOBAL_CALCULO_CCI_ALT$graf  = curvasitens[[3+4]][[1]]
      GLOBAL_CALCULO_CIIU_ALT$graf = curvasitens[[2+4]]     
      GLOBAL_CALCULO_CII_ALT$graf  = curvasitens[[4+4]][[1]]     
      GLOBAL_CALCULO_CII_T_ALT$graf = NULL
      GLOBAL_CALCULO_CII_TU_ALT$graf= NULL
      
      # atualiza a lista de opções do combo de itens
      itens = sub("I", "Item ", nomesitens)
      updateSelectInput(session, "uiExpGraf_si_cci_itens_dicot", choices=itens)
      updateSelectInput(session, "uiExpGraf_si_cii_itens_dicot", choices=itens)
      GLOBAL_CALCULO_CI$nomeitens = itens
      
    })
    
    mens = paste("Plotando gráficos...") 
    withProgress(message = mens, value=0, {
      incProgress()
      # plota tudo
      output$uiMod_Dicot_dt_Habilidades  <- renderDataTable({ datatable(tabhabilidades,option=DT_TRADUZIDOS) })
      output$uiMod_Dicot_dt_Coeficientes <- renderDataTable({ datatable(tabitens,      option=DT_TRADUZIDOS) })
      
      output$uiMod_Dicot_py_histograma   <- renderPlotly   ({histbox[[1]] })
      output$uiExpGraf_Hist              <- renderPlotly   ({histbox[[1+2]] })

      output$uiMod_Dicot_py_boxplot      <- renderPlotly   ({histbox[[2]] })
      output$uiExpGraf_Box               <- renderPlotly   ({histbox[[2+2]] })

      output$uiMod_Dicot_py_cit          <- renderPlotly   ({curvasmodelo[[2]] })
      output$uiExpGraf_cit               <- renderPlotly   ({curvasmodelo[[2+1]] }) # é a versao "save" do grafico :-)

      output$uiMod_Dicot_py_cciunica     <- renderPlotly   ({curvasitens[[1]] })
      output$uiExpGraf_cciunica          <- renderPlotly   ({curvasitens[[1+4]] }) # é a versao "save" do grafico :-)
      
      output$uiMod_Dicot_py_ciiunica     <- renderPlotly   ({curvasitens[[2]] })
      output$uiExpGraf_ciiunica          <- renderPlotly   ({curvasitens[[2+4]] }) # é a versao "save" do grafico :-)
      
      output$uiMod_Dicot_py_ccimosaico   <- renderPlotly   ({curvasitens[[3]] })        # subplot
      output$uiExpGraf_cci_dicot         <- renderPlotly   ({curvasitens[[3+4]][[1]] }) # vetor de graficos
      
      output$uiMod_Dicot_py_ciimosaico   <- renderPlotly   ({curvasitens[[4]] })        # subplot
      output$uiExpGraf_cii_dicot         <- renderPlotly   ({curvasitens[[4+4]][[1]] }) # vetor de graficos
    })
    
  })
  
  output$uiMod_Dicot_txt_Texto1      <- renderText ({ if (difLog()[[4]]  != "") paste0("Pelo método de regressão logística: ",difLog()[[4]]) })                                                       
  output$uiMod_Dicot_txt_Texto2      <- renderText ({ if (difLord()[[4]] != "") paste0("Pelo método de Lord: ",difLord()[[4]]) })   
  output$uiMod_Dicot_dt_Habilidades  <- renderDataTable({ datatable(resultmodelo_Dicot()[[1]],option=DT_TRADUZIDOS) })

  #----------------------------------------------------
  # AJUSTE DO MODELO POLITOMICO
  #----------------------------------------------------
  resultmodelo_Polit <- reactive({
    
    mens = paste("Modelo:",input$uiMod_Polit_rb_modelo,"...") 
    withProgress(message = mens, value=0, {
      incProgress()

      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      banco      = extraiGrupo(DF)[[1]]
      nomesitens = colnames(banco)
      tipomodelo = input$uiMod_Polit_rb_modelo

      # atualiza combo dos itens para a selecao da retirada do ajuste do modelo
      updateSelectInput(session, "uiMod_Polit_si_itens", choices=c(as.character(nomesitens)))
    
      #  banco <- retiraColunas(banco) FAZER AINDA
      modelo         = ajustamodelo(banco, tipomodelo)
      tabhabilidades = tabulahabilidades(modelo, banco)
      # salva para a exportacao
      GLOBAL_CALCULO_MOD$modelo = modelo        
      GLOBAL_CALCULO_TL$dados   = tabhabilidades
      GLOBAL_CALCULO_COEF$itens    = NULL
      GLOBAL_CALCULO_HIST$graf     = NULL
      GLOBAL_CALCULO_HIST_ALT$graf = NULL
      GLOBAL_CALCULO_BOX_ALT$graf  = NULL
      GLOBAL_CALCULO_CIT_ALT$graf  = NULL
      GLOBAL_CALCULO_CI_DICOT$graf = NULL    
      GLOBAL_CALCULO_CI_POLIT$graf = NULL    
      GLOBAL_CALCULO_CI$nomeitens  = NULL     
      GLOBAL_CALCULO_CCIU_ALT$graf = NULL     
      GLOBAL_CALCULO_CCI_ALT$graf  = NULL     
      GLOBAL_CALCULO_CIIU_ALT$graf = NULL     
      GLOBAL_CALCULO_CII_ALT$graf  = NULL     
      GLOBAL_CALCULO_CII_T_ALT$graf = NULL     
      GLOBAL_CALCULO_CII_TU_ALT$graf= NULL     
      return(list(tabhabilidades))
    })
  })
  
  observeEvent(input$uiMod_Polit_btn_Ajustar, {

    mens = paste("Ajustando modelo:",input$uiMod_Polit_rb_modelo,"...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      # converte o vetor de itens excluidos I4, I6, I7 (exemplo) em 4, 6, 7
      vetCol <- input$uiMod_Polit_si_itens
      if (!is.null(vetCol)) {
        vetCol <- sub("I", "", vetCol) # retira a string "I"
        vetCol <- as.numeric(vetCol)   # converte em numeric
      }
      
      # gera dataframe
      DF <- read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      banco = extraiGrupo(DF)[[1]]
      if (!is.null(vetCol)) banco = banco[,-vetCol] # retira as colunas dos elementos selecionados
      nomesitens = colnames(banco)
      tipomodelo = input$uiMod_Polit_rb_modelo
      categ      = categoriasPolit(banco)
      
      # Calcula tudo
      cod_modelo     = input$uiMod_Polit_rb_modelo
      modelo         = ajustamodelo(banco, tipomodelo)
      tabitens       = tabulacoefitens(modelo, nomesitens,"polit",categ, cod_modelo)
      tabhabilidades = tabulahabilidades(modelo, banco)
      histbox        = plotdisthabilidades(modelo)
      curvasmodelo   = plotcurvasgerais(modelo)
      curvasitens    = plotcurvasitenspolit(modelo, nomesitens, categ)

      # salva para a exportacao
      GLOBAL_CALCULO_MOD$modelo = modelo          
      GLOBAL_CALCULO_TL$dados   = tabhabilidades  
      GLOBAL_CALCULO_COEF$itens = tabitens        
      GLOBAL_CALCULO_HIST$graf     = histbox      
      GLOBAL_CALCULO_HIST_ALT$graf = histbox[[1+2]] 
      GLOBAL_CALCULO_BOX_ALT$graf  = histbox[[2+2]] 
      GLOBAL_CALCULO_CIT_ALT$graf  = curvasmodelo[[2+1]]
      GLOBAL_CALCULO_CI_DICOT$graf = NULL     
      GLOBAL_CALCULO_CI_POLIT$graf = curvasitens     
      GLOBAL_CALCULO_CCIU_ALT$graf = NULL   # no Polit não tem      
      GLOBAL_CALCULO_CCI_ALT$graf  = curvasitens[[1+2]][[1]]
      GLOBAL_CALCULO_CIIU_ALT$graf = NULL   # no Polit não tem     
      GLOBAL_CALCULO_CII_ALT$graf  = curvasitens[[2+2]][[1]]     
      GLOBAL_CALCULO_CII_T_ALT$graf = curvasitens[[6]][[1]]     
      GLOBAL_CALCULO_CII_TU_ALT$graf= curvasitens[[8]][[1]]     
      
      # atualiza a lista de opções do combo de itens
      itens = sub("I", "Item ", nomesitens)
      updateSelectInput(session, "uiExpGraf_si_cci_itens_polit", choices=itens)
      updateSelectInput(session, "uiExpGraf_si_cii_itens_polit", choices=itens)
      updateSelectInput(session, "uiExpGraf_si_cii_t_itens_polit", choices=itens)
      GLOBAL_CALCULO_CI$nomeitens = itens
      
    })
    
    mens = paste("Plotando gráficos...") 
    withProgress(message = mens, value=0, {
      incProgress()
      
      # plota tudo
      output$uiMod_Polit_dt_Habilidades  <- renderDataTable({ datatable(tabhabilidades,option=DT_TRADUZIDOS) })
      output$uiMod_Polit_dt_Coeficientes <- renderDataTable({ datatable(tabitens,      option=DT_TRADUZIDOS) })

      output$uiMod_Polit_py_histograma   <- renderPlotly   ({histbox[[1]] })
      output$uiExpGraf_Hist              <- renderPlotly   ({histbox[[1+2]] })

      output$uiMod_Polit_py_boxplot      <- renderPlotly   ({histbox[[2]] })
      output$uiExpGraf_Box               <- renderPlotly   ({histbox[[2+2]] })
      
      output$uiMod_Polit_py_cit          <- renderPlotly   ({curvasmodelo[[2]] })
      output$uiExpGraf_cit               <- renderPlotly   ({curvasmodelo[[2+1]] }) # é a versao "save" do grafico :-)
      
      output$uiMod_Polit_py_cci          <- renderPlotly   ({curvasitens[[1]] })
      output$uiExpGraf_cci_polit         <- renderPlotly   ({curvasitens[[1+2]][[1]] }) # vetor de graficos
      
      output$uiMod_Polit_py_cii          <- renderPlotly   ({curvasitens[[2]] }) 
      output$uiMod_Polit_py_cii_t        <- renderPlotly   ({curvasitens[[5]] }) 
      output$uiMod_Polit_py_ciiunica     <- renderPlotly   ({curvasitens[[7]] }) 
            
      output$uiExpGraf_cii_polit         <- renderPlotly   ({curvasitens[[2+2]][[1]] }) # vetor de graficos
      output$uiExpGraf_cii_t_polit       <- renderPlotly   ({curvasitens[[6]][[1]] })   # vetor de graficos
      output$uiExpGraf_ciiunica_polit    <- renderPlotly   ({curvasitens[[8]][[1]] })   # vetor de graficos
      
      output$uiExpGraf_cciunica          <- renderPlotly   ({ NULL })              # NO POLIT NAO TEM
      output$uiExpGraf_ciiunica          <- renderPlotly   ({ NULL })              # NO POLIT NAO TEM
    })
  })
  
  output$uiMod_Polit_txt_Texto1 <- renderText ({ if (difLogit()[[4]]    != "") paste0("Pelo Logit cumulativo: ",difLogit()[[4]]) })                                                       
  output$uiMod_Polit_txt_Texto2 <- renderText ({ if (difLogitAdj()[[4]] != "") paste0("Pelo Logit adjacente.: ",difLogitAdj()[[4]]) })
  output$uiMod_Polit_dt_Habilidades  <- renderDataTable({ datatable(resultmodelo_Polit()[[1]],option=DT_TRADUZIDOS) })

  #----------------------------------------------------
  # EXPORTACAO: tabelas
  #----------------------------------------------------
  exportacaoTxt <- reactive({
    
    # habilita/desabilita box das abas dos graficos
    if (GLOBAL_Dados$tipo == "D") {
      showTab(inputId = "abasExportaGraficos", target = "Curva característica do item")  # dicot
      showTab(inputId = "abasExportaGraficos", target = "Curva informação do item")      # dicot
      hideTab(inputId = "abasExportaGraficos", target = "Curva característica do item ") # polit (tem um espaço ao final)
      hideTab(inputId = "abasExportaGraficos", target = "Curva informação do item ")     # polit (tem um espaço ao final)
    }
    if (GLOBAL_Dados$tipo == "P") {
      hideTab(inputId = "abasExportaGraficos", target = "Curva característica do item")  # dicot
      hideTab(inputId = "abasExportaGraficos", target = "Curva informação do item")      # dicot
      showTab(inputId = "abasExportaGraficos", target = "Curva característica do item ") # polit (tem um espaço ao final)
      showTab(inputId = "abasExportaGraficos", target = "Curva informação do item ")     # polit (tem um espaço ao final)
    }
    
    # obtem a string do código selecionado no combo
    if (input$uiMod_Dicot_rb_modelo=="Rasch") modeloDicot="RASCH"
    if (input$uiMod_Dicot_rb_modelo=="1PL")   modeloDicot="Logístico de 1 parâmetro  (1PL)"
    if (input$uiMod_Dicot_rb_modelo=="2PL")   modeloDicot="Logístico de 2 parâmetros (2PL)"
    if (input$uiMod_Dicot_rb_modelo=="3PL")   modeloDicot="Logístico de 3 parâmetros (3PL)"
    #
    if (input$uiMod_Polit_rb_modelo=="graded")  modeloPolit="Politômico ordinal de resposta gradual (graded)" 
    if (input$uiMod_Polit_rb_modelo=="rsm")     modeloPolit="Escala gradual (rsm)"
    if (input$uiMod_Polit_rb_modelo=="Rasch")   modeloPolit="Crédito parcial (Rasch)"
    if (input$uiMod_Polit_rb_modelo=="gpcmIRT") modeloPolit="Crédito parcial generalizado (gpcmIRT)"
    
    txt1 = "Após o ajuste do modelo { TIPO } '{ MODELO }', os dados da matriz de entrada são juntados às estimativas dos 
           traços latentes dos respondentes e podem ser exportados num arquivo CSV para download aqui."
    if (GLOBAL_Dados$tipo == "N") txt1 <- str_glue (txt1, TIPO="indefinido",         MODELO="??") 
    if (GLOBAL_Dados$tipo == "D") txt1 <- str_glue (txt1, TIPO="dicotômico",         MODELO=modeloDicot)
    if (GLOBAL_Dados$tipo == "P") txt1 <- str_glue (txt1, TIPO="politômico ordinal", MODELO=modeloPolit)
    
    txt2 = "Após o ajuste do modelo { TIPO } '{ MODELO }', os coeficientes estimados dos itens podem ser exportados num 
    arquivo CSV para download aqui{ EXCLUIDOS }"
    if (GLOBAL_Dados$tipo == "D") txtItens <- listaStr(input$uiMod_Dicot_si_itens)
    if (GLOBAL_Dados$tipo == "P") txtItens <- listaStr(input$uiMod_Polit_si_itens)
    if (txtItens == "") txtExclu <- "." else txtExclu <- paste0(", já excluídos os itens ",txtItens)
    
    if (GLOBAL_Dados$tipo == "N") txt2 <- str_glue(txt2, TIPO="indefinido",         MODELO="??"       , EXCLUIDOS=txtExclu) 
    if (GLOBAL_Dados$tipo == "D") txt2 <- str_glue(txt2, TIPO="dicotômico",         MODELO=modeloDicot, EXCLUIDOS=txtExclu)
    if (GLOBAL_Dados$tipo == "P") txt2 <- str_glue(txt2, TIPO="politômico ordinal", MODELO=modeloPolit, EXCLUIDOS=txtExclu)
    
    return(list(txt1, txt2))
  })
  output$uiExpTab_txt_Dados <- renderText({ exportacaoTxt()[[1]]  })
  output$uiExpTab_txt_Itens <- renderText({ exportacaoTxt()[[2]]  })
  
  
  output$uiExpTab_btn_TL <- downloadHandler(
    filename = function() { ARQ_EXP_CALC_DADOS },
    content  = function(file) {
      # prepara e grava os dados do Traço Latente
      MatDicot = read.csv(text=input$uiEntrada_ace_dicot, sep="\t")
      DF       = cbind(MatDicot,GLOBAL_CALCULO_TL$dados) 
      write.csv(DF,file)
  })

  output$uiExpTab_btn_COEF <- downloadHandler(
    filename = function() { ARQ_EXP_CALC_ITENS },
    content  = function(file) {
      write.csv(GLOBAL_CALCULO_COEF$itens,file)
  })

  #----------------------------------------------------
  # EXPORTACAO: gráficos
  #----------------------------------------------------

  # histograma
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_Hist_Aplicar, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_HIST$graf[[1+2]]
    x       = input$uiExpGraf_txt_Hist_X
    y       = input$uiExpGraf_txt_Hist_Y
    
    # redesenha o grafico
    output$uiExpGraf_Hist <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_HIST_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
    
  })
  observeEvent(input$uiExpGraf_btn_Hist_Padrao, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_HIST$graf[[1+2]]
    x       = PLOTDISTHABILIDADADES_HIST_X
    y       = PLOTDISTHABILIDADADES_HIST_Y
    
    # redesenha o grafico
    output$uiExpGraf_Hist <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_HIST_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_HIST_png <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_HIST_PNG },
    content =function(f) { export(GLOBAL_CALCULO_HIST_ALT$graf, file=f) }
  )
  output$uiExpGraf_btn_HIST_pdf <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_HIST_PDF },
    content =function(f) { export(GLOBAL_CALCULO_HIST_ALT$graf, file=f) }
  )
  
  # boxPlot
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_Box_Aplicar, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_HIST$graf[[2+2]]
    x       = input$uiExpGraf_txt_Box_X
    y       = NULL
    
    # redesenha o grafico
    output$uiExpGraf_Box <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_BOX_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_Box_Padrao, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_HIST$graf[[2+2]]
    x       = PLOTDISTHABILIDADADES_BOX_X
    y       = NULL
    
    # redesenha o grafico
    output$uiExpGraf_Box <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_BOX_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_BOX_png <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_BOX_PNG },
    content =function(f) { export(GLOBAL_CALCULO_BOX_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_BOX_pdf <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_BOX_PDF },
    content =function(f) { export(GLOBAL_CALCULO_BOX_ALT$graf, file=f)}
  )
  
  # modelo
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cit_Aplicar, {
    # ajusta os argumentos da funcao
    modelo= GLOBAL_CALCULO_MOD$modelo
    x     = input$uiExpGraf_txt_cit_X
    y     = input$uiExpGraf_txt_cit_Y
    s1    = input$uiExpGraf_txt_cit_s1
    s2    = input$uiExpGraf_txt_cit_s2
    
    # redesenha o grafico
    output$uiExpGraf_cit <- renderPlotly({ 
      tryCatch({ g = plotcurvasgerais(modelo,x,y,s1,s2)[[3]] 
                 GLOBAL_CALCULO_CIT_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_cit_Padrao, {
    # ajusta os argumentos da funcao
    modelo= GLOBAL_CALCULO_MOD$modelo
    x     = PLOTCURVASGERAIS_X
    y     = PLOTCURVASGERAIS_Y
    s1    = PLOTCURVASGERAIS_S1
    s2    = PLOTCURVASGERAIS_S2
    
    # redesenha o grafico
    output$uiExpGraf_cit <- renderPlotly({ 
      tryCatch({ g = plotcurvasgerais(modelo,x,y,s1,s2)[[3]] 
                 GLOBAL_CALCULO_CIT_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CIT_png <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CIT_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CIT_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CIT_pdf <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CIT_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CIT_ALT$graf, file=f)}
  )
  
  # curva caract dos itens - todas as curvas - Apenas DICOT tem
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cciunica_Aplicar, {
      # ajusta os argumentos da funcao
      grafico = GLOBAL_CALCULO_CI_DICOT$graf[[1+4]]
      x       = input$uiExpGraf_txt_cciunica_X
      y       = input$uiExpGraf_txt_cciunica_Y
      
      # redesenha o grafico
      output$uiExpGraf_cciunica <- renderPlotly({ 
        tryCatch({ g = editagrafico(grafico,x,y) 
                   GLOBAL_CALCULO_CCIU_ALT$graf = g
                   return(g) },
        error = function(e) {})
      })
  })
  observeEvent(input$uiExpGraf_btn_cciunica_Padrao, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[1+4]]
    x       = PLOTCURVASITENSDICOT_CCI_X
    y       = PLOTCURVASITENSDICOT_CCI_Y
    
    # redesenha o grafico
    output$uiExpGraf_cciunica <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CCIU_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CCIUNICA_png <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CCIU_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CCIU_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CCIUNICA_pdf <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CCIU_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CCIU_ALT$graf, file=f)}
  )
  
  # curva caract dos itens - escolher o item - do DICOT
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cci_Aplicar_dicot, {
    # ajusta os argumentos da funcao
    
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cci_itens_dicot,GLOBAL_CALCULO_CI$nomeitens)

    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[3+4]][[item]]
    x       = input$uiExpGraf_txt_cci_X_dicot
    y       = input$uiExpGraf_txt_cci_Y_dicot
    
    # redesenha o grafico
    output$uiExpGraf_cci_dicot <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CCI_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })

  })
  observeEvent(input$uiExpGraf_btn_cci_Padrao_dicot, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cci_itens_dicot,GLOBAL_CALCULO_CI$nomeitens)

    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[3+4]][[item]]
    x       = PLOTCURVASITENSDICOT_CCI_X
    y       = PLOTCURVASITENSDICOT_CCI_Y
    
    # redesenha o grafico
    output$uiExpGraf_cci_dicot <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CCI_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CCI_png_dicot <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CCI_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CCI_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CCI_pdf_dicot <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CCI_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CCI_ALT$graf, file=f)}
  )

  # curva caract dos itens - escolher o item - do POLIT
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cci_Aplicar_polit, {
    # ajusta os argumentos da funcao
    
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cci_itens_polit,GLOBAL_CALCULO_CI$nomeitens)
    
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[1+2]][[item]]
    x       = input$uiExpGraf_txt_cci_X_polit
    y       = input$uiExpGraf_txt_cci_Y_polit
    
    # redesenha o grafico
    output$uiExpGraf_cci_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CCI_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_cci_Padrao_polit, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cci_itens_polit,GLOBAL_CALCULO_CI$nomeitens)
    
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[1+2]][[item]]
    x       = PLOTCURVASITENSDICOT_CCI_X
    y       = PLOTCURVASITENSDICOT_CCI_Y
    
    # redesenha o grafico
    output$uiExpGraf_cci_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CCI_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CCI_png_polit <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CCI_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CCI_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CCI_pdf_polit <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CCI_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CCI_ALT$graf, file=f)}
  )
  

  # curva info dos itens - todas as curvas - dicot
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_ciiunica_Aplicar, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[2+4]]
    x       = input$uiExpGraf_txt_ciiunica_X
    y       = input$uiExpGraf_txt_ciiunica_Y
    
    # redesenha o grafico
    output$uiExpGraf_ciiunica <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CIIU_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_ciiunica_Padrao, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[2+4]]
    x       = PLOTCURVASITENSDICOT_CII_X
    y       = PLOTCURVASITENSDICOT_CII_Y
    
    # redesenha o grafico
    output$uiExpGraf_ciiunica <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CIIU_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CIIUNICA_png <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CIIU_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CIIU_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CIIUNICA_pdf <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CIIU_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CIIU_ALT$graf, file=f)}
  )
  
  # curva info dos itens - escolher o item - do DICOT
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cii_Aplicar_dicot, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cii_itens_dicot,GLOBAL_CALCULO_CI$nomeitens)
    
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[4+4]][[item]]
    x       = input$uiExpGraf_txt_cii_X_dicot
    y       = input$uiExpGraf_txt_cii_Y_dicot
    
    # redesenha o grafico
    output$uiExpGraf_cii_dicot <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                GLOBAL_CALCULO_CII_ALT$graf = g
                return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_cii_Padrao_dicot, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cii_itens_dicot,GLOBAL_CALCULO_CI$nomeitens)

    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_DICOT$graf[[4+4]][[item]]
    x       = PLOTCURVASITENSDICOT_CII_X
    y       = PLOTCURVASITENSDICOT_CII_Y
    
    # redesenha o grafico
    output$uiExpGraf_cii_dicot <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                GLOBAL_CALCULO_CII_ALT$graf = g
                return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CII_png_dicot <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CII_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CII_pdf_dicot <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CII_ALT$graf, file=f)}
  )

  
  # curva info dos itens - escolher o item - do Polit
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cii_Aplicar_polit, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cii_itens_polit,GLOBAL_CALCULO_CI$nomeitens)
    
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[2+2]][[item]]
    x       = input$uiExpGraf_txt_cii_X_polit
    y       = input$uiExpGraf_txt_cii_Y_polit
    
    # redesenha o grafico
    output$uiExpGraf_cii_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CII_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_cii_Padrao_polit, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cii_itens_polit,GLOBAL_CALCULO_CI$nomeitens)
    
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[2+2]][[item]]
    x       = PLOTCURVASITENSPOLIT_CII_X
    y       = PLOTCURVASITENSPOLIT_CII_Y
    
    # redesenha o grafico
    output$uiExpGraf_cii_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
                 GLOBAL_CALCULO_CII_ALT$graf = g
                 return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CII_png_polit <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CII_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CII_pdf_polit <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CII_ALT$graf, file=f)}
  )
  
  # curva info TOTAL dos itens - escolher o item - do Polit
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_cii_t_Aplicar_polit, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cii_t_itens_polit,GLOBAL_CALCULO_CI$nomeitens)
    
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[6]][[item]]
    x       = input$uiExpGraf_txt_cii_t_X_polit
    y       = input$uiExpGraf_txt_cii_t_Y_polit
    
    # redesenha o grafico
    output$uiExpGraf_cii_t_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
      GLOBAL_CALCULO_CII_T_ALT$graf = g
      return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_cii_t_Padrao_polit, {
    # retorna o indice da opção do combo
    item = match(input$uiExpGraf_si_cii_t_itens_polit,GLOBAL_CALCULO_CI$nomeitens)
    
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[2+2]][[item]]
    x       = PLOTCURVASITENSPOLIT_CII_X
    y       = PLOTCURVASITENSPOLIT_CII_Y
    
    # redesenha o grafico
    output$uiExpGraf_cii_t_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
      GLOBAL_CALCULO_CII_T_ALT$graf = g
      return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CII_t_png_polit <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CII_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CII_t_pdf_polit <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CII_ALT$graf, file=f)}
  )
  
  
  # curva info TOTAL dos itens - todas as curvas - do polit
  #- - - - - - - - - - - - - - - - - - - - - - - - 
  observeEvent(input$uiExpGraf_btn_ciiunica_polit_Aplicar, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[8]]
    x       = input$uiExpGraf_txt_ciiunica_polit_X
    y       = input$uiExpGraf_txt_ciiunica_polit_Y
    
    # redesenha o grafico
    output$uiExpGraf_ciiunica_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
      GLOBAL_CALCULO_CII_TU_ALT$graf = g
      return(g) },
      error = function(e) {})
    })
  })
  observeEvent(input$uiExpGraf_btn_ciiunica_polit_Padrao, {
    # ajusta os argumentos da funcao
    grafico = GLOBAL_CALCULO_CI_POLIT$graf[[8]]
    x       = PLOTCURVASITENSPOLIT_CII_X
    y       = PLOTCURVASITENSPOLIT_CII_Y
    
    # redesenha o grafico
    output$uiExpGraf_ciiunica_polit <- renderPlotly({ 
      tryCatch({ g = editagrafico(grafico,x,y) 
      GLOBAL_CALCULO_CII_TU_ALT$graf = g
      return(g) },
      error = function(e) {})
    })
  })
  output$uiExpGraf_btn_CIIUNICA_polit_png <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PNG },
    content =function(f) { export(GLOBAL_CALCULO_CIIU_ALT$graf, file=f)}
  )
  output$uiExpGraf_btn_CIIUNICA_pdf <- downloadHandler(
    filename=function()  { ARQ_EXP_CALC_CII_PDF },
    content =function(f) { export(GLOBAL_CALCULO_CIIU_ALT$graf, file=f)}
  )
  
  
}
