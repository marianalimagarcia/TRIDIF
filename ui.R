
ui <- 
  dashboardPage(

  skin = GLOBAL_COR_SKIN,
 
  dashboardHeader(title = "TRIDIF",
                  titleWidth = 450,
                  tags$li(class = "dropdown",
                          tags$style(HTML(GLOBAL_CSS_HEADER)),
                          tags$a(href="sobre.txt", target="_blank", tags$h5("Sobre"))
                  ),
                  tags$li(class = "dropdown",
                          tags$style(HTML(GLOBAL_CSS_HEADER)),
                          tags$a(href="ajuda-v.2.2.pdf", target="_blank",tags$h5("Ajuda"))
                  )
  ),

  dashboardSidebar(

    tags$style(HTML(GLOBAL_CSS_SIDEBAR)),
    disable = TRUE

  ),

  dashboardBody( 

    useShinyjs(),
    useShinyalert(),  

    tags$style(HTML(GLOBAL_CSS_BODY)),
    tags$style(HTML(GLOBAL_CSS_NAVBAR)),
    tags$style(HTML(GLOBAL_CSS_BOX)),
    tags$style(HTML(GLOBAL_CSS_SUBBOX)),
    tags$style(HTML(GLOBAL_CSS_ACE)),
    
    navbarPage(title="", id="tabsNavBar",
               
      # Menu Entrada de dados ----
      tabPanel(title="Entrada de dados",
               
        box(title="Matriz de dados", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
            column(width = 2, 
                   wellPanel(
                     #tags$li("Pode ser usado o '",em("copia e cola"),"'"),
                     tags$li("A coluna ",em(span("Grupo", style=DIV_DICOT))," deve ser a última coluna "),
                     tags$li("Valores faltantes devem ser", em("vazio")," ou NA")
                   )
            ),
            column(width = 3,
                   radioButtons(inputId="uiEntrada_rb_separador", label="Separador", selected=",", inline=T,
                                choices= c("Vírgula"         = ",",
                                           "Ponto e vírgula" = ";",
                                           "Tab"             = "\t")
                            ),
                   fileInput(inputId="uiEntrada_file_dicot", label="Selecione o arquivo CSV para upload ou copie e cole direto na matriz", multiple= FALSE,
                             buttonLabel = "Escolha",
                             placeholder = "Nenhum arquivo selecionado",
                             accept      = c("text/csv","text/comma-separated-values,text/plain",".csv")
                   )
                   
            ), #column

            column(width = 3,      
                   actionButton(inputId="uiEntrada_btn_LimparAceEditor",         label=" Limpar matriz ", icon=icon("columns")),
                   br(),
                   actionButton(inputId="uiEntrada_btn_Carregar_Dicot_AceEditor",label=" Carregar dados dicotômicos de exemplo",  icon=icon("clipboard-list")),
                   br(),
                   actionButton(inputId="uiEntrada_btn_Carregar_Polit_AceEditor",label=" Carregar dados politômicos de exemplo",  icon=icon("clipboard-list")),
                   br(),
                   actionButton(inputId="uiEntrada_btn_Verificar_AceEditor",     label=" Verificar matriz ",  icon=icon("search")),
                   br()
            ),
            column(width = 4,  valueBoxOutput("uiEntrada_info_ace")),
            
            column(width = 8, 
                   p(style=DIV_MENSAGEM, "Itens que serão considerados nas análises"),
                   aceEditor(outputId="uiEntrada_ace_dicot", value=ACE_EDITOR_DADOS_DEFAULT, readOnly=F, height="200px")),
            column(width = 4, 
                   p(style=DIV_MENSAGEM, "Itens que NÃO serão considerados nas análises"),
                   div(style=DIV_FONT, htmlOutput("uiEntrada_txt_Texto")))

        ), #box

        box(title="Código R", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
            column(width = 12,
                   wellPanel(p(""),code(style=ESTILO_CODIGO_R,includeText(NOME_ARQ_DADOS)))
            )
        )
      ), #tabPanel
      

      # Menu Análises descritivas ----
      tabPanel(title="Análises descritivas",
      
        box(title="Análises descritivas", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
            column(width = 4,valueBoxOutput("uiDesc_info_ace")),
            column(width = 8,wellPanel(div(style=DIV_MENSAGEM, width="100%", textOutput("uiDesc_txt_Texto")))),
            
            column(width = 12,
                   tabBox(title=" ", width="100%",
                          tabPanel("Tabela"," ",p(style=DIV_MENSAGEM, "Valores faltantes em cada item:"),
                                                div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDesc_dt_Faltantes"))),
                          tabPanel("Ajuda" ," ",AJUDA_DESC_FALT() )
                   )
            ),

            column(width = 12, 
                     column(width = 6,
                            tabBox(title=" ", width="100%",
                                   tabPanel("Tabela"," ",p(style=DIV_MENSAGEM, "Proporção de respostas"),
                                                         div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDesc_dt_Respostas"))),
                                   tabPanel("Ajuda" ," ",AJUDA_DESC_RESP() )
                            )
                     ),
                     column(width = 6,
                            tabBox(title=" ", width="100%",
                                   tabPanel("Tabela"," ",p(style=DIV_MENSAGEM, "Alfa de Cronbach"),
                                                         div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDesc_dt_Alfa"))),
                                   tabPanel("Ajuda" ," ",AJUDA_DESC_ALFA() )
                            )
                     )
            )
        ), #box
        
        box(title="Código R", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
            column(width = 12,
                   wellPanel(p(""),code(style=ESTILO_CODIGO_R,includeText(NOME_ARQ_DESCRITIVAS)))
            )
        )

      ), #tabPanel
               

      # Menu Unidimensionalidade ----
      tabPanel(title="Unidimensionalidade",

        box(title="Verificando a unidimensionalidade", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
            column(width = 4,valueBoxOutput("uiUni_info_ace")),
            column(width = 8,
                   wellPanel(div(style=DIV_SUPOSICAO, width="100%",textOutput("uiUni_txt_Suposicao")),
                             div(style=DIV_MENSAGEM, width="100%",textOutput("uiUni_txt_Texto")))),
            
            column(width = 3,""),
            column(width = 6,plotlyOutput("uiUni_grafico", height="100%")),
            column(width = 3,"")
        ),
        box(title="Código R", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
            column(width = 12,
                   wellPanel(p(""),code(style=ESTILO_CODIGO_R,includeText(NOME_ARQ_UNIDIMEN)))
            )
        )
      ), #tabPanel
      

      # Menu Presença de DIF (dicot) ----
      tabPanel(title="Presença de DIF",  

               # Inicio DIF Log ----    
               box(title="Método de regressão logística ", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,

                   column(width = 4,valueBoxOutput("uiDIFLog_info_ace")),
                   column(width = 4,
                          radioButtons(inputId="uiDIFLog_rb_tipo", label="Tipo de DIF", inline=TRUE, selected="both",
                                      choices= c("Uniforme"    = "udif",
                                                 "Não unifome" = "nudif",
                                                 "Ambos"       = "both"))
                   ),
                   column(width = 4,
                          selectInput(inputId="uiDIFLog_si_correcao", label="Método de Correção", selected="none",                 
                                      choices= c("Benjamini-Hochberg"  = "BH",
                                                 "Benjamini-Yekutieli" = "BY",
                                                 "Bonferroni"          = "bonferroni",
                                                 "Holm"                = "holm",
                                                 "Hochberg"            = "hochberg",
                                                 "Hommel"              = "hommel",
                                                 "Nenhum"              = "none"))
                   ),
                         
                   # box detecção dos itens ----       
                   box(title="Detecção dos itens", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,

                       column(width = 12,
                              tabBox(title=" ", width="100%",
                                     tabPanel("Tabela"," ",div(style=DIV_MENSAGEM,  width="100%",textOutput("uiDIFLog_txt_Texto1")),
                                                           hr(),
                                                           div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDIFLog_dt_Resultados"))),
                                     tabPanel("Ajuda" ," ",AJUDA_DICOT_DIF_LOG_TAB() )
                              )
                        )
                   ),

                   # box análise gráfica ----       
                   box(title="Análise gráfica", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                       column(width = 12,
                              tabBox(title=" ", width="100%",
                                     tabPanel("Gráfico"," ",div(style=DIV_MENSAGEM, width="100%",textOutput("uiDIFLog_txt_Texto2")),
                                                            hr(),
                                                            div(width="100%", plotlyOutput("uiDIFLog_Graficos", height="100%"))),
                                     tabPanel("Ajuda" ," ",AJUDA_DICOT_DIF_LOG_GRAF() )
                              )
                        )
                   ),
                         
                   # box código R ----       
                   box(title="Código R", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                       column(width = 12,
                              wellPanel(p(""),code(style=ESTILO_CODIGO_R,textOutput("uiR_NOME_ARQ_DIF_LOGISTIC")))
                       )
                   )

               ), # Final DIF Log 

               # Inicio DIF Lord ----    
               box(title="Método de Lord", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 4,valueBoxOutput("uiDIFLord_info_ace")),
                   column(width = 4,
                          radioButtons(inputId="uiDIFLord_rb_modelo", label="Modelo", inline=TRUE, selected="2PL",
                                       choices= c("1PL" = "1PL",
                                                  "2PL" = "2PL",
                                                  "3PL" = "3PL"))
                    ),
                    column(width = 4,
                           selectInput(inputId="uiDIFLord_si_correcao", label="Método de Correção", selected="none",                 
                                       choices= c("Benjamini-Hochberg"  = "BH",
                                                  "Benjamini-Yekutieli" = "BY",
                                                  "Bonferroni"          = "bonferroni",
                                                  "Holm"                = "holm",
                                                  "Hochberg"            = "hochberg",
                                                  "Hommel"              = "hommel",
                                                  "Nenhum"              = "none"))
                    ),
                         
                    # box detecção dos itens ----       
                    box(title="Detecção dos itens", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               tabBox(title=" ", width="100%",
                                      tabPanel("Tabela"," ",div(style=DIV_MENSAGEM,  width="100%",textOutput("uiDIFLord_txt_Texto1")),
                                                            hr(),
                                                            div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDIFLord_dt_Resultados"))),
                                      tabPanel("Ajuda" ," ",AJUDA_DICOT_DIF_LORD_TAB() )
                               )
                        )
                    ),
                         
                    # box análise gráfica ----      
                    box(title="Análise gráfica", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               tabBox(title=" ", width="100%",
                                      tabPanel("Gráfico"," ",div(style=DIV_MENSAGEM, width="100%",textOutput("uiDIFLord_txt_Texto2")),
                                                             hr(),
                                                             div(width="100%", plotlyOutput("uiDIFLord_Graficos", height="100%"))),
                                      tabPanel("Ajuda" ," ",AJUDA_DICOT_DIF_LORD_GRAF() )
                               )
                        )
                    ),
                         
                    # box código R ----       
                    box(title="Código R", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               wellPanel(p(""),code(style=ESTILO_CODIGO_R,textOutput("uiR_NOME_ARQ_DIF_LORD")))
                        )
                    )

               ) # Final DIF Lord 
      
      ), #tabpanel    
      
      
      # Menu Presença de DIF (polit) ---- 
      tabPanel(title="Presença de DIF ",  # politomico. A DIFERENÇA COM O DICOT É UM ESPAÇO NO FINAL

               # Inicio DIF Logit Cumulativo ----    
               box(title="Logit cumulativo", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 4,valueBoxOutput("uiDIFLogit_info_ace")),
                   column(width = 4,
                          radioButtons(inputId="uiDIFLogit_rb_tipo", label="Tipo de DIF", inline=TRUE, selected="both",
                                       choices= c("Uniforme"    = "udif",
                                                  "Não unifome" = "nudif",
                                                  "Ambos"       = "both"))
                    ),
                    column(width = 4,
                           selectInput(inputId="uiDIFLogit_si_correcao", label="Método de Correção", selected="none",                 
                                       choices= c("Benjamini-Hochberg"  = "BH",
                                                  "Benjamini-Yekutieli" = "BY",
                                                  "Bonferroni"          = "bonferroni",
                                                  "Holm"                = "holm",
                                                  "Hochberg"            = "hochberg",
                                                  "Hommel"              = "hommel",
                                                  "Nenhum"              = "none"))
                    ),
                         
                    # box detecção dos itens ----       
                    box(title="Detecção dos itens", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               tabBox(title=" ", width="100%",
                                      tabPanel("Tabela"," ",div(style=DIV_MENSAGEM,  width="100%",textOutput("uiDIFLogit_txt_Texto1")),
                                                            hr(),
                                                            div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDIFLogit_dt_Resultados"))),
                                      tabPanel("Ajuda" ," ",AJUDA_POLIT_DIF_LOGIT_C_TAB() )
                               )
                        ) 
                    ),
                         
                    # box análise gráfica ----       
                    box(title="Análise gráfica", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               tabBox(title=" ", width="100%",
                                      tabPanel("Gráfico"," ",div(style=DIV_MENSAGEM, width="100%",textOutput("uiDIFLogit_txt_Texto2")),
                                                             hr(),
                                                             div(width="100%", plotlyOutput("uiDIFLogit_Graficos", height="100%"))),
                                      tabPanel("Ajuda" ," ",AJUDA_POLIT_DIF_LOGIT_C_GRAF() )
                               )
                         )
                    ),
                         
                    # box código R ----       
                    box(title="Código R", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               wellPanel(p(""),code(style=ESTILO_CODIGO_R,textOutput("uiR_NOME_ARQ_DIF_LOGIT")))
                        )
                    )

               ), # Final DIF Logit Cumulativo
      
               # Inicio DIF Logit Adjacente ----    
               box(title="Logit de categorias adjacentes", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
    
                   column(width = 4,valueBoxOutput("uiDIFLogitAdj_info_ace")),
                   column(width = 4,
                          radioButtons(inputId="uiDIFLogitAdj_rb_tipo", label="Tipo de DIF", inline=TRUE, selected="both",
                                       choices= c("Uniforme"    = "udif",
                                                  "Não unifome" = "nudif",
                                                  "Ambos"       = "both"))
                   ),
                   column(width = 4,
                          selectInput(inputId="uiDIFLogitAdj_si_correcao", label="Método de Correção", selected="none",                 
                                     choices= c("Benjamini-Hochberg"  = "BH",
                                                "Benjamini-Yekutieli" = "BY",
                                                "Bonferroni"          = "bonferroni",
                                                "Holm"                = "holm",
                                                "Hochberg"            = "hochberg",
                                                "Hommel"              = "hommel",
                                                "Nenhum"             = "none"))
                    ),
                    
                    # box detecção dos itens ----         
                    box(title="Detecção dos itens", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               tabBox(title=" ", width="100%",
                                      tabPanel("Tabela"," ",div(style=DIV_MENSAGEM,  width="100%",textOutput("uiDIFLogitAdj_txt_Texto1")),
                                                            hr(),
                                                            div(style=DIV_DATATABLE, width="100%",dataTableOutput("uiDIFLogitAdj_dt_Resultados"))),
                                      tabPanel("Ajuda" ," ",AJUDA_POLIT_DIF_LOGIT_A_TAB() )
                               )
                         )
                    ),
                         
                    # box análise gráfica ----       
                    box(title="Análise gráfica", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               tabBox(title=" ", width="100%",
                                      tabPanel("Gráfico"," ",div(style=DIV_MENSAGEM, width="100%",textOutput("uiDIFLogitAdj_txt_Texto2")),
                                                             hr(),
                                                             div(width="100%", plotlyOutput("uiDIFLogitAdj_Graficos", height="100%"))),
                                      tabPanel("Ajuda" ," ",AJUDA_POLIT_DIF_LOGIT_A_GRAF() )
                               )
                        )
                    ),
                         
                    # box código R ----
                    box(title="Código R", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                        column(width = 12,
                               wellPanel(p(""),code(style=ESTILO_CODIGO_R,textOutput("uiR_NOME_ARQ_DIF_LOGIT_ADJ")))
                        )
                    )
                    
               
               ) # Final DIF Logit Adjacente
      
      ), #tabpanel    

      # Menu Ajuste do modelo (dicot) ----
      tabPanel(title="Ajuste do modelo",  # dicotomico

               # box escolha de opções ----     
               box(title=" Ajuste do modelo ", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,

                   column(width = 4,
                          wellPanel(valueBoxOutput("uiDicot_info_ace"),
                                    div(textOutput("uiMod_Dicot_Titulo")), 
                                    div(style=DIV_DICOT,textOutput("uiMod_Dicot_Medida1")),
                                    div(style=DIV_DICOT,textOutput("uiMod_Dicot_Medida2")),
                                    div(style=DIV_DICOT,textOutput("uiMod_Dicot_Medida3")),
                                    div(style=DIV_DICOT,textOutput("uiMod_Dicot_Medida4"))
                          )
                   ),
                   column(width = 8,    
                          
                          column(width = 7,
                                 wellPanel(div(textOutput("uiMod_Dicot_txt_Titulo")),
                                           div(style=DIV_MENSAGEM, textOutput("uiMod_Dicot_txt_Texto1")),
                                           div(style=DIV_MENSAGEM, textOutput("uiMod_Dicot_txt_Texto2")),
                                           br(),
                                           div(style=DIV_MENSAGEM, "Selecione os itens a serem EXCLUÍDOS do ajuste do modelo (clique no componente e selecione os itens)"),
                                           selectInput (inputId="uiMod_Dicot_si_itens", label=NULL, multiple = TRUE, choices=NULL)
                                 )
                          ),
                          column(width = 5,            
                                 # ATENCAO: se alterar algo aqui, alterar no server, na parte da exportacao de dados!!!
                                 wellPanel(radioButtons(inputId="uiMod_Dicot_rb_modelo", label="Modelo", selected="2PL",
                                              choices= c("RASCH"                           = "Rasch",
                                                         "Logístico de 1 parâmetro  (1PL)" = "1PL",
                                                         "Logístico de 2 parâmetros (2PL)" = "2PL",
                                                         "Logístico de 3 parâmetros (3PL)" = "3PL")),
                                           actionButton(inputId="uiMod_Dicot_btn_Ajustar",  label=" Ajustar e mostrar gráficos "),
                                           tags$div(style=ESTILO_BOTAODOWNLOAD, title=MENS_BOTAODOWNLOAD_XLSX,
                                                    downloadButton("uiMod_Dicot_btn_Download",label=" Download "))
                                 )
                          )
                   ), #column 8

                   # box traços latentes ----    
                   box(title="Traços latentes", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,
                              column(width = 12,
                                     wellPanel(p(style=DIV_MENSAGEM, "Estimativas dos traços latentes dos respondentes"),
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Tabela"," ",div(style=DIV_DATATABLE, width="100%", dataTableOutput("uiMod_Dicot_dt_Habilidades"))),
                                                      tabPanel("Ajuda" ," ",AJUDA_DICOT_HABIL() )
                                               )
                                     )
                              ),
                          
                              column(width = 6, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Histograma dos traços latentes"),
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Gráfico"," ",plotlyOutput("uiMod_Dicot_py_histograma", height="100%")),
                                                      tabPanel("Ajuda"  ," ",AJUDA_DICOT_HIST() )
                                               )
                                     )
                              ),
                              column(width = 6, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Boxplot dos traços latentes"),
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Gráfico"," ",plotlyOutput("uiMod_Dicot_py_boxplot", height="100%")),
                                                      tabPanel("Ajuda"  ," ",AJUDA_DICOT_BOX() ) 
                                               )
                                     )
                              ),

                              column(width = 12, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Curva de informação do modelo"),  
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Gráfico"," ", plotlyOutput("uiMod_Dicot_py_cit", height="100%")),
                                                      tabPanel("Ajuda",  " ", AJUDA_DICOT_CIT() )
                                               )
                                     )
                              )
                           
                   ), # box traços latentes
                          
                   # box itens do modelo ----  
                   box(title="Itens do modelo", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,

                               column(width = 12,
                                      wellPanel(p(style=DIV_MENSAGEM, "Coeficientes estimados dos itens"),
                                                tabBox(title=" ", width="100%",
                                                       tabPanel("Tabela"," ",div(style=DIV_DATATABLE, width="100%", dataTableOutput("uiMod_Dicot_dt_Coeficientes"))),
                                                       tabPanel("Ajuda" ," ",AJUDA_DICOT_COEF() )
                                                )
                                      )
                               ),                     
                           
                               column(width = 6, 
                                      wellPanel(p(style=DIV_MENSAGEM, "Curva característica dos itens"),
                                                tabBox(title=" ", width="100%",
                                                       tabPanel("Gráfico"," ",plotlyOutput("uiMod_Dicot_py_cciunica", height="100%")),
                                                       tabPanel("Ajuda"  ," ",AJUDA_DICOT_CCI() )
                                                )
                                      )
                               ),
                               column(width = 6, 
                                      wellPanel(p(style=DIV_MENSAGEM, "Curva de informação dos itens"),
                                                tabBox(title=" ", width="100%",
                                                       tabPanel("Gráfico"," ",plotlyOutput("uiMod_Dicot_py_ciiunica", height="100%")),
                                                       tabPanel("Ajuda"  ," ",AJUDA_DICOT_CII() )
                                                )
                                      )
                               ),
                           
                               column(width = 6, 
                                      wellPanel(p(style=DIV_MENSAGEM, "Curva característica dos itens"),
                                                tabBox(title=" ", width="100%",
                                                       tabPanel("Gráfico"," ",plotlyOutput("uiMod_Dicot_py_ccimosaico", height="100%")),
                                                       tabPanel("Ajuda"  ," ",AJUDA_DICOT_CCI_MOS() )
                                                )
                                      )
                               ),
                               column(width = 6, 
                                      wellPanel(p(style=DIV_MENSAGEM, "Curva de informação dos itens"),
                                                tabBox(title=" ", width="100%",
                                                       tabPanel("Gráfico"," ",plotlyOutput("uiMod_Dicot_py_ciimosaico", height="100%")),
                                                       tabPanel("Ajuda"  ," ",AJUDA_DICOT_CII_MOS() )
                                                )
                                      )
                               )
                           
                   ) # box itens do modelo
                          
               ), # box escolha de opções
      
               # box código R ----    
               box(title="Código R", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 12,
                          wellPanel(p(""),code(style=ESTILO_CODIGO_R,textOutput("uiR_NOME_ARQ_CALC_DICOT")))
                   )
               )

      ), # box ajuste do modelo (este é o DICOT)
      
      # Menu Ajuste do modelo (polit) ----
      tabPanel(title="Ajuste do modelo ", # politomico. A DIFERENÇA COM O DICOT É UM ESPAÇO NO FINAL

               # box escolha de opções ----    
               box(title=" Ajuste do modelo ", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 4,
                          wellPanel(valueBoxOutput("uiPolit_info_ace"),
                                    div(textOutput("uiMod_Polit_Titulo")), 
                                    div(style=DIV_DICOT,textOutput("uiMod_Polit_Medida1")),
                                    div(style=DIV_DICOT,textOutput("uiMod_Polit_Medida2")),
                                    div(style=DIV_DICOT,textOutput("uiMod_Polit_Medida3")),
                                    div(style=DIV_DICOT,textOutput("uiMod_Polit_Medida4"))
                          )
                   ),

                   column(width = 8,   
                          
                          column(width = 7,
                                 wellPanel(div(textOutput("uiMod_Polit_txt_Titulo")),
                                           div(style=DIV_MENSAGEM, textOutput("uiMod_Polit_txt_Texto1")),
                                           div(style=DIV_MENSAGEM, textOutput("uiMod_Polit_txt_Texto2")),
                                           br(),
                                           div(style=DIV_MENSAGEM, "Selecione os itens a serem EXCLUÍDOS do ajuste do modelo (clique no componente e selecione os itens)"),
                                           selectInput (inputId="uiMod_Polit_si_itens",  label=NULL, multiple = TRUE, choices=NULL)
                                 )
                          ),

                          # ATENCAO: se alterar algo aqui, alterar no server, na parte da exportacao de dados!!!
                          column(width = 5,                    
                                 wellPanel(radioButtons(inputId="uiMod_Polit_rb_modelo", label="Modelo", selected="graded",
                                                        choices= 
                                                                 c("Resposta gradual"             = "graded",   
                                                                   "Crédito parcial"              = "Rasch",
                                                                   "Crédito parcial generalizado" = "gpcmIRT",
                                                                   "Escala gradual"               = "rsm"
                                                                 )),
                                           actionButton(inputId="uiMod_Polit_btn_Ajustar", label=" Ajustar e mostrar gráficos "),
                                           tags$div(style=ESTILO_BOTAODOWNLOAD, title=MENS_BOTAODOWNLOAD_XLSX,
                                                    downloadButton("uiMod_Polit_btn_Download",label=" Download "))
                                )
                          )
                   ), #column 8    

                   # box traços latentes ----    
                   box(title="Traços latentes", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,

                              column(width = 12,
                                     wellPanel(
                                         p(style=DIV_MENSAGEM, "Estimativas dos traços latentes dos respondentes"),
                                         tabBox(title=" ", width="100%",
                                                tabPanel("Tabela"," ",div(style=DIV_DATATABLE, width="100%", dataTableOutput("uiMod_Polit_dt_Habilidades"))),
                                                tabPanel("Ajuda" ," ",AJUDA_POLIT_HABIL() )
                                         )
                                       )
                              ),                     
                              
                              column(width = 6, 
                                     wellPanel(
                                         p(style=DIV_MENSAGEM, "Histograma dos traços latentes"),
                                           tabBox(title=" ", width="100%",
                                                  tabPanel("Gráfico"," ",plotlyOutput("uiMod_Polit_py_histograma", height="100%")),
                                                  tabPanel("Ajuda"  ," ",AJUDA_POLIT_HIST() )
                                         )
                                     )
                              ),
                              column(width = 6, 
                                     wellPanel(
                                         p(style=DIV_MENSAGEM, "Boxplot dos traços latentes"),
                                           tabBox(title=" ", width="100%",
                                                  tabPanel("Gráfico"," ",plotlyOutput("uiMod_Polit_py_boxplot", height="100%")),
                                                  tabPanel("Ajuda"  ," ",AJUDA_POLIT_BOX() )
                                         )
                                     )
                              ),

                              column(width = 12, 
                                     wellPanel(
                                           p(style=DIV_MENSAGEM, "Curva de informação do modelo"),  
                                           tabBox(title=" ", width="100%",
                                                  tabPanel("Gráfico"," ", plotlyOutput("uiMod_Polit_py_cit", height="100%")),
                                                  tabPanel("Ajuda",  " ", AJUDA_POLIT_CIT() ) 
                                           )
                                         )
                              )
                   ), # box traços latentes
                        
                   # box itens do modelo ----
                   box(title="Itens do modelo", width=12, status=GLOBAL_COR_SUBBOX, solidHeader=TRUE, collapsible=TRUE,

                              column(width = 12,
                                     wellPanel(p(style=DIV_MENSAGEM, "Coeficientes estimados dos itens"),
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Tabela"," ",div(style=DIV_DATATABLE, width="100%", dataTableOutput("uiMod_Polit_dt_Coeficientes"))),
                                                      tabPanel("Ajuda" ," ",AJUDA_POLIT_COEF() ))
                                     )
                              ),                     

                              column(width = 12, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Curva de informação total dos itens"),
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Gráfico"," ",plotlyOutput("uiMod_Polit_py_ciiunica", height="100%")),
                                                      tabPanel("Ajuda"  ," ",AJUDA_POLIT_CII_T_TODOS() ) )
                                     )
                              ),
                          
                              column(width = 4, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Curva característica dos itens"),
                                               tabBox(title=" ", width="100%",
                                                      tabPanel("Gráfico"," ",plotlyOutput("uiMod_Polit_py_cci", height="100%")),
                                                      tabPanel("Ajuda"  ," ",AJUDA_POLIT_CCI() ))
                                     )
                              ),
                              column(width = 4, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Curva de informação das categorias dos itens"),
                                              tabBox(title=" ", width="100%",
                                                      tabPanel("Gráfico"," ",plotlyOutput("uiMod_Polit_py_cii", height="100%")),
                                                      tabPanel("Ajuda"  ," ",AJUDA_POLIT_CII() ))
                                     )
                              ),
                              column(width = 4, 
                                     wellPanel(p(style=DIV_MENSAGEM, "Curva de informação total dos itens"),
                                               tabBox(title=" ", width="100%",
                                                       tabPanel("Gráfico"," ",plotlyOutput("uiMod_Polit_py_cii_t", height="100%")),
                                                       tabPanel("Ajuda"  ," ",AJUDA_POLIT_CII_T() ))
                                    )
                              )
                          
                   ) # box itens do modelo

               ), # box escolha de opções
                    
               # box código R ----
               box(title="Código R", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 12,
                          wellPanel(p(""),code(style=ESTILO_CODIGO_R,textOutput("uiR_NOME_ARQ_CALC_POLIT")))
                   )
               )

      ), #tabpanel ajuste do modelo (este é o POLIT)


      # Menu Exportação de Dados ----
      tabPanel(title="Exportação de dados",
               
               # box exportação de tabelas ----                    
               box(title="Exportação de tabelas de resultados", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 12,
                          column(width=5, wellPanel(div(style=DIV_MENSAGEM, width="100%", textOutput("uiExpTab_txt_Dados")))),
                          column(width=3, downloadButton('uiExpTab_btn_TL', ' Download ')),
                          column(width=4, valueBoxOutput("uiExp_info_ace"))
                    )
               ), # box exportacao de tabelas
                 
               # box exportação de gráficos ----                  
               box(title="Exportação de gráficos", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   
                   tabBox(title=" ", id="abasExportaGraficos" ,width="100%",
                          tabPanel("Histograma"," ",
                                  box(title=" ", width="100%",
                                      column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_Hist", height="100%"))),
                                      column(width=5,
                                             wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"), 
                                                      textInput     (inputId="uiExpGraf_txt_Hist_X",      label="Eixo X", value=PLOTDISTHABILIDADADES_HIST_X),
                                                      textInput     (inputId="uiExpGraf_txt_Hist_Y",      label="Eixo Y", value=PLOTDISTHABILIDADADES_HIST_Y),
                                                      actionButton  (inputId="uiExpGraf_btn_Hist_Aplicar",label=" Redesenhar ", icon=icon("columns")),
                                                      actionButton  (inputId="uiExpGraf_btn_Hist_Padrao", label=" Restaurar " , icon=icon("home")),
                                                      div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                  ) #box
                          ),
                          tabPanel("Boxplot"," ",
                                  box(title=" ", width="100%",
                                      column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_Box", height="100%"))),
                                      column(width=5,
                                             wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                       textInput     (inputId="uiExpGraf_txt_Box_X",label="Eixo X",value=PLOTDISTHABILIDADADES_BOX_X),
                                                       actionButton  (inputId="uiExpGraf_btn_Box_Aplicar",label=" Redesenhar ", icon=icon("columns")),
                                                       actionButton  (inputId="uiExpGraf_btn_Box_Padrao", label=" Restaurar " , icon=icon("home")),
                                                       div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                  ) #box
                          ),
                          tabPanel("Curva de informação do modelo"," ",
                                   box(title=" ", width="100%",
                                       column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cit", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                       textInput     (inputId="uiExpGraf_txt_cit_X",      label="Eixo X", value=PLOTCURVASGERAIS_X),
                                                       textInput     (inputId="uiExpGraf_txt_cit_Y",      label="Eixo Y", value=PLOTCURVASGERAIS_Y),
                                                       textInput     (inputId="uiExpGraf_txt_cit_s2",     label=paste0("Nome da série '",PLOTCURVASGERAIS_S2,"'"), value=PLOTCURVASGERAIS_S2),
                                                       textInput     (inputId="uiExpGraf_txt_cit_s1",     label=paste0("Nome da série '",PLOTCURVASGERAIS_S1,"'"), value=PLOTCURVASGERAIS_S1),
                                                       actionButton  (inputId="uiExpGraf_btn_cit_Aplicar",label=" Redesenhar ", icon=icon("columns")),
                                                       actionButton  (inputId="uiExpGraf_btn_cit_Padrao" ,label=" Restaurar  ", icon=icon("home")),
                                                       div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ) #box
                          ),
                          tabPanel("Curva característica do item"," ",  # DICOT
                                   box(title=" ", width="100%",
                                       column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cciunica", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                        textInput     (inputId="uiExpGraf_txt_cciunica_X",      label="Eixo X", value=PLOTCURVASITENSDICOT_CCI_X),
                                                        textInput     (inputId="uiExpGraf_txt_cciunica_Y",      label="Eixo Y", value=PLOTCURVASITENSDICOT_CCI_Y),
                                                        actionButton  (inputId="uiExpGraf_btn_cciunica_Aplicar",label=" Redesenhar ", icon=icon("columns")),
                                                        actionButton  (inputId="uiExpGraf_btn_cciunica_Padrao", label=" Restaurar " , icon=icon("home")),
                                                        div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ), #box

                                   box(title=" ", width="100%",
                                       column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cci_dicot", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                        selectInput   (inputId="uiExpGraf_si_cci_itens_dicot",   label="Item", choices=NULL),
                                                        textInput     (inputId="uiExpGraf_txt_cci_X_dicot",      label="Eixo X", value=PLOTCURVASITENSDICOT_CCI_X),
                                                        textInput     (inputId="uiExpGraf_txt_cci_Y_dicot",      label="Eixo Y", value=PLOTCURVASITENSDICOT_CCI_Y),
                                                        actionButton  (inputId="uiExpGraf_btn_cci_Aplicar_dicot",label=" Redesenhar ", icon=icon("columns")),
                                                        actionButton  (inputId="uiExpGraf_btn_cci_Padrao_dicot", label=" Restaurar " , icon=icon("home")),
                                                        div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ) #box
                          ),
                          tabPanel("Curva característica do item "," ",  # POLIT (tem um espaço ao final do titulo)
                                   box(title=" ", width="100%",
                                       column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cci_polit", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                        selectInput   (inputId="uiExpGraf_si_cci_itens_polit",   label="Item", choices=NULL),
                                                        textInput     (inputId="uiExpGraf_txt_cci_X_polit",      label="Eixo X", value=PLOTCURVASITENSDICOT_CCI_X),
                                                        textInput     (inputId="uiExpGraf_txt_cci_Y_polit",      label="Eixo Y", value=PLOTCURVASITENSDICOT_CCI_Y),
                                                        actionButton  (inputId="uiExpGraf_btn_cci_Aplicar_polit",label=" Redesenhar ", icon=icon("columns")),
                                                        actionButton  (inputId="uiExpGraf_btn_cci_Padrao_polit", label=" Restaurar " , icon=icon("home")),
                                                        div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ) #box
                          ),
                          
                          tabPanel("Curva informação do item"," ",   # DICOT
                                  box(title="", width="100%",
                                      column(width=7,wellPanel(width="100%",height="100%",plotlyOutput("uiExpGraf_ciiunica", height="100%"))),
                                      column(width=5,
                                             wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                       textInput     (inputId="uiExpGraf_txt_ciiunica_X",      label="Eixo X", value=PLOTCURVASITENSDICOT_CII_X),
                                                       textInput     (inputId="uiExpGraf_txt_ciiunica_Y",      label="Eixo Y", value=PLOTCURVASITENSDICOT_CII_Y),
                                                       actionButton  (inputId="uiExpGraf_btn_ciiunica_Aplicar",label=" Redesenhar ", icon=icon("columns")),
                                                       actionButton  (inputId="uiExpGraf_btn_ciiunica_Padrao", label=" Restaurar " , icon=icon("home")),
                                                       div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                  ), #box
                                            
                                  box(title="", width="100%",
                                      column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cii_dicot", height="100%"))),
                                      column(width=5,
                                             wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                      selectInput   (inputId="uiExpGraf_si_cii_itens_dicot",   label="Item", choices=NULL),
                                                      textInput     (inputId="uiExpGraf_txt_cii_X_dicot",      label="Eixo X", value=PLOTCURVASITENSDICOT_CII_X),
                                                      textInput     (inputId="uiExpGraf_txt_cii_Y_dicot",      label="Eixo Y", value=PLOTCURVASITENSDICOT_CII_Y),
                                                      actionButton  (inputId="uiExpGraf_btn_cii_Aplicar_dicot",label=" Redesenhar ", icon=icon("columns")),
                                                      actionButton  (inputId="uiExpGraf_btn_cii_Padrao_dicot", label=" Restaurar " , icon=icon("home")),
                                                      div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                  ) #box
                          ),
                          
                          tabPanel("Curva informação do item "," ",   # POLIT (tem um espaço ao final do titulo)

                                   box(title="", width="100%",                                           
                                       column(width=7,wellPanel(width="100%",height="100%",plotlyOutput("uiExpGraf_ciiunica_polit", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                        textInput     (inputId="uiExpGraf_txt_ciiunica_polit_X",      label="Eixo X", value=PLOTCURVASITENSPOLIT_CII_X),
                                                        textInput     (inputId="uiExpGraf_txt_ciiunica_polit_Y",      label="Eixo Y", value=PLOTCURVASITENSPOLIT_CII_Y),
                                                        actionButton  (inputId="uiExpGraf_btn_ciiunica_polit_Aplicar",label=" Redesenhar ", icon=icon("columns")),
                                                        actionButton  (inputId="uiExpGraf_btn_ciiunica_polit_Padrao", label=" Restaurar " , icon=icon("home")),
                                                        div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ), #box
                                   
                                   box(title="", width="100%",
                                       column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cii_t_polit", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                        selectInput   (inputId="uiExpGraf_si_cii_t_itens_polit",   label="Item", choices=NULL),
                                                        textInput     (inputId="uiExpGraf_txt_cii_t_X_polit",      label="Eixo X", value=PLOTCURVASITENSPOLIT_CII_X),
                                                        textInput     (inputId="uiExpGraf_txt_cii_t_Y_polit",      label="Eixo Y", value=PLOTCURVASITENSPOLIT_CII_Y),
                                                        actionButton  (inputId="uiExpGraf_btn_cii_t_Aplicar_polit",label=" Redesenhar ", icon=icon("columns")),
                                                        actionButton  (inputId="uiExpGraf_btn_cii_t_Padrao_polit", label=" Restaurar " , icon=icon("home")),
                                                        div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ), #box
                                   
                                   box(title="", width="100%",
                                       column(width=7,wellPanel(width="100%", height="100%",plotlyOutput("uiExpGraf_cii_polit", height="100%"))),
                                       column(width=5,
                                              wellPanel(div(style=DIV_MENSAGEM, width="100%", height="100%"),
                                                        selectInput   (inputId="uiExpGraf_si_cii_itens_polit",   label="Item", choices=NULL),
                                                        textInput     (inputId="uiExpGraf_txt_cii_X_polit",      label="Eixo X", value=PLOTCURVASITENSDICOT_CII_X),
                                                        textInput     (inputId="uiExpGraf_txt_cii_Y_polit",      label="Eixo Y", value=PLOTCURVASITENSDICOT_CII_Y),
                                                        actionButton  (inputId="uiExpGraf_btn_cii_Aplicar_polit",label=" Redesenhar ", icon=icon("columns")),
                                                        actionButton  (inputId="uiExpGraf_btn_cii_Padrao_polit", label=" Restaurar " , icon=icon("home")),
                                                        div(style=DIV_MENSAGEM, MENS_BOTAODOWNLOAD)))
                                   ) #box
                                   
                                   
                          )
                          
                          
                     ) #tabBox
               ), # box exportacao de graficos
               
               # box código R ----                     
               box(title="Código R", width=12, status=GLOBAL_COR_BOX, solidHeader=TRUE, collapsible=TRUE,
                   column(width = 12,
                          wellPanel(p(""),code(style=ESTILO_CODIGO_R,includeText(NOME_ARQ_EXPORTACAO)))
                   )
               )

      ) # tabPanel exportacao de dados
      
      
    ) #navbar Page
  ) #dashboardBody
) #dashboardPage
