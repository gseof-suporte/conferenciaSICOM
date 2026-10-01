# Instala apenas dependências ausentes para execução em outros computadores.
pacotes_emp <- c("shiny", "shinythemes", "dplyr", "stringr", "readxl", "readr")
for (pkg in pacotes_emp) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# ==============================================================================
# 1. PACOTES E DEPENDÊNCIAS
# ==============================================================================
options(shiny.maxRequestSize = 100 * 1024^2)
library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(tools)


# ==============================================================================
# 2. FUNÇÕES AUXILIARES
# ==============================================================================

clean_string_code <- function(x) {
  
  if (is.null(x)) return("")
  
  x <- as.character(x)
  x <- str_trim(x)
  
  x[is.na(x)] <- ""
  
  return(x)
}


clean_numeric <- function(x) {
  
  if (is.null(x)) return(0)
  
  x <- as.character(x)
  x <- str_trim(x)
  
  x[is.na(x)] <- ""
  
  x <- gsub(
    "[^0-9,.-]",
    "",
    x
  )
  
  tem_virgula <- grepl(
    ",",
    x
  )
  
  x[tem_virgula] <- gsub(
    ".",
    "",
    x[tem_virgula],
    fixed = TRUE
  )
  
  x[tem_virgula] <- sub(
    ",",
    ".",
    x[tem_virgula],
    fixed = TRUE
  )
  
  num <- suppressWarnings(
    as.numeric(x)
  )
  
  num[is.na(num)] <- 0
  
  return(num)
}


fmt_reais <- function(valor) {
  
  formatC(
    coalesce(valor, 0),
    format = "f",
    digits = 2,
    big.mark = ".",
    decimal.mark = ","
  )
}


normalizar_codigo_comparacao <- function(x) {
  
  x <- clean_string_code(x)
  
  x_num <- gsub(
    "[^0-9]",
    "",
    x
  )
  
  sem_numero <- x_num == ""
  
  x_num <- sub(
    "^0+",
    "",
    x_num
  )
  
  x_num[
    x_num == "" &
      !sem_numero
  ] <- "0"
  
  x_num[
    sem_numero
  ] <- x[
    sem_numero
  ]
  
  return(x_num)
}


# ==============================================================================
# NORMALIZAÇÃO DO CONTRATO
#
# Sempre retorna CHARACTER.
#
# Exemplo:
#
# SOF:
# 01202308000176
#
# SICOM:
# 1202308000176
#
# Ambos tornam-se:
# "1202308000176"
# ==============================================================================

normalizar_contrato <- function(x) {
  
  if (is.null(x)) return("")
  
  x <- as.character(x)
  x <- stringr::str_trim(x)
  
  x[is.na(x)] <- ""
  
  # Identifica linhas originalmente vazias
  estava_vazio <- x == ""
  
  # Remove caracteres não numéricos
  x <- gsub("[^0-9]", "", x)
  
  # Remove zeros à esquerda temporariamente
  x <- sub("^0+", "", x)
  
  # Aplica o preenchimento com zeros até atingir 13 dígitos
  x <- stringr::str_pad(x, width = 13, side = "left", pad = "0")
  
  # Restaura como string vazia os campos que originalmente não tinham conteúdo ou zeros válidos
  x[estava_vazio | x == "0000000000000"] <- ""
  
  return(as.character(x))
}
# ==============================================================================
# NORMALIZAÇÃO DO PROCESSO LICITATÓRIO
#
# Sempre retorna CHARACTER.
#
# Exemplo:
#
# SOF:
# 007435
#
# SICOM:
# 7435
#
# Ambos:
# "7435"
# ==============================================================================

normalizar_processo <- function(x) {
  # Processos são identificadores, não números para cálculo.
  # Nunca remover o E e o expoente de uma notação científica,
  # pois isso incorporaria indevidamente o expoente ao processo.
  if (is.null(x)) return("")
  x <- stringr::str_trim(as.character(x))
  x[is.na(x)] <- ""
  
  # readxl pode antepor esta mensagem ao conteúdo de células XLS antigas.
  # Remover a mensagem LITERAL, nunca o prefixo numérico 16.
  x <- gsub("*failed to decode utf16*", "", x, fixed = TRUE)
  x <- stringr::str_trim(x)
  
  cientifico <- grepl("^[+-]?[0-9]+([.,][0-9]+)?[eE][+-]?[0-9]+$", x)
  if (any(cientifico)) {
    exemplos <- paste(utils::head(unique(x[cientifico]), 3), collapse = ", ")
    stop(paste0(
      "O SOF/SICOM contém número de processo em notação científica (", exemplos,
      "). O Excel pode ter perdido dígitos. Formate a coluna de processo como TEXTO ",
      "na origem e exporte novamente; não é seguro remover o expoente automaticamente."
    ))
  }
  
  # Remove apenas separadores visuais de processos já textuais.
  # Ex.: 00123/2026-45 -> 123202645.
  estava_vazio <- x == ""
  x <- gsub("[^0-9]", "", x)
  x <- sub("^0+", "", x)
  x[x == "" & !estava_vazio] <- "0"
  x[estava_vazio] <- ""
  as.character(x)
}


# ==============================================================================
# NORMALIZAÇÃO DO TERMO ADITIVO
#
# NOVA CORREÇÃO
#
# Para termo aditivo:
#
# ""      = sem termo aditivo
# NA      = sem termo aditivo
# 0       = sem termo aditivo
# 00      = sem termo aditivo
# 000     = sem termo aditivo
#
# Além disso:
#
# 01 = 1
# 02 = 2
#
# Dessa forma:
#
# SOF 0 e SICOM vazio -> CONFORME
# SOF vazio e SICOM 0 -> CONFORME
# SOF 01 e SICOM 1    -> CONFORME
# ==============================================================================

normalizar_aditivo <- function(x) {
  
  if (is.null(x)) return("")
  
  x <- as.character(x)
  
  x <- str_trim(x)
  
  x[is.na(x)] <- ""
  
  # Remove tudo que não seja dígito
  x_num <- gsub(
    "[^0-9]",
    "",
    x
  )
  
  # Campo sem conteúdo
  vazio <- x_num == ""
  
  # Remove zeros à esquerda
  x_num <- sub(
    "^0+",
    "",
    x_num
  )
  
  # Se era vazio ou composto exclusivamente por zeros,
  # representa como ausência de termo aditivo
  x_num[
    vazio |
      x_num == ""
  ] <- ""
  
  return(
    as.character(x_num)
  )
}


# ==============================================================================
# FUNÇÕES DE LIMPEZA ESPECÍFICAS
# ==============================================================================

clean_document_id <- function(x) {
  
  normalizar_contrato(x)
}


clean_processo_licitatorio <- function(x) {
  
  normalizar_processo(x)
}


# ==============================================================================
# PRIMEIRO PROCESSO VÁLIDO
#
# Impede que reframe() elimine o empenho quando processo estiver vazio.
# ==============================================================================

primeiro_processo_valido <- function(x) {
  
  x <- normalizar_processo(x)
  
  validos <- x[
    !is.na(x) &
      x != "" &
      x != "0"
  ]
  
  if (
    length(validos) > 0
  ) {
    
    return(
      as.character(
        validos[1]
      )
    )
  }
  
  return("")
}


# ==============================================================================
# EXTRAÇÃO DA UO DO SICOM
# ==============================================================================

extrair_uo_sicom <- function(x) {
  
  x_str <- stringr::str_trim(
    as.character(x)
  )
  
  x_str[
    is.na(x_str)
  ] <- ""
  
  # --------------------------------------------------------------------------
  # REGRA:
  #
  # Retirar o terceiro caractere
  #
  # 2001  -> 201
  # 02001 -> 0201 -> 201
  # 40001 -> 4001
  # --------------------------------------------------------------------------
  
  x_mod <- dplyr::case_when(
    
    nchar(x_str) >= 3 ~
      
      paste0(
        
        stringr::str_sub(
          x_str,
          1,
          2
        ),
        
        stringr::str_sub(
          x_str,
          4,
          -1
        )
      ),
    
    TRUE ~
      x_str
  )
  
  num <- suppressWarnings(
    
    as.numeric(
      
      gsub(
        "[^0-9]",
        "",
        x_mod
      )
    )
  )
  
  return(
    dplyr::coalesce(
      num,
      0
    )
  )
}


# ==============================================================================
# EXTRAÇÃO DO EMPENHO DO SICOM
# ==============================================================================

extrair_empenho_sicom <- function(x) {
  
  x_str <- stringr::str_trim(
    as.character(x)
  )
  
  x_str[
    is.na(x_str)
  ] <- ""
  
  # --------------------------------------------------------------------------
  # Regra:
  #
  # posições 9 até 14
  #
  # 202602010005560000
  #
  # posições 9 a 14:
  # 000556
  #
  # resultado:
  # 556
  # --------------------------------------------------------------------------
  
  emp_extraido <- stringr::str_sub(
    x_str,
    9,
    14
  )
  
  res_num <- suppressWarnings(
    as.numeric(
      emp_extraido
    )
  )
  
  res_num[
    is.na(res_num)
  ] <- 0
  
  return(
    as.character(
      res_num
    )
  )
}


# ==============================================================================
# EXTRAÇÃO DO ANO
# ==============================================================================

extrair_ano_sicom <- function(x) {
  
  x_str <- stringr::str_trim(
    as.character(x)
  )
  
  res <- stringr::str_sub(
    x_str,
    1,
    4
  )
  
  res[
    is.na(res) |
      nchar(x_str) < 4
  ] <- ""
  
  return(res)
}


# ==============================================================================
# DATA SICOM
# ==============================================================================

parse_sicom_date <- function(x) {
  
  suppressWarnings(
    
    as.Date(
      x,
      format = "%Y-%m-%d"
    )
  )
}


# ==============================================================================
# 3. INTERFACE DO USUÁRIO
#
# LAYOUT MANTIDO
# ==============================================================================

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel(
    "Conferência SOF x SICOM - Empenhos"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      h4(
        "Arquivos de Entrada"
      ),
      
      fileInput(
        "file_sof",
        "Relatório SOF (.xls, .xlsx, .csv, .txt):",
        accept = c(
          ".xls",
          ".xlsx",
          ".txt",
          ".csv"
        )
      ),
      
      fileInput(
        "file_sicom",
        "Arquivo SICOM (.txt, .csv):",
        accept = c(
          ".txt",
          ".csv"
        )
      ),
      
      hr(),
      
      actionButton(
        "btn_executar",
        "Processar e Cruzar Dados",
        class = "btn-primary btn-block",
        icon = icon("play")
      ),
      
      br(),
      
      downloadButton(
        "dl_excel",
        "Baixar Relatório (CSV)",
        class = "btn-success btn-block"
      )
    ),
    
    
    mainPanel(
      
      width = 9,
      
      tabsetPanel(
        
        type = "tabs",
        
        tabPanel(
          
          "Resumo",
          
          br(),
          
          wellPanel(
            uiOutput("box_resumo")
          )
        ),
        
        
        tabPanel(
          
          "Registro 10",
          
          br(),
          
          h4(
            "Divergências Encontradas - Registro 10"
          ),
          
          tableOutput("tbl_r10")
        ),
        
        
        tabPanel(
          
          "Registro 11",
          
          br(),
          
          h4(
            "Divergências Encontradas - Registro 11"
          ),
          
          tableOutput("tbl_r11")
        )
      )
    )
  )
)


# ==============================================================================
# 4. REGRA DE NEGÓCIO E SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  
  
  dados_processados <- eventReactive(
    input$btn_executar,
    {
      
      req(
        input$file_sof,
        input$file_sicom
      )
      
      
      withProgress(
        
        message = "Iniciando verificação...",
        
        value = 0,
        
        {
          
          
          tryCatch({
            
            
            # ==================================================================
            # 1. CARREGAMENTO DO SOF
            # ==================================================================
            
            setProgress(
              value = 0.10,
              detail = "Carregando relatório SOF..."
            )
            
            
            ext_sof <- tools::file_ext(
              input$file_sof$name
            )
            
            
            if (
              tolower(ext_sof) %in%
              c(
                "xls",
                "xlsx"
              )
            ) {
              
              sof_raw <- readxl::read_excel(
                
                input$file_sof$datapath,
                
                sheet = 1,
                
                col_types = "text",
                
                progress = FALSE
              )
              
            } else {
              
              sof_raw <- readr::read_delim(
                
                input$file_sof$datapath,
                
                delim = ";",
                
                col_types =
                  readr::cols(
                    .default = "c"
                  ),
                
                show_col_types = FALSE
              )
            }
            
            
            # ==================================================================
            # NOMES DAS COLUNAS
            # ==================================================================
            
            names(sof_raw) <- tolower(
              
              str_trim(
                
                as.character(
                  names(sof_raw)
                )
              )
            )
            
            
            names(sof_raw) <- iconv(
              names(sof_raw),
              to = "ASCII//TRANSLIT"
            )
            
            
            # ==================================================================
            # COLUNAS ESPERADAS
            # ==================================================================
            
            colunas_esperadas <- c(
              
              "uo",
              "empenho",
              "vl_empenhado",
              "modalidade_empenho",
              "funcao",
              "subfuncao",
              "programa",
              "projativ",
              "sub_acao",
              "numero_parceria_grp",
              "numero_congenere_grp",
              "natureza_despesa",
              "natureza_despesa_sicom",
              "numero_ata_registro_preco",
              "processo",
              "numero_contrato_grp",
              "numero_proc_modalidade",
              "numero_processo_compra_grp",
              "numero_ata_adesao",
              "numero_ultimo_aditivo_grp",
              "cod_siafic_grupo",
              "cod_siafic_fonte",
              "modalidade",
              "exercicio_proc_modalidade",
              "exercicio_contrato",
              "item_despesa_sicom"
            )
            
            
            for (
              col in colunas_esperadas
            ) {
              
              if (
                !col %in%
                names(sof_raw)
              ) {
                
                sof_raw[[col]] <- "0"
              }
            }
            
            
            # ==================================================================
            # PROCESSAMENTO SOF
            # ==================================================================
            
            sof_proc <- sof_raw %>%
              
              mutate(
                
                uo_num =
                  suppressWarnings(
                    as.numeric(
                      uo
                    )
                  ),
                
                emp_num =
                  suppressWarnings(
                    as.numeric(
                      empenho
                    )
                  )
                
              ) %>%
              
              
              filter(
                
                !is.na(
                  uo_num
                ),
                
                !is.na(
                  emp_num
                ),
                
                uo_num > 0
                
              ) %>%
              
              
              mutate(
                
                # --------------------------------------------------------------
                # CHAVE
                # --------------------------------------------------------------
                
                chave =
                  paste0(
                    uo_num,
                    "_",
                    emp_num
                  ),
                
                
                sof_existe =
                  TRUE,
                
                
                # --------------------------------------------------------------
                # FUNÇÃO
                # --------------------------------------------------------------
                
                f_funcao =
                  normalizar_codigo_comparacao(
                    funcao
                  ),
                
                
                # --------------------------------------------------------------
                # SUBFUNÇÃO
                # --------------------------------------------------------------
                
                f_subfuncao =
                  normalizar_codigo_comparacao(
                    subfuncao
                  ),
                
                
                # --------------------------------------------------------------
                # PROGRAMA
                # --------------------------------------------------------------
                
                f_programa =
                  normalizar_codigo_comparacao(
                    programa
                  ),
                
                
                # --------------------------------------------------------------
                # PROJATIV
                # --------------------------------------------------------------
                
                f_projativ =
                  normalizar_codigo_comparacao(
                    projativ
                  ),
                
                
                # --------------------------------------------------------------
                # ITEM / SUBELEMENTO
                # --------------------------------------------------------------
                
                f_sub_acao = normalizar_codigo_comparacao(sub_acao),
                f_parceria = normalizar_codigo_comparacao(numero_parceria_grp),
                f_congenere = normalizar_codigo_comparacao(numero_congenere_grp),
                
                f_item =
                  normalizar_codigo_comparacao(
                    item_despesa_sicom
                  ),
                
                
                # --------------------------------------------------------------
                # NATUREZA
                # --------------------------------------------------------------
                
                f_natureza =
                  stringr::str_remove_all(
                    
                    clean_string_code(
                      if_else(
                        clean_string_code(natureza_despesa_sicom) != "",
                        natureza_despesa_sicom,
                        natureza_despesa
                      )
                    ),
                    
                    "[^0-9]"
                  ),
                
                
                # ==============================================================
                # TERMO ADITIVO SOF
                #
                # NOVA NORMALIZAÇÃO
                #
                # vazio = 0 = 00 = ausência de termo
                # ==============================================================
                
                f_modalidade_empenho =
                  normalizar_codigo_comparacao(modalidade_empenho),
                
                
                f_aditivo =
                  normalizar_aditivo(
                    numero_ultimo_aditivo_grp
                  ),
                
                
                f_valor =
                  clean_numeric(
                    vl_empenhado
                  ),
                
                
                f_grupo =
                  clean_string_code(
                    cod_siafic_grupo
                  ),
                
                
                f_fonte =
                  clean_string_code(
                    cod_siafic_fonte
                  ),
                
                
                # --------------------------------------------------------------
                # MODALIDADE
                # --------------------------------------------------------------
                
                f_modalidade =
                  stringr::str_trim(
                    
                    toupper(
                      
                      iconv(
                        
                        as.character(
                          modalidade
                        ),
                        
                        to = "ASCII//TRANSLIT"
                      )
                    )
                  ),
                
                
                # --------------------------------------------------------------
                # ANO DA LICITAÇÃO
                #
                # Se exercicio_proc_modalidade < 2012,
                # eventual divergência de modalidade será ignorada.
                # --------------------------------------------------------------
                
                ano_proc_modalidade =
                  suppressWarnings(
                    as.numeric(
                      gsub(
                        "[^0-9]",
                        "",
                        as.character(
                          exercicio_proc_modalidade
                        )
                      )
                    )
                  ),
                
                
                # --------------------------------------------------------------
                # CONTRATO
                # --------------------------------------------------------------
                
                str_f_contrato =
                  normalizar_contrato(
                    numero_contrato_grp
                  ),
                
                
                # --------------------------------------------------------------
                # PROCESSOS
                # --------------------------------------------------------------
                
                str_f_processo =
                  normalizar_processo(
                    processo
                  ),
                
                
                str_f_proc_compra =
                  normalizar_processo(
                    numero_processo_compra_grp
                  ),
                
                
                str_f_proc_modalidade =
                  normalizar_processo(
                    numero_proc_modalidade
                  ),
                
                
                str_f_ata_adesao =
                  normalizar_processo(
                    numero_ata_adesao
                  ),
                
                
                str_f_ata_reg_preco =
                  normalizar_processo(
                    numero_ata_registro_preco
                  )
              )
            
            
            # ==================================================================
            # 2. CARREGAMENTO SICOM
            # ==================================================================
            
            setProgress(
              value = 0.25,
              detail = "Carregando relatório SICOM..."
            )
            
            
            linhas <- readLines(
              
              input$file_sicom$datapath,
              
              warn = FALSE,
              
              encoding = "latin1"
            )
            
            
            linhas <- iconv(
              
              linhas,
              
              from = "latin1",
              
              to = "UTF-8",
              
              sub = ""
            )
            
            
            linhas <- gsub(
              
              "[\\x00]",
              
              "",
              
              linhas,
              
              perl = TRUE
            )
            
            
            linhas <- str_trim(
              linhas
            )
            
            
            # ==================================================================
            # REGISTRO 10
            # ==================================================================
            
            setProgress(
              value = 0.40,
              detail = "Lendo Registro 10..."
            )
            
            
            linhas_10 <- grep(
              
              "^10;",
              
              linhas,
              
              value = TRUE
            )
            
            
            message("EMP | Registro 10: ", length(linhas_10), " linhas")
            if (length(linhas_10) == 0) {
              linhas_10 <- "10;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
            }
            
            
            message("EMP | Separando as colunas do Registro 10 | ", format(Sys.time(), "%H:%M:%S"))
            setProgress(value = .45, detail = "Separando as colunas do Registro 10...")
            
            matriz_10 <- str_split_fixed(
              
              linhas_10,
              
              ";",
              
              40
            )
            
            
            # ==================================================================
            # REGISTRO 10 BRUTO
            # ==================================================================
            
            message("EMP | Interpretando o Registro 10 | ", format(Sys.time(), "%H:%M:%S"))
            setProgress(value = .50, detail = "Interpretando o Registro 10...")
            
            sicom_10_raw <-
              
              as.data.frame(
                
                matriz_10,
                
                stringsAsFactors = FALSE
                
              ) %>%
              
              
              mutate(
                
                uo =
                  extrair_uo_sicom(
                    V3
                  ),
                
                
                empenho =
                  extrair_empenho_sicom(
                    V11
                  ),
                
                
                ano_empenho =
                  extrair_ano_sicom(
                    V11
                  ),
                
                
                vlBruto_num =
                  clean_numeric(
                    V15
                  )
              ) %>%
              
              
              filter(
                
                uo > 0,
                
                empenho != "0"
              )
            
            
            # ==================================================================
            # TOTAL REGISTRO 10
            #
            # NÃO ALTERADO
            # ==================================================================
            
            tot_r10 <- sum(
              
              sicom_10_raw$vlBruto_num,
              
              na.rm = TRUE
            )
            
            
            # ==================================================================
            # CONSOLIDAÇÃO REGISTRO 10
            # ==================================================================
            
            message("EMP | Consolidando empenhos do Registro 10 | ", format(Sys.time(), "%H:%M:%S"))
            setProgress(value = .55, detail = "Consolidando empenhos do Registro 10...")
            
            sicom_10_pre <-
              
              sicom_10_raw %>%
              
              
              mutate(
                
                chave =
                  paste0(
                    uo,
                    "_",
                    empenho
                  ),
                
                
                # --------------------------------------------------------------
                # TIPO SICOM
                # --------------------------------------------------------------
                
                temp_licitacao_ext =
                  normalizar_codigo_comparacao(
                    V30
                  ),
                
                
                # --------------------------------------------------------------
                # PROCESSO
                # --------------------------------------------------------------
                
                str_s_processo =
                  normalizar_processo(
                    V33
                  ),
                
                
                # --------------------------------------------------------------
                # CONTRATO
                # --------------------------------------------------------------
                
                str_s_contrato =
                  normalizar_contrato(
                    V20
                  ),
                
                
                dt_pos_21 =
                  parse_sicom_date(
                    V12
                  ),
                
                
                nro_conge_pos_27 =
                  clean_string_code(
                    V27
                  ),
                
                
                dt_conge_pos_29 =
                  parse_sicom_date(
                    V29
                  ),
                
                
                pos_10_raw =
                  clean_string_code(
                    V10
                  ),
                
                
                pos_17_raw =
                  clean_string_code(
                    V17
                  ),
                
                
                # ==============================================================
                # TERMO ADITIVO SICOM
                #
                # V22 = nroSequencialTermoAditivo
                #
                # vazio = 0 = ausência de termo
                # ==============================================================
                
                aditivo_sicom_norm =
                  normalizar_aditivo(
                    V22
                  ),
                
                
                pos_col_V =
                  clean_string_code(
                    V22
                  ),
                
                
                pos_col_W =
                  clean_string_code(
                    V23
                  ),
                
                
                pos_col_Z =
                  clean_string_code(
                    V26
                  )
              )
            
            # Quando cada chave UO+empenho é única, não há nada a agrupar.
            # Evita milhares de execuções individuais de funções de texto no reframe().
            message("EMP | Chaves únicas Registro 10: ",
                    dplyr::n_distinct(sicom_10_pre$chave),
                    " / ", nrow(sicom_10_pre), " linhas")
            if (!anyDuplicated(sicom_10_pre$chave)) {
              message("EMP | Registro 10 sem duplicatas: normalização vetorizada")
              sicom_10_proc <- sicom_10_pre %>%
                transmute(
                  chave = chave,
                  emp_vis = as.character(empenho),
                  uo_vis = as.character(uo),
                  ano_emp = ano_empenho,
                  s_modalidade_empenho = normalizar_codigo_comparacao(V13),
                  s_exercicio_contrato_21 = normalizar_codigo_comparacao(V21),
                  s_funcao = normalizar_codigo_comparacao(V4),
                  s_subfuncao = normalizar_codigo_comparacao(V5),
                  s_programa = normalizar_codigo_comparacao(V6),
                  s_projativ = normalizar_codigo_comparacao(V7),
                  s_sub_acao = normalizar_codigo_comparacao(V8),
                  s_parceria_ou_congenere = normalizar_codigo_comparacao(V27),
                  s_natureza = stringr::str_remove_all(clean_string_code(V9), "[^0-9]"),
                  s_item = normalizar_codigo_comparacao(V10),
                  s_pos_10 = clean_string_code(V10),
                  s_pos_17 = pos_17_raw,
                  s_aditivo = aditivo_sicom_norm,
                  s_valor = vlBruto_num,
                  sicom_existe = TRUE,
                  s_licitacao = temp_licitacao_ext,
                  permitir_v_vazio = pos_col_W == "1" | pos_col_Z == "1",
                  v_está_vazio = pos_col_V %in% c("0", ""),
                  ignorar_modalidade = normalizar_codigo_comparacao(pos_col_Z) == "1",
                  str_s_processo = str_s_processo,
                  str_s_contrato = str_s_contrato,
                  dt_pos_21 = dt_pos_21,
                  nro_conge_pos_27 = nro_conge_pos_27,
                  dt_conge_pos_29 = dt_conge_pos_29
                )
            } else {
              # Mantém integralmente a lógica anterior quando existem empenhos repetidos.
              message("EMP | Registro 10 com chaves repetidas: consolidando grupos")
              sicom_10_proc <- sicom_10_pre %>%
                group_by(
                  chave
                ) %>%
                
                
                reframe(
                  
                  emp_vis =
                    as.character(
                      first(
                        empenho
                      )
                    ),
                  
                  
                  uo_vis =
                    as.character(
                      first(
                        uo
                      )
                    ),
                  
                  
                  ano_emp =
                    first(
                      ano_empenho
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # FUNÇÃO
                  # --------------------------------------------------------------
                  
                  s_modalidade_empenho =
                    normalizar_codigo_comparacao(first(V13)),
                  
                  s_exercicio_contrato_21 =
                    normalizar_codigo_comparacao(first(V21)),
                  
                  
                  s_funcao =
                    normalizar_codigo_comparacao(
                      first(
                        V4
                      )
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # SUBFUNÇÃO
                  # --------------------------------------------------------------
                  
                  s_subfuncao =
                    normalizar_codigo_comparacao(
                      first(
                        V5
                      )
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # PROGRAMA
                  # --------------------------------------------------------------
                  
                  s_programa =
                    normalizar_codigo_comparacao(
                      first(
                        V6
                      )
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # PROJATIV
                  # --------------------------------------------------------------
                  
                  s_projativ =
                    normalizar_codigo_comparacao(
                      first(
                        V7
                      )
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # NATUREZA
                  # --------------------------------------------------------------
                  
                  s_sub_acao = normalizar_codigo_comparacao(first(V8)),
                  s_parceria_ou_congenere = normalizar_codigo_comparacao(first(V27)),
                  
                  s_natureza =
                    stringr::str_remove_all(
                      
                      clean_string_code(
                        first(
                          V9
                        )
                      ),
                      
                      "[^0-9]"
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # ITEM / SUBELEMENTO
                  # --------------------------------------------------------------
                  
                  s_item =
                    normalizar_codigo_comparacao(
                      first(
                        V10
                      )
                    ),
                  
                  
                  s_pos_10 =
                    clean_string_code(
                      first(
                        V10
                      )
                    ),
                  
                  
                  s_pos_17 =
                    first(
                      pos_17_raw
                    ),
                  
                  
                  # ==============================================================
                  # TERMO ADITIVO SICOM
                  #
                  # Já normalizado
                  # ==============================================================
                  
                  s_aditivo =
                    normalizar_aditivo(
                      first(
                        aditivo_sicom_norm
                      )
                    ),
                  
                  
                  # --------------------------------------------------------------
                  # VALOR
                  # --------------------------------------------------------------
                  
                  s_valor =
                    sum(
                      vlBruto_num,
                      na.rm = TRUE
                    ),
                  
                  
                  sicom_existe =
                    TRUE,
                  
                  
                  # --------------------------------------------------------------
                  # TIPOS DO SICOM
                  # --------------------------------------------------------------
                  
                  s_licitacao =
                    paste(
                      
                      sort(
                        
                        unique(
                          
                          temp_licitacao_ext[
                            temp_licitacao_ext != ""
                          ]
                        )
                      ),
                      
                      collapse = ","
                    ),
                  
                  
                  permitir_v_vazio =
                    any(
                      
                      pos_col_W == "1" |
                        pos_col_Z == "1",
                      
                      na.rm = TRUE
                    ),
                  
                  
                  v_está_vazio =
                    any(
                      
                      pos_col_V == "0" |
                        pos_col_V == "",
                      
                      na.rm = TRUE
                    ),
                  
                  
                  ignorar_modalidade =
                    any(
                      
                      normalizar_codigo_comparacao(
                        pos_col_Z
                      ) == "1",
                      
                      na.rm = TRUE
                    ),
                  
                  
                  str_s_processo =
                    primeiro_processo_valido(
                      str_s_processo
                    ),
                  
                  
                  str_s_contrato =
                    as.character(
                      first(
                        str_s_contrato
                      )
                    ),
                  
                  
                  dt_pos_21 =
                    first(
                      dt_pos_21
                    ),
                  
                  
                  nro_conge_pos_27 =
                    first(
                      nro_conge_pos_27
                    ),
                  
                  
                  dt_conge_pos_29 =
                    first(
                      dt_conge_pos_29
                    )
                ) %>%
                
                
                distinct(
                  chave,
                  .keep_all = TRUE
                )
              
              
            }
            message("EMP | Registro 10 consolidado: ", nrow(sicom_10_proc), " empenhos | ", format(Sys.time(), "%H:%M:%S"))
            
            # ==================================================================
            # REGISTRO 11
            # ==================================================================
            
            setProgress(
              value = 0.65,
              detail = "Lendo Registro 11..."
            )
            
            
            linhas_11 <- grep(
              
              "^11;",
              
              linhas,
              
              value = TRUE
            )
            
            
            message("EMP | Registro 11: ", length(linhas_11), " linhas")
            if (
              length(
                linhas_11
              ) > 0
            ) {
              
              
              message("EMP | Separando as colunas do Registro 11 | ", format(Sys.time(), "%H:%M:%S"))
              setProgress(value = .70, detail = "Separando as colunas do Registro 11...")
              
              matriz_11 <- str_split_fixed(
                
                linhas_11,
                
                ";",
                
                10
              )
              
              
              message("EMP | Consolidando empenhos do Registro 11 | ", format(Sys.time(), "%H:%M:%S"))
              setProgress(value = .74, detail = "Consolidando empenhos do Registro 11...")
              
              sicom_11_proc <-
                
                as.data.frame(
                  
                  matriz_11,
                  
                  stringsAsFactors = FALSE
                  
                ) %>%
                
                
                mutate(
                  
                  uo =
                    extrair_uo_sicom(
                      V2
                    ),
                  
                  
                  empenho =
                    extrair_empenho_sicom(
                      V3
                    )
                ) %>%
                
                
                filter(
                  
                  uo > 0,
                  
                  empenho != "0"
                ) %>%
                
                
                mutate(
                  
                  chave =
                    paste0(
                      uo,
                      "_",
                      empenho
                    ),
                  
                  
                  font_limpo =
                    str_trim(
                      as.character(
                        V4
                      )
                    ),
                  
                  
                  s_grupo =
                    clean_string_code(
                      
                      substr(
                        font_limpo,
                        1,
                        1
                      )
                    ),
                  
                  
                  s_fonte =
                    clean_string_code(
                      
                      substr(
                        font_limpo,
                        2,
                        4
                      )
                    ),
                  
                  
                  val_fonte =
                    clean_numeric(
                      V6
                    )
                ) %>%
                
                
                group_by(
                  chave
                ) %>%
                
                
                reframe(
                  
                  s_grupo =
                    paste(
                      unique(
                        s_grupo
                      ),
                      collapse = ","
                    ),
                  
                  
                  s_fonte =
                    paste(
                      unique(
                        s_fonte
                      ),
                      collapse = ","
                    ),
                  
                  
                  s_valor_r11 =
                    sum(
                      val_fonte,
                      na.rm = TRUE
                    ),
                  
                  
                  sicom_existe_r11 =
                    TRUE
                ) %>%
                
                
                distinct(
                  chave,
                  .keep_all = TRUE
                )
              
              
            } else {
              
              
              sicom_11_proc <- data.frame(
                
                chave =
                  character(0),
                
                s_grupo =
                  character(0),
                
                s_fonte =
                  character(0),
                
                s_valor_r11 =
                  numeric(0),
                
                sicom_existe_r11 =
                  logical(0)
              )
            }
            
            
            # ==================================================================
            # 3. CRUZAMENTO REGISTRO 10
            # ==================================================================
            
            setProgress(
              value = 0.80,
              detail = "Cruzando SOF e SICOM..."
            )
            
            
            message("EMP | Comparando campos do Registro 10 | ", format(Sys.time(), "%H:%M:%S"))
            setProgress(value = .83, detail = "Comparando campos do Registro 10...")
            
            analise_10 <-
              
              full_join(
                
                sof_proc,
                
                sicom_10_proc,
                
                by = "chave"
                
              ) %>%
              
              
              mutate(
                
                Empenho_Vis =
                  coalesce(
                    
                    as.character(
                      emp_num
                    ),
                    
                    emp_vis
                  ),
                
                
                UO_Vis =
                  coalesce(
                    
                    as.character(
                      uo_num
                    ),
                    
                    uo_vis
                  ),
                
                
                Diferenca =
                  round(
                    
                    coalesce(
                      f_valor,
                      0
                    ) -
                      
                      coalesce(
                        s_valor,
                        0
                      ),
                    
                    2
                  ),
                
                
                # ==============================================================
                # CONTRATO
                # ==============================================================
                
                ignorar_pos_17 =
                  coalesce(
                    s_pos_17,
                    "0"
                  ) == "2",
                
                
                contrato_conforme =
                  if_else(
                    
                    ignorar_pos_17,
                    
                    TRUE,
                    
                    coalesce(
                      str_f_contrato,
                      ""
                    ) ==
                      coalesce(
                        str_s_contrato,
                        ""
                      )
                  ),
                
                
                # ==============================================================
                # PROCESSO LICITATÓRIO
                # ==============================================================
                
                ignorar_pos_30_proc =
                  str_detect(
                    
                    paste0(
                      ",",
                      coalesce(
                        s_licitacao,
                        ""
                      ),
                      ","
                    ),
                    
                    ",(1|99),"
                  ),
                
                
                processo_conforme =
                  case_when(
                    
                    ignorar_pos_30_proc ~
                      TRUE,
                    
                    
                    is.na(
                      str_s_processo
                    ) |
                      str_s_processo == "0" |
                      str_s_processo == "" ~
                      TRUE,
                    
                    
                    coalesce(
                      str_s_processo ==
                        str_f_processo,
                      FALSE
                    ) ~
                      TRUE,
                    
                    
                    coalesce(
                      str_s_processo ==
                        str_f_proc_compra,
                      FALSE
                    ) ~
                      TRUE,
                    
                    
                    coalesce(
                      str_s_processo ==
                        str_f_proc_modalidade,
                      FALSE
                    ) ~
                      TRUE,
                    
                    
                    coalesce(
                      str_s_processo ==
                        str_f_ata_adesao,
                      FALSE
                    ) ~
                      TRUE,
                    
                    
                    coalesce(
                      str_s_processo ==
                        str_f_ata_reg_preco,
                      FALSE
                    ) ~
                      TRUE,
                    
                    
                    TRUE ~
                      FALSE
                  ),
                
                
                # ==============================================================
                # TERMO ADITIVO
                #
                # NOVA CORREÇÃO
                #
                # f_aditivo e s_aditivo já estão normalizados.
                #
                # Exemplos considerados iguais:
                #
                # ""  e ""
                # "0" e ""
                # ""  e "0"
                # 01  e 1
                # ==============================================================
                
                aditivo_conforme =
                  if_else(
                    
                    coalesce(
                      permitir_v_vazio,
                      FALSE
                    ) &
                      
                      coalesce(
                        v_está_vazio,
                        FALSE
                      ),
                    
                    TRUE,
                    
                    coalesce(
                      f_aditivo,
                      ""
                    ) ==
                      coalesce(
                        s_aditivo,
                        ""
                      )
                  ),
                
                
                # --------------------------------------------------------------
                # CONGE
                # --------------------------------------------------------------
                
                conge_ausente =
                  
                  !is.na(
                    dt_conge_pos_29
                  ) &
                  
                  dt_conge_pos_29 >
                  as.Date(
                    "2014-01-01"
                  ) &
                  
                  (
                    nro_conge_pos_27 ==
                      "0" |
                      
                      nro_conge_pos_27 ==
                      ""
                  ),
                
                
                is_antes_2012 =
                  
                  !is.na(
                    dt_pos_21
                  ) &
                  
                  dt_pos_21 <
                  as.Date(
                    "2012-12-31"
                  ),
                
                
                # ==============================================================
                # MODALIDADE
                #
                # Pregão                              -> 2, 5 ou 9
                # Dispensa                            -> 1, 3 ou 6
                # Não se aplica                       -> 99
                # Inexigibilidade                     -> 3 ou 6
                # Concorrência                        -> 2 ou 5
                # Inexigibilidade/Credenciamento      -> 3 ou 6
                # Procedimento agência estrangeira    -> 99
                # RDC                                 -> 7
                # Tomada de preços                    -> 2 ou 5
                # Chamamento                          -> 3 ou 6
                # Procedimento de credenciamento      -> 3 ou 6
                # ==============================================================
                
                modalidade_calculada =
                  case_when(
                    
                    is_antes_2012 &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",99,"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade == "PREGAO" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(2|5|9),"
                      ) ~
                      TRUE,
                    
                    f_modalidade == "PROCEDIMENTO PROPRIO" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(2|5),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade == "DISPENSA" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(1|3|6),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade == "NAO SE APLICA" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",99,"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade == "INEXIGIBILIDADE" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(3|6),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade == "CONCORRENCIA" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(2|5),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade ==
                      "INEXIGIBILIDADE/CREDENCIAMENTO" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(3|6),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade ==
                      "PROCEDIMENTO DE AGENCIA ESTRANGEIRA" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",99,"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade == "RDC" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",7,"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade ==
                      "TOMADA DE PRECOS" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(2|5),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade ==
                      "CHAMAMENTO" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(3|6),"
                      ) ~
                      TRUE,
                    
                    
                    f_modalidade ==
                      "PROCEDIMENTO DE CREDENCIAMENTO" &
                      str_detect(
                        paste0(
                          ",",
                          coalesce(
                            s_licitacao,
                            ""
                          ),
                          ","
                        ),
                        ",(3|6),"
                      ) ~
                      TRUE,
                    
                    
                    is.na(
                      f_modalidade
                    ) |
                      f_modalidade == "0" |
                      f_modalidade == "" ~
                      TRUE,
                    
                    
                    TRUE ~
                      FALSE
                  ),
                
                
                # Quando houver parceria OU congênere informado no SOF,
                # o código 99 na posição 30 do SICOM é permitido.
                # A regra não dispensa outras verificações do empenho.
                parceria_ou_congenere_preenchido =
                  (!is.na(f_parceria) & f_parceria != "" & f_parceria != "0") |
                  (!is.na(f_congenere) & f_congenere != "" & f_congenere != "0"),
                
                modalidade_99_com_parceria =
                  parceria_ou_congenere_preenchido &
                  stringr::str_detect(
                    paste0(",", coalesce(s_licitacao, ""), ","),
                    stringr::regex(",99,")
                  ),
                
                # DISPENSA no SOF e modalidade 99 no SICOM (posição 30)
                # também são compatíveis quando o exercício do processo
                # de modalidade é menor ou igual a 2012.
                modalidade_99_dispensa_ate_2012 =
                  coalesce(f_modalidade == "DISPENSA", FALSE) &
                  !is.na(ano_proc_modalidade) &
                  ano_proc_modalidade <= 2012 &
                  stringr::str_detect(
                    paste0(",", coalesce(s_licitacao, ""), ","),
                    stringr::regex(",99,")
                  ),
                
                modalidade_conforme =
                  case_when(
                    modalidade_99_com_parceria ~ TRUE,
                    modalidade_99_dispensa_ate_2012 ~ TRUE,
                    
                    !is.na(
                      ano_proc_modalidade
                    ) &
                      ano_proc_modalidade < 2012 ~
                      TRUE,
                    
                    coalesce(
                      ignorar_modalidade,
                      FALSE
                    ) ~
                      TRUE,
                    
                    TRUE ~
                      modalidade_calculada
                  ),
                
                
                # ==============================================================
                # ITEM / SUBELEMENTO
                # ==============================================================
                
                ignorar_item =
                  coalesce(
                    
                    normalizar_codigo_comparacao(
                      s_pos_10
                    ),
                    
                    "0"
                  ) == "30",
                
                
                item_conforme =
                  ignorar_item |
                  (
                    f_item ==
                      s_item
                  ),
                
                
                # ==============================================================
                # TODAS AS DIVERGÊNCIAS DO UO + EMPENHO
                # ==============================================================
                motivo_conge = if_else(coalesce(conge_ausente,FALSE),"Número do conge não informado",""),
                motivo_modalidade_empenho = if_else(
                  coalesce(f_modalidade_empenho,"") != coalesce(s_modalidade_empenho,""),
                  paste0("Modalidade do empenho: SOF ",coalesce(f_modalidade_empenho,""),
                         " diferente de SICOM ",coalesce(s_modalidade_empenho,"")),""),
                # Exercício do contrato: SOF exercicio_contrato x SICOM V21.
                # V17 = 2: não apontar divergência deste campo.
                motivo_exercicio_contrato = if_else(
                  coalesce(s_pos_17, "0") != "2" &
                    normalizar_codigo_comparacao(coalesce(exercicio_contrato, "")) !=
                    coalesce(s_exercicio_contrato_21, ""),
                  paste0(
                    "Exercício do contrato: SOF ",
                    ifelse(normalizar_codigo_comparacao(coalesce(exercicio_contrato, "")) == "",
                           "(vazio)", normalizar_codigo_comparacao(coalesce(exercicio_contrato, ""))),
                    " diferente de SICOM posição 21 ",
                    ifelse(coalesce(s_exercicio_contrato_21, "") == "",
                           "(vazio)", coalesce(s_exercicio_contrato_21, ""))
                  ),
                  ""
                ),
                motivo_modalidade = if_else(!coalesce(modalidade_conforme,TRUE),
                                            paste0("Modalidade diferente SOF '",coalesce(f_modalidade,""),
                                                   "' e SICOM ",coalesce(s_licitacao,"")),""),
                motivo_processo = if_else(!coalesce(processo_conforme,TRUE),
                                          paste0("Processo licitatório divergente: SOF ",coalesce(str_f_processo,""),
                                                 " / SICOM ",coalesce(str_s_processo,""),". Avaliar"),""),
                motivo_contrato = if_else(!coalesce(contrato_conforme,TRUE),
                                          paste0("Contrato divergente: SOF ",coalesce(str_f_contrato,""),
                                                 " diferente de SICOM ",coalesce(str_s_contrato,"")),""),
                motivo_funcao = if_else(coalesce(f_funcao,"") != coalesce(s_funcao,""),
                                        paste0("Função: SOF ",coalesce(f_funcao,"")," diferente de SICOM ",coalesce(s_funcao,"")),""),
                motivo_subfuncao = if_else(coalesce(f_subfuncao,"") != coalesce(s_subfuncao,""),
                                           paste0("Subfunção: SOF ",coalesce(f_subfuncao,"")," diferente de SICOM ",coalesce(s_subfuncao,"")),""),
                motivo_programa = if_else(coalesce(f_programa,"") != coalesce(s_programa,""),
                                          paste0("Programa: SOF ",coalesce(f_programa,"")," diferente de SICOM ",coalesce(s_programa,"")),""),
                motivo_projativ = if_else(coalesce(f_projativ,"") != coalesce(s_projativ,""),
                                          paste0("ProjAtiv/idAcao: SOF ",coalesce(f_projativ,"")," diferente de SICOM ",coalesce(s_projativ,"")),""),
                motivo_sub_acao = if_else(coalesce(f_sub_acao, "") != coalesce(s_sub_acao, ""),
                                          paste0("Subação: SOF ", coalesce(f_sub_acao, ""),
                                                 " diferente de SICOM ", coalesce(s_sub_acao, "")), ""),
                motivo_parceria_congenere = if_else(
                  coalesce(s_parceria_ou_congenere, "") != coalesce(f_parceria, "") &
                    coalesce(s_parceria_ou_congenere, "") != coalesce(f_congenere, ""),
                  paste0("Parceria/Congênere: SICOM nroConvenioConge ",
                         ifelse(coalesce(s_parceria_ou_congenere, "") == "", "(vazio)", s_parceria_ou_congenere),
                         " diferente de SOF parceria ",
                         ifelse(coalesce(f_parceria, "") == "", "(vazio)", f_parceria),
                         " e SOF congênere ",
                         ifelse(coalesce(f_congenere, "") == "", "(vazio)", f_congenere)), ""),
                motivo_natureza = if_else(coalesce(f_natureza,"") != coalesce(s_natureza,""),
                                          paste0("Natureza: SOF ",coalesce(f_natureza,"")," diferente de SICOM ",coalesce(s_natureza,"")),""),
                motivo_item = if_else(!coalesce(item_conforme,TRUE),
                                      paste0("Item/SubElemento: SOF ",coalesce(f_item,"")," diferente de SICOM ",coalesce(s_item,"")),""),
                motivo_aditivo = if_else(!coalesce(ignorar_pos_17,FALSE) & !coalesce(aditivo_conforme,TRUE),
                                         paste0("Termo Aditivo: SOF ",ifelse(coalesce(f_aditivo,"")=="","(vazio)",coalesce(f_aditivo,"")),
                                                " diferente de SICOM ",ifelse(coalesce(s_aditivo,"")=="","(vazio)",coalesce(s_aditivo,""))),""),
                motivo_valor = if_else(coalesce(Diferenca,0)!=0,
                                       paste0("Valor divergente: Dif R$ ",fmt_reais(coalesce(Diferenca,0))),"")
              ) %>%
              rowwise() %>%
              mutate(
                Motivo = case_when(
                  is.na(sof_existe) ~ "Ausente no SOF",
                  is.na(sicom_existe) ~ "Ausente no SICOM (Reg 10)",
                  TRUE ~ {
                    m <- c(motivo_conge,motivo_modalidade_empenho,motivo_exercicio_contrato,motivo_modalidade,
                           motivo_processo,motivo_contrato,motivo_funcao,motivo_subfuncao,motivo_programa,
                           motivo_projativ,motivo_sub_acao,motivo_natureza,motivo_item,
                           motivo_parceria_congenere,motivo_aditivo,motivo_valor)
                    m <- m[!is.na(m) & m!=""]
                    if(length(m)==0) "Conforme" else paste(m,collapse="; ")
                  }
                )
              ) %>%
              ungroup() %>%
              
              
              filter(
                Motivo != "Conforme"
              )
            
            
            # ==================================================================
            # REGISTRO 11
            # ==================================================================
            
            message("EMP | Comparando campos do Registro 11 | ", format(Sys.time(), "%H:%M:%S"))
            setProgress(value = .90, detail = "Comparando campos do Registro 11...")
            
            analise_11 <-
              
              full_join(
                
                sof_proc,
                
                sicom_11_proc,
                
                by = "chave"
                
              ) %>%
              
              
              mutate(
                
                Empenho_Vis =
                  coalesce(
                    
                    as.character(
                      emp_num
                    ),
                    
                    "Extraído SICOM"
                  ),
                
                
                UO_Vis =
                  coalesce(
                    
                    as.character(
                      uo_num
                    ),
                    
                    "Extraído SICOM"
                  ),
                
                
                grupo_match =
                  stringr::str_detect(
                    
                    coalesce(
                      s_grupo,
                      ""
                    ),
                    
                    stringr::fixed(
                      
                      coalesce(
                        f_grupo,
                        ""
                      )
                    )
                  ),
                
                
                fonte_match =
                  stringr::str_detect(
                    
                    coalesce(
                      s_fonte,
                      ""
                    ),
                    
                    stringr::fixed(
                      
                      coalesce(
                        f_fonte,
                        ""
                      )
                    )
                  ),
                
                
                Motivo =
                  case_when(
                    
                    is.na(
                      sof_existe
                    ) ~
                      "Ausente no SOF",
                    
                    
                    is.na(
                      sicom_existe_r11
                    ) ~
                      "Ausente no SICOM (Reg 11)",
                    
                    
                    !grupo_match ~
                      paste0(
                        
                        "Grupo Fonte: SOF ",
                        
                        f_grupo,
                        
                        " diferente do SICOM ",
                        
                        s_grupo
                      ),
                    
                    
                    !fonte_match ~
                      paste0(
                        
                        "Fonte: SOF ",
                        
                        f_fonte,
                        
                        " diferente do SICOM ",
                        
                        s_fonte
                      ),
                    
                    
                    TRUE ~
                      "Conforme"
                  )
              ) %>%
              
              
              filter(
                Motivo != "Conforme"
              )
            
            
            # ==================================================================
            # TOTALIZADORES
            #
            # NÃO ALTERADOS
            # ==================================================================
            
            tot_sof <- sum(
              
              sof_proc$f_valor,
              
              na.rm = TRUE
            )
            
            
            tot_r11 <- sum(
              
              sicom_11_proc$s_valor_r11,
              
              na.rm = TRUE
            )
            
            
            setProgress(
              value = 1.0,
              detail = "Finalizado!"
            )
            
            
            return(
              
              list(
                
                
                r10 =
                  analise_10,
                
                r11 =
                  analise_11,
                
                tot_sof =
                  tot_sof,
                
                tot_r10 =
                  tot_r10,
                
                tot_r11 =
                  tot_r11
              )
            )
            
            
          }, error = function(e) {
            
            # Mostra a causa verdadeira, em vez de ocultá-la com uma
            # notificação de tipo inválido. A mensagem permanece no console.
            erro <- conditionMessage(e)
            message("ERRO NO PROCESSAMENTO EMP: ", erro)
            message("Chamadas ativas no momento do erro:")
            chamadas <- sys.calls()
            for (chamada in tail(chamadas, 12)) {
              message(paste(deparse(chamada), collapse = " "))
            }
            
            showNotification(
              paste0("Erro no processamento EMP: ", erro),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
            showModal(modalDialog(
              title = "Erro no processamento EMP",
              tags$p(erro),
              tags$p("Copie esta mensagem e envie para identificar a etapa que falhou."),
              easyClose = TRUE,
              footer = modalButton("Fechar")
            ))
            
            return(NULL)
          })
        }
      )
    }
  )
  
  
  # ==============================================================================
  # RESUMO FINANCEIRO
  #
  # LAYOUT E TOTALIZADORES MANTIDOS
  # ==============================================================================
  
  output$box_resumo <- renderUI({
    
    res <- dados_processados()
    
    
    if (
      is.null(
        res
      )
    ) {
      
      return(
        
        p(
          "Aguardando carregamento de arquivos e execução do processamento."
        )
      )
    }
    
    
    v_sof <-
      res$tot_sof
    
    
    v_r10 <-
      res$tot_r10
    
    
    v_r11 <-
      res$tot_r11
    
    
    dif_r10 <-
      round(
        v_sof -
          v_r10,
        2
      )
    
    
    dif_r11 <-
      round(
        v_sof -
          v_r11,
        2
      )
    
    
    tagList(
      
      h3(
        "Resumo Financeiro"
      ),
      
      hr(),
      
      p(
        
        strong(
          "Total SOF: "
        ),
        
        "R$ ",
        
        fmt_reais(
          v_sof
        )
      ),
      
      
      p(
        
        strong(
          "Total SICOM Registro 10: "
        ),
        
        "R$ ",
        
        fmt_reais(
          v_r10
        )
      ),
      
      
      p(
        
        strong(
          "Total SICOM Registro 11: "
        ),
        
        "R$ ",
        
        fmt_reais(
          v_r11
        )
      ),
      
      
      hr(),
      
      
      p(
        
        strong(
          "Diferença entre SOF e Registro 10: "
        ),
        
        "R$ ",
        
        fmt_reais(
          dif_r10
        )
      ),
      
      
      p(
        
        strong(
          "Diferença entre SOF e Registro 11: "
        ),
        
        "R$ ",
        
        fmt_reais(
          dif_r11
        )
      )
    )
  })
  
  
  # ==============================================================================
  # TABELA REGISTRO 10
  # ==============================================================================
  
  output$tbl_r10 <- renderTable({
    
    res <- dados_processados()
    
    
    if (
      is.null(
        res
      )
    ) {
      
      return(NULL)
    }
    
    
    res$r10 %>%
      
      select(
        
        UO_Vis,
        
        Empenho_Vis,
        
        Motivo
        
      ) %>%
      
      head(100)
  })
  
  
  # ==============================================================================
  # TABELA REGISTRO 11
  # ==============================================================================
  
  output$tbl_r11 <- renderTable({
    
    res <- dados_processados()
    
    
    if (
      is.null(
        res
      )
    ) {
      
      return(NULL)
    }
    
    
    res$r11 %>%
      
      select(
        
        UO_Vis,
        
        Empenho_Vis,
        
        Motivo
        
      ) %>%
      
      head(100)
  })
  
  
  # ==============================================================================
  # DOWNLOAD CSV
  # ==============================================================================
  
  output$dl_excel <- downloadHandler(
    
    filename = function() {
      
      paste0(
        
        "Relatorio_Divergencias_SOF_SICOM_",
        
        Sys.Date(),
        
        ".csv"
      )
    },
    
    
    content = function(file) {
      
      res <- dados_processados()
      
      
      if (
        !is.null(
          res
        )
      ) {
        
        write.csv2(
          
          res$r10,
          
          file,
          
          row.names = FALSE
        )
      }
    }
  )
}


# ==============================================================================
# 5. EXECUÇÃO DO APLICATIVO
# ==============================================================================

shinyApp(
  ui = ui,
  server = server
)