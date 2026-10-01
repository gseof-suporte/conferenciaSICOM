# ==============================================================================
# SUBAPP - CONFERÊNCIA LQD (ADMINISTRAÇÃO DIRETA)
# AUDITORIA CAMPO A CAMPO + VALORES
# ==============================================================================


# ==============================================================================
# 1. PACOTES
# ==============================================================================

packages_needed <- c(
  "shiny",
  "shinythemes",
  "readxl",
  "dplyr",
  "tidyverse",
  "DT",
  "data.table",
  "tibble",
  "tidyr",
  "purrr",
  "openxlsx",
  "stringi"
)

for (pkg in packages_needed) {
  
  if (!require(pkg, character.only = TRUE)) {
    
    install.packages(
      pkg,
      repos = "https://cloud.r-project.org"
    )
    
    library(
      pkg,
      character.only = TRUE
    )
  }
}


library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(DT)
library(openxlsx)
library(stringi)


# ==============================================================================
# 2. FUNÇÕES AUXILIARES
# ==============================================================================


# ------------------------------------------------------------------------------
# Normaliza textos/nome de colunas
# ------------------------------------------------------------------------------

normalize_string <- function(s) {
  
  s <- as.character(s)
  
  s[is.na(s)] <- ""
  
  s <- tolower(s)
  
  s <- stringi::stri_trans_general(
    s,
    "Latin-ASCII"
  )
  
  s <- stringr::str_remove_all(
    s,
    "[^a-z0-9]"
  )
  
  return(s)
}


# ------------------------------------------------------------------------------
# Localiza coluna
# ------------------------------------------------------------------------------

find_col_safe <- function(df, target_names) {
  
  if (
    is.null(df) ||
    ncol(df) == 0
  ) {
    
    return(NULL)
  }
  
  actual_cols <- colnames(df)
  
  cols_clean <- sapply(
    actual_cols,
    normalize_string
  )
  
  targets_clean <- sapply(
    target_names,
    normalize_string
  )
  
  
  # Primeiro tenta igualdade exata
  for (t in targets_clean) {
    
    match_idx <- which(
      cols_clean == t
    )
    
    if (length(match_idx) > 0) {
      
      return(
        actual_cols[
          match_idx[1]
        ]
      )
    }
  }
  
  
  # Depois tenta ocorrência parcial
  for (t in targets_clean) {
    
    match_idx <- which(
      stringr::str_detect(
        cols_clean,
        stringr::fixed(t)
      )
    )
    
    if (length(match_idx) > 0) {
      
      return(
        actual_cols[
          match_idx[1]
        ]
      )
    }
  }
  
  
  return(NULL)
}


# ------------------------------------------------------------------------------
# Exige que determinada coluna exista
#
# Evita o problema anterior de transformar campo não encontrado em zero
# silenciosamente.
# ------------------------------------------------------------------------------

require_col <- function(
    df,
    nomes,
    descricao,
    registro
) {
  
  coluna <- find_col_safe(
    df,
    nomes
  )
  
  
  if (is.null(coluna)) {
    
    stop(
      paste0(
        "Registro ",
        registro,
        ": não foi possível localizar no arquivo SOF o campo obrigatório '",
        descricao,
        "'. Colunas disponíveis: ",
        paste(
          colnames(df),
          collapse = ", "
        )
      )
    )
  }
  
  
  return(coluna)
}


# ------------------------------------------------------------------------------
# Conversão numérica brasileira
# ------------------------------------------------------------------------------

to_numeric_br_safe <- function(x) {
  
  if (is.null(x)) {
    return(numeric(0))
  }
  
  
  x <- as.character(x)
  
  x[is.na(x)] <- ""
  
  x <- stringr::str_trim(x)
  
  x <- stringr::str_remove_all(
    x,
    "\\s+"
  )
  
  
  tem_virgula <- stringr::str_detect(
    x,
    ","
  )
  
  
  x[tem_virgula] <- stringr::str_replace_all(
    x[tem_virgula],
    "\\.",
    ""
  )
  
  
  x[tem_virgula] <- stringr::str_replace_all(
    x[tem_virgula],
    ",",
    "."
  )
  
  
  resultado <- suppressWarnings(
    as.numeric(x)
  )
  
  
  resultado[
    is.na(resultado)
  ] <- 0
  
  
  return(resultado)
}


# ------------------------------------------------------------------------------
# Normalização de código numérico mantido como texto
#
# 0004 -> "4"
# 04   -> "4"
# 4    -> "4"
#
# IMPORTANTE:
# o resultado é CHARACTER
# ------------------------------------------------------------------------------

normalizar_codigo <- function(x) {
  
  x <- as.character(x)
  
  x[is.na(x)] <- ""
  
  x <- trimws(x)
  
  vazio_original <- x == ""
  
  # Remove .0 eventualmente criado pelo Excel
  x <- stringr::str_remove(
    x,
    "\\.0$"
  )
  
  # Somente dígitos
  x <- gsub(
    "[^0-9]",
    "",
    x
  )
  
  # Retira zeros à esquerda
  x <- sub(
    "^0+",
    "",
    x
  )
  
  # Se era composto somente por zeros
  x[
    x == "" &
      !vazio_original
  ] <- "0"
  
  # Vazio continua vazio
  x[
    vazio_original
  ] <- ""
  
  
  return(
    as.character(x)
  )
}


# ------------------------------------------------------------------------------
# CPF
# ------------------------------------------------------------------------------

normalizar_cpf <- function(x) {
  
  x <- as.character(x)
  
  x[is.na(x)] <- ""
  
  x <- gsub(
    "[^0-9]",
    "",
    x
  )
  
  x <- sub(
    "^0+",
    "",
    x
  )
  
  return(x)
}


# ------------------------------------------------------------------------------
# Tipo de liquidação do SOF
#
# Despesa do Exercício = 1
# Despesas Restos a Pagar Não Processados = 2
# ------------------------------------------------------------------------------

normalizar_tipo_liquidacao_sof <- function(x) {
  
  texto <- as.character(x)
  
  texto[is.na(texto)] <- ""
  
  texto <- tolower(texto)
  
  texto <- stringi::stri_trans_general(
    texto,
    "Latin-ASCII"
  )
  
  texto <- trimws(texto)
  
  texto <- gsub(
    "\\s+",
    " ",
    texto
  )
  
  
  resultado <- dplyr::case_when(
    
    stringr::str_detect(
      texto,
      "restos a pagar nao processados"
    ) ~ "2",
    
    
    stringr::str_detect(
      texto,
      "despesa do exercicio"
    ) ~ "1",
    
    
    texto %in% c(
      "1",
      "01"
    ) ~ "1",
    
    
    texto %in% c(
      "2",
      "02"
    ) ~ "2",
    
    
    TRUE ~
      normalizar_codigo(
        texto
      )
  )
  
  
  return(resultado)
}


# ------------------------------------------------------------------------------
# Extração do número interno do identificador SICOM
#
# Mantida a regra do código anterior:
# posições 10 a 14
# ------------------------------------------------------------------------------

extract_sicom_sub <- function(vec) {
  
  val_str <- as.character(vec)
  
  val_str[is.na(val_str)] <- ""
  
  val_str <- stringr::str_remove(
    val_str,
    "\\.0$"
  )
  
  val_digits <- stringr::str_replace_all(
    val_str,
    "\\D",
    ""
  )
  
  
  resultado <- vapply(
    
    val_digits,
    
    FUN.VALUE = character(1),
    
    FUN = function(valor) {
      
      if (
        nchar(valor) >= 14
      ) {
        
        miolo <- substr(
          valor,
          10,
          14
        )
        
      } else {
        
        miolo <- valor
      }
      
      
      miolo <- normalizar_codigo(
        miolo
      )
      
      
      if (
        is.na(miolo) ||
        miolo == ""
      ) {
        
        miolo <- "0"
      }
      
      
      return(miolo)
    }
  )
  
  
  return(resultado)
}


# ------------------------------------------------------------------------------
# Normalização de datas LQD
# ------------------------------------------------------------------------------
normalizar_data_lqd <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  resultado <- rep("", length(x))
  for (i in seq_along(x)) {
    valor <- x[i]
    if (valor == "") next
    digitos <- gsub("[^0-9]", "", valor)
    if (nchar(digitos) == 8 && !grepl("-", valor)) {
      resultado[i] <- paste0(substr(digitos,5,8),"-",substr(digitos,3,4),"-",substr(digitos,1,2))
    } else if (grepl("^\\d{4}-\\d{2}-\\d{2}", valor)) {
      resultado[i] <- substr(valor,1,10)
    } else if (grepl("^\\d{2}/\\d{2}/\\d{4}", valor)) {
      resultado[i] <- paste0(substr(valor,7,10),"-",substr(valor,4,5),"-",substr(valor,1,2))
    } else {
      resultado[i] <- valor
    }
  }
  resultado
}

extrair_ano_id_sicom <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  ifelse(nchar(x) >= 4, substr(x,1,4), "")
}

extrair_uo_sof_id_sicom <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  normalizar_codigo(ifelse(nchar(x) >= 8, substr(x,5,8), ""))
}


# ------------------------------------------------------------------------------
# Leitura segura
# ------------------------------------------------------------------------------

read_data_safe <- function(
    file_path,
    has_headers = TRUE
) {
  
  ext <- tools::file_ext(
    file_path
  )
  
  
  if (
    tolower(ext) == "csv"
  ) {
    
    first_line <- readLines(
      file_path,
      n = 1,
      warn = FALSE
    )
    
    
    my_delim <- if (
      length(first_line) > 0 &&
      stringr::str_detect(
        first_line[1],
        ";"
      )
    ) {
      
      ";"
      
    } else {
      
      ","
    }
    
    
    return(
      
      suppressWarnings(
        
        readr::read_delim(
          file_path,
          delim = my_delim,
          col_names = has_headers,
          col_types = readr::cols(
            .default = "c"
          ),
          show_col_types = FALSE
        )
      )
    )
    
  } else {
    
    return(
      
      readxl::read_excel(
        file_path,
        col_names = has_headers,
        col_types = "text"
      )
    )
  }
}


# ------------------------------------------------------------------------------
# Colapsa valores únicos para comparação
#
# Útil quando uma chave possui mais de uma linha.
# ------------------------------------------------------------------------------

colapsar_unicos <- function(x) {
  
  x <- as.character(x)
  
  x[is.na(x)] <- ""
  
  x <- x[
    x != ""
  ]
  
  
  if (
    length(x) == 0
  ) {
    
    return("")
  }
  
  
  paste(
    sort(
      unique(x)
    ),
    collapse = ","
  )
}


# ------------------------------------------------------------------------------
# Formatação do valor para mensagem
# ------------------------------------------------------------------------------

fmt_money <- function(val) {
  
  if (
    length(val) == 0 ||
    is.na(val)
  ) {
    
    val <- 0
  }
  
  
  paste0(
    
    "R$ ",
    
    format(
      round(
        val,
        2
      ),
      big.mark = ".",
      decimal.mark = ",",
      nsmall = 2,
      scientific = FALSE
    )
  )
}


# ==============================================================================
# 3. INTERFACE DO USUÁRIO (UI)
#
# LAYOUT COPIADO DO MODELO DE CONFERÊNCIA DE DADOS CONTÁBEIS
# REGRAS DE NEGÓCIO DO LQD MANTIDAS SEM ALTERAÇÃO
# ==============================================================================

file_types_allowed <- c(
  ".csv",
  ".xlsx",
  ".xls",
  "text/csv",
  "text/comma-separated-values",
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "application/vnd.ms-excel"
)


ui <- fluidPage(
  
  titlePanel(
    "Conferência de Dados Contábeis: SOF x SICOM - LQD"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "file_sof10",
        "Relatório LQD SOF Reg 10 (.csv/.xlsx)",
        accept = file_types_allowed
      ),
      
      fileInput(
        "file_sof11",
        "Relatório LQD SOF Reg 11 (.csv/.xlsx)",
        accept = file_types_allowed
      ),
      
      fileInput(
        "file_sof12",
        "Relatório LQD SOF Reg 12 (.csv/.xlsx)",
        accept = file_types_allowed
      ),
      
      fileInput(
        "file_sof20",
        "Relatório LQD SOF Reg 20 (.csv/.xlsx)",
        accept = file_types_allowed
      ),
      
      fileInput(
        "file_sicom",
        "Relatório LQD SICOM Bruto (.csv/.xlsx)",
        accept = file_types_allowed
      ),
      
      actionButton(
        "btn_executar",
        "Executar Conferência",
        class = "btn-primary",
        style = "width: 100%; margin-top: 10px;"
      ),
      
      hr(),
      
      helpText(
        paste0(
          "Faça o upload do arquivo SICOM e dos relatórios SOF que deseja conferir ",
          "e clique no botão acima para rodar o cruzamento."
        )
      )
    ),
    
    mainPanel(
      
      # ------------------------------------------------------------------------
      # MENSAGENS
      # ------------------------------------------------------------------------
      
      uiOutput(
        "output_alertas"
      ),
      
      # ------------------------------------------------------------------------
      # TABELAS DETALHADAS INTERATIVAS
      # ------------------------------------------------------------------------
      
      tabsetPanel(
        
        id = "tabs_auditoria",
        
        tabPanel(
          "Resumo da Consolidação",
          verbatimTextOutput(
            "txt_resumo"
          )),
        
        tabPanel(
          "Divergências Registro 10",
          DTOutput(
            "tbl_reg10"
          )
        ),
        
        tabPanel(
          "Divergências Registro 11",
          DTOutput(
            "tbl_reg11"
          )
        ),
        
        tabPanel(
          "Divergências Registro 12",
          DTOutput(
            "tbl_reg12"
          )
        ),
        
        tabPanel(
          "Divergências Registro 20",
          DTOutput(
            "tbl_reg20"
          )
        )
        
        
      )
    )
  )
)


# ==============================================================================
# 4. SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  
  dados_processados <- reactiveValues(
    
    sucesso = FALSE,
    
    logs = "",
    
    erro = NULL,
    
    r10 = NULL,
    
    r11 = NULL,
    
    r12 = NULL,
    
    r20 = NULL
  )
  
  
  observeEvent(
    input$btn_executar,
    {
      
      
      dados_processados$sucesso <- FALSE
      
      dados_processados$erro <- NULL
      
      
      tryCatch({
        
        
        # ======================================================================
        # SICOM OBRIGATÓRIO
        # ======================================================================
        
        if (
          is.null(
            input$file_sicom
          )
        ) {
          
          stop(
            "O arquivo do SICOM é obrigatório."
          )
        }
        
        
        
        # ======================================================================
        # LEITURA SICOM
        # ======================================================================
        
        sicom_raw <- read_data_safe(
          input$file_sicom$datapath,
          has_headers = FALSE
        )
        
        
        if (
          ncol(
            sicom_raw
          ) < 2
        ) {
          
          stop(
            "Falha na formatação do SICOM: colunas não detectadas."
          )
        }
        
        
        # ======================================================================
        # POSIÇÕES FIXAS DO ARQUIVO SICOM
        #
        # NÃO SÃO MAIS UTILIZADOS ARQUIVOS EXTERNOS DE CABEÇALHO.
        # Todas as conferências do SICOM são feitas por posição de coluna.
        # ======================================================================
        
        POS_R10_COD_RED     <- 2
        POS_R10_ORGAO       <- 3
        POS_R10_UO          <- 4
        POS_R10_TIPO_LIQ    <- 5
        POS_R10_EMPENHO_ID  <- 6
        POS_R10_DATA_EMP    <- 7
        POS_R10_DATA_LIQ    <- 8
        POS_R10_LIQ_ID      <- 9
        POS_R10_VALOR       <- 10
        POS_R10_CPF         <- 11
        
        POS_R11_COD_RED     <- 2
        POS_R11_FONTE       <- 3
        POS_R11_VALOR       <- 5
        
        POS_R12_COD_RED     <- 2
        POS_R12_EXERCICIO   <- 4
        POS_R12_VALOR       <- 5
        
        POS_R20_COD_RED     <- 2
        POS_R20_FONTE       <- 3
        POS_R20_CO          <- 4
        POS_R20_VALOR       <- 7
        
        # ======================================================================
        # SEPARA OS REGISTROS SICOM SEM CABEÇALHO
        # ======================================================================
        
        split_layout <- function(df_bruto, id_alvo) {
          
          if (is.null(df_bruto) || nrow(df_bruto) == 0) {
            return(as_tibble(df_bruto))
          }
          
          subconjunto <- df_bruto[
            !is.na(df_bruto[[1]]) &
              as.character(df_bruto[[1]]) == as.character(id_alvo),
          ]
          
          return(as_tibble(subconjunto))
        }
        
        s10 <- split_layout(sicom_raw, "10")
        s11 <- split_layout(sicom_raw, "11")
        s12 <- split_layout(sicom_raw, "12")
        s20 <- split_layout(sicom_raw, "20")
        
        
        # ======================================================================
        # FILTRO DIRETA
        #
        # codReduzido = 2ª coluna em todos os registros SICOM.
        # ======================================================================
        
        filter_direta <- function(df) {
          
          if (is.null(df) || nrow(df) == 0 || ncol(df) < 2) {
            return(df)
          }
          
          vetor_red <- as.character(df[[2]])
          
          df <- df[
            !is.na(vetor_red) &
              nchar(vetor_red) > 9,
          ]
          
          return(df)
        }
        
        s10 <- filter_direta(s10)
        s11 <- filter_direta(s11)
        s12 <- filter_direta(s12)
        s20 <- filter_direta(s20)
        
        
        # ======================================================================
        # CHAVE SICOM REGISTRO 10
        #
        # Empenho = coluna 6
        # Liquidação = coluna 9
        # codReduzido = coluna 2
        # ======================================================================
        
        if (nrow(s10) > 0) {
          
          if (ncol(s10) < POS_R10_CPF) {
            stop("Registro 10 SICOM possui menos colunas do que o esperado.")
          }
          
          s10$Empenho_SICOM <- extract_sicom_sub(s10[[POS_R10_EMPENHO_ID]])
          s10$Liquidacao_SICOM <- extract_sicom_sub(s10[[POS_R10_LIQ_ID]])
          
          s10$chave_join <- paste0(
            s10$Empenho_SICOM,
            "-",
            s10$Liquidacao_SICOM
          )
          
          mapa_chaves_r10 <- s10 %>%
            transmute(
              codReduzido_comum = normalizar_codigo(.data[[colnames(s10)[POS_R10_COD_RED]]]),
              chave_join = chave_join
            ) %>%
            filter(codReduzido_comum != "") %>%
            distinct(codReduzido_comum, .keep_all = TRUE)
          
          adicionar_chave_sicom <- function(df_reg, pos_cod_red) {
            
            if (is.null(df_reg) || nrow(df_reg) == 0) {
              return(df_reg)
            }
            
            if (ncol(df_reg) < pos_cod_red) {
              stop("Registro auxiliar SICOM possui menos colunas do que o esperado.")
            }
            
            df_reg$codReduzido_comum <- normalizar_codigo(df_reg[[pos_cod_red]])
            
            df_reg <- df_reg %>%
              left_join(mapa_chaves_r10, by = "codReduzido_comum") %>%
              select(-codReduzido_comum)
            
            return(df_reg)
          }
          
          s11 <- adicionar_chave_sicom(s11, POS_R11_COD_RED)
          s12 <- adicionar_chave_sicom(s12, POS_R12_COD_RED)
          s20 <- adicionar_chave_sicom(s20, POS_R20_COD_RED)
        }
        
        
        # ======================================================================
        # LEITURA SOF
        # ======================================================================
        
        sof10 <-
          if (!is.null(input$file_sof10)) {
            read_data_safe(
              input$file_sof10$datapath
            )
          } else {
            NULL
          }
        
        
        sof11 <-
          if (!is.null(input$file_sof11)) {
            read_data_safe(
              input$file_sof11$datapath
            )
          } else {
            NULL
          }
        
        
        sof12 <-
          if (!is.null(input$file_sof12)) {
            read_data_safe(
              input$file_sof12$datapath
            )
          } else {
            NULL
          }
        
        
        sof20 <-
          if (!is.null(input$file_sof20)) {
            read_data_safe(
              input$file_sof20$datapath
            )
          } else {
            NULL
          }
        
        
        # ======================================================================
        # CHAVE SOF
        # ======================================================================
        
        construir_chave_sof <- function(
    df,
    registro
        ) {
          
          if (
            is.null(df) ||
            nrow(df) == 0
          ) {
            
            return(df)
          }
          
          
          col_emp <- require_col(
            df,
            c(
              "numero_empenho",
              "empenho",
              "num_empenho"
            ),
            "numero_empenho",
            registro
          )
          
          
          col_liq <- require_col(
            df,
            c(
              "numero_liquidacao",
              "liquidacao",
              "num_liquidacao"
            ),
            "numero_liquidacao",
            registro
          )
          
          
          df$Empenho_SOF <-
            normalizar_codigo(
              df[[col_emp]]
            )
          
          
          df$Liquidacao_SOF <-
            normalizar_codigo(
              df[[col_liq]]
            )
          
          
          df$chave_join <-
            paste0(
              df$Empenho_SOF,
              "-",
              df$Liquidacao_SOF
            )
          
          
          return(df)
        }
        
        
        if (!is.null(sof10)) {
          sof10 <- construir_chave_sof(
            sof10,
            "10"
          )
        }
        
        if (!is.null(sof11)) {
          sof11 <- construir_chave_sof(
            sof11,
            "11"
          )
        }
        
        if (!is.null(sof12)) {
          sof12 <- construir_chave_sof(
            sof12,
            "12"
          )
        }
        
        if (!is.null(sof20)) {
          sof20 <- construir_chave_sof(
            sof20,
            "20"
          )
        }
        
        
        # ======================================================================
        # FUNÇÃO DE MENSAGEM DE EXISTÊNCIA
        # ======================================================================
        
        mensagem_existencia <- function(
    existe_sof,
    existe_sicom
        ) {
          
          if (
            !isTRUE(existe_sof)
          ) {
            
            return(
              "Registro existente somente no SICOM"
            )
          }
          
          
          if (
            !isTRUE(existe_sicom)
          ) {
            
            return(
              "Registro existente somente no SOF"
            )
          }
          
          
          return("")
        }
        
        
        # ======================================================================
        # REGISTRO 10
        #
        # SICOM:
        # coluna 3  -> codigo_orgao
        # coluna 4  -> uo_sicom
        # coluna 5  -> tipo_liquidacao
        # coluna 10 -> valor
        # coluna 11 -> cpf_liquidante
        # ======================================================================
        
        if (
          !is.null(sof10) &&
          nrow(sof10) > 0
        ) {
          
          
          if (
            nrow(s10) == 0
          ) {
            
            stop(
              "Não foram encontrados registros 10 no arquivo SICOM."
            )
          }
          
          
          # --------------------------------------------------------------------
          # Campos SOF obrigatórios
          # --------------------------------------------------------------------
          
          sof10_orgao <- require_col(
            sof10,
            "codigo_orgao",
            "codigo_orgao",
            "10"
          )
          
          sof10_uo_sof <- require_col(
            sof10, "uo_sof", "uo_sof", "10"
          )
          
          sof10_uo <- require_col(
            sof10,
            "uo_sicom",
            "uo_sicom",
            "10"
          )
          
          sof10_tipo <- require_col(
            sof10,
            "tipo_liquidacao",
            "tipo_liquidacao",
            "10"
          )
          
          sof10_ano <- require_col(
            sof10, "ano_empenho", "ano_empenho", "10"
          )
          
          sof10_data_emp <- require_col(
            sof10, "data_empenho", "data_empenho", "10"
          )
          
          sof10_data_liq <- require_col(
            sof10, "data_liquidacao", "data_liquidacao", "10"
          )
          
          sof10_valor <- require_col(
            sof10,
            "valor",
            "valor",
            "10"
          )
          
          sof10_cpf <- require_col(
            sof10,
            "cpf_liquidante",
            "cpf_liquidante",
            "10"
          )
          
          
          # --------------------------------------------------------------------
          # Prepara SICOM
          # --------------------------------------------------------------------
          
          s10_comp <-
            s10 %>%
            
            transmute(
              
              chave_join,
              
              Empenho_SICOM,
              
              Liquidacao_SICOM,
              
              Orgao_SICOM =
                normalizar_codigo(
                  .data[[colnames(s10)[POS_R10_ORGAO]]]
                ),
              
              UO_SICOM =
                normalizar_codigo(
                  .data[[colnames(s10)[POS_R10_UO]]]
                ),
              
              TipoLiquidacao_SICOM =
                normalizar_codigo(
                  .data[[colnames(s10)[POS_R10_TIPO_LIQ]]]
                ),
              
              UO_SOF_SICOM =
                extrair_uo_sof_id_sicom(
                  .data[[colnames(s10)[POS_R10_EMPENHO_ID]]]
                ),
              
              AnoEmpenho_SICOM =
                extrair_ano_id_sicom(
                  .data[[colnames(s10)[POS_R10_EMPENHO_ID]]]
                ),
              
              DataEmpenho_SICOM =
                normalizar_data_lqd(
                  .data[[colnames(s10)[POS_R10_DATA_EMP]]]
                ),
              
              DataLiquidacao_SICOM =
                normalizar_data_lqd(
                  .data[[colnames(s10)[POS_R10_DATA_LIQ]]]
                ),
              
              Valor_SICOM =
                to_numeric_br_safe(
                  .data[[colnames(s10)[POS_R10_VALOR]]]
                ),
              
              CPF_SICOM =
                normalizar_cpf(
                  .data[[colnames(s10)[POS_R10_CPF]]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              Empenho_SICOM =
                colapsar_unicos(
                  Empenho_SICOM
                ),
              
              Liquidacao_SICOM =
                colapsar_unicos(
                  Liquidacao_SICOM
                ),
              
              Orgao_SICOM =
                colapsar_unicos(
                  Orgao_SICOM
                ),
              
              UO_SICOM =
                colapsar_unicos(
                  UO_SICOM
                ),
              
              TipoLiquidacao_SICOM =
                colapsar_unicos(
                  TipoLiquidacao_SICOM
                ),
              
              UO_SOF_SICOM = colapsar_unicos(UO_SOF_SICOM),
              AnoEmpenho_SICOM = colapsar_unicos(AnoEmpenho_SICOM),
              DataEmpenho_SICOM = colapsar_unicos(DataEmpenho_SICOM),
              DataLiquidacao_SICOM = colapsar_unicos(DataLiquidacao_SICOM),
              
              Valor_SICOM =
                sum(
                  Valor_SICOM,
                  na.rm = TRUE
                ),
              
              CPF_SICOM =
                colapsar_unicos(
                  CPF_SICOM
                ),
              
              Existe_SICOM =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          # --------------------------------------------------------------------
          # Prepara SOF
          # --------------------------------------------------------------------
          
          sof10_comp <-
            sof10 %>%
            
            transmute(
              
              chave_join,
              
              Empenho_SOF,
              
              Liquidacao_SOF,
              
              Orgao_SOF =
                normalizar_codigo(
                  .data[[sof10_orgao]]
                ),
              
              UO_SOF =
                normalizar_codigo(
                  .data[[sof10_uo]]
                ),
              
              TipoLiquidacao_SOF =
                normalizar_tipo_liquidacao_sof(
                  .data[[sof10_tipo]]
                ),
              
              UO_SOF_Origem = normalizar_codigo(.data[[sof10_uo_sof]]),
              AnoEmpenho_SOF = normalizar_codigo(.data[[sof10_ano]]),
              DataEmpenho_SOF = normalizar_data_lqd(.data[[sof10_data_emp]]),
              DataLiquidacao_SOF = normalizar_data_lqd(.data[[sof10_data_liq]]),
              
              Valor_SOF =
                to_numeric_br_safe(
                  .data[[sof10_valor]]
                ),
              
              CPF_SOF =
                normalizar_cpf(
                  .data[[sof10_cpf]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              Empenho_SOF =
                colapsar_unicos(
                  Empenho_SOF
                ),
              
              Liquidacao_SOF =
                colapsar_unicos(
                  Liquidacao_SOF
                ),
              
              Orgao_SOF =
                colapsar_unicos(
                  Orgao_SOF
                ),
              
              UO_SOF =
                colapsar_unicos(
                  UO_SOF
                ),
              
              TipoLiquidacao_SOF =
                colapsar_unicos(
                  TipoLiquidacao_SOF
                ),
              
              UO_SOF_Origem = colapsar_unicos(UO_SOF_Origem),
              AnoEmpenho_SOF = colapsar_unicos(AnoEmpenho_SOF),
              DataEmpenho_SOF = colapsar_unicos(DataEmpenho_SOF),
              DataLiquidacao_SOF = colapsar_unicos(DataLiquidacao_SOF),
              
              Valor_SOF =
                sum(
                  Valor_SOF,
                  na.rm = TRUE
                ),
              
              CPF_SOF =
                colapsar_unicos(
                  CPF_SOF
                ),
              
              Existe_SOF =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          # --------------------------------------------------------------------
          # Cruzamento
          # --------------------------------------------------------------------
          
          audit10 <-
            full_join(
              sof10_comp,
              s10_comp,
              by = "chave_join"
            ) %>%
            
            mutate(
              
              Existe_SOF =
                coalesce(
                  Existe_SOF,
                  FALSE
                ),
              
              Existe_SICOM =
                coalesce(
                  Existe_SICOM,
                  FALSE
                ),
              
              Diferenca =
                coalesce(
                  Valor_SICOM,
                  0
                ) -
                coalesce(
                  Valor_SOF,
                  0
                )
            )
          
          
          audit10$Motivo <-
            pmap_chr(
              
              audit10,
              
              function(...) {
                
                r <- list(...)
                
                
                msg_existencia <-
                  mensagem_existencia(
                    r$Existe_SOF,
                    r$Existe_SICOM
                  )
                
                
                if (
                  msg_existencia != ""
                ) {
                  
                  return(
                    msg_existencia
                  )
                }
                
                
                motivos <- character(0)
                
                
                if (
                  coalesce(
                    r$Orgao_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$Orgao_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Código órgão: SOF ",
                      r$Orgao_SOF,
                      " x SICOM ",
                      r$Orgao_SICOM
                    )
                  )
                }
                
                
                if (
                  coalesce(
                    r$UO_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$UO_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "UO: SOF ",
                      r$UO_SOF,
                      " x SICOM ",
                      r$UO_SICOM
                    )
                  )
                }
                
                
                if (
                  coalesce(
                    r$TipoLiquidacao_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$TipoLiquidacao_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Tipo liquidação: SOF ",
                      r$TipoLiquidacao_SOF,
                      " x SICOM ",
                      r$TipoLiquidacao_SICOM
                    )
                  )
                }
                
                
                if (coalesce(r$UO_SOF_Origem,"") != coalesce(r$UO_SOF_SICOM,"")) {
                  motivos <- c(motivos, paste0("UO SOF: SOF ",r$UO_SOF_Origem," x SICOM ",r$UO_SOF_SICOM))
                }
                
                if (coalesce(r$AnoEmpenho_SOF,"") != coalesce(r$AnoEmpenho_SICOM,"")) {
                  motivos <- c(motivos, paste0("Ano empenho: SOF ",r$AnoEmpenho_SOF," x SICOM ",r$AnoEmpenho_SICOM))
                }
                if (
                  round(
                    coalesce(
                      r$Valor_SOF,
                      0
                    ),
                    2
                  ) !=
                  round(
                    coalesce(
                      r$Valor_SICOM,
                      0
                    ),
                    2
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Valor: SOF ",
                      fmt_money(
                        coalesce(
                          r$Valor_SOF,
                          0
                        )
                      ),
                      " x SICOM ",
                      fmt_money(
                        coalesce(
                          r$Valor_SICOM,
                          0
                        )
                      )
                    )
                  )
                }
                
                
                if (
                  coalesce(
                    r$CPF_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$CPF_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "CPF liquidante: SOF ",
                      r$CPF_SOF,
                      " x SICOM ",
                      r$CPF_SICOM
                    )
                  )
                }
                
                
                if (
                  length(motivos) == 0
                ) {
                  
                  return(
                    "Conforme"
                  )
                }
                
                
                paste(
                  motivos,
                  collapse = " | "
                )
              }
            )
          
          
          r10_div <-
            audit10 %>%
            
            filter(
              Motivo != "Conforme"
            ) %>%
            
            tidyr::separate(
              chave_join,
              into = c(
                "Empenho",
                "Liquidação"
              ),
              sep = "-",
              fill = "right",
              remove = TRUE
            ) %>%
            
            select(
              Empenho,
              Liquidação,
              Orgao_SOF,
              Orgao_SICOM,
              UO_SOF,
              UO_SICOM,
              TipoLiquidacao_SOF,
              TipoLiquidacao_SICOM,
              UO_SOF_Origem,
              UO_SOF_SICOM,
              AnoEmpenho_SOF,
              AnoEmpenho_SICOM,
              Valor_SOF,
              Valor_SICOM,
              CPF_SOF,
              CPF_SICOM,
              Diferenca,
              Motivo
            )
          
          
          total_r10_sof <-
            sum(
              sof10_comp$Valor_SOF,
              na.rm = TRUE
            )
          
          
          total_r10_sicom <-
            sum(
              s10_comp$Valor_SICOM,
              na.rm = TRUE
            )
          
          
        } else {
          
          r10_div <- tibble()
          
          total_r10_sof <- 0
          
          total_r10_sicom <-
            if (
              nrow(s10) > 0 &&
              ncol(s10) >= 10
            ) {
              sum(
                to_numeric_br_safe(
                  s10[[colnames(s10)[POS_R10_VALOR]]]
                ),
                na.rm = TRUE
              )
            } else {
              0
            }
        }
        
        
        # ======================================================================
        # REGISTRO 11
        #
        # Fonte de recurso + valor
        #
        # IMPORTANTE:
        # fonte NÃO integra mais a chave.
        # Ela é CAMPO AUDITADO.
        # ======================================================================
        
        if (
          !is.null(sof11) &&
          nrow(sof11) > 0
        ) {
          
          
          if (
            nrow(s11) == 0
          ) {
            
            stop(
              "Não foram encontrados registros 11 no arquivo SICOM."
            )
          }
          
          
          if (ncol(s11) < POS_R11_VALOR) {
            stop("Registro 11 SICOM possui menos de 5 colunas.")
          }
          
          s11_fonte <- colnames(s11)[POS_R11_FONTE]
          s11_valor <- colnames(s11)[POS_R11_VALOR]
          
          
          sof11_fonte <- require_col(
            sof11,
            "codigo_fonte_recurso",
            "codigo_fonte_recurso",
            "11"
          )
          
          
          sof11_valor <- require_col(
            sof11,
            "valor",
            "valor",
            "11"
          )
          
          
          s11_comp <-
            s11 %>%
            
            filter(
              !is.na(chave_join),
              chave_join != ""
            ) %>%
            
            transmute(
              
              chave_join,
              
              Fonte_SICOM =
                normalizar_codigo(
                  .data[[s11_fonte]]
                ),
              
              Valor_SICOM =
                to_numeric_br_safe(
                  .data[[s11_valor]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              Fonte_SICOM =
                colapsar_unicos(
                  Fonte_SICOM
                ),
              
              Valor_SICOM =
                sum(
                  Valor_SICOM,
                  na.rm = TRUE
                ),
              
              Existe_SICOM =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          sof11_comp <-
            sof11 %>%
            
            transmute(
              
              chave_join,
              
              Fonte_SOF =
                normalizar_codigo(
                  .data[[sof11_fonte]]
                ),
              
              Valor_SOF =
                to_numeric_br_safe(
                  .data[[sof11_valor]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              Fonte_SOF =
                colapsar_unicos(
                  Fonte_SOF
                ),
              
              Valor_SOF =
                sum(
                  Valor_SOF,
                  na.rm = TRUE
                ),
              
              Existe_SOF =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          audit11 <-
            full_join(
              sof11_comp,
              s11_comp,
              by = "chave_join"
            ) %>%
            
            mutate(
              
              Existe_SOF =
                coalesce(
                  Existe_SOF,
                  FALSE
                ),
              
              Existe_SICOM =
                coalesce(
                  Existe_SICOM,
                  FALSE
                ),
              
              Diferenca =
                coalesce(
                  Valor_SICOM,
                  0
                ) -
                coalesce(
                  Valor_SOF,
                  0
                )
            )
          
          
          audit11$Motivo <-
            pmap_chr(
              
              audit11,
              
              function(...) {
                
                r <- list(...)
                
                
                msg_existencia <-
                  mensagem_existencia(
                    r$Existe_SOF,
                    r$Existe_SICOM
                  )
                
                
                if (
                  msg_existencia != ""
                ) {
                  
                  return(
                    msg_existencia
                  )
                }
                
                
                motivos <- character(0)
                
                
                if (
                  coalesce(
                    r$Fonte_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$Fonte_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Fonte de recurso: SOF ",
                      r$Fonte_SOF,
                      " x SICOM ",
                      r$Fonte_SICOM
                    )
                  )
                }
                
                
                if (
                  round(
                    coalesce(
                      r$Valor_SOF,
                      0
                    ),
                    2
                  ) !=
                  round(
                    coalesce(
                      r$Valor_SICOM,
                      0
                    ),
                    2
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Valor: SOF ",
                      fmt_money(
                        coalesce(
                          r$Valor_SOF,
                          0
                        )
                      ),
                      " x SICOM ",
                      fmt_money(
                        coalesce(
                          r$Valor_SICOM,
                          0
                        )
                      )
                    )
                  )
                }
                
                
                if (
                  length(motivos) == 0
                ) {
                  
                  return("Conforme")
                }
                
                
                paste(
                  motivos,
                  collapse = " | "
                )
              }
            )
          
          
          r11_div <-
            audit11 %>%
            
            filter(
              Motivo != "Conforme"
            ) %>%
            
            separate(
              chave_join,
              into = c(
                "Empenho",
                "Liquidação"
              ),
              sep = "-",
              fill = "right",
              remove = TRUE
            )
          
          
          total_r11_sof <-
            sum(
              sof11_comp$Valor_SOF,
              na.rm = TRUE
            )
          
          
          total_r11_sicom <-
            sum(
              s11_comp$Valor_SICOM,
              na.rm = TRUE
            )
          
          
        } else {
          
          r11_div <- tibble()
          
          total_r11_sof <- 0
          
          total_r11_sicom <- if (nrow(s11) > 0 && ncol(s11) >= POS_R11_VALOR) {
            sum(to_numeric_br_safe(s11[[colnames(s11)[POS_R11_VALOR]]]), na.rm = TRUE)
          } else 0
        }
        
        
        # ======================================================================
        # REGISTRO 12
        #
        # SICOM coluna 4 -> exercicio_competencia SOF
        #
        # Mantém também o valor contábil já existente na conferência.
        # ======================================================================
        
        if (
          !is.null(sof12) &&
          nrow(sof12) > 0
        ) {
          
          
          if (
            nrow(s12) == 0
          ) {
            
            stop(
              "Não foram encontrados registros 12 no arquivo SICOM."
            )
          }
          
          
          if (
            ncol(s12) < 4
          ) {
            
            stop(
              "Registro 12 SICOM possui menos de 4 colunas."
            )
          }
          
          
          sof12_exercicio <- require_col(
            sof12,
            "exercicio_competencia",
            "exercicio_competencia",
            "12"
          )
          
          
          sof12_valor <- require_col(
            sof12,
            "valor",
            "valor",
            "12"
          )
          
          
          if (ncol(s12) < POS_R12_VALOR) {
            stop("Registro 12 SICOM possui menos de 5 colunas.")
          }
          
          s12_valor <- colnames(s12)[POS_R12_VALOR]
          
          
          s12_comp <-
            s12 %>%
            
            filter(
              !is.na(chave_join),
              chave_join != ""
            ) %>%
            
            transmute(
              
              chave_join,
              
              ExercicioCompetencia_SICOM =
                normalizar_codigo(
                  .data[[colnames(s12)[POS_R12_EXERCICIO]]]
                ),
              
              Valor_SICOM =
                to_numeric_br_safe(
                  .data[[s12_valor]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              ExercicioCompetencia_SICOM =
                colapsar_unicos(
                  ExercicioCompetencia_SICOM
                ),
              
              Valor_SICOM =
                sum(
                  Valor_SICOM,
                  na.rm = TRUE
                ),
              
              Existe_SICOM =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          sof12_comp <-
            sof12 %>%
            
            transmute(
              
              chave_join,
              
              ExercicioCompetencia_SOF =
                normalizar_codigo(
                  .data[[sof12_exercicio]]
                ),
              
              Valor_SOF =
                to_numeric_br_safe(
                  .data[[sof12_valor]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              ExercicioCompetencia_SOF =
                colapsar_unicos(
                  ExercicioCompetencia_SOF
                ),
              
              Valor_SOF =
                sum(
                  Valor_SOF,
                  na.rm = TRUE
                ),
              
              Existe_SOF =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          audit12 <-
            full_join(
              sof12_comp,
              s12_comp,
              by = "chave_join"
            ) %>%
            
            mutate(
              
              Existe_SOF =
                coalesce(
                  Existe_SOF,
                  FALSE
                ),
              
              Existe_SICOM =
                coalesce(
                  Existe_SICOM,
                  FALSE
                ),
              
              Diferenca =
                coalesce(
                  Valor_SICOM,
                  0
                ) -
                coalesce(
                  Valor_SOF,
                  0
                )
            )
          
          
          audit12$Motivo <-
            pmap_chr(
              
              audit12,
              
              function(...) {
                
                r <- list(...)
                
                
                msg_existencia <-
                  mensagem_existencia(
                    r$Existe_SOF,
                    r$Existe_SICOM
                  )
                
                
                if (
                  msg_existencia != ""
                ) {
                  
                  return(
                    msg_existencia
                  )
                }
                
                
                motivos <- character(0)
                
                
                if (
                  coalesce(
                    r$ExercicioCompetencia_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$ExercicioCompetencia_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Exercício competência: SOF ",
                      r$ExercicioCompetencia_SOF,
                      " x SICOM ",
                      r$ExercicioCompetencia_SICOM
                    )
                  )
                }
                
                
                if (
                  round(
                    coalesce(
                      r$Valor_SOF,
                      0
                    ),
                    2
                  ) !=
                  round(
                    coalesce(
                      r$Valor_SICOM,
                      0
                    ),
                    2
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Valor: SOF ",
                      fmt_money(
                        coalesce(
                          r$Valor_SOF,
                          0
                        )
                      ),
                      " x SICOM ",
                      fmt_money(
                        coalesce(
                          r$Valor_SICOM,
                          0
                        )
                      )
                    )
                  )
                }
                
                
                if (
                  length(motivos) == 0
                ) {
                  
                  return("Conforme")
                }
                
                
                paste(
                  motivos,
                  collapse = " | "
                )
              }
            )
          
          
          r12_div <-
            audit12 %>%
            
            filter(
              Motivo != "Conforme"
            ) %>%
            
            separate(
              chave_join,
              into = c(
                "Empenho",
                "Liquidação"
              ),
              sep = "-",
              fill = "right",
              remove = TRUE
            )
          
          
          total_r12_sof <-
            sum(
              sof12_comp$Valor_SOF,
              na.rm = TRUE
            )
          
          
          total_r12_sicom <-
            sum(
              s12_comp$Valor_SICOM,
              na.rm = TRUE
            )
          
          
        } else {
          
          r12_div <- tibble()
          
          total_r12_sof <- 0
          
          total_r12_sicom <- if (nrow(s12) > 0 && ncol(s12) >= POS_R12_VALOR) {
            sum(to_numeric_br_safe(s12[[colnames(s12)[POS_R12_VALOR]]]), na.rm = TRUE)
          } else 0
        }
        
        
        # ======================================================================
        # REGISTRO 20
        #
        # SICOM coluna 3 -> codigo_fonte_recurso
        # SICOM coluna 4 -> codigo_co
        # SICOM coluna 7 -> valor
        #
        # Fonte e CO NÃO fazem parte da chave.
        # São campos auditados.
        # ======================================================================
        
        if (
          !is.null(sof20) &&
          nrow(sof20) > 0
        ) {
          
          
          if (
            nrow(s20) == 0
          ) {
            
            stop(
              "Não foram encontrados registros 20 no arquivo SICOM."
            )
          }
          
          
          if (
            ncol(s20) < 7
          ) {
            
            stop(
              "Registro 20 SICOM possui menos de 7 colunas."
            )
          }
          
          
          sof20_fonte <- require_col(
            sof20,
            "codigo_fonte_recurso",
            "codigo_fonte_recurso",
            "20"
          )
          
          
          sof20_co <- require_col(
            sof20,
            "codigo_co",
            "codigo_co",
            "20"
          )
          
          
          sof20_valor <- require_col(
            sof20,
            "valor",
            "valor",
            "20"
          )
          
          
          s20_comp <-
            s20 %>%
            
            filter(
              !is.na(chave_join),
              chave_join != ""
            ) %>%
            
            transmute(
              
              chave_join,
              
              Fonte_SICOM =
                normalizar_codigo(
                  .data[[colnames(s20)[POS_R20_FONTE]]]
                ),
              
              CO_SICOM =
                normalizar_codigo(
                  .data[[colnames(s20)[POS_R20_CO]]]
                ),
              
              Valor_SICOM =
                to_numeric_br_safe(
                  .data[[colnames(s20)[POS_R20_VALOR]]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              Fonte_SICOM =
                colapsar_unicos(
                  Fonte_SICOM
                ),
              
              CO_SICOM =
                colapsar_unicos(
                  CO_SICOM
                ),
              
              Valor_SICOM =
                sum(
                  Valor_SICOM,
                  na.rm = TRUE
                ),
              
              Existe_SICOM =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          sof20_comp <-
            sof20 %>%
            
            transmute(
              
              chave_join,
              
              Fonte_SOF =
                normalizar_codigo(
                  .data[[sof20_fonte]]
                ),
              
              CO_SOF =
                normalizar_codigo(
                  .data[[sof20_co]]
                ),
              
              Valor_SOF =
                to_numeric_br_safe(
                  .data[[sof20_valor]]
                )
            ) %>%
            
            group_by(
              chave_join
            ) %>%
            
            summarise(
              
              Fonte_SOF =
                colapsar_unicos(
                  Fonte_SOF
                ),
              
              CO_SOF =
                colapsar_unicos(
                  CO_SOF
                ),
              
              Valor_SOF =
                sum(
                  Valor_SOF,
                  na.rm = TRUE
                ),
              
              Existe_SOF =
                TRUE,
              
              .groups =
                "drop"
            )
          
          
          audit20 <-
            full_join(
              sof20_comp,
              s20_comp,
              by = "chave_join"
            ) %>%
            
            mutate(
              
              Existe_SOF =
                coalesce(
                  Existe_SOF,
                  FALSE
                ),
              
              Existe_SICOM =
                coalesce(
                  Existe_SICOM,
                  FALSE
                ),
              
              Diferenca =
                coalesce(
                  Valor_SICOM,
                  0
                ) -
                coalesce(
                  Valor_SOF,
                  0
                )
            )
          
          
          audit20$Motivo <-
            pmap_chr(
              
              audit20,
              
              function(...) {
                
                r <- list(...)
                
                
                msg_existencia <-
                  mensagem_existencia(
                    r$Existe_SOF,
                    r$Existe_SICOM
                  )
                
                
                if (
                  msg_existencia != ""
                ) {
                  
                  return(
                    msg_existencia
                  )
                }
                
                
                motivos <- character(0)
                
                
                if (
                  coalesce(
                    r$Fonte_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$Fonte_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Fonte de recurso: SOF ",
                      r$Fonte_SOF,
                      " x SICOM ",
                      r$Fonte_SICOM
                    )
                  )
                }
                
                
                if (
                  coalesce(
                    r$CO_SOF,
                    ""
                  ) !=
                  coalesce(
                    r$CO_SICOM,
                    ""
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Código CO: SOF ",
                      r$CO_SOF,
                      " x SICOM ",
                      r$CO_SICOM
                    )
                  )
                }
                
                
                if (
                  round(
                    coalesce(
                      r$Valor_SOF,
                      0
                    ),
                    2
                  ) !=
                  round(
                    coalesce(
                      r$Valor_SICOM,
                      0
                    ),
                    2
                  )
                ) {
                  
                  motivos <- c(
                    motivos,
                    paste0(
                      "Valor: SOF ",
                      fmt_money(
                        coalesce(
                          r$Valor_SOF,
                          0
                        )
                      ),
                      " x SICOM ",
                      fmt_money(
                        coalesce(
                          r$Valor_SICOM,
                          0
                        )
                      )
                    )
                  )
                }
                
                
                if (
                  length(motivos) == 0
                ) {
                  
                  return("Conforme")
                }
                
                
                paste(
                  motivos,
                  collapse = " | "
                )
              }
            )
          
          
          r20_div <-
            audit20 %>%
            
            filter(
              Motivo != "Conforme"
            ) %>%
            
            separate(
              chave_join,
              into = c(
                "Empenho",
                "Liquidação"
              ),
              sep = "-",
              fill = "right",
              remove = TRUE
            )
          
          
          total_r20_sof <-
            sum(
              sof20_comp$Valor_SOF,
              na.rm = TRUE
            )
          
          
          total_r20_sicom <-
            sum(
              s20_comp$Valor_SICOM,
              na.rm = TRUE
            )
          
          
        } else {
          
          r20_div <- tibble()
          
          total_r20_sof <- 0
          
          total_r20_sicom <- if (nrow(s20) > 0 && ncol(s20) >= POS_R20_VALOR) {
            sum(to_numeric_br_safe(s20[[colnames(s20)[POS_R20_VALOR]]]), na.rm = TRUE)
          } else 0
        }
        
        
        # ======================================================================
        # SALVA RESULTADOS
        # ======================================================================
        
        dados_processados$r10 <- r10_div
        
        dados_processados$r11 <- r11_div
        
        dados_processados$r12 <- r12_div
        
        dados_processados$r20 <- r20_div
        
        
        # ======================================================================
        # RESUMO
        # ======================================================================
        
        dados_processados$logs <-
          paste0(
            
            "============================================================\n",
            "                   RESUMO DA CONFERÊNCIA                    \n",
            "============================================================\n\n",
            
            "MOTOR UTILIZADO: AUDITORIA CAMPO A CAMPO + VALORES\n\n",
            
            
            "REGISTRO 10:\n",
            "  Total SOF Registro 10:   ",
            fmt_money(total_r10_sof),
            "\n",
            
            "  Total SICOM Registro 10: ",
            fmt_money(total_r10_sicom),
            "\n",
            
            "  Diferença:                ",
            fmt_money(
              total_r10_sicom -
                total_r10_sof
            ),
            "\n",
            
            "  Divergências encontradas: ",
            nrow(r10_div),
            "\n\n",
            
            
            "REGISTRO 11:\n",
            "  Total SOF Registro 11:   ",
            fmt_money(total_r11_sof),
            "\n",
            
            "  Total SICOM Registro 11: ",
            fmt_money(total_r11_sicom),
            "\n",
            
            "  Diferença:                ",
            fmt_money(
              total_r11_sicom -
                total_r11_sof
            ),
            "\n",
            
            "  Divergências encontradas: ",
            nrow(r11_div),
            "\n\n",
            
            
            "REGISTRO 12:\n",
            "  Total SOF Registro 12:   ",
            fmt_money(total_r12_sof),
            "\n",
            
            "  Total SICOM Registro 12: ",
            fmt_money(total_r12_sicom),
            "\n",
            
            "  Diferença:                ",
            fmt_money(
              total_r12_sicom -
                total_r12_sof
            ),
            "\n",
            
            "  Divergências encontradas: ",
            nrow(r12_div),
            "\n\n",
            
            
            "REGISTRO 20:\n",
            "  Total SOF Registro 20:   ",
            fmt_money(total_r20_sof),
            "\n",
            
            "  Total SICOM Registro 20: ",
            fmt_money(total_r20_sicom),
            "\n",
            
            "  Diferença:                ",
            fmt_money(
              total_r20_sicom -
                total_r20_sof
            ),
            "\n",
            
            "  Divergências encontradas: ",
            nrow(r20_div),
            "\n\n",
            
            "------------------------------------------------------------\n",
            
            "Verificação concluída com sucesso."
          )
        
        
        dados_processados$sucesso <- TRUE
        
        
        
      }, error = function(e) {
        
        
        dados_processados$sucesso <- FALSE
        
        dados_processados$erro <- e$message
        
        
        showNotification(
          paste0(
            "Erro na auditoria: ",
            e$message
          ),
          type = "error",
          duration = 10
        )
      })
    }
  )
  
  
  # ==============================================================================
  # RESUMO
  # ==============================================================================
  
  output$txt_resumo <- renderText({
    
    if (
      dados_processados$sucesso
    ) {
      
      dados_processados$logs
      
    } else if (
      !is.null(
        dados_processados$erro
      )
    ) {
      
      paste(
        "⚠️ SISTEMA BLOQUEADO POR ERRO:\n\n",
        dados_processados$erro
      )
      
    } else {
      
      ""
    }
  })
  
  
  # ==============================================================================
  # ALERTAS
  # ==============================================================================
  
  output$output_alertas <- renderUI({
    
    if (
      !is.null(
        dados_processados$erro
      )
    ) {
      
      div(
        class = "alert alert-danger",
        role = "alert",
        
        tags$b(
          "Erro Encontrado: "
        ),
        
        dados_processados$erro
      )
      
    } else if (
      dados_processados$sucesso
    ) {
      
      div(
        class = "alert alert-success",
        role = "alert",
        
        tags$b(
          "Sucesso! "
        ),
        
        "Conferência executada com sucesso!"
      )
    }
  })
  
  
  # ==============================================================================
  # TABELAS
  # ==============================================================================
  #
  # IMPORTANTE:
  # A função recebe agora uma FUNÇÃO REATIVA (df_reactive), e não o valor
  # de dados_processados$rXX diretamente. Isso evita que o R force o argumento
  # uma única vez e mantenha a primeira tabela em cache após novas execuções.
  # ==============================================================================
  
  render_audit_table <- function(
    df_reactive,
    nome_arquivo
  ) {
    
    renderDataTable({
      
      req(
        dados_processados$sucesso
      )
      
      # Reavalia o resultado a CADA execução da auditoria
      df_divergencia <- df_reactive()
      
      if (
        is.null(df_divergencia)
      ) {
        
        df_divergencia <- tibble()
      }
      
      tabela <- datatable(
        
        df_divergencia,
        
        extensions = "Buttons",
        
        rownames = FALSE,
        
        options = list(
          
          dom = "Bfrtip",
          
          pageLength = 10,
          
          buttons = c(
            "copy",
            "csv",
            "excel"
          ),
          
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
          )
        )
      )
      
      colunas_moeda <- intersect(
        c(
          "Valor_SICOM",
          "Valor_SOF",
          "Diferenca"
        ),
        colnames(df_divergencia)
      )
      
      if (
        length(colunas_moeda) > 0
      ) {
        
        tabela <- tabela %>%
          formatCurrency(
            columns = colunas_moeda,
            currency = "R$ ",
            mark = ".",
            dec.mark = ","
          )
      }
      
      tabela
    })
  }
  
  
  output$tbl_reg10 <-
    render_audit_table(
      function() dados_processados$r10,
      "Divergencias_Registro_10"
    )
  
  
  output$tbl_reg11 <-
    render_audit_table(
      function() dados_processados$r11,
      "Divergencias_Registro_11"
    )
  
  
  output$tbl_reg12 <-
    render_audit_table(
      function() dados_processados$r12,
      "Divergencias_Registro_12"
    )
  
  
  output$tbl_reg20 <-
    render_audit_table(
      function() dados_processados$r20,
      "Divergencias_Registro_20"
    )
  
}


# ==============================================================================
# 5. EXECUÇÃO
# ==============================================================================

shinyApp(
  ui = ui,
  server = server
)
