# =========================================
# Instalar pacotes automaticamente
# =========================================

packages_needed <- c(
  "shiny",
  "readxl",
  "dplyr",
  "DT",
  "data.table",
  "tibble",
  "stringr"
)

for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# =========================================
# Cabeçalhos Fixos
# =========================================

cab_10 <- c(
  "10 - Detalhamento das Notas Fiscais",
  "codNotaFiscal",
  "codOrgao",
  "nfNumero",
  "nfSerie",
  "tipoDocumento 1 - CPF;\n2 - CNPJ;\n3 - Documento de Estrangeiros.\n",
  "nroDocumento",
  "nomeEstrangeiro",
  "nroInscEstadual",
  "nroInscMunicipal",
  "nomeMunicipio",
  "cepMunicipio",
  "ufCredor",
  "notaFiscalEletronica  1 – Sim, padrão Estadual ou SINIEF 07/05;\n2 – Sim, chave de acesso municipal ou outra;\n3 – Não;\n4 – Sim, padrão Estadual ou SINIEF 07/05 – Avulsa.\n",
  "chaveAcesso ",
  "outraChaveAcesso",
  "nfAIDF",
  "dtEmissaoNF",
  "dtVencimentoNF",
  "nfValorTotal",
  "nfValorDesconto",
  "nfValorLiquido"
)

cab_20 <- c(
  "20 - Detalhamento da Liquidação da Nota Fiscal",
  "nfNumero",
  "nfSerie",
  "tipoDocumento",
  "nroDocumento",
  "nomeEstrangeiro",
  "chaveAcesso ",
  "dtEmissaoNF",
  "codUnidadeSub",
  "dtEmpenho",
  "nroEmpenho",
  "dtLiquidacao",
  "nroLiquidacao"
)


# =========================================
# Funções auxiliares
# =========================================

formata_real <- function(x) {
  
  x_limpo <- ifelse(is.na(x), 0, x)
  x_limpo <- x_limpo + 0
  
  paste0(
    "R$ ",
    formatC(
      x_limpo,
      format = "f",
      big.mark = ".",
      decimal.mark = ",",
      digits = 2
    )
  )
}


to_numeric_br <- function(x) {
  
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  x <- as.character(x)
  x <- trimws(x)
  
  x[x %in% c(
    "",
    "NA",
    "NULL",
    "NaN"
  )] <- NA
  
  x <- gsub(
    "[^0-9,.-]",
    "",
    x
  )
  
  tem_ponto_e_virgula <-
    grepl("\\.", x) &
    grepl(",", x)
  
  x[tem_ponto_e_virgula] <-
    gsub(
      "\\.",
      "",
      x[tem_ponto_e_virgula]
    )
  
  x[tem_ponto_e_virgula] <-
    gsub(
      ",",
      ".",
      x[tem_ponto_e_virgula]
    )
  
  so_virgula <-
    !tem_ponto_e_virgula &
    grepl(",", x)
  
  x[so_virgula] <-
    gsub(
      ",",
      ".",
      x[so_virgula]
    )
  
  suppressWarnings(
    as.numeric(x)
  )
}


normalizar_texto <- function(x) {
  
  if (is.numeric(x)) {
    x <- format(
      x,
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  x <- as.character(x)
  x <- trimws(x)
  
  x[x %in% c(
    "",
    "NA",
    "NULL",
    "NaN"
  )] <- NA_character_
  
  x
}


normalizar_nf <- function(x) {
  
  if (is.numeric(x)) {
    x <- format(
      x,
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  x <- as.character(x)
  x <- trimws(x)
  
  # Remove texto espúrio gerado na leitura de alguns XLS
  x <- gsub("\\*failed to decode utf16\\*", "", x, ignore.case = TRUE)
  
  x <- gsub(
    "\\.0$",
    "",
    x
  )
  
  x <- gsub(
    "\\s+",
    "",
    x
  )
  
  x <- gsub(
    "^0+",
    "",
    x
  )
  
  x[x == ""] <- NA_character_
  
  x
}


normalizar_documento <- function(x) {
  
  if (is.numeric(x)) {
    x <- format(
      x,
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  x <- as.character(x)
  
  # IMPORTANTE: remover o marcador ANTES de manter apenas números.
  # Caso contrário, o "16" de "utf16" vira parte do CPF/CNPJ.
  x <- gsub("\\*failed to decode utf16\\*", "", x, ignore.case = TRUE)
  
  x <- gsub(
    "[^0-9]",
    "",
    x
  )
  
  x <- gsub(
    "^0+",
    "",
    x
  )
  
  x[x == ""] <- NA_character_
  
  x
}


# =========================================
# Normalização da Inscrição Estadual
# =========================================

normalizar_ie <- function(x) {
  
  if (is.null(x)) {
    return(0)
  }
  
  x_char <- trimws(
    as.character(x)
  )
  
  # Remove texto espúrio antes da extração dos números
  x_char <- gsub("\\*failed to decode utf16\\*", "", x_char, ignore.case = TRUE)
  
  # Remove eventual .0 do Excel
  x_char <- gsub(
    "\\.0$",
    "",
    x_char
  )
  
  # Mantém somente números
  x_num_str <- gsub(
    "[^0-9]",
    "",
    x_char
  )
  
  # =======================================
  # CORREÇÃO DO PREFIXO 16
  # =======================================
  # Quando a IE vier com 14 dígitos e
  # começar por 16, remove os dois primeiros
  # dígitos.
  # =======================================
  
  x_num_str <- ifelse(
    nchar(x_num_str) == 14 &
      substr(
        x_num_str,
        1,
        2
      ) == "16",
    
    substr(
      x_num_str,
      3,
      nchar(x_num_str)
    ),
    
    x_num_str
  )
  
  x_num <- suppressWarnings(
    as.numeric(x_num_str)
  )
  
  dplyr::coalesce(
    x_num,
    0
  )
}


normalizar_flag_s <- function(x) {
  
  x <- as.character(x)
  x <- trimws(x)
  
  x[x %in% c(
    "",
    "NA",
    "NULL",
    "NaN"
  )] <- NA_character_
  
  ifelse(
    is.na(x),
    NA_character_,
    toupper(x)
  )
}


normalizar_data <- function(x) {
  
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  x_char <- trimws(
    as.character(x)
  )
  
  x_char <- gsub(
    "\\.0$",
    "",
    x_char
  )
  
  x_char[x_char %in% c(
    "",
    "NA",
    "NULL",
    "NaN"
  )] <- NA_character_
  
  res <- as.Date(
    rep(
      NA_character_,
      length(x_char)
    )
  )
  
  x_num <- suppressWarnings(
    as.numeric(x_char)
  )
  
  # Excel
  idx_excel <-
    !is.na(x_num) &
    x_num >= 30000 &
    x_num <= 60000
  
  if (any(idx_excel)) {
    
    res[idx_excel] <- as.Date(
      x_num[idx_excel],
      origin = "1899-12-30"
    )
  }
  
  # SICOM no formato DDMMAAAA
  idx_sicom_num <-
    !is.na(x_num) &
    x_num >= 100000 &
    x_num <= 99999999
  
  if (any(idx_sicom_num)) {
    
    padded <- stringr::str_pad(
      as.character(
        x_num[idx_sicom_num]
      ),
      width = 8,
      side = "left",
      pad = "0"
    )
    
    res[idx_sicom_num] <-
      suppressWarnings(
        as.Date(
          padded,
          format = "%d%m%Y"
        )
      )
  }
  
  # Datas texto
  idx_str <-
    is.na(res) &
    !is.na(x_char)
  
  if (any(idx_str)) {
    
    res[idx_str] <-
      suppressWarnings(
        as.Date(
          x_char[idx_str],
          format = "%d/%m/%Y"
        )
      )
    
    idx_ainda_na <-
      is.na(res) &
      idx_str
    
    if (any(idx_ainda_na)) {
      
      res[idx_ainda_na] <-
        suppressWarnings(
          as.Date(
            x_char[idx_ainda_na],
            format = "%Y-%m-%d"
          )
        )
    }
  }
  
  res
}


validar_colunas <- function(
    df,
    cols,
    nome_base
) {
  
  faltantes <-
    cols[
      !cols %in% names(df)
    ]
  
  if (length(faltantes) > 0) {
    
    stop(
      paste0(
        "No arquivo ",
        nome_base,
        ", não encontrei a(s) coluna(s): ",
        paste(
          faltantes,
          collapse = ", "
        )
      )
    )
  }
}


# =========================================
# Interface do Usuário
# =========================================

ui <- fluidPage(
  
  titlePanel(
    "Conferência SOF x SICOM - NTF"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "arquivo_ntf",
        "Selecione o relatório SICOM (.csv)",
        accept = ".csv"
      ),
      
      fileInput(
        "arquivo_sof",
        "Selecione o relatório SOF (.xls/.xlsx/.csv)",
        accept = c(
          ".xls",
          ".xlsx",
          ".csv"
        )
      ),
      
      br(),
      
      actionButton(
        "executar",
        "Executar",
        class = "btn-primary"
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Resumo Geral",
          br(),
          
          tags$h4(
            "Resumo Financeiro da Comparação"
          ),
          
          verbatimTextOutput(
            "mensagem"
          )
        ),
        
        tabPanel(
          "Divergências Reg. 10",
          br(),
          
          tags$h4(
            "Divergências Encontradas no Registro 10"
          ),
          
          DTOutput(
            "tabela_div_reg10"
          )
        ),
        
        tabPanel(
          "Divergências Reg. 20",
          br(),
          
          tags$h4(
            "Divergências Encontradas no Registro 20"
          ),
          
          DTOutput(
            "tabela_div_reg20"
          )
        ),
        
        tabPanel(
          "Exclusivas Reg 20",
          br(),
          
          tags$h4(
            "Notas do SOF contidas exclusivamente no Registro 20"
          ),
          
          DTOutput(
            "tabela_reg20_sof"
          )
        )
      )
    )
  )
)


# =========================================
# Servidor
# =========================================

server <- function(
    input,
    output,
    session
) {
  
  resultado <- reactiveVal(NULL)
  
  
  observeEvent(
    input$executar,
    {
      
      tryCatch({
        
        req(
          input$arquivo_ntf,
          input$arquivo_sof
        )
        
        
        # =========================================
        # Inicialização
        # =========================================
        
        tabela_reg20_sof <- tibble::tibble()
        tabela_div_10 <- tibble::tibble()
        tabela_div_20 <- tibble::tibble()
        
        
        # =========================================
        # 1) LER SICOM
        # =========================================
        
        ntf <- data.table::fread(
          input$arquivo_ntf$datapath,
          sep = ";",
          header = FALSE,
          fill = TRUE,
          encoding = "Latin-1",
          data.table = FALSE,
          showProgress = FALSE
        )
        
        
        if (
          ncol(ntf) == 0 ||
          nrow(ntf) == 0
        ) {
          
          stop(
            paste0(
              "O arquivo SICOM selecionado está vazio ",
              "ou não pôde ser lido corretamente."
            )
          )
        }
        
        
        # Primeira coluna = tipo de registro
        colnames(ntf)[1] <-
          "Tipo_Registro"
        
        
        reg_10_raw <- ntf %>%
          dplyr::filter(
            Tipo_Registro == 10
          )
        
        
        reg_20_raw <- ntf %>%
          dplyr::filter(
            Tipo_Registro == 20
          )
        
        
        # =========================================
        # REGISTRO 10
        # =========================================
        
        if (nrow(reg_10_raw) > 0) {
          
          reg_10 <-
            reg_10_raw[
              ,
              seq_len(
                min(
                  ncol(reg_10_raw),
                  length(cab_10)
                )
              ),
              drop = FALSE
            ]
          
          
          if (
            ncol(reg_10) <
            length(cab_10)
          ) {
            
            for (
              i in seq_len(
                length(cab_10) -
                ncol(reg_10)
              )
            ) {
              
              reg_10[[
                paste0(
                  "vazia_",
                  i
                )
              ]] <- NA
            }
          }
          
          
          reg_10 <-
            reg_10[
              ,
              seq_along(cab_10),
              drop = FALSE
            ]
          
          
          colnames(reg_10) <-
            cab_10
          
          
          validar_colunas(
            reg_10,
            c(
              "nfNumero",
              "nroDocumento",
              "nfValorLiquido",
              "nroInscEstadual",
              "codNotaFiscal"
            ),
            "Registro 10"
          )
          
          
          reg_10 <- reg_10 %>%
            dplyr::mutate(
              
              nfNumero_raw =
                nfNumero,
              
              nfNumero =
                normalizar_nf(
                  nfNumero
                ),
              
              nroDocumento_norm =
                normalizar_documento(
                  nroDocumento
                ),
              
              nfValorLiquido =
                round(
                  to_numeric_br(
                    nfValorLiquido
                  ),
                  2
                ),
              
              nroInscEstadual =
                normalizar_ie(
                  nroInscEstadual
                ),
              
              ano_sicom =
                suppressWarnings(
                  as.numeric(
                    substr(
                      as.character(codNotaFiscal),
                      1,
                      4
                    )
                  )
                ),
              
              chave_cruzamento =
                paste0(
                  coalesce(
                    nfNumero,
                    "VAZIO"
                  ),
                  "_",
                  coalesce(
                    nroDocumento_norm,
                    "VAZIO"
                  )
                )
            )
          
        } else {
          
          stop(
            "Nenhum dado de Registro 10 encontrado no arquivo do SICOM."
          )
        }
        
        
        # =========================================
        # REGISTRO 20
        # =========================================
        
        tem_reg20 <-
          nrow(reg_20_raw) > 0
        
        
        if (tem_reg20) {
          
          reg_20 <-
            reg_20_raw[
              ,
              seq_len(
                min(
                  ncol(reg_20_raw),
                  length(cab_20)
                )
              ),
              drop = FALSE
            ]
          
          
          if (
            ncol(reg_20) <
            length(cab_20)
          ) {
            
            for (
              i in seq_len(
                length(cab_20) -
                ncol(reg_20)
              )
            ) {
              
              reg_20[[
                paste0(
                  "vazia_",
                  i
                )
              ]] <- NA
            }
          }
          
          
          reg_20 <-
            reg_20[
              ,
              seq_along(cab_20),
              drop = FALSE
            ]
          
          
          colnames(reg_20) <-
            cab_20
          
          
          validar_colunas(
            reg_20,
            c(
              "nfNumero",
              "nroDocumento",
              "dtEmissaoNF"
            ),
            "Registro 20"
          )
          
          
          reg_20 <- reg_20 %>%
            dplyr::mutate(
              
              # Guarda valor original para exibição
              nfNumero_raw =
                nfNumero,
              
              nfNumero =
                normalizar_nf(
                  nfNumero
                ),
              
              nroDocumento_norm =
                normalizar_documento(
                  nroDocumento
                ),
              
              dtEmissaoNF =
                normalizar_data(
                  dtEmissaoNF
                ),
              
              chave_cruzamento =
                paste0(
                  coalesce(
                    nfNumero,
                    "VAZIO"
                  ),
                  "_",
                  coalesce(
                    nroDocumento_norm,
                    "VAZIO"
                  )
                )
            )
        }
        
        
        # =========================================
        # 2) LER SOF
        # =========================================
        
        ext <-
          tools::file_ext(
            input$arquivo_sof$name
          )
        
        
        if (
          tolower(ext) == "csv"
        ) {
          
          sof <-
            data.table::fread(
              input$arquivo_sof$datapath,
              encoding = "UTF-8",
              data.table = FALSE
            )
          
        } else {
          
          sof <-
            readxl::read_excel(
              input$arquivo_sof$datapath
            )
        }
        
        
        sof <- sof %>%
          dplyr::mutate(
            linha_sof_original =
              dplyr::row_number()
          )
        
        
        validar_colunas(
          sof,
          c(
            "valor_nf",
            "nf_numero",
            "nro_documento",
            "data_emissao",
            "inscrição_estadual",
            "ano_npd"
          ),
          "SOF"
        )
        
        
        if (
          !"ind_npd_anterior" %in%
          names(sof)
        ) {
          
          sof$ind_npd_anterior <-
            NA_character_
        }
        
        
        sof <- sof %>%
          dplyr::mutate(
            
            valor_nf =
              round(
                to_numeric_br(
                  valor_nf
                ),
                2
              ),
            
            nf_numero_raw =
              nf_numero,
            
            nf_numero =
              normalizar_nf(
                nf_numero
              ),
            
            nro_documento_norm =
              normalizar_documento(
                nro_documento
              ),
            
            data_emissao =
              normalizar_data(
                data_emissao
              ),
            
            inscrição_estadual =
              normalizar_ie(
                inscrição_estadual
              ),
            
            ano_npd =
              suppressWarnings(
                as.numeric(ano_npd)
              ),
            
            ind_npd_anterior =
              normalizar_flag_s(
                ind_npd_anterior
              ),
            
            chave_cruzamento =
              paste0(
                coalesce(
                  nf_numero,
                  "VAZIO"
                ),
                "_",
                coalesce(
                  nro_documento_norm,
                  "VAZIO"
                )
              )
          )
        
        
        # =========================================
        # IDENTIFICAR NFs DE NPD ANTERIOR VÁLIDAS
        # =========================================
        #
        # Uma NF do SOF com ind_npd_anterior = "S" que não está
        # no Registro 10, mas está no Registro 20, NÃO deve ser
        # tratada como divergência do Registro 10.
        #
        # A chave continua sendo: NF + documento do credor.
        # =========================================
        
        if (tem_reg20) {
          
          chaves_reg10 <- unique(
            reg_10$chave_cruzamento[
              !is.na(reg_10$chave_cruzamento) &
                reg_10$chave_cruzamento != "VAZIO_VAZIO"
            ]
          )
          
          chaves_reg20 <- unique(
            reg_20$chave_cruzamento[
              !is.na(reg_20$chave_cruzamento) &
                reg_20$chave_cruzamento != "VAZIO_VAZIO"
            ]
          )
          
          # Somente chaves que estão no Reg. 20 e NÃO estão no Reg. 10
          chaves_exclusivas_reg20 <- setdiff(
            chaves_reg20,
            chaves_reg10
          )
          
        } else {
          
          chaves_exclusivas_reg20 <- character(0)
        }
        
        
        # =========================================
        # AUDITORIA DETALHADA - REGISTRO 10
        # =========================================
        #
        # ALTERAÇÃO:
        # FULL JOIN permite identificar:
        #
        # 1. Registro somente no SOF
        # 2. Registro somente no SICOM
        # 3. Registro nos dois com divergência
        #
        # =========================================
        
        
        sof_auditoria_10 <- sof %>%
          
          # A auditoria deve usar a mesma regra de deduplicação do fechamento.
          # Assim, uma linha repetida no SOF não vira falsa divergência contra
          # uma única ocorrência correta no Registro 10.
          dplyr::distinct(
            valor_nf, nf_numero, nro_documento_norm, data_emissao,
            .keep_all = TRUE
          ) %>%
          
          # Não leva para a auditoria do Registro 10 as NFs que:
          # 1) são de NPD anterior;
          # 2) não estão no Registro 10; e
          # 3) estão corretamente no Registro 20.
          dplyr::filter(
            !(
              dplyr::coalesce(ind_npd_anterior, "") == "S" &
                chave_cruzamento %in% chaves_exclusivas_reg20
            )
          ) %>%
          
          dplyr::select(
            chave_cruzamento,
            nome_credor,
            nf_numero_raw,
            nf_numero,
            nro_documento,
            nro_documento_norm,
            valor_nf,
            inscrição_estadual,
            ano_npd
          ) %>%
          # IMPORTANTE: uma mesma NF/documento pode aparecer mais de uma vez.
          # Ordenamos pelo valor e criamos uma ocorrência para fazer pareamento
          # 1 a 1, evitando o produto cartesiano do many-to-many.
          dplyr::group_by(chave_cruzamento) %>%
          dplyr::arrange(valor_nf, .by_group = TRUE) %>%
          dplyr::mutate(ocorrencia_chave = dplyr::row_number()) %>%
          dplyr::ungroup()
        
        
        reg10_auditoria <- reg_10 %>%
          dplyr::select(
            chave_cruzamento,
            
            sicom_nf =
              nfNumero_raw,
            
            sicom_nf_norm =
              nfNumero,
            
            sicom_doc =
              nroDocumento,
            
            sicom_doc_norm =
              nroDocumento_norm,
            
            sicom_valor =
              nfValorLiquido,
            
            sicom_ie =
              nroInscEstadual,
            
            sicom_ano =
              ano_sicom
          ) %>%
          # Mesma regra do SOF: pareamento 1 a 1 das ocorrências repetidas
          # da chave NF + documento, ordenando pelo valor.
          dplyr::group_by(chave_cruzamento) %>%
          dplyr::arrange(sicom_valor, .by_group = TRUE) %>%
          dplyr::mutate(ocorrencia_chave = dplyr::row_number()) %>%
          dplyr::ungroup()
        
        
        tabela_div_10 <-
          dplyr::full_join(
            sof_auditoria_10,
            reg10_auditoria,
            by = c("chave_cruzamento", "ocorrencia_chave")
          ) %>%
          
          dplyr::mutate(
            
            # =====================================
            # EXISTÊNCIA EM CADA BASE
            # =====================================
            
            existe_sof =
              !is.na(
                nf_numero_raw
              ),
            
            existe_sicom =
              !is.na(
                sicom_nf
              ),
            
            
            # =====================================
            # NÚMERO DA NF
            # =====================================
            
            div_nf_logico =
              dplyr::case_when(
                
                !existe_sof |
                  !existe_sicom ~ TRUE,
                
                TRUE ~
                  coalesce(
                    nf_numero,
                    ""
                  ) !=
                  coalesce(
                    sicom_nf_norm,
                    ""
                  )
              ),
            
            
            # =====================================
            # DOCUMENTO
            # =====================================
            
            div_doc_logico =
              dplyr::case_when(
                
                !existe_sof |
                  !existe_sicom ~ TRUE,
                
                TRUE ~
                  coalesce(
                    nro_documento_norm,
                    ""
                  ) !=
                  coalesce(
                    sicom_doc_norm,
                    ""
                  )
              ),
            
            
            # =====================================
            # VALOR
            # =====================================
            
            div_valor_logico =
              dplyr::case_when(
                
                !existe_sof |
                  !existe_sicom ~ TRUE,
                
                TRUE ~
                  abs(
                    coalesce(
                      valor_nf,
                      0
                    ) -
                      coalesce(
                        sicom_valor,
                        0
                      )
                  ) > 0.01
              ),
            
            
            # =====================================
            # INSCRIÇÃO ESTADUAL
            # =====================================
            
            div_ie_logico =
              dplyr::case_when(
                
                !existe_sof |
                  !existe_sicom ~ TRUE,
                
                TRUE ~
                  coalesce(
                    inscrição_estadual,
                    0
                  ) !=
                  coalesce(
                    sicom_ie,
                    0
                  )
              ),
            
            
            # =====================================
            # ANO
            # =====================================
            
            div_ano_logico =
              dplyr::case_when(
                
                !existe_sof |
                  !existe_sicom ~ TRUE,
                
                is.na(ano_npd) &
                  is.na(sicom_ano) ~ FALSE,
                
                is.na(ano_npd) |
                  is.na(sicom_ano) ~ TRUE,
                
                TRUE ~
                  ano_npd !=
                  sicom_ano
              ),
            
            
            # =====================================
            # SITUAÇÃO
            # =====================================
            
            situacao =
              dplyr::case_when(
                
                existe_sof &
                  !existe_sicom ~
                  "Somente no SOF",
                
                !existe_sof &
                  existe_sicom ~
                  "Somente no SICOM",
                
                TRUE ~
                  "SOF e SICOM"
              ),
            
            
            div_nf =
              ifelse(
                div_nf_logico,
                "Sim",
                "Não"
              ),
            
            div_doc =
              ifelse(
                div_doc_logico,
                "Sim",
                "Não"
              ),
            
            div_valor =
              ifelse(
                div_valor_logico,
                "Sim",
                "Não"
              ),
            
            div_ie =
              ifelse(
                div_ie_logico,
                "Sim",
                "Não"
              ),
            
            div_ano =
              ifelse(
                div_ano_logico,
                "Sim",
                "Não"
              )
          ) %>%
          
          # Mantém somente divergências
          dplyr::filter(
            
            situacao !=
              "SOF e SICOM" |
              
              div_nf_logico |
              
              div_doc_logico |
              
              div_valor_logico |
              
              div_ie_logico |
              
              div_ano_logico
          ) %>%
          
          dplyr::select(
            
            Situação =
              situacao,
            
            Credor =
              nome_credor,
            
            `Nº NF SOF` =
              nf_numero_raw,
            
            `Nº NF SICOM (Reg 10)` =
              sicom_nf,
            
            `Diverge Número NF` =
              div_nf,
            
            `Doc. SOF` =
              nro_documento,
            
            `Doc. SICOM (Reg 10)` =
              sicom_doc,
            
            `Diverge Doc` =
              div_doc,
            
            `Valor SOF` =
              valor_nf,
            
            `Valor SICOM (Reg 10)` =
              sicom_valor,
            
            `Diverge Valor` =
              div_valor,
            
            `I.E. SOF` =
              inscrição_estadual,
            
            `I.E. SICOM (Reg 10)` =
              sicom_ie,
            
            `Diverge I.E.` =
              div_ie,
            
            `Ano SOF` =
              ano_npd,
            
            `Ano SICOM (Reg 10)` =
              sicom_ano,
            
            `Diverge Ano` =
              div_ano
          )
        
        
        # =========================================
        # AUDITORIA DETALHADA - REGISTRO 20
        # =========================================
        #
        # REGRA CORRIGIDA:
        # - elimina duplicidades idênticas do SOF;
        # - considera TODAS as ocorrências do Registro 20;
        # - se existir ao menos uma ocorrência com a mesma chave
        #   (NF + documento) E a mesma data, não há divergência;
        # - evita escolher arbitrariamente a primeira data do Reg. 20.
        # =========================================
        
        if (tem_reg20) {
          
          sof_auditoria_20 <- sof %>%
            dplyr::distinct(
              nf_numero, nro_documento_norm, data_emissao,
              .keep_all = TRUE
            ) %>%
            dplyr::select(
              chave_cruzamento,
              nome_credor,
              nf_numero_raw,
              nf_numero,
              nro_documento,
              nro_documento_norm,
              data_emissao
            )
          
          reg20_auditoria <- reg_20 %>%
            dplyr::select(
              chave_cruzamento,
              sicom_nf = nfNumero_raw,
              sicom_nf_norm = nfNumero,
              sicom_doc = nroDocumento,
              sicom_doc_norm = nroDocumento_norm,
              sicom_data = dtEmissaoNF
            ) %>%
            dplyr::mutate(
              sicom_nf = as.character(sicom_nf),
              sicom_nf_norm = as.character(sicom_nf_norm),
              sicom_doc = as.character(sicom_doc),
              sicom_doc_norm = as.character(sicom_doc_norm)
            ) %>%
            dplyr::distinct(
              chave_cruzamento, sicom_data,
              .keep_all = TRUE
            )
          
          # Chaves existentes em cada base
          chaves_sof_20 <- unique(sof_auditoria_20$chave_cruzamento)
          chaves_sicom_20 <- unique(reg20_auditoria$chave_cruzamento)
          
          # 1) Registros do SOF cuja chave não existe no Reg. 20
          div20_so_sof <- sof_auditoria_20 %>%
            dplyr::filter(!chave_cruzamento %in% chaves_sicom_20) %>%
            dplyr::transmute(
              Situação = "Somente no SOF",
              `Nº Nota SOF` = nf_numero_raw,
              `Nº Nota SICOM` = NA_character_,
              Credor = nome_credor,
              `Doc. SOF` = as.character(nro_documento),
              `Doc. SICOM` = NA_character_,
              `Data SOF` = data_emissao,
              `Data SICOM (Reg 20)` = as.Date(NA),
              `Diverge Data` = "Sim"
            )
          
          # 2) Chaves presentes nos dois arquivos, mas sem nenhuma data coincidente
          #    para aquela ocorrência do SOF. Se qualquer linha do Reg. 20 tiver
          #    a mesma data, a NF está correta e não aparece como divergência.
          sof_com_chave_reg20 <- sof_auditoria_20 %>%
            dplyr::filter(chave_cruzamento %in% chaves_sicom_20)
          
          datas_reg20 <- reg20_auditoria %>%
            dplyr::select(chave_cruzamento, sicom_data) %>%
            dplyr::distinct()
          
          div20_data <- sof_com_chave_reg20 %>%
            dplyr::left_join(
              datas_reg20 %>%
                dplyr::rename(data_emissao = sicom_data) %>%
                dplyr::mutate(data_encontrada = TRUE),
              by = c("chave_cruzamento", "data_emissao")
            ) %>%
            dplyr::filter(is.na(data_encontrada)) %>%
            dplyr::left_join(
              reg20_auditoria %>%
                dplyr::group_by(chave_cruzamento) %>%
                dplyr::summarise(
                  sicom_nf = dplyr::first(sicom_nf),
                  sicom_doc = dplyr::first(sicom_doc),
                  sicom_data = dplyr::first(sicom_data),
                  .groups = "drop"
                ),
              by = "chave_cruzamento"
            ) %>%
            dplyr::transmute(
              Situação = "SOF e SICOM",
              `Nº Nota SOF` = nf_numero_raw,
              `Nº Nota SICOM` = sicom_nf,
              Credor = nome_credor,
              `Doc. SOF` = as.character(nro_documento),
              `Doc. SICOM` = as.character(sicom_doc),
              `Data SOF` = data_emissao,
              `Data SICOM (Reg 20)` = sicom_data,
              `Diverge Data` = "Sim"
            )
          
          # 3) Registros do Reg. 20 cuja chave não existe no SOF
          div20_so_sicom <- reg20_auditoria %>%
            dplyr::filter(!chave_cruzamento %in% chaves_sof_20) %>%
            dplyr::transmute(
              Situação = "Somente no SICOM",
              `Nº Nota SOF` = NA_character_,
              `Nº Nota SICOM` = sicom_nf,
              Credor = NA_character_,
              `Doc. SOF` = NA_character_,
              `Doc. SICOM` = as.character(sicom_doc),
              `Data SOF` = as.Date(NA),
              `Data SICOM (Reg 20)` = sicom_data,
              `Diverge Data` = "Sim"
            )
          
          # Padroniza identificadores como texto antes do bind_rows.
          # Isso evita erro ao combinar colunas character e integer64
          # (por exemplo, Nº Nota SICOM lida pelo fread como integer64).
          padronizar_tipos_div20 <- function(df) {
            df %>%
              dplyr::mutate(
                `Nº Nota SOF` = as.character(`Nº Nota SOF`),
                `Nº Nota SICOM` = as.character(`Nº Nota SICOM`),
                `Doc. SOF` = as.character(`Doc. SOF`),
                `Doc. SICOM` = as.character(`Doc. SICOM`)
              )
          }
          
          tabela_div_20 <- dplyr::bind_rows(
            padronizar_tipos_div20(div20_so_sof),
            padronizar_tipos_div20(div20_data),
            padronizar_tipos_div20(div20_so_sicom)
          ) %>%
            dplyr::distinct()
        }
        
        
        # =========================================
        # 3) COMPARAÇÃO INICIAL
        # =========================================
        
        valor_total_sof <-
          sum(
            sof$valor_nf,
            na.rm = TRUE
          )
        
        
        valor_total_reg10 <-
          sum(
            reg_10$nfValorLiquido,
            na.rm = TRUE
          )
        
        
        diferenca_inicial <-
          round(
            valor_total_sof -
              valor_total_reg10,
            2
          )
        
        
        msg <- paste0(
          
          "Valor total SOF: ",
          formata_real(
            valor_total_sof
          ),
          "\n",
          
          "Valor total Registro 10: ",
          formata_real(
            valor_total_reg10
          ),
          "\n",
          
          "Diferença inicial: ",
          formata_real(
            diferenca_inicial
          ),
          "\n",
          
          "Divergências encontradas no Reg. 10: ",
          nrow(
            tabela_div_10
          ),
          "\n",
          
          "Divergências encontradas no Reg. 20: ",
          nrow(
            tabela_div_20
          ),
          "\n\n"
        )
        
        
        # =========================================
        # SE TOTAL JÁ CONFERE
        # =========================================
        
        if (
          round(
            diferenca_inicial,
            2
          ) == 0
        ) {
          
          msg <- paste0(
            msg,
            "Conferência OK"
          )
          
          
          resultado(
            list(
              
              mensagem =
                msg,
              
              tabela_reg20_sof =
                tabela_reg20_sof,
              
              tabela_div_10 =
                tabela_div_10,
              
              tabela_div_20 =
                tabela_div_20
            )
          )
          
          return()
        }
        
        
        # =========================================
        # 4) REMOVER DUPLICADOS DO SOF
        # =========================================
        
        sof_sem_duplicados <- sof %>%
          
          dplyr::distinct(
            
            valor_nf,
            nf_numero,
            nro_documento_norm,
            data_emissao,
            
            .keep_all = TRUE
          )
        
        
        valor_total_sof_sem_duplicados <-
          sum(
            sof_sem_duplicados$valor_nf,
            na.rm = TRUE
          )
        
        
        diferenca_sem_duplicados <-
          round(
            
            valor_total_sof_sem_duplicados -
              valor_total_reg10,
            
            2
          )
        
        
        msg <- paste0(
          
          msg,
          
          "Após remover duplicados do SOF:\n",
          
          "Valor total SOF sem duplicados: ",
          formata_real(
            valor_total_sof_sem_duplicados
          ),
          "\n",
          
          "Diferença após remover duplicados: ",
          formata_real(
            diferenca_sem_duplicados
          ),
          "\n\n"
        )
        
        
        if (
          round(
            diferenca_sem_duplicados,
            2
          ) == 0
        ) {
          
          msg <- paste0(
            msg,
            "Conferência OK"
          )
          
          
          resultado(
            list(
              
              mensagem =
                msg,
              
              tabela_reg20_sof =
                tabela_reg20_sof,
              
              tabela_div_10 =
                tabela_div_10,
              
              tabela_div_20 =
                tabela_div_20
            )
          )
          
          return()
        }
        
        
        # =========================================
        # 5) NFs EXCLUSIVAS DO REGISTRO 20
        # =========================================
        
        if (tem_reg20) {
          
          nf_reg10 <- unique(
            
            reg_10$chave_cruzamento[
              !is.na(
                reg_10$chave_cruzamento
              ) &
                reg_10$chave_cruzamento !=
                "VAZIO_VAZIO"
            ]
          )
          
          
          nf_reg20 <- unique(
            
            reg_20$chave_cruzamento[
              !is.na(
                reg_20$chave_cruzamento
              ) &
                reg_20$chave_cruzamento !=
                "VAZIO_VAZIO"
            ]
          )
          
          
          nf_exclusivas_reg20 <-
            setdiff(
              nf_reg20,
              nf_reg10
            )
          
          
          tabela_reg20_sof <-
            sof_sem_duplicados %>%
            
            dplyr::filter(
              
              ind_npd_anterior ==
                "S",
              
              chave_cruzamento %in%
                nf_exclusivas_reg20
            ) %>%
            
            dplyr::arrange(
              
              chave_cruzamento,
              data_emissao,
              valor_nf,
              linha_sof_original
            )
        }
        
        
        value_reg20_exclusivo_no_sof <-
          sum(
            tabela_reg20_sof$valor_nf,
            na.rm = TRUE
          )
        
        
        valor_sof_ajustado <-
          valor_total_sof_sem_duplicados -
          value_reg20_exclusivo_no_sof
        
        
        diferenca_final <-
          round(
            
            valor_sof_ajustado -
              valor_total_reg10,
            
            2
          )
        
        
        msg <- paste0(
          
          msg,
          
          paste0(
            "Após retirar do SOF as NFs exclusivas ",
            "do Registro 20 com ind_npd_anterior = S:\n"
          ),
          
          paste0(
            "Valor das NFs exclusivas do Registro 20 ",
            "encontradas no SOF: "
          ),
          
          formata_real(
            value_reg20_exclusivo_no_sof
          ),
          
          "\n",
          
          "Valor SOF ajustado: ",
          
          formata_real(
            valor_sof_ajustado
          ),
          
          "\n",
          
          "Diferença final: ",
          
          formata_real(
            diferenca_final
          ),
          
          "\n\n"
        )
        
        
        # =========================================
        # RESULTADO FINAL
        # =========================================
        
        if (
          round(
            diferenca_final,
            2
          ) == 0
        ) {
          
          msg <- paste0(
            msg,
            "Conferência OK"
          )
          
        } else {
          
          msg <- paste0(
            
            msg,
            
            paste0(
              "Verificar se houve credor(es) com numeração ",
              "repetida da NF ou divergência de chaves básicas ",
              "(Número/Documento) entre os arquivos. Verificar ano."
            )
          )
        }
        
        
        resultado(
          list(
            
            mensagem =
              msg,
            
            tabela_reg20_sof =
              tabela_reg20_sof,
            
            tabela_div_10 =
              tabela_div_10,
            
            tabela_div_20 =
              tabela_div_20
          )
        )
        
        
      }, error = function(e) {
        
        resultado(
          list(
            
            mensagem =
              paste(
                "ERRO:",
                e$message
              ),
            
            tabela_reg20_sof =
              tibble::tibble(),
            
            tabela_div_10 =
              tibble::tibble(),
            
            tabela_div_20 =
              tibble::tibble()
          )
        )
      })
    }
  )
  
  
  # =========================================
  # RESUMO
  # =========================================
  
  output$mensagem <- renderText({
    
    res <- resultado()
    
    if (is.null(res)) {
      return("")
    }
    
    res$mensagem
  })
  
  
  # =========================================
  # TABELA REGISTRO 10
  # =========================================
  
  output$tabela_div_reg10 <- renderDT({
    
    res <- resultado()
    
    
    if (
      is.null(res) ||
      is.null(
        res$tabela_div_10
      ) ||
      nrow(
        res$tabela_div_10
      ) == 0
    ) {
      
      return(
        DT::datatable(
          
          data.frame(
            Mensagem =
              paste0(
                "Nenhuma divergência encontrada ",
                "no Registro 10."
              )
          ),
          
          options =
            list(
              dom = "t"
            ),
          
          rownames = FALSE
        )
      )
    }
    
    
    tabela <- res$tabela_div_10
    
    
    DT::datatable(
      
      tabela,
      
      extensions =
        "Buttons",
      
      options =
        list(
          
          dom =
            "Bfrtip",
          
          buttons =
            c(
              "copy",
              "csv",
              "excel"
            ),
          
          pageLength =
            20,
          
          scrollX =
            TRUE
        ),
      
      rownames =
        FALSE
    ) %>%
      
      DT::formatCurrency(
        c(
          "Valor SOF",
          "Valor SICOM (Reg 10)"
        ),
        currency = "R$ ",
        mark = ".",
        dec.mark = ",",
        digits = 2
      )
  })
  
  
  # =========================================
  # TABELA REGISTRO 20
  # =========================================
  
  output$tabela_div_reg20 <- renderDT({
    
    res <- resultado()
    
    
    if (
      is.null(res) ||
      is.null(
        res$tabela_div_20
      ) ||
      nrow(
        res$tabela_div_20
      ) == 0
    ) {
      
      return(
        DT::datatable(
          
          data.frame(
            Mensagem =
              paste0(
                "Nenhuma divergência encontrada ",
                "no Registro 20."
              )
          ),
          
          options =
            list(
              dom = "t"
            ),
          
          rownames =
            FALSE
        )
      )
    }
    
    
    DT::datatable(
      
      res$tabela_div_20,
      
      extensions =
        "Buttons",
      
      options =
        list(
          
          dom =
            "Bfrtip",
          
          buttons =
            c(
              "copy",
              "csv",
              "excel"
            ),
          
          pageLength =
            20,
          
          scrollX =
            TRUE
        ),
      
      rownames =
        FALSE
    )
  })
  
  
  # =========================================
  # TABELA EXCLUSIVAS REGISTRO 20
  # =========================================
  
  output$tabela_reg20_sof <-
    DT::renderDT({
      
      res <- resultado()
      
      
      if (
        is.null(res) ||
        is.null(
          res$tabela_reg20_sof
        ) ||
        nrow(
          res$tabela_reg20_sof
        ) == 0
      ) {
        
        return(
          DT::datatable(
            
            data.frame(
              Mensagem =
                paste0(
                  "Nenhuma NF exclusiva do Registro 20 ",
                  "encontrada no SOF."
                )
            ),
            
            options =
              list(
                dom = "t"
              ),
            
            rownames =
              FALSE
          )
        )
      }
      
      
      DT::datatable(
        
        res$tabela_reg20_sof,
        
        extensions =
          "Buttons",
        
        options =
          list(
            
            dom =
              "Bfrtip",
            
            buttons =
              c(
                "copy",
                "csv",
                "excel"
              ),
            
            pageLength =
              50,
            
            scrollX =
              TRUE
          ),
        
        rownames =
          FALSE
      )
      
    }, server = FALSE)
}


# =========================================
# EXECUTAR APLICAÇÃO
# =========================================

shinyApp(
  ui,
  server
)