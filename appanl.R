# =========================================
# 1. Instalar e Carregar Pacotes
# =========================================
packages_needed <- c("shiny", "readxl", "dplyr", "DT", "data.table", "shinythemes", "stringr", "openxlsx")
for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# =========================================
# 2. Funções Auxiliares de Sanitização de Dados
# =========================================
formata_real <- function(x) {
  paste0("R$ ", formatC(ifelse(is.na(x), 0, x), format = "f", big.mark = ".", decimal.mark = ",", digits = 2))
}

to_numeric_br <- function(x) {
  if (is.numeric(x)) return(round(x, 2))
  x <- as.character(x); x <- gsub("R\\$", "", x); x <- trimws(x)
  if (length(x) == 0 || x == "" || is.na(x)) return(0)
  
  if (grepl(",", x)) {
    x <- gsub("\\.", "", x)  
    x <- gsub(",", ".", x)   
  }
  
  res <- suppressWarnings(as.numeric(x))
  res[is.na(res)] <- 0
  return(round(res, 2))
}

extrair_id_sicom <- function(campo) {
  txt <- trimws(as.character(campo))
  if (length(txt) == 0 || txt == "" || is.na(txt) || txt == "NA") return(0)
  
  if (nchar(txt) == 18) {
    txt_miolo <- substr(txt, 10, 14)
    num <- suppressWarnings(as.integer(txt_miolo))
    return(ifelse(is.na(num), 0, num))
  }
  
  num <- suppressWarnings(as.integer(stringr::str_remove_all(txt, "\\D")))
  return(ifelse(is.na(num), 0, num))
}

limpar_ID_sof <- function(campo) {
  if (is.numeric(campo)) return(as.integer(campo))
  txt <- trimws(as.character(campo))
  if (length(txt) == 0 || txt == "" || is.na(txt)) return(0)
  txt_limpo <- stringr::str_remove_all(txt, "\\D")
  num <- suppressWarnings(as.integer(txt_limpo))
  return(ifelse(is.na(num), 0, num))
}

limpar_uo_sof <- function(uo_txt) {
  uo_txt <- gsub("[^0-9]", "", as.character(uo_txt))
  suppressWarnings(as.numeric(uo_txt))
}

extrair_uo_sicom <- function(uo_txt) {
  uo_txt <- gsub("[^0-9]", "", as.character(uo_txt))
  res <- case_when(
    nchar(uo_txt) == 4 ~ paste0(substr(uo_txt, 1, 2), substr(uo_txt, 4, 4)),
    nchar(uo_txt) == 5 ~ paste0(substr(uo_txt, 1, 2), substr(uo_txt, 4, 5)),
    TRUE                ~ uo_txt
  )
  suppressWarnings(as.numeric(res))
}

# --- FUNÇÃO ATUALIZADA COM A REGRA DE DETALHAMENTO >= 500 ---
tratar_detalhamento_fonte <- function(d_txt) {
  d_txt <- gsub("\\.0+$", "", trimws(as.character(d_txt)))
  d_txt <- stringr::str_remove_all(d_txt, "\\D")
  
  if (is.na(d_txt) || d_txt == "" || d_txt == "NA") return("000")
  
  num_val <- suppressWarnings(as.numeric(d_txt))
  if (is.na(num_val) || num_val == 0) return("000")
  
  # REGRA SICOM: Detalhamento superior ou igual a 500 vira "000"
  if (num_val >= 500) return("000")
  
  # Padroniza para 3 dígitos se for menor que 500 (ex: 2 -> "002")
  return(stringr::str_pad(as.character(num_val), width = 3, side = "left", pad = "0"))
}

converter_codigo_orcamentario <- function(co_txt) {
  co_txt <- gsub("\\.0+$", "", trimws(as.character(co_txt)))
  if (is.na(co_txt) || co_txt == "" || co_txt == "NA") return("0")
  
  num_val <- suppressWarnings(as.numeric(co_txt))
  if (is.na(num_val)) return(co_txt)
  
  if (num_val == 9000) return("0")
  if (num_val >= 9218 && num_val <= 9230) return("0")
  if (num_val == 9998 || num_val == 9999) return("0")
  if (num_val == 9001) return("1001")
  if (num_val == 9002) return("1002")
  if (num_val == 9311) return("3110")
  if (num_val == 9312) return("3120")
  if (num_val == 9313) return("3130")
  if (num_val == 9314) return("3140")
  if (num_val == 9321) return("3210")
  if (num_val == 9322) return("3220")
  
  return(as.character(num_val))
}

# =========================================
# 3. Interface do Usuário (UI)
# =========================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Sistema de Conferência SOF x SICOM - ANL"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("arquivo_anl", "1. Arquivo ANL SICOM (.CSV)", accept = c(".csv", ".txt")),
      fileInput("arquivo_sof", "2. Relatório SOF (.XLS / .XLSX / .CSV)", accept = c(".xlsx", ".xls", ".csv")),
      hr(),
      actionButton("executar", "Executar Conferência", class = "btn-primary btn-lg", style="width: 100%"),
      br(), br(),
      uiOutput("botao_exportar_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumo", 
                 br(),
                 verbatimTextOutput("resumo_texto")),
        tabPanel("Divergências Registro 10", 
                 br(),
                 DTOutput("tabela_divergencias_r10")),
        tabPanel("Divergências Registro 11", 
                 br(),
                 DTOutput("tabela_divergencias_r11"))
      )
    )
  )
)

# =========================================
# 4. Lógica do Servidor (SERVER)
# =========================================
server <- function(input, output, session) {
  
  resultado <- reactiveVal(NULL)
  
  output$botao_exportar_ui <- renderUI({
    req(resultado())
    downloadButton("exportar_excel", "Exportar Divergências (.XLSX)", class = "btn-success btn-lg", style="width: 100%")
  })
  
  observeEvent(input$executar, {
    tryCatch({
      req(input$arquivo_anl, input$arquivo_sof)
      
      # --- 4.1. LEITURA E TRATAMENTO: ANL SICOM ---
      anl_raw <- data.table::fread(input$arquivo_anl$datapath, sep = ";", header = FALSE, 
                                   fill = TRUE, encoding = "Latin-1", data.table = FALSE,
                                   colClasses = "character")
      
      reg10 <- anl_raw[anl_raw[[1]] == "10", , drop = FALSE]
      reg11 <- anl_raw[anl_raw[[1]] == "11", , drop = FALSE]
      
      if(nrow(reg10) == 0) stop("Nenhum registro tipo '10' encontrado no arquivo SICOM.")
      
      total_r10_original <- sum(sapply(reg10[[10]], to_numeric_br), na.rm = TRUE)
      total_r11_original <- if(nrow(reg11) > 0 && ncol(reg11) >= 7) sum(sapply(reg11[[7]], to_numeric_br), na.rm = TRUE) else 0
      
      df_r10_detalhe <- data.frame(
        uo        = sapply(reg10[[3]], extrair_uo_sicom),
        empenho   = sapply(reg10[[4]], extrair_id_sicom),
        anulacao  = sapply(reg10[[7]], extrair_id_sicom),
        vl_bruto  = sapply(reg10[[10]], to_numeric_br),
        stringsAsFactors = FALSE
      ) %>%
        group_by(uo, anulacao) %>% 
        summarise(
          empenho_SICOM = paste(sort(unique(empenho)), collapse = ", "),
          vlAnulacao_SICOM = round(sum(vl_bruto, na.rm = TRUE), 2), 
          .groups = "drop"
        )
      
      df_r11_detalhe <- if(nrow(reg11) > 0) {
        data.frame(
          uo                  = sapply(reg11[[2]], extrair_uo_sicom),
          empenho             = sapply(reg11[[3]], extrair_id_sicom),
          anulacao            = sapply(reg11[[4]], extrair_id_sicom),
          codFontRecursos_SIC = trimws(as.character(reg11[[5]])),
          codCO_SICOM         = sapply(reg11[[6]], limpar_ID_sof),
          vl_bruto            = sapply(reg11[[7]], to_numeric_br),
          stringsAsFactors = FALSE
        ) %>%
          group_by(uo, anulacao) %>% 
          summarise(
            empenho_SICOM       = paste(sort(unique(empenho)), collapse = ", "),
            codFontRecursos_SIC = paste(sort(unique(codFontRecursos_SIC)), collapse = ", "),
            codCO_SICOM         = paste(sort(unique(codCO_SICOM)), collapse = ", "),
            vlAnulacaoFonte_SICOM = round(sum(vl_bruto, na.rm = TRUE), 2), 
            .groups = "drop"
          )
      } else {
        data.frame(uo=numeric(), anulacao=numeric(), empenho_SICOM=character(), codFontRecursos_SIC=character(), codCO_SICOM=character(), vlAnulacaoFonte_SICOM=numeric())
      }
      
      # --- 4.2. LEITURA E TRATAMENTO: RELATÓRIO SOF ---
      ext <- tools::file_ext(input$arquivo_sof$datapath)
      
      if(tolower(ext) == "csv") {
        sof_raw <- data.table::fread(input$arquivo_sof$datapath, data.table = FALSE, 
                                     encoding = "Latin-1", colClasses = "character")
      } else {
        sof_raw <- readxl::read_excel(input$arquivo_sof$datapath, col_types = "text")
      }
      
      req(sof_raw)
      names(sof_raw) <- tolower(names(sof_raw))
      
      if(!all(c("uo", "anulacao", "vl_anulado") %in% names(sof_raw))) {
        stop("O arquivo SOF precisa conter as colunas: 'uo', 'anulacao' e 'vl_anulado'.")
      }
      
      total_sof_original <- sum(sapply(sof_raw$vl_anulado, to_numeric_br), na.rm = TRUE)
      
      campos_fonte_obrigatorios <- c("cod_siafic_grupo", "cod_siafic_fonte", "cod_siafic_detalhamento_fonte")
      tem_fontes_sof <- all(campos_fonte_obrigatorios %in% names(sof_raw))
      tem_codigo_orcamentario <- "cod_siafic_codigo_orcamentario" %in% names(sof_raw)
      tem_empenho_sof <- "empenho" %in% names(sof_raw)
      
      sof_transformado <- sof_raw %>%
        mutate(across(everything(), as.character)) %>%
        mutate(
          uo_num       = sapply(uo, limpar_uo_sof),
          anulacao_num = sapply(anulacao, limpar_ID_sof),
          vl_anulado_num = sapply(vl_anulado, to_numeric_br),
          empenho_num  = if(tem_empenho_sof) sapply(empenho, limpar_ID_sof) else { 0 },
          
          codFontRecursos_SOF = if(tem_fontes_sof) {
            g <- stringr::str_remove_all(gsub("\\.0+$", "", trimws(cod_siafic_grupo)), "\\D")
            f <- stringr::str_remove_all(gsub("\\.0+$", "", trimws(cod_siafic_fonte)), "\\D")
            g[is.na(g) | g == "NA"] <- ""
            f[is.na(f) | f == "NA"] <- ""
            
            f <- ifelse(nchar(f) > 0 & nchar(f) < 3, stringr::str_pad(f, 3, "left", "0"), f)
            d_tratado <- sapply(cod_siafic_detalhamento_fonte, tratar_detalhamento_fonte)
            
            paste0(g, f, d_tratado)
          } else { "0" },
          
          codCO_SOF = if(tem_codigo_orcamentario) {
            sapply(sapply(cod_siafic_codigo_orcamentario, converter_codigo_orcamentario), limpar_ID_sof)
          } else { 0 }
        )
      
      df_sof_para_r10 <- sof_transformado %>%
        group_by(uo = uo_num, anulacao = anulacao_num) %>%
        summarise(
          empenho_SOF = paste(sort(unique(empenho_num)), collapse = ", "),
          vl_anulado_SOF = round(sum(vl_anulado_num, na.rm = TRUE), 2), 
          .groups = "drop"
        )
      
      df_sof_para_r11 <- sof_transformado %>%
        group_by(uo = uo_num, anulacao = anulacao_num) %>%
        summarise(
          empenho_SOF         = paste(sort(unique(empenho_num)), collapse = ", "),
          codFontRecursos_SOF = paste(sort(unique(codFontRecursos_SOF)), collapse = ", "),
          codCO_SOF           = paste(sort(unique(codCO_SOF)), collapse = ", "),
          vl_anulado_SOF      = round(sum(vl_anulado_num, na.rm = TRUE), 2), 
          .groups = "drop"
        )
      
      # --- 4.3. BATIMENTOS ---
      divergencias_r10 <- full_join(df_r10_detalhe, df_sof_para_r10, by = c("uo", "anulacao")) %>%
        mutate(
          empenho_SICOM    = coalesce(as.character(empenho_SICOM), "N/A"),
          empenho_SOF      = coalesce(as.character(empenho_SOF), "N/A"),
          vlAnulacao_SICOM = coalesce(vlAnulacao_SICOM, 0),
          vl_anulado_SOF   = coalesce(vl_anulado_SOF, 0),
          Diferenca        = round(vlAnulacao_SICOM - vl_anulado_SOF, 2)
        ) %>%
        filter(abs(Diferenca) >= 0.01 | empenho_SICOM != empenho_SOF) %>%
        arrange(uo, anulacao)
      
      divergencias_r11 <- full_join(df_r11_detalhe, df_sof_para_r11, by = c("uo", "anulacao")) %>%
        mutate(
          empenho_SICOM         = coalesce(as.character(empenho_SICOM), "N/A"),
          empenho_SOF           = coalesce(as.character(empenho_SOF), "N/A"),
          vlAnulacaoFonte_SICOM = coalesce(vlAnulacaoFonte_SICOM, 0),
          vl_anulado_SOF        = coalesce(vl_anulado_SOF, 0),
          Diferenca             = round(vlAnulacaoFonte_SICOM - vl_anulado_SOF, 2),
          codFontRecursos_SIC   = coalesce(as.character(codFontRecursos_SIC), "N/A"),
          codFontRecursos_SOF   = coalesce(as.character(codFontRecursos_SOF), "N/A"),
          codCO_SICOM           = coalesce(as.character(codCO_SICOM), "N/A"),
          codCO_SOF             = coalesce(as.character(codCO_SOF), "N/A")
        ) %>%
        filter(abs(Diferenca) >= 0.01 | 
                 codFontRecursos_SIC != codFontRecursos_SOF | 
                 codCO_SICOM != codCO_SOF |
                 empenho_SICOM != empenho_SOF) %>%
        arrange(uo, anulacao)
      
      resultado(list(
        erros_r10 = divergencias_r10,
        erros_r11 = divergencias_r11,
        t_sof     = total_sof_original,
        t_r10     = total_r10_original,
        t_r11     = total_r11_original
      ))
      
    }, error = function(e) {
      showNotification(paste("Erro Operacional:", e$message), type = "error", duration = 10)
    })
  })
  
  # --- 4.4. CONTROLE DE DOWNLOAD ---
  output$exportar_excel <- downloadHandler(
    filename = function() {
      paste0("divergencias_sicom_sof_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(resultado())
      res <- resultado()
      
      df_resumo_excel <- data.frame(
        Metrica = c("Total Relatório SOF", "Total ANL SICOM Registro 10", "Total ANL SICOM Registro 11", "Diferença Absoluta (R10-SOF)", "Quantidade Inconsistências R10", "Quantidade Inconsistências R11"),
        Valor = c(res$t_sof, res$t_r10, res$t_r11, (res$t_r10 - res$t_sof), nrow(res$erros_r10), nrow(res$erros_r11))
      )
      
      wb <- createWorkbook()
      addWorksheet(wb, "Resumo Global")
      addWorksheet(wb, "Divergencias R10")
      addWorksheet(wb, "Divergencias R11")
      
      writeData(wb, "Resumo Global", df_resumo_excel)
      writeData(wb, "Divergencias R10", res$erros_r10)
      writeData(wb, "Divergencias R11", res$erros_r11)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # --- 4.5. RENDERS ---
  output$resumo_texto <- renderText({
    req(resultado()); res <- resultado()
    paste0(
      "RESUMO DA CONFERÊNCIA GLOBAL (REGRAS CADASTRAIS ATIVAS)\n",
      "-------------------------------------------\n",
      "Total Relatório SOF:          ", formata_real(res$t_sof), "\n",
      "Total ANL SICOM Registro 10:  ", formata_real(res$t_r10), "\n",
      "Total ANL SICOM Registro 11:  ", formata_real(res$t_r11), "\n",
      "Diferença Absoluta (R10-SOF): ", formata_real(res$t_r10 - res$t_sof), "\n",
      "-------------------------------------------\n",
      "Divergências no Registro 10 (Valor/Empenho):      ", nrow(res$erros_r10), " item(ns) encontrado(s).\n",
      "Divergências no Registro 11 (Valor/Fontes/coCO):  ", nrow(res$erros_r11), " item(ns) encontrado(s)."
    )
  })
  
  output$tabela_divergencias_r10 <- renderDT({
    req(resultado())
    dados_view <- resultado()$erros_r10 %>%
      mutate(
        vlAnulacao_SICOM = sapply(vlAnulacao_SICOM, formata_real),
        vl_anulado_SOF   = sapply(vl_anulado_SOF, formata_real),
        Diferenca        = sapply(Diferenca, formata_real)
      ) %>%
      select(uo, anulacao, empenho_SICOM, empenho_SOF, vlAnulacao_SICOM, vl_anulado_SOF, Diferenca)
    
    datatable(dados_view, rownames = FALSE,
              colnames = c("UO", "Nº Anulação", "Empenho SICOM", "Empenho SOF", "Valor SICOM (R10)", "Valor SOF", "Diferença Fin."),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tabela_divergencias_r11 <- renderDT({
    req(resultado())
    dados_view <- resultado()$erros_r11 %>%
      mutate(
        vlAnulacaoFonte_SICOM = sapply(vlAnulacaoFonte_SICOM, formata_real),
        vl_anulado_SOF        = sapply(vl_anulado_SOF, formata_real),
        Diferenca             = sapply(Diferenca, formata_real)
      ) %>%
      select(uo, anulacao, empenho_SICOM, empenho_SOF, codFontRecursos_SIC, codFontRecursos_SOF, codCO_SICOM, codCO_SOF, vlAnulacaoFonte_SICOM, vl_anulado_SOF, Diferenca)
    
    datatable(dados_view, rownames = FALSE,
              colnames = c("UO", "Nº Anulação", "Empenho SICOM", "Empenho SOF", "Fonte SICOM", "Fonte SOF", "codCO SICOM", "codCO SOF", "Valor SICOM (R11)", "Valor SOF", "Diferença Fin."),
              options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)