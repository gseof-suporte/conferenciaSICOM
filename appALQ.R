# ==============================================================================
# CARREGAMENTO DE PACOTES E BIBLIOTECAS
# ==============================================================================
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(scales)

# ==============================================================================
# FUNÇÕES AUXILIARES DE SUPORTE
# ==============================================================================

# Formatação monetária padrão BRL (R$)
formatar_moeda <- label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 0.01)

# Converter meses por extenso ou numéricos para padrão numérico
mes_para_numero <- function(mes) {
  if (is.na(mes)) return(NA)
  meses <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
             "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
  res <- match(tolower(str_trim(as.character(mes))), meses)
  if (is.na(res)) {
    return(as.numeric(mes))
  }
  return(res)
}

# Tratamento flexível de separadores decimais e milhares
limpar_e_converter_num <- function(val) {
  if (is.numeric(val)) return(val)
  val_str <- as.character(val)
  val_str <- str_replace_all(val_str, ",", ".")
  return(as.numeric(val_str))
}

# Leitor seguro para arquivos do SOF (Excel ou CSV)
ler_sof_seguro <- function(caminho) {
  if (is.null(caminho)) return(tibble())
  extensao <- tolower(tools::file_ext(caminho$name))
  
  df <- tryCatch({
    if (extensao %in% c("xls", "xlsx")) {
      read_excel(caminho$datapath)
    } else {
      read_delim(caminho$datapath, delim = ";", escape_double = FALSE, 
                 trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = "."))
    }
  }, error = function(e) {
    return(tibble())
  })
  
  if (ncol(df) > 0) {
    names(df) <- make.names(ifelse(is.na(names(df)) | names(df) == "", "COL_MUTAVEL", names(df)), unique = TRUE)
  }
  return(df)
}

# ==============================================================================
# INTERFACE DO USUÁRIO (UI)
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .resumo-box { padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 20px; border-left: 5px solid #007bff; }
      .resumo-title { font-weight: bold; font-size: 1.1em; margin-top: 10px; color: #1c3d5a; }
    "))
  ),
  
  titlePanel("Dashboard de Conferência ALQ: SOF vs SICOM"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Upload de Arquivos"),
      p("Insira os relatórios necessários para a execução da rotina."),
      
      fileInput("sicom", "Relatório ALQ SICOM (Obrigatório, .csv)", accept = c(".csv", ".txt")),
      hr(),
      fileInput("sof10", "Relatório ALQ Reg 10 SOF (Opcional)", accept = c(".csv", ".xlsx", ".xls")),
      fileInput("sof11", "Relatório ALQ Reg 11 SOF (Opcional)", accept = c(".csv", ".xlsx", ".xls")),
      fileInput("sof12", "Relatório ALQ Reg 12 SOF (Opcional)", accept = c(".csv", ".xlsx", ".xls")),
      fileInput("sof20", "Relatório ALQ Reg 20 SOF (Opcional)", accept = c(".csv", ".xlsx", ".xls")),
      
      actionButton("executar", "Executar Conferência", class = "btn-primary btn-lg", width = "100%", icon = icon("play"))
    ),
    
    mainPanel(
      uiOutput("resumo_geral"),
      hr(),
      tabsetPanel(
        tabPanel("Divergências Registro 10", DTOutput("tabela_reg10")),
        tabPanel("Divergências Registro 11", DTOutput("tabela_reg11")),
        tabPanel("Divergências Registro 12", DTOutput("tabela_reg12")),
        tabPanel("Divergências Registro 20", DTOutput("tabela_reg20"))
      )
    )
  )
)

# ==============================================================================
# LÓGICA BACKEND (SERVER)
# ==============================================================================
server <- function(input, output, session) {
  
  resultados <- reactiveValues(
    resumo = NULL,
    div10 = NULL,
    div11 = NULL,
    div12 = NULL,
    div20 = NULL
  )
  
  observeEvent(input$executar, {
    req(input$sicom)
    
    tryCatch({
      withProgress(message = 'Processando dados e conferências...', value = 0, {
        
        # ----------------------------------------------------------------------
        # ETAPA 1 e 2: CARREGAMENTO BASE SICOM
        # ----------------------------------------------------------------------
        incProgress(0.1, detail = "Lendo arquivo base SICOM")
        
        sicom_raw <- read_delim(
          input$sicom$datapath,
          delim = ";",
          col_names = FALSE,
          col_types = cols(.default = "c"),
          locale = locale(grouping_mark = ".")
        )
        
        names(sicom_raw) <- paste0("X", 1:ncol(sicom_raw))
        coluna_reg <- names(sicom_raw)[1]
        
        cabs <- list(
          reg10 = c("Reg", "codReduzido", "codOrgao", "codUnidadeSub", "nroEmpenho", "dtEmpenho", "dtLiquidacao", "nroLiquidacao", "dtAnulacaoLiq", "nroLiquidacaoANL", "tpLiquidacao", "justificativaAnulacao", "vlAnulado"),
          reg11 = c("Reg", "codReduzido", "codFontRecursos", "CO", "valorFonte"),
          reg12 = c("Reg", "codReduzido", "mesCompetencia", "exercicioCompetencia", "vlAnuladoDspExerAnt"),
          reg20 = c("Reg", "codReduzido", "codFontRecursos", "codCO", "tipoRetencao", "descricaoRetencao", "vlRetencaoFonte")
        )
        
        sicom_10_raw <- sicom_raw %>% filter(str_trim(!!sym(coluna_reg)) == "10")
        sicom_11_raw <- sicom_raw %>% filter(str_trim(!!sym(coluna_reg)) == "11")
        sicom_12_raw <- sicom_raw %>% filter(str_trim(!!sym(coluna_reg)) == "12")
        sicom_20_raw <- sicom_raw %>% filter(str_trim(!!sym(coluna_reg)) == "20")
        
        if (nrow(sicom_10_raw) > 0) {
          qtd_col10 <- min(ncol(sicom_10_raw), length(cabs$reg10))
          sicom_10 <- setNames(sicom_10_raw[, 1:qtd_col10, drop = FALSE], cabs$reg10[1:qtd_col10])
        } else { sicom_10 <- tibble() }
        
        if (nrow(sicom_11_raw) > 0) {
          qtd_col11 <- min(ncol(sicom_11_raw), length(cabs$reg11))
          sicom_11 <- setNames(sicom_11_raw[, 1:qtd_col11, drop = FALSE], cabs$reg11[1:qtd_col11])
        } else { sicom_11 <- tibble() }
        
        if (nrow(sicom_12_raw) > 0) {
          qtd_col12 <- min(ncol(sicom_12_raw), length(cabs$reg12))
          sicom_12 <- setNames(sicom_12_raw[, 1:qtd_col12, drop = FALSE], cabs$reg12[1:qtd_col12])
        } else { sicom_12 <- tibble() }
        
        if (nrow(sicom_20_raw) > 0) {
          qtd_col20 <- min(ncol(sicom_20_raw), length(cabs$reg20))
          sicom_20 <- setNames(sicom_20_raw[, 1:qtd_col20, drop = FALSE], cabs$reg20[1:qtd_col20])
        } else { sicom_20 <- tibble() }
        
        incProgress(0.3, detail = "Carregando bases do SOF")
        sof_10 <- ler_sof_seguro(input$sof10)
        sof_11 <- ler_sof_seguro(input$sof11)
        sof_12 <- ler_sof_seguro(input$sof12)
        sof_20 <- ler_sof_seguro(input$sof20)
        
        # ----------------------------------------------------------------------
        # TRATAMENTO E ESTRUTURAÇÃO DOS REGISTROS (ETAPAS 3 A 10)
        # ----------------------------------------------------------------------
        incProgress(0.4, detail = "Processando Registro 10")
        if (nrow(sicom_10) > 0) {
          sicom_10 <- sicom_10 %>%
            mutate(
              numero_empenho = as.numeric(str_sub(nroEmpenho, -9, -5)),
              numero_liquidacao = as.numeric(str_sub(nroLiquidacaoANL, -9, -5)),
              valido = nchar(str_trim(codReduzido)) >= 12 | 
                str_detect(justificativaAnulacao, "^DEVOLUCAO DE SALDO NAO UTILIZADO EM ADIANTAMENTO FINANCEIRO")
            ) %>%
            filter(valido & !is.na(numero_empenho) & !is.na(numero_liquidacao)) %>%
            mutate(
              vlAnulado = limpar_e_converter_num(vlAnulado),
              codOrgao = as.numeric(codOrgao),
              codUnidadeSub = as.numeric(codUnidadeSub),
              chave = paste(as.character(numero_empenho), as.character(numero_liquidacao), sep = "-")
            )
        }
        total_sicom_10 <- sum(sicom_10$vlAnulado, na.rm = TRUE)
        
        if (nrow(sof_10) > 0 && all(c("numero_empenho", "numero_liquidacao", "codigo_orgao", "uo_sicom", "valor") %in% names(sof_10))) {
          sof_10 <- sof_10 %>%
            mutate(
              numero_empenho = as.numeric(numero_empenho),
              numero_liquidacao = as.numeric(numero_liquidacao)
            ) %>%
            filter(!is.na(numero_empenho) & !is.na(numero_liquidacao)) %>%
            mutate(
              codigo_orgao = as.numeric(codigo_orgao),
              uo_sicom = as.numeric(uo_sicom),
              valor_sof = limpar_e_converter_num(valor),
              chave = paste(as.character(numero_empenho), as.character(numero_liquidacao), sep = "-")
            )
        } else {
          sof_10 <- tibble(numero_empenho = numeric(), numero_liquidacao = numeric(), codigo_orgao = numeric(), uo_sicom = numeric(), valor_sof = numeric(), chave = character())
        }
        total_sof_10 <- sum(sof_10$valor_sof, na.rm = TRUE)
        
        # REGISTRO 11
        incProgress(0.5, detail = "Processando Registro 11")
        cods_excecao_reg10 <- if (nrow(sicom_10) > 0) {
          sicom_10 %>%
            filter(str_detect(justificativaAnulacao, "^DEVOLUCAO DE SALDO NAO UTILIZADO EM ADIANTAMENTO FINANCEIRO")) %>%
            pull(codReduzido)
        } else {
          character()
        }
        
        if (nrow(sicom_11) > 0) {
          sicom_11 <- sicom_11 %>%
            mutate(
              codReduzido_clean = str_pad(str_trim(codReduzido), width = 14, side = "left", pad = "0"),
              numero_anulacao    = as.numeric(str_sub(codReduzido_clean, 5, 9)),
              numero_empenho     = as.numeric(str_sub(codReduzido_clean, 10, 14)),
              valido             = nchar(str_trim(codReduzido)) >= 12 | (codReduzido %in% cods_excecao_reg10)
            ) %>%
            filter(valido & !is.na(numero_empenho) & !is.na(numero_anulacao)) %>%
            mutate(
              valor_sicom = limpar_e_converter_num(valorFonte),
              fonte_recurso_SICOM = as.numeric(codFontRecursos),
              chave = paste(as.character(numero_empenho), as.character(numero_anulacao), sep = "-")
            )
        }
        total_sicom_11 <- sum(sicom_11$valor_sicom, na.rm = TRUE)
        
        if (nrow(sof_11) > 0 && all(c("numero_empenho", "numero_anulacao", "codigo_fonte_recurso", "valor") %in% names(sof_11))) {
          sof_11 <- sof_11 %>%
            mutate(
              numero_empenho = as.numeric(numero_empenho),
              numero_anulacao = as.numeric(numero_anulacao)
            ) %>%
            filter(!is.na(numero_empenho) & !is.na(numero_anulacao)) %>%
            mutate(
              fonte_recurso_SOF = as.numeric(codigo_fonte_recurso),
              valor_sof = limpar_e_converter_num(valor),
              chave = paste(as.character(numero_empenho), as.character(numero_anulacao), sep = "-")
            )
        } else {
          sof_11 <- tibble(numero_empenho = numeric(), numero_anulacao = numeric(), fonte_recurso_SOF = numeric(), valor_sof = numeric(), chave = character())
        }
        total_sof_11 <- sum(sof_11$valor_sof, na.rm = TRUE)
        
        # REGISTRO 12
        incProgress(0.6, detail = "Processando Registro 12")
        if (nrow(sicom_12) > 0) {
          sicom_12 <- sicom_12 %>%
            mutate(
              numero_anulacao = as.numeric(str_sub(codReduzido, 5, 9)),
              numero_empenho = as.numeric(str_sub(codReduzido, 10, 14))
            ) %>%
            filter(!is.na(numero_empenho) & !is.na(numero_anulacao)) %>%
            mutate(
              valor_sicom = limpar_e_converter_num(vlAnuladoDspExerAnt),
              mesCompetencia = as.numeric(mesCompetencia),
              exercicioCompetencia = as.numeric(exercicioCompetencia),
              chave = paste(as.character(numero_empenho), as.character(numero_anulacao), sep = "-")
            )
        }
        total_sicom_12 <- sum(sicom_12$valor_sicom, na.rm = TRUE)
        
        if (nrow(sof_12) > 0 && all(c("numero_empenho", "numero_anulacao", "mes_competencia", "exercicio_competencia", "valor") %in% names(sof_12))) {
          sof_12 <- sof_12 %>%
            mutate(
              numero_empenho = as.numeric(numero_empenho),
              numero_anulacao = as.numeric(numero_anulacao)
            ) %>%
            filter(!is.na(numero_empenho) & !is.na(numero_anulacao)) %>%
            mutate(
              mes_competencia_num = sapply(mes_competencia, mes_para_numero),
              exercicio_competencia = as.numeric(exercicio_competencia),
              valor_sof = limpar_e_converter_num(valor),
              chave = paste(as.character(numero_empenho), as.character(numero_anulacao), sep = "-")
            )
        } else {
          sof_12 <- tibble(numero_empenho = numeric(), numero_anulacao = numeric(), mes_competencia_num = numeric(), exercicio_competencia = numeric(), valor_sof = numeric(), chave = character())
        }
        total_sof_12 <- sum(sof_12$valor_sof, na.rm = TRUE)
        
        # REGISTRO 20 (CORRIGIDO ALINHAMENTO DO CODREDUZIDO)
        incProgress(0.7, detail = "Processando Registro 20")
        if (nrow(sicom_20) > 0) {
          sicom_20 <- sicom_20 %>%
            mutate(
              codReduzido_clean = str_pad(str_trim(codReduzido), width = 14, side = "left", pad = "0"),
              numero_anulacao    = as.numeric(str_sub(codReduzido_clean, 5, 9)),
              numero_empenho     = as.numeric(str_sub(codReduzido_clean, 10, 14))
            ) %>%
            filter(!is.na(numero_empenho) & !is.na(numero_anulacao)) %>%
            mutate(
              valor_sicom = limpar_e_converter_num(vlRetencaoFonte),
              fonte_recurso_SICOM = as.numeric(codFontRecursos),
              codCO = as.numeric(codCO),
              tipoRetencao = as.numeric(tipoRetencao),
              chave = paste(as.character(numero_empenho), as.character(numero_anulacao), sep = "-")
            )
        }
        total_sicom_20 <- sum(sicom_20$valor_sicom, na.rm = TRUE)
        
        if (nrow(sof_20) > 0 && all(c("numero_empenho", "numero_anulacao", "codigo_fonte_recurso", "codigo_co", "tipo_retencao", "valor") %in% names(sof_20))) {
          sof_20 <- sof_20 %>%
            mutate(
              numero_empenho = as.numeric(numero_empenho),
              numero_anulacao = as.numeric(numero_anulacao)
            ) %>%
            filter(!is.na(numero_empenho) & !is.na(numero_anulacao)) %>%
            mutate(
              fonte_recurso_SOF = as.numeric(codigo_fonte_recurso),
              codigo_co = as.numeric(codigo_co),
              tipo_retencao_num = as.numeric(str_extract(as.character(tipo_retencao), "^\\d+")),
              valor_sof = limpar_e_converter_num(valor),
              chave = paste(as.character(numero_empenho), as.character(numero_anulacao), sep = "-")
            )
        } else {
          sof_20 <- tibble(numero_empenho = numeric(), numero_anulacao = numeric(), fonte_recurso_SOF = numeric(), codigo_co = numeric(), tipo_retencao_num = numeric(), valor_sof = numeric(), chave = character())
        }
        total_sof_20 <- sum(sof_20$valor_sof, na.rm = TRUE)
        
        # ----------------------------------------------------------------------
        # PROCESSO DE CRUZAMENTO E EXTRAÇÃO DAS DIVERGÊNCIAS
        # ----------------------------------------------------------------------
        incProgress(0.8, detail = "Identificando Divergências de Chaves")
        
        # Cruzamento Registro 10
        div_10 <- tibble()
        if (nrow(sicom_10) > 0 && nrow(sof_10) > 0) {
          div_10 <- full_join(sicom_10, sof_10, by = "chave", suffix = c("_SICOM", "_SOF")) %>%
            mutate(
              numero_empenho = ifelse(is.na(numero_empenho_SICOM), numero_empenho_SOF, numero_empenho_SICOM),
              numero_liquidacao = ifelse(is.na(numero_liquidacao_SICOM), numero_liquidacao_SOF, numero_liquidacao_SICOM),
              diff_orgao = ifelse(codOrgao != codigo_orgao | is.na(codOrgao) | is.na(codigo_orgao), "Sim", "Não"),
              diff_unidade = ifelse(codUnidadeSub != uo_sicom | is.na(codUnidadeSub) | is.na(uo_sicom), "Sim", "Não")
            ) %>%
            filter(diff_orgao == "Sim" | diff_unidade == "Sim") %>%
            select(numero_empenho, numero_liquidacao, orgao_SICOM = codOrgao, orgao_SOF = codigo_orgao, 
                   unidade_SICOM = codUnidadeSub, unidade_SOF = uo_sicom, valor_SOF = valor_sof, valor_SICOM = vlAnulado)
        }
        
        # Cruzamento Registro 11
        div_11 <- tibble()
        if (nrow(sicom_11) > 0 && nrow(sof_11) > 0) {
          div_11 <- full_join(sicom_11, sof_11, by = "chave", suffix = c("_SICOM", "_SOF")) %>%
            mutate(
              numero_empenho = ifelse(is.na(numero_empenho_SICOM), numero_empenho_SOF, numero_empenho_SICOM),
              numero_anulacao = ifelse(is.na(numero_anulacao_SICOM), numero_anulacao_SOF, numero_anulacao_SICOM),
              diff_fonte = ifelse(!is.na(fonte_recurso_SICOM) & !is.na(fonte_recurso_SOF) & fonte_recurso_SICOM != fonte_recurso_SOF, "Sim", "Não")
            ) %>%
            filter(diff_fonte == "Sim") %>%
            select(numero_empenho, numero_anulacao, fonte_recurso_SOF, fonte_recurso_SICOM, 
                   valor_SOF = valor_sof, valor_SICOM = valor_sicom)
        }
        
        # Cruzamento Registro 12
        div_12 <- tibble()
        if (nrow(sicom_12) > 0 && nrow(sof_12) > 0) {
          div_12 <- full_join(sicom_12, sof_12, by = "chave", suffix = c("_SICOM", "_SOF")) %>%
            mutate(
              numero_empenho = ifelse(is.na(numero_empenho_SICOM), numero_empenho_SOF, numero_empenho_SICOM),
              numero_anulacao = ifelse(is.na(numero_anulacao_SICOM), numero_anulacao_SOF, numero_anulacao_SICOM),
              diff_mes = ifelse(mesCompetencia != mes_competencia_num | is.na(mesCompetencia) | is.na(mes_competencia_num), "Sim", "Não"),
              diff_exerc = ifelse(exercicioCompetencia != exercicio_competencia | is.na(exercicioCompetencia) | is.na(exercicio_competencia), "Sim", "Não")
            ) %>%
            filter(diff_mes == "Sim" | diff_exerc == "Sim") %>%
            select(numero_empenho, numero_anulacao, mes_competencia_SICOM = mesCompetencia, mes_competencia_SOF = mes_competencia_num, 
                   exercicio_SICOM = exercicioCompetencia, exercicio_SOF = exercicio_competencia, valor_SOF = valor_sof, valor_SICOM = valor_sicom)
        }
        
        # Cruzamento Registro 20 (CORRIGIDO PARA SÓ COMPARAR FONTE QUANDO EXISTIR AMBAS AS CHAVES)
        div_20 <- tibble()
        if (nrow(sicom_20) > 0 && nrow(sof_20) > 0) {
          div_20 <- full_join(sicom_20, sof_20, by = "chave", suffix = c("_SICOM", "_SOF")) %>%
            mutate(
              numero_empenho = ifelse(is.na(numero_empenho_SICOM), numero_empenho_SOF, numero_empenho_SICOM),
              numero_anulacao = ifelse(is.na(numero_anulacao_SICOM), numero_anulacao_SOF, numero_anulacao_SICOM),
              diff_fonte = ifelse(!is.na(fonte_recurso_SICOM) & !is.na(fonte_recurso_SOF) & fonte_recurso_SICOM != fonte_recurso_SOF, "Sim", "Não"),
              diff_co = ifelse(!is.na(codCO) & !is.na(codigo_co) & codCO != codigo_co, "Sim", "Não"),
              diff_retencao = ifelse(!is.na(tipoRetencao) & !is.na(tipo_retencao_num) & tipoRetencao != tipo_retencao_num, "Sim", "Não")
            ) %>%
            filter(diff_fonte == "Sim" | diff_co == "Sim" | diff_retencao == "Sim") %>%
            select(numero_empenho, numero_anulacao, fonte_recurso_SOF, fonte_recurso_SICOM, 
                   valor_SOF = valor_sof, valor_SICOM = valor_sicom,
                   co_SICOM = codCO, co_SOF = codigo_co, tipo_retencao_SICOM = tipoRetencao, tipo_retencao_SOF = tipo_retencao_num)
        }
        
        resultados$div10 <- div_10
        resultados$div11 <- div_11
        resultados$div12 <- div_12
        resultados$div20 <- div_20
        
        # ----------------------------------------------------------------------
        # ESTRUTURAÇÃO DO RESUMO GLOBAL
        # ----------------------------------------------------------------------
        resultados$resumo <- HTML(paste0(
          "<div class='resumo-box'>",
          "<div class='resumo-title'>RESUMO DA CONFERÊNCIA REGISTRO 10</div>",
          "Total SOF Registro 10: ", formatar_moeda(total_sof_10), "<br>",
          "Total SICOM Registro 10: ", formatar_moeda(total_sicom_10), "<br>",
          "<strong>Diferença (SOF Reg. 10 - Total SICOM Reg. 10): ", formatar_moeda(total_sof_10 - total_sicom_10), "</strong><br><br>",
          
          "<div class='resumo-title'>RESUMO DA CONFERÊNCIA REGISTRO 11</div>",
          "Total SOF Registro 11: ", formatar_moeda(total_sof_11), "<br>",
          "Total SICOM Registro 11: ", formatar_moeda(total_sicom_11), "<br>",
          "<strong>Diferença (SOF Reg. 11 - Total SICOM Reg. 11): ", formatar_moeda(total_sof_11 - total_sicom_11), "</strong><br><br>",
          
          "<div class='resumo-title'>RESUMO DA CONFERÊNCIA REGISTRO 12</div>",
          "Total SOF Registro 12: ", formatar_moeda(total_sof_12), "<br>",
          "Total SICOM Registro 12: ", formatar_moeda(total_sicom_12), "<br>",
          "<strong>Diferença (SOF Reg. 12 - Total SICOM Reg. 12): ", formatar_moeda(total_sof_12 - total_sicom_12), "</strong><br><br>",
          
          "<div class='resumo-title'>RESUMO DA CONFERÊNCIA REGISTRO 20</div>",
          "Total SOF Registro 20: ", formatar_moeda(total_sof_20), "<br>",
          "Total SICOM Registro 20: ", formatar_moeda(total_sicom_20), "<br>",
          "<strong>Diferença (SOF Reg. 20 - Total SICOM Reg. 20): ", formatar_moeda(total_sof_20 - total_sicom_20), "</strong>",
          "</div>"
        ))
        
        incProgress(1, detail = "Concluído com Sucesso!")
      })
      
    }, error = function(e) {
      showNotification(paste("Erro no processamento:", e$message), type = "error", duration = NULL)
    })
  })
  
  # ==============================================================================
  # RENDERIZAÇÃO DE TABELAS INTERATIVAS (DT)
  # ==============================================================================
  output$resumo_geral <- renderUI({
    req(resultados$resumo)
    resultados$resumo
  })
  
  opcoes_dt <- list(
    dom = 'Bfrtip',
    buttons = c('copy', 'excel'),
    pageLength = 10,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
  )
  
  output$tabela_reg10 <- renderDT({
    req(resultados$div10)
    datatable(resultados$div10, extensions = 'Buttons', options = opcoes_dt, rownames = FALSE) %>%
      formatCurrency(c('valor_SOF', 'valor_SICOM'), currency = "R$ ", mark = ".", dec.mark = ",")
  })
  
  output$tabela_reg11 <- renderDT({
    req(resultados$div11)
    datatable(resultados$div11, extensions = 'Buttons', options = opcoes_dt, rownames = FALSE) %>%
      formatCurrency(c('valor_SOF', 'valor_SICOM'), currency = "R$ ", mark = ".", dec.mark = ",")
  })
  
  output$tabela_reg12 <- renderDT({
    req(resultados$div12)
    datatable(resultados$div12, extensions = 'Buttons', options = opcoes_dt, rownames = FALSE) %>%
      formatCurrency(c('valor_SOF', 'valor_SICOM'), currency = "R$ ", mark = ".", dec.mark = ",")
  })
  
  output$tabela_reg20 <- renderDT({
    req(resultados$div20)
    datatable(resultados$div20, extensions = 'Buttons', options = opcoes_dt, rownames = FALSE) %>%
      formatCurrency(c('valor_SOF', 'valor_SICOM'), currency = "R$ ", mark = ".", dec.mark = ",")
  })
}

# ==============================================================================
# INICIALIZAÇÃO DA APLICAÇÃO SHINY
# ==============================================================================
shinyApp(ui, server)