# ==============================================================================
# 1. PACOTES E DEPENDÊNCIAS
# ==============================================================================

library(shiny)
library(readxl)
library(tidyverse)
library(DT)
library(stringi)


# ==============================================================================
# 2. CABEÇALHO DO ARQUIVO SICOM
# ==============================================================================

CABECALHO_SICOM <- c(
  "tipoRegistro",
  "tipoResponsavel",
  "cpf",
  "crcContador",
  "ufCrcContador",
  "cargoOrdDespDeleg",
  "dtInicio",
  "dtFinal",
  "email",
  "numeroTelefone"
)


# ==============================================================================
# 3. FUNÇÕES AUXILIARES
# ==============================================================================


# ------------------------------------------------------------------------------
# FUNÇÃO GERAL DE LIMPEZA DE TEXTO
#
# Utilizada, por exemplo, na comparação de e-mails.
#
# Regras:
# - converte para texto
# - remove cedilha
# - remove acentos
# - converte para minúsculas
# - remove espaços nas extremidades
# - transforma vários espaços seguidos em apenas um
# ------------------------------------------------------------------------------

limpar_texto <- function(texto) {
  
  if (is.null(texto)) {
    return("")
  }
  
  texto <- as.character(texto)
  
  texto[is.na(texto)] <- ""
  
  # Trata cedilha
  texto <- chartr(
    "Çç",
    "Cc",
    texto
  )
  
  # Remove acentos
  texto <- stringi::stri_trans_general(
    texto,
    "Latin-ASCII"
  )
  
  # Minúsculas
  texto <- tolower(texto)
  
  # Remove espaços no início e no final
  texto <- trimws(texto)
  
  # Substitui vários espaços por apenas um
  texto <- gsub(
    "\\s+",
    " ",
    texto
  )
  
  return(texto)
}


# ------------------------------------------------------------------------------
# FUNÇÃO ESPECÍFICA PARA LIMPEZA DOS CARGOS
#
# Na comparação dos cargos serão desconsiderados:
#
# - acentos
# - cedilha
# - diferenças entre maiúsculas/minúsculas
# - hífens
# - vírgulas
# - espaços duplicados
#
# Exemplos:
#
# "PROCURADOR-GERAL DO MUNICÍPIO"
#               =
# "PROCURADOR GERAL DO MUNICIPIO"
#
#
# "SUBSECRETÁRIO DE PLANEJAMENTO, GESTÃO E FINANÇAS"
#               =
# "SUBSECRETARIO DE PLANEJAMENTO GESTAO E FINANCAS"
# ------------------------------------------------------------------------------

limpar_cargo <- function(texto) {
  
  if (is.null(texto)) {
    return("")
  }
  
  # Sempre trabalha como texto
  texto <- as.character(texto)
  
  # NA passa a ser vazio
  texto[is.na(texto)] <- ""
  
  # --------------------------------------------------------------------------
  # CEDILHA
  # --------------------------------------------------------------------------
  
  texto <- chartr(
    "Çç",
    "Cc",
    texto
  )
  
  
  # --------------------------------------------------------------------------
  # ACENTOS
  # --------------------------------------------------------------------------
  
  texto <- stringi::stri_trans_general(
    texto,
    "Latin-ASCII"
  )
  
  
  # --------------------------------------------------------------------------
  # MINÚSCULAS
  # --------------------------------------------------------------------------
  
  texto <- tolower(texto)
  
  
  # --------------------------------------------------------------------------
  # HÍFENS
  # --------------------------------------------------------------------------
  
  texto <- gsub(
    "-",
    " ",
    texto,
    fixed = TRUE
  )
  
  
  # --------------------------------------------------------------------------
  # VÍRGULAS
  # --------------------------------------------------------------------------
  
  texto <- gsub(
    ",",
    " ",
    texto,
    fixed = TRUE
  )
  
  
  # --------------------------------------------------------------------------
  # ESPAÇOS
  # --------------------------------------------------------------------------
  
  texto <- trimws(texto)
  
  texto <- gsub(
    "\\s+",
    " ",
    texto
  )
  
  
  return(texto)
}


# ------------------------------------------------------------------------------
# FUNÇÃO PARA NORMALIZAR CPF COMO NUMÉRICO
#
# IMPORTANTE:
# Tanto o CPF do SOF quanto o CPF do SICOM passam por esta mesma função.
#
# Exemplos:
#
# SOF:
# "01234567890"
#
# SICOM:
# 1234567890
#
# Após a normalização:
#
# 1234567890
#
#
# Também trata:
#
# "012.345.678-90" -> 1234567890
# "01234567890"    -> 1234567890
# 1234567890       -> 1234567890
#
# Desta forma, zeros à esquerda não interferem no cruzamento.
# ------------------------------------------------------------------------------

normalizar_cpf_numerico <- function(x) {
  
  if (is.null(x)) {
    return(NA_real_)
  }
  
  
  # --------------------------------------------------------------------------
  # PRIMEIRO CONVERTE PARA TEXTO
  #
  # Isso evita problemas quando uma das bases já traz o CPF como número
  # e a outra como texto.
  # --------------------------------------------------------------------------
  
  x <- as.character(x)
  
  
  # --------------------------------------------------------------------------
  # REMOVE ESPAÇOS
  # --------------------------------------------------------------------------
  
  x <- trimws(x)
  
  
  # --------------------------------------------------------------------------
  # REMOVE ".0" CASO TENHA SIDO IMPORTADO ASSIM
  #
  # Exemplo:
  #
  # "1234567890.0"
  #
  # vira:
  #
  # "1234567890"
  # --------------------------------------------------------------------------
  
  x <- gsub(
    "\\.0$",
    "",
    x
  )
  
  
  # --------------------------------------------------------------------------
  # MANTÉM SOMENTE ALGARISMOS
  #
  # Exemplo:
  #
  # "012.345.678-90"
  #
  # vira:
  #
  # "01234567890"
  # --------------------------------------------------------------------------
  
  x <- gsub(
    "[^0-9]",
    "",
    x
  )
  
  
  # --------------------------------------------------------------------------
  # CAMPOS VAZIOS PASSAM A NA
  # --------------------------------------------------------------------------
  
  x[x == ""] <- NA_character_
  
  
  # --------------------------------------------------------------------------
  # CONVERSÃO FINAL PARA NUMÉRICO
  #
  # Neste momento os zeros à esquerda são naturalmente eliminados.
  #
  # "01234567890"
  #
  # vira:
  #
  # 1234567890
  # --------------------------------------------------------------------------
  
  suppressWarnings(
    as.numeric(x)
  )
}


# ==============================================================================
# 4. INTERFACE DO USUÁRIO
#
# LAYOUT MANTIDO
# ==============================================================================

ui <- fluidPage(
  
  titlePanel(
    "Conferência de Dados Contábeis: SOF x SICOM - ÓRGÃO TIPOS 1 E 4"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput(
        "file_sof",
        "Selecione o Relatório SOF (.xlsx)",
        accept = c(
          ".xlsx",
          ".xls"
        )
      ),
      
      fileInput(
        "file_sicom",
        "Selecione o Arquivo SICOM (.csv)",
        accept = c(
          ".csv",
          ".txt"
        )
      ),
      
      actionButton(
        "btn_processar",
        "Executar Conferência",
        class = "btn-primary",
        style = "width: 100%; margin-top: 10px;"
      ),
      
      hr(),
      
      helpText(
        paste0(
          "Faça o upload de ambos os arquivos obrigatórios ",
          "e clique no botão acima para rodar o cruzamento."
        )
      )
    ),
    
    
    mainPanel(
      
      # ------------------------------------------------------------------------
      # MENSAGENS
      # ------------------------------------------------------------------------
      
      uiOutput(
        "ui_mensagem"
      ),
      
      
      # ------------------------------------------------------------------------
      # TABELAS
      # ------------------------------------------------------------------------
      tabsetPanel(
        
        tabPanel(
          "Resumo da Consolidação",
          DTOutput(
            "tbl_resumo_geral"
          )
        ),
        
        tabPanel(
          "Divergências de CPFs",
          DTOutput(
            "tbl_cpf"
          )
        ),
        
        tabPanel(
          "Divergências de Cargos",
          DTOutput(
            "tbl_cargo"
          )
        ),
        
        tabPanel(
          "Divergências de E-mails",
          DTOutput(
            "tbl_email"
          )
        )
      ))
  )
)


# ==============================================================================
# 5. LÓGICA DO SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  
  
  # ============================================================================
  # PROCESSAMENTO PRINCIPAL
  # ============================================================================
  
  resultados <- eventReactive(
    input$btn_processar,
    {
      
      
      # ------------------------------------------------------------------------
      # VALIDAÇÃO DOS ARQUIVOS
      # ------------------------------------------------------------------------
      
      if (
        is.null(input$file_sof) ||
        is.null(input$file_sicom)
      ) {
        
        stop(
          paste0(
            "É obrigatório selecionar ambos os arquivos ",
            "(SOF e SICOM) antes de prosseguir."
          )
        )
      }
      
      
      # ------------------------------------------------------------------------
      # TRATAMENTO DE ERROS
      # ------------------------------------------------------------------------
      
      tryCatch({
        
        
        # ======================================================================
        # 1. CARREGAMENTO DO SOF
        # ======================================================================
        
        df_sof_raw <- read_excel(
          input$file_sof$datapath,
          col_types = "text"
        )
        
        
        # ----------------------------------------------------------------------
        # PADRONIZAÇÃO DO CABEÇALHO
        # ----------------------------------------------------------------------
        
        colnames(df_sof_raw) <- trimws(
          tolower(
            colnames(df_sof_raw)
          )
        )
        
        
        # ======================================================================
        # FILTRO DE VIGÊNCIA NO SOF
        #
        # Desconsidera registros cujo campo:
        #
        # dt_fim_vigencia_atualizacao
        #
        # esteja preenchido.
        # ======================================================================
        
        if (
          "dt_fim_vigencia" %in%
          colnames(df_sof_raw)
        ) {
          
          df_sof <- df_sof_raw %>%
            
            filter(
              
              is.na(
                dt_fim_vigencia
              ) |
                
                trimws(
                  as.character(
                    dt_fim_vigencia
                  )
                ) == ""
            )
          
        } else {
          
          df_sof <- df_sof_raw
        }
        
        
        # ======================================================================
        # 2. CARREGAMENTO DO SICOM
        # ======================================================================
        
        # ----------------------------------------------------------------------
        # Primeiro tenta ponto e vírgula
        # ----------------------------------------------------------------------
        
        df_sicom_raw <- read.csv2(
          
          input$file_sicom$datapath,
          
          header = FALSE,
          
          stringsAsFactors = FALSE
        )
        
        
        # ----------------------------------------------------------------------
        # Se encontrou apenas uma coluna, tenta vírgula
        # ----------------------------------------------------------------------
        
        if (
          ncol(df_sicom_raw) == 1
        ) {
          
          df_sicom_raw <- read.csv(
            
            input$file_sicom$datapath,
            
            header = FALSE,
            
            stringsAsFactors = FALSE
          )
        }
        
        
        # ======================================================================
        # FILTRO DOS REGISTROS TIPO RESPONSÁVEL 1 E 4 NO SICOM
        # ======================================================================
        
        df_sicom <- df_sicom_raw %>%
          
          filter(
            V2 %in% c(
              1,
              "1",
              4,
              "4"
            )
          )
        
        
        # ======================================================================
        # CABEÇALHO SICOM
        # ======================================================================
        
        colnames(
          df_sicom
        )[
          1:min(
            length(CABECALHO_SICOM),
            ncol(df_sicom)
          )
        ] <-
          
          CABECALHO_SICOM[
            1:min(
              length(CABECALHO_SICOM),
              ncol(df_sicom)
            )
          ]
        
        
        # ======================================================================
        # 3. SANITIZAÇÃO DO CPF
        #
        # ALTERAÇÃO:
        #
        # OS DOIS CAMPOS SÃO TRANSFORMADOS EM NUMÉRICO ANTES
        # DE QUALQUER BATIMENTO.
        #
        # Exemplo:
        #
        # SOF:
        # cpf_ordenador = "01234567890"
        #
        # SICOM:
        # cpf = 1234567890
        #
        # Resultado dos dois:
        #
        # cpf_clean = 1234567890
        #
        # Portanto os registros serão considerados iguais.
        # ======================================================================
        
        df_sof <- df_sof %>%
          
          mutate(
            
            cpf_clean =
              normalizar_cpf_numerico(
                cpf_ordenador
              )
          )
        
        
        df_sicom <- df_sicom %>%
          
          mutate(
            
            cpf_clean =
              normalizar_cpf_numerico(
                cpf
              )
          )
        
        
        # ======================================================================
        # 4. CONFERÊNCIA DE CPFs
        #
        # A divergência de existência continua sendo apurada pelo CPF.
        # Quando o CPF existir no SICOM, o(s) tipo(s) 1/4 encontrados serão
        # informados na coluna Tipo_Responsavel.
        #
        # Para CPF existente apenas no SOF não é possível inferir o tipo,
        # pois essa informação pertence ao arquivo SICOM.
        # ======================================================================
        
        cpfs_sof <- unique(
          na.omit(
            df_sof$cpf_clean
          )
        )
        
        cpfs_sicom <- unique(
          na.omit(
            df_sicom$cpf_clean
          )
        )
        
        tipos_por_cpf <- df_sicom %>%
          filter(
            !is.na(cpf_clean)
          ) %>%
          mutate(
            tipoResponsavel = as.character(tipoResponsavel)
          ) %>%
          distinct(
            cpf_clean,
            tipoResponsavel
          ) %>%
          group_by(
            cpf_clean
          ) %>%
          summarise(
            Tipo_Responsavel = paste(
              sort(unique(tipoResponsavel)),
              collapse = " / "
            ),
            .groups = "drop"
          )
        
        todos_cpfs <- unique(
          c(
            cpfs_sof,
            cpfs_sicom
          )
        )
        
        div_cpf <- data.frame(
          cpf = todos_cpfs
        ) %>%
          mutate(
            No_SOF = cpf %in% cpfs_sof,
            No_SICOM = cpf %in% cpfs_sicom,
            Status = case_when(
              No_SOF & !No_SICOM ~ "CPF presente apenas no SOF",
              !No_SOF & No_SICOM ~ "CPF presente apenas no SICOM",
              TRUE ~ "OK"
            )
          ) %>%
          filter(
            Status != "OK"
          ) %>%
          left_join(
            tipos_por_cpf,
            by = c("cpf" = "cpf_clean")
          ) %>%
          mutate(
            Tipo_Responsavel = ifelse(
              is.na(Tipo_Responsavel) |
                trimws(Tipo_Responsavel) == "",
              "Não identificado no SICOM",
              Tipo_Responsavel
            ),
            Divergencia = "CPFs divergentes"
          ) %>%
          select(
            Tipo_Responsavel,
            cpf,
            No_SOF,
            No_SICOM,
            Status,
            Divergencia
          )
        
        
        # ======================================================================
        # 5. CRUZAMENTO PELO CPF
        #
        # IMPORTANTE:
        #
        # cpf_clean é NUMÉRICO nas duas bases.
        # ======================================================================
        
        df_merged <- inner_join(
          
          df_sof,
          
          df_sicom,
          
          by = "cpf_clean",
          
          suffix = c(
            "_sof",
            "_sicom"
          )
        )
        
        
        # ======================================================================
        # 6. CONFERÊNCIA DOS CARGOS
        #
        # REGRAS:
        #
        # - tipos tipoResponsavel 1 e 4
        # - ignora acentos
        # - ignora cedilha
        # - ignora maiúsculas/minúsculas
        # - ignora hífen
        # - ignora vírgula
        # - ignora espaços duplicados
        #
        # Mantida a regra existente de truncamento do cargo SOF
        # pelo tamanho do cargo SICOM.
        # ======================================================================
        
        div_cargo <- df_merged %>%
          
          filter(
            tipoResponsavel %in%
              c(
                1,
                "1",
                4,
                "4"
              )
          ) %>%
          
          mutate(
            
            # --------------------------------------------------------------
            # LIMPEZA DO CARGO DO SOF
            # --------------------------------------------------------------
            
            cargo_sof_clean =
              limpar_cargo(
                cargo_ord_despesa
              ),
            
            
            # --------------------------------------------------------------
            # LIMPEZA DO CARGO DO SICOM
            # --------------------------------------------------------------
            
            cargo_sicom_clean =
              limpar_cargo(
                cargoOrdDespDeleg
              ),
            
            
            # --------------------------------------------------------------
            # TAMANHO DO TEXTO DO SICOM JÁ LIMPO
            # --------------------------------------------------------------
            
            tam_sicom =
              nchar(
                cargo_sicom_clean
              ),
            
            
            # --------------------------------------------------------------
            # TRUNCAMENTO
            #
            # Mantém a regra que já existia:
            #
            # compara somente a quantidade de caracteres disponível
            # no SICOM.
            # --------------------------------------------------------------
            
            cargo_sof_trunc =
              ifelse(
                
                tam_sicom > 0,
                
                substr(
                  cargo_sof_clean,
                  1,
                  tam_sicom
                ),
                
                cargo_sof_clean
              )
          ) %>%
          
          
          # --------------------------------------------------------------------
        # APENAS DIVERGÊNCIAS REAIS
        # --------------------------------------------------------------------
        
        filter(
          
          cargo_sof_trunc !=
            cargo_sicom_clean
          
        ) %>%
          
          
          select(
            Tipo_Responsavel = tipoResponsavel,
            CPF = cpf_clean,
            Cargo_SOF = cargo_ord_despesa,
            Cargo_SICOM = cargoOrdDespDeleg
          ) %>%
          
          
          mutate(
            
            Mensagem =
              "Cargos divergentes. Verificar"
          )
        
        
        # ======================================================================
        # 7. CONFERÊNCIA DOS E-MAILS
        # ======================================================================
        
        div_email <- df_merged %>%
          
          filter(
            tipoResponsavel %in% c(1, "1", 4, "4")
          ) %>%
          
          mutate(
            
            email_sof_clean =
              limpar_texto(
                e_mail
              ),
            
            email_sicom_clean =
              limpar_texto(
                email
              )
          ) %>%
          
          
          filter(
            
            email_sof_clean !=
              email_sicom_clean
          ) %>%
          
          
          select(
            Tipo_Responsavel = tipoResponsavel,
            CPF = cpf_clean,
            Email_SOF = e_mail,
            Email_SICOM = email
          ) %>%
          
          
          mutate(
            
            Mensagem =
              "E-mails divergente. Verificar"
          )
        
        
        # ======================================================================
        # 7.1 FILTRO DAS DIVERGÊNCIAS QUE SERÃO EXIBIDAS
        #
        # REGRA:
        # - divergências do tipo 1 NÃO aparecem nos quadros;
        # - somente divergências do tipo 4 ficam visíveis.
        #
        # O processamento dos tipos 1 e 4 pode continuar ocorrendo internamente.
        # ======================================================================
        
        filtrar_somente_tipo4 <- function(df) {
          
          if (
            !"Tipo_Responsavel" %in% colnames(df) ||
            nrow(df) == 0
          ) {
            return(df)
          }
          
          df %>%
            filter(
              grepl(
                "(^| / )4($| / )",
                as.character(Tipo_Responsavel)
              )
            )
        }
        
        
        div_cpf_exibir <- filtrar_somente_tipo4(
          div_cpf
        )
        
        div_cargo_exibir <- filtrar_somente_tipo4(
          div_cargo
        )
        
        div_email_exibir <- filtrar_somente_tipo4(
          div_email
        )
        
        
        # ======================================================================
        # 8. CONSOLIDAÇÃO
        #
        # O quadro-resumo apresenta SOMENTE as divergências do
        # tipoResponsavel = 4.
        #
        # As abas detalhadas continuam mostrando os tipos 1 e 4.
        # ======================================================================
        
        contar_tipo <- function(df, tipo) {
          
          if (
            !"Tipo_Responsavel" %in% colnames(df) ||
            nrow(df) == 0
          ) {
            return(0L)
          }
          
          sum(
            grepl(
              paste0("(^| / )", tipo, "($| / )"),
              as.character(df$Tipo_Responsavel)
            ),
            na.rm = TRUE
          )
        }
        
        resumo_geral <- data.frame(
          
          Tipo_de_Conferencia =
            c(
              "Divergência de Existência de CPF",
              "Divergência de Cargo",
              "Divergência de E-mail"
            ),
          
          Quantidade_Divergencias =
            c(
              nrow(div_cpf_exibir),
              nrow(div_cargo_exibir),
              nrow(div_email_exibir)
            )
        )
        
        
        # ======================================================================
        # RESULTADO
        # ======================================================================
        
        return(
          
          list(
            
            erro =
              FALSE,
            
            div_cpf =
              div_cpf_exibir,
            
            div_cargo =
              div_cargo_exibir,
            
            div_email =
              div_email_exibir,
            
            resumo_geral =
              resumo_geral
          )
        )
        
        
      }, error = function(e) {
        
        
        return(
          
          list(
            
            erro =
              TRUE,
            
            mensagem =
              paste(
                "Erro ao processar os arquivos:",
                e$message
              )
          )
        )
      })
    }
  )
  
  
  # ============================================================================
  # 9. MENSAGEM DE STATUS
  # ============================================================================
  
  output$ui_mensagem <- renderUI({
    
    res <- resultados()
    
    
    if (
      res$erro
    ) {
      
      div(
        
        class =
          "alert alert-danger",
        
        role =
          "alert",
        
        res$mensagem
      )
      
    } else {
      
      div(
        
        class =
          "alert alert-success",
        
        role =
          "alert",
        
        "Conferência executada com sucesso!"
      )
    }
  })
  
  
  # ============================================================================
  # 10. FUNÇÃO PARA AS TABELAS
  # ============================================================================
  
  render_dt_custom <- function(df) {
    
    datatable(
      
      df,
      
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
            10,
          
          language =
            list(
              
              url =
                "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
            )
        ),
      
      rownames =
        FALSE
    )
  }
  
  
  # ============================================================================
  # 11. TABELA CPFs
  # ============================================================================
  
  output$tbl_cpf <- renderDT({
    
    req(
      !resultados()$erro
    )
    
    render_dt_custom(
      resultados()$div_cpf
    )
  })
  
  
  # ============================================================================
  # 12. TABELA CARGOS
  # ============================================================================
  
  output$tbl_cargo <- renderDT({
    
    req(
      !resultados()$erro
    )
    
    render_dt_custom(
      resultados()$div_cargo
    )
  })
  
  
  # ============================================================================
  # 13. TABELA E-MAILS
  # ============================================================================
  
  output$tbl_email <- renderDT({
    
    req(
      !resultados()$erro
    )
    
    render_dt_custom(
      resultados()$div_email
    )
  })
  
  
  # ============================================================================
  # 14. TABELA RESUMO
  # ============================================================================
  
  output$tbl_resumo_geral <- renderDT({
    
    req(
      !resultados()$erro
    )
    
    render_dt_custom(
      resultados()$resumo_geral
    )
  })
}


# ==============================================================================
# 15. INICIALIZAÇÃO DA APLICAÇÃO
# ==============================================================================

shinyApp(
  ui = ui,
  server = server
)