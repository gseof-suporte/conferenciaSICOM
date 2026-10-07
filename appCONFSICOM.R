# =========================================
# APP MESTRE - CONFERÊNCIA SICOM
# =========================================

# 1. PACOTES (Mestre + Todos os Sub-Apps)
packages_needed <- c(
  "shiny", "bslib", "shinyWidgets", "DT", 
  "readxl", "dplyr", "data.table", "shinythemes", 
  "stringr", "openxlsx", "tools", "readr", "tidyr", "stringi"
)

# Instala apenas os pacotes ausentes no computador do usuário
new_pkgs <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, dependencies = TRUE, repos = "https://cloud.r-project.org")
}

# Carrega todos os pacotes de uma só vez
invisible(lapply(packages_needed, library, character.only = TRUE))


# =========================================
# UI (INTERFACE DO USUÁRIO)
# =========================================
ui <- page_navbar(
  id = "main_nav",
  title = "SISTEMA DE CONFERÊNCIA SICOM",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50"
  ),
  
  nav_panel(
    title = "Menu Principal",
    value = "menu_principal",
    icon = icon("th-large"),
    
    tags$div(
      style = "max-width: 900px; margin: 60px auto; text-align: center;",
      
      tags$style(HTML("
        .card-menu {
          cursor: pointer;
          transition: transform 0.3s;
          height: 120px;
          display: flex;
          align-items: center;
          justify-content: center;
          border-radius: 15px;
        }

        .card-menu:hover {
          transform: translateY(-5px);
          box-shadow: 0 10px 20px rgba(0,0,0,0.1);
          border-color: #2c3e50;
        }

        .card-title-custom {
          font-weight: bold;
          font-size: 2.2rem;
          color: #2c3e50;
        }
      ")),
      
      uiOutput("ui_menu_dinamico")
    )
  ),
  
  nav_panel(
    title = "Execução",
    value = "painel_exec",
    
    actionLink(
      inputId = "voltar",
      label = "← Voltar ao Menu Principal",
      style = "font-weight: bold; color: #e74c3c;"
    ),
    
    hr(),
    
    uiOutput("ui_dinamica")
  )
)

# =========================================
# SERVER (LÓGICA DO SISTEMA)
# =========================================
server <- function(input, output, session) {
  
  app_atual <- reactiveVal(NULL)
  
  # Menu Principal
  output$ui_menu_dinamico <- renderUI({
    tagList(
      fluidRow(
        column(4, actionLink("btn_ntf", label = bslib::card(class = "card-menu", tags$span(class = "card-title-custom", "NTF")))),
        column(4, actionLink("btn_anl", label = bslib::card(class = "card-menu", tags$span(class = "card-title-custom", "ANL")))),
        column(4, actionLink("btn_lqd", label = bslib::card(class = "card-menu", tags$span(class = "card-title-custom", "LQD"))))
      ),
      br(),
      fluidRow(
        column(4, actionLink("btn_emp", label = bslib::card(class = "card-menu", tags$span(class = "card-title-custom", "EMP")))),
        column(4, actionLink("btn_alq", label = bslib::card(class = "card-menu", tags$span(class = "card-title-custom", "ALQ")))),
        column(4, actionLink("btn_orgao", label = bslib::card(class = "card-menu", tags$span(class = "card-title-custom", "ÓRGÃO"))))
      )
    )
  })
  
  # Função para abrir os sub-apps de forma nativa e integrada
  carregar_sub_app <- function(caminho_arquivo) {
    if (!file.exists(caminho_arquivo)) {
      showNotification(paste("Arquivo não encontrado:", caminho_arquivo), type = "error")
      return()
    }
    
    app_obj <- tryCatch({
      # Executa o arquivo app.R / appEMP.R / etc. diretamente
      source(caminho_arquivo, local = TRUE)$value
    }, error = function(e) {
      message("Erro ao carregar ", caminho_arquivo, ": ", e$message)
      showNotification(paste("Erro ao carregar o módulo:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(app_obj)) {
      app_atual(app_obj)
      updateNavbarPage(session, "main_nav", selected = "painel_exec")
    }
  }
  
  # Eventos dos Botões do Menu
  observeEvent(input$btn_ntf,   { carregar_sub_app("app.R") })
  observeEvent(input$btn_anl,   { carregar_sub_app("appanl.R") })
  observeEvent(input$btn_lqd,   { carregar_sub_app("appLQD.R") })
  observeEvent(input$btn_emp,   { carregar_sub_app("appEMP.R") })
  observeEvent(input$btn_alq,   { carregar_sub_app("appALQ.R") })
  observeEvent(input$btn_orgao, { carregar_sub_app("appORGAO.R") })
  
  # Botão Voltar
  observeEvent(input$voltar, {
    app_atual(NULL)
    updateNavbarPage(session, "main_nav", selected = "menu_principal")
  })
  
  # Renderização do Sub-App Selecionado
  output$ui_dinamica <- renderUI({
    req(app_atual())
    app_atual()
  })
}

# Executa a Aplicação
shinyApp(ui = ui, server = server)
