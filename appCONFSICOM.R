# =========================================
# APP MESTRE - CONFERÊNCIA SICOM
# =========================================

# =========================================
# PACOTES
# =========================================
packages <- c("shiny", "bslib", "shinyWidgets", "DT")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

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

        .disabled-card {
          opacity: 0.4;
          cursor: not-allowed;
          background-color: #f9f9f9;
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
  
  conteudo_app <- reactiveValues(
    ui = NULL,
    server = NULL
  )
  
  # =========================================
  # MENU PRINCIPAL
  # =========================================
  output$ui_menu_dinamico <- renderUI({
    
    tagList(
      
      fluidRow(
        
        column(
          4,
          actionLink(
            inputId = "btn_ntf",
            label = bslib::card(
              class = "card-menu",
              tags$span(
                class = "card-title-custom",
                "NTF"
              )
            )
          )
        ),
        
        column(
          4,
          actionLink(
            inputId = "btn_anl",
            label = bslib::card(
              class = "card-menu",
              tags$span(
                class = "card-title-custom",
                "ANL"
              )
            )
          )
        ),
        
        column(
          4,
          actionLink(
            inputId = "btn_lqd",
            label = bslib::card(
              class = "card-menu",
              tags$span(
                class = "card-title-custom",
                "LQD"
              )
            )
          )
        )
      ),
      
      br(),
      
      fluidRow(
        
        column(
          4,
          actionLink(
            inputId = "btn_emp",
            label = bslib::card(
              class = "card-menu",
              tags$span(
                class = "card-title-custom",
                "EMP"
              )
            )
          )
        ),
        
        column(
          4,
          actionLink(
            inputId = "btn_alq",
            label = bslib::card(
              class = "card-menu",
              tags$span(
                class = "card-title-custom",
                "ALQ"
              )
            )
          )
        ),
        
        column(
          4,
          actionLink(
            inputId = "btn_orgao",
            label = bslib::card(
              class = "card-menu",
              tags$span(
                class = "card-title-custom",
                "ÓRGÃO"
              )
            )
          )
        )
      )
    )
  })
  
  # =========================================
  # FUNÇÃO PARA CARREGAR OS SUB-APPS
  # =========================================
  carregar_sub_app <- function(arquivo) {
    
    temp_env <- new.env(parent = globalenv())
    
    tryCatch({
      
      if (!file.exists(arquivo)) {
        stop(
          paste0(
            "Arquivo não encontrado: ",
            arquivo
          )
        )
      }
      
      source(
        arquivo,
        local = temp_env
      )
      
      if (!exists("ui", envir = temp_env)) {
        stop(
          paste0(
            "O arquivo ",
            arquivo,
            " não possui o objeto 'ui'."
          )
        )
      }
      
      if (!exists("server", envir = temp_env)) {
        stop(
          paste0(
            "O arquivo ",
            arquivo,
            " não possui o objeto 'server'."
          )
        )
      }
      
      conteudo_app$ui <- temp_env$ui
      conteudo_app$server <- temp_env$server
      
      updateNavbarPage(
        session = session,
        inputId = "main_nav",
        selected = "painel_exec"
      )
      
      conteudo_app$server(
        input,
        output,
        session
      )
      
    }, error = function(e) {
      
      # =========================================
      # NÃO EXIBE POP-UP DE ERRO
      # =========================================
      # O erro continua sendo registrado no
      # Console do R/RStudio para diagnóstico.
      
      message(
        paste(
          "ERRO NO ARQUIVO:",
          arquivo,
          "-",
          e$message
        )
      )
      
    })
  }
  
  # =========================================
  # BOTÕES DO MENU PRINCIPAL
  # =========================================
  
  observeEvent(input$btn_ntf, {
    carregar_sub_app("app.R")
  })
  
  observeEvent(input$btn_anl, {
    carregar_sub_app("appanl.R")
  })
  
  observeEvent(input$btn_lqd, {
    carregar_sub_app("appLQD.R")
  })
  
  observeEvent(input$btn_emp, {
    carregar_sub_app("appEMP.R")
  })
  
  observeEvent(input$btn_alq, {
    carregar_sub_app("appALQ.R")
  })
  
  observeEvent(input$btn_orgao, {
    carregar_sub_app("appORGAO.R")
  })
  
  # =========================================
  # BOTÃO VOLTAR
  # =========================================
  observeEvent(input$voltar, {
    
    conteudo_app$ui <- NULL
    conteudo_app$server <- NULL
    
    updateNavbarPage(
      session = session,
      inputId = "main_nav",
      selected = "Menu Principal"
    )
  })
  
  # =========================================
  # UI DO SUB-APP CARREGADO
  # =========================================
  output$ui_dinamica <- renderUI({
    
    req(conteudo_app$ui)
    
    conteudo_app$ui
  })
}

# =========================================
# EXECUTA APP
# =========================================
shinyApp(
  ui = ui,
  server = server
)