###############################################################################
# OLA 5 – Sidequest: interaktiv lånemuligheds-beregner (Shiny)
# Gruppe 11
#
# Sådan bruges den:
#   1. Kør først hele "opgave3_clm.R" (så m4, farver, baggrund() osv. findes)
#   2. Kør derefter denne fil – så åbner beregneren i et vindue
#
# Shiny er R's pakke til interaktive apps. En app består af to dele:
#   ui     = hvordan appen ser ud (skydeknapper, knapper, plot)
#   server = hvad der sker, når man ændrer noget (beregning + plot)
###############################################################################

# install.packages("shiny")   # kør kun én gang
library(shiny)

# Tjek at modellen fra opgave3_clm.R findes
if (!exists("m4")) stop("Kør først opgave3_clm.R, så modellen m4 findes")


# ---- UI: brugerfladen -------------------------------------------------------
ui <- fluidPage(
  # Brune overskrifter, så appen passer til resten af opgaven
  tags$style("h2, h4 { color: #3B2620; } .irs-bar, .irs-single { background: #8A5444 !important; border-color: #8A5444 !important; }"),
  
  titlePanel("Lånemuligheds-beregner"),
  
  sidebarLayout(
    sidebarPanel(
      # Antal ansatte skrives ind som et tal (5-1000).
      # Modellen bruger log(ansatte), men det klarer laaneberegner() selv.
      numericInput("ans", "Antal ansatte", value = 16, min = 5, max = 1000, step = 1),
      sliderInput("sol", "Soliditetsgrad (%)", min = 8, max = 88, value = 41),
      sliderInput("alder", "Virksomhedens alder i 2026 (år)", min = 16, max = 76, value = 27),
      radioButtons("sektor", "Sektor",
                   choices = c("Byggeri", "Industri", "Transport", "Øvrige"),
                   selected = "Industri", inline = TRUE)
    ),
    
    mainPanel(
      plotOutput("plot", height = "420px"),
      h4("Sandsynligheder"),
      tableOutput("tabel")
    )
  )
)


# ---- Server: beregning og plot ----------------------------------------------
server <- function(input, output, session) {
  
  # Antal ansatte holdes mellem 5 og 1000 (også hvis man skriver noget andet)
  ansatte <- reactive({
    a <- input$ans
    if (is.null(a) || is.na(a)) a <- 16
    min(max(round(a), 5), 1000)
  })
  
  # Plottet tegnes med laaneberegner() fra opgave3_clm.R.
  # Den opdateres automatisk, hver gang en skyder flyttes.
  output$plot <- renderPlot({
    laaneberegner(ansatte(), input$sol, input$alder, input$sektor)
  })
  
  # Tabel med sandsynlighederne i procent
  output$tabel <- renderTable({
    ny <- data.frame(soliditet   = input$sol,
                     log_ansatte = log(ansatte()),
                     alder       = input$alder,
                     sektor      = factor(input$sektor, levels = levels(model_data$sektor)))
    p <- predict(m4, newdata = ny, type = "prob")$fit[1, ]
    data.frame(Svar = niveauer,
               Sandsynlighed = paste0(format(round(100 * p, 1), decimal.mark = ","), " %"))
  })
}


# ---- Start appen ------------------------------------------------------------
shinyApp(ui = ui, server = server)
