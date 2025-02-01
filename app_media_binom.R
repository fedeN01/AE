library(shiny)

ui <- fluidPage(
  #Un poco de introduccion
  titlePanel("Simulaciones de \U0058\U0304\U2099 con {X\U2081,...,X\U2099}~Ber(p)"),
  hr(),
  sidebarLayout(
    #######SIDEBARPANEL lo que va al costado
    sidebarPanel(
      numericInput("seed","Semilla",123),
      sliderInput("sup","Borde superior del gráfico",value=3,
                  min = 0,
                  max = 20,step=0.1),
      numericInput("N_rep","Cantidad de medias muestrales simuladas",1000,min=1),
      sliderInput("n","n",
                  min = 10,
                  max = 500,
                  value = 10),
      sliderInput("p_binom","p",
                  min = 0,
                  max = 1,
                  value = 0.5)
      ),
    ####### MAIN PANEL lo principal que aparece a la derecha
    mainPanel(plotOutput("histog"),
              verbatimTextOutput("data")
    )
  )
)


server <- function(input, output) {
  output$histog <- renderPlot({
    set.seed(input$seed)
    xs=replicate(input$N_rep,mean(rbinom(input$n,1,input$p_binom)))
      hist(xs, xlim=c(0,1),ylim=c(0,input$sup),probability = TRUE,
           breaks = seq(0,1,0.02),
           xlab="", main =bquote("Histograma de simulaciones de"~bar(X)[n]))
    })
  
  output$data <- renderPrint({
    set.seed(input$seed)
    xs=replicate(input$N_rep,mean(rbinom(input$n,1,input$p_binom)))
    return(cat("Datos:",round(xs,2)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


