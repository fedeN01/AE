library(shiny)

ui <- fluidPage(
  #Un poco de introduccion
  titlePanel("Simulaciones e histogramas"),
  hr(),
  sidebarLayout(
    #######SIDEBARPANEL lo que va al costado
    sidebarPanel(
      numericInput("seed","Semilla",123),
      strong("Restricciones de gráfico:"),
      sliderInput("laterales", "Bordes laterales", min = -30, max = 30, value = c(-3, 3)),
      sliderInput("y_max","Borde superior", min =0, max = 10,value=0.5,step=0.1),
      sliderInput("replic","Número de replicaciones:",
                             min = 1,
                             max = 200,
                             value = 30,
                            step=1),
      selectInput("distrib","Distribuciones de X:",c("Bi(n,p)"="binom","Geom(p)"="geom","Pois(\U03BB)"="pois","N(\U03BC,\U03C3\U00B2)"="norm","Exp(\U03BB)"="expon","Unif(a,b)"="unif"),selected="norm"),
      strong("Parámetros:"),      
      conditionalPanel("input.distrib=='binom'",
                                  numericInput("n_binom","n",1,min=1), numericInput("p_binom","p",0.5,min=0,max=1,step=0.01)),
      conditionalPanel("input.distrib=='geom'",
                       numericInput("p_geom","p",0.5,min=0,max=1,step=0.01)),
      conditionalPanel("input.distrib=='pois'",
                       numericInput("l_pois","\U03BB",1,min=0)),
      conditionalPanel("input.distrib=='norm'",
                       numericInput("mu","\U03BC",0), numericInput("sigma2","\U03C3\U00B2",1,min=0)),
      conditionalPanel("input.distrib=='expon'",
                       numericInput("l_exp","\U03BB",1,min=0)),
      conditionalPanel("input.distrib=='unif'",
                       numericInput("a_unif","a",0), numericInput("b_unif","b",1))
    ),
    ####### MAIN PANEL lo principal que aparece a la derecha
    mainPanel(plotOutput("histog"),
              verbatimTextOutput("data")
    )
  )
)


server <- function(input, output) {
    output$histog <- renderPlot({
      x_min=input$laterales[1]
      x_max=input$laterales[2]
      y_max=input$y_max
      N_rep=input$replic
      set.seed(input$seed)
      
      if(input$distrib=="binom"){
        n=input$n_binom
        p=input$p_binom
        xs=rbinom(N_rep,n,p)
        hist(xs, xlim=c(x_min,x_max),ylim=c(0,y_max),probability = TRUE,
           breaks = seq(min(xs)-0.5, max(xs)+0.5,1),
           xlab="", main ="Histograma de simulaciones de X")
        grilla <- seq(max(x_min,0),min(x_max,n))
        points(grilla, dbinom(grilla,n,p) , pch =16 , col=" red")
      }
      if(input$distrib=="geom"){
        p=input$p_geom
        xs <- rgeom(N_rep ,p)+1
        hist(xs, xlim=c(x_min,x_max),ylim=c(0,y_max),probability = TRUE,
             breaks = seq(min(xs)-0.5, max(xs)+0.5,1),
             xlab="", main ="Histograma de simulaciones de X")
        grilla <- seq(max(x_min,1),x_max)
        points ( grilla , dgeom(grilla-1 ,p) , pch =16 , col=" red")
      }
      if(input$distrib=="pois"){
        l=input$l_pois
        xs <- rpois(N_rep ,l)
        hist(xs, xlim=c(x_min,x_max),ylim=c(0,y_max),probability = TRUE,
             breaks = seq(min(xs)-0.5, max(xs)+0.5,1),
             xlab="", main ="Histograma de simulaciones de X")
        grilla <- seq(max(x_min,0),x_max)
        points ( grilla , dpois ( grilla ,l) , pch =16 , col=" red")
      }
      if(input$distrib=="norm"){
        esp=input$mu
        de=sqrt(input$sigma2)
        xs<-rnorm(N_rep,esp,de)
        hist(xs, xlim=c(x_min,x_max),ylim=c(0,y_max), probability = TRUE ,
             breaks = seq(floor(min(xs)), ceiling(max(xs)),0.2),
             xlab="", main =" Histograma de simulaciones de X ")
        curve(dnorm(x, esp,de), add= TRUE , pch =16 , col="red", lwd =2)
        points(xs,rep(0,N_rep))
      }
      if(input$distrib=="expon"){
        l=input$l_exp
        xs <- rexp(N_rep ,l)
        hist(xs, xlim=c(x_min,x_max),ylim=c(0,y_max), probability = TRUE ,
             breaks = seq(floor(min(xs)), ceiling(max(xs)),0.2),
             xlab="", main ="Histograma de simulaciones de X")
        curve( dexp(x,l), from=0, add= TRUE , pch =16 , col="red", lwd =2)
        points(xs,rep(0,N_rep))
      }
      if(input$distrib=="unif"){
        a=input$a_unif
        b=input$b_unif
        xs <- runif(N_rep ,a,b)
        hist(xs, xlim=c(x_min,x_max),ylim=c(0,y_max), probability = TRUE ,
             breaks = seq(floor(min(xs)), ceiling(max(xs)),0.2),
             xlab="", main ="Histograma de simulaciones de X")
        curve( dunif(x, a, b), from=a, to=b, add= TRUE , pch =16 , col="red", lwd =2)
        points(xs,rep(0,N_rep))
      }
    })
  
    output$data <- renderPrint({
      N_rep=input$replic
      set.seed(input$seed)
      
      if(input$distrib=="binom"){
        n=input$n_binom
        p=input$p_binom
        xs=rbinom(N_rep,n,p)
      }
      if(input$distrib=="geom"){
        p=input$p_geom
        xs <- rgeom(N_rep ,p)+1
      }
      if(input$distrib=="pois"){
        l=input$l_pois
        xs <- rpois(N_rep ,l)
      }
      if(input$distrib=="norm"){
        esp=input$mu
        de=sqrt(input$sigma2)
        xs<-rnorm(N_rep,esp,de)
      }
      if(input$distrib=="expon"){
        l=input$l_exp
        xs <- rexp(N_rep ,l)
      }
      if(input$distrib=="unif"){
        a=input$a_unif
        b=input$b_unif
        xs <- runif(N_rep ,a,b)
      }
      return(cat("Datos:",round(xs,2)))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


