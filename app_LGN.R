library(shiny)

ui <- fluidPage(
  #Un poco de introduccion
  titlePanel("Simulaciones del evento {|\U0058\U0304\U2099-E(X)|\U2265\U03B5}"),
  hr(),
  sidebarLayout(
    #######SIDEBARPANEL lo que va al costado
    sidebarPanel(
      numericInput("seed","Semilla",123),
      sliderInput("lat","Borde lateral del gráfico",min = 0,max = 10,step=0.1,value=0.5),
      numericInput("N_rep","Cantidad de medias muestrales simuladas",10),
      numericInput("n","n",30,min=1),
      numericInput("eps","\U03B5",0.1,min=0,step=0.01),
      selectInput("distrib","Distribuciones:",c("Bi(n,p)"="binom","Geom(p)"="geom","Pois(\U03BB)"="pois","N(\U03BC,\U03C3\U00B2)"="norm","Exp(\U03BB)"="expon","Unif(a,b)"="unif"),selected="pois"),
      strong("Parámetros:"),      
      conditionalPanel("input.distrib=='binom'",
                       numericInput("n_binom","n",1,min=1), numericInput("p_binom","p",value=0.5,min=0,max=1,step=0.01)),
      conditionalPanel("input.distrib=='geom'",
                       numericInput("p_geom","p",value=0.5,min=0,max=1,step=0.01)),
      conditionalPanel("input.distrib=='pois'",
                       numericInput("l_pois","\U03BB",value=2,min=0)),
      conditionalPanel("input.distrib=='norm'",
                       numericInput("mu","\U03BC",0), numericInput("sigma2","\U03C3\U00B2",value=1,min=0)),
      conditionalPanel("input.distrib=='expon'",
                       numericInput("l_exp","\U03BB",value=1,min=0)),
      conditionalPanel("input.distrib=='unif'",
                       numericInput("a_unif","a",0), numericInput("b_unif","b",1))
    ),
    ####### MAIN PANEL lo principal que aparece a la derecha
    mainPanel(verbatimTextOutput("data"),plotOutput("scatter",height = "700px")
    )
  )
)


server <- function(input, output) {
  output$scatter <- renderPlot({
    set.seed(input$seed)
    lat=input$lat
    N_rep=input$N_rep
    n=input$n
    eps=input$eps
    
    if(input$distrib=="binom"){
      n_binom=input$n_binom
      p=input$p_binom
      esp=n_binom*p
      xs=replicate(N_rep,mean(rbinom(n,n_binom,p)))
      }
    if(input$distrib=="geom"){
      p=input$p_geom
      esp=1/p
      xs <- replicate(N_rep,mean(rgeom(n,p)+1))
    }
    if(input$distrib=="pois"){
      l=input$l_pois
      esp=l
      xs <- replicate(N_rep,mean(rpois(n,l)))
    }
    if(input$distrib=="norm"){
      esp=input$mu
      de=sqrt(input$sigma2)
      xs<-replicate(N_rep,mean(rnorm(n,esp,de)))
    }
    if(input$distrib=="expon"){
      l=input$l_exp
      esp=1/l
      xs <- replicate(N_rep,mean(rexp(n,l)))
    }
    if(input$distrib=="unif"){
      a=input$a_unif
      b=input$b_unif
      esp=0.5*(a+b)
      xs <- replicate(N_rep,mean(runif(n,a,b)))
    }
    
    par(mfrow=c(2,1))
    
    plot(xs,jitter(rep(0,N_rep)),xlim=c(esp-lat,esp+lat),ylim=c(-1,1),
         yaxt="n",xlab="\U0058\U0304\U2099",ylab="",main="Ubicación de las simulaciones de \U0058\U0304\U2099")
    abline(v=c(esp-eps,esp,esp+eps),col="red",lty=c(2,1,2))
    text(x=c(esp-eps,esp,esp+eps), y=rep(-0.5,3), c("E(X)-\U03B5","E(X)","E(X)+\U03B5"),col="red")
    
    frec_abs <- sum(abs(xs-esp)>= eps)
    pie (c(frec_abs, N_rep- frec_abs) ,
         label =c(frec_abs, N_rep- frec_abs) ,
         main = "Proporción del evento {|\U0058\U0304\U2099-E(X)|\U2265\U03B5} " ,
         col=c("green","red"),radius=1)
    legend ("bottomright", c("Se cumple", "No se cumple") ,
          fill =c("green","red"))
  })
    
  output$data <- renderPrint({
    N_rep=input$N_rep
    n=input$n
    set.seed(input$seed)
    
    if(input$distrib=="binom"){
      n_binom=input$n_binom
      p=input$p_binom
      xs=replicate(N_rep,mean(rbinom(n,n_binom,p)))
    }
    if(input$distrib=="geom"){
      p=input$p_geom
      xs <- replicate(N_rep,mean(rgeom(n,p)+1))
    }
    if(input$distrib=="pois"){
      l=input$l_pois
      xs <- replicate(N_rep,mean(rpois(n,l)))
    }
    if(input$distrib=="norm"){
      esp=input$mu
      de=sqrt(input$sigma2)
      xs<-replicate(N_rep,mean(rnorm(n,esp,de)))
    }
    if(input$distrib=="expon"){
      l=input$l_exp
      xs <- replicate(N_rep,mean(rexp(n,l)))
    }
    if(input$distrib=="unif"){
      a=input$a_unif
      b=input$b_unif
      xs <- replicate(N_rep,mean(runif(n,a,b)))
    }
    return(cat("Datos \U0058\U0304\U2099:",round(xs,2)))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



