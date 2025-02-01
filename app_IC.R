library(shiny)
library(plotrix)

ui <- fluidPage(
  #Un poco de introduccion
  titlePanel("Simulaciones de intervalos de confianza para E(X)"),
  hr(),
  sidebarLayout(
    #######SIDEBARPANEL lo que va al costado
    sidebarPanel(
      numericInput("seed","Semilla",123),
      sliderInput("level","Nivel (1-\U03B1)",value=0.95,min=0,max=1,step=0.01),
      numericInput("n","n",5,min=1),
      selectInput("ic","Intervalos:",c("Varianza conocida (normal)"="I1","Varianza desconocida (normal)"="I2","Asintótico"="I3")),      
      conditionalPanel("input.ic=='I1'",
                       withMathJax("$$\\left[\\overline{X}_n-z_{1-\\frac{\\alpha}{2}}\\sqrt{\\frac{Var(X)}{n}};\\overline{X}_n+z_{1-\\frac{\\alpha}{2}}\\sqrt{\\frac{Var(X)}{n}}\\right]$$"),
                       selectInput("dist_I1","Distribuciones:",c("N(\U03BC,\U03C3\U00B2)"="norm")),
                       conditionalPanel("input.dist_I1=='norm'",
                                        numericInput("mu","\U03BC",0), numericInput("sigma2","\U03C3\U00B2",1,min=0))
      ),
      conditionalPanel("input.ic=='I2'",
                      withMathJax("$$\\left[\\overline{X}_n-t_{n-1,1-\\frac{\\alpha}{2}}\\sqrt{\\frac{S_n^2}{n}};\\overline{X}_n+t_{n-1,1-\\frac{\\alpha}{2}}\\sqrt{\\frac{S_n^2}{n}}\\right]$$"),
                      selectInput("dist_I2","Distribuciones:",c("Bi(n,p)"="binom","Geom(p)"="geom","Pois(\U03BB)"="pois","N(\U03BC,\U03C3\U00B2)"="norm","Exp(\U03BB)"="expon","Unif(a,b)"="unif"),selected="norm"),
                      conditionalPanel("input.dist_I2=='binom'",
                        numericInput("n_binom","n",1,min=1), numericInput("p_binom","p",0.5,min=0,max=1,step=0.01)),
                      conditionalPanel("input.dist_I2=='geom'",
                        numericInput("p_geom","p",0.5,min=0,max=1,step=0.01)),
                      conditionalPanel("input.dist_I2=='pois'",
                        numericInput("l_pois","\U03BB",2,min=0)),
                      conditionalPanel("input.dist_I2=='norm'",
                        numericInput("mu","\U03BC",0), numericInput("sigma2","\U03C3\U00B2",1,min=0)),
                      conditionalPanel("input.dist_I2=='expon'",
                        numericInput("l_exp","\U03BB",1,min=0)),
                      conditionalPanel("input.dist_I2=='unif'",
                        numericInput("a_unif","a",0), numericInput("b_unif","b",1))
      ),
      conditionalPanel("input.ic=='I3'",
                         withMathJax("$$\\left[\\overline{X}_n-z_{1-\\frac{\\alpha}{2}}\\sqrt{\\frac{S_n^2}{n}};\\overline{X}_n+z_{1-\\frac{\\alpha}{2}}\\sqrt{\\frac{S_n^2}{n}}\\right]$$"),
                         selectInput("dist_I3","Distribuciones:",c("Bi(n,p)"="binom","Geom(p)"="geom","Pois(\U03BB)"="pois","N(\U03BC,\U03C3\U00B2)"="norm","Exp(\U03BB)"="expon","Unif(a,b)"="unif"),selected="norm"),
                         conditionalPanel("input.dist_I3=='binom'",
                                          numericInput("n_binom","n",1,min=1), numericInput("p_binom","p",0.5,min=0,max=1,step=0.01)),
                         conditionalPanel("input.dist_I3=='geom'",
                                          numericInput("p_geom","p",0.5,min=0,max=1,step=0.01)),
                         conditionalPanel("input.dist_I3=='pois'",
                                          numericInput("l_pois","\U03BB",2,min=0)),
                         conditionalPanel("input.dist_I3=='norm'",
                                          numericInput("mu","\U03BC",0), numericInput("sigma2","\U03C3\U00B2",1,min=0)),
                         conditionalPanel("input.dist_I3=='expon'",
                                          numericInput("l_exp","\U03BB",1,min=0)),
                         conditionalPanel("input.dist_I3=='unif'",
                                          numericInput("a_unif","a",0), numericInput("b_unif","b",1))
      )
    ),
    ####### MAIN PANEL lo principal que aparece a la derecha
    mainPanel(plotOutput("intervalos",height = "700px")
    )
  )
)


server <- function(input, output) {
  output$intervalos <- renderPlot({
    semilla=input$seed
    N_rep=100
    n=input$n
    nivel=input$level
    
    if(input$ic=="I1"){
      quant <- qnorm((1+nivel)/2)
      esp <- input$mu
      vza <- input$sigma2
      set.seed(semilla)
      X_n <- replicate(N_rep,mean(rnorm(n,esp,sqrt(vza))))
    }
    if(input$ic=="I2"){
      quant <- qt((1+nivel)/2,n-1)
      if(input$dist_I2=="binom"){
        n_binom=input$n_binom
        p=input$p_binom
        esp=n_binom*p
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rbinom(n,n_binom,p)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rbinom(n,n_binom,p)))
        }
      if(input$dist_I2=="geom"){
        p=input$p_geom
        esp=1/p
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rgeom(n,p)+1))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rgeom(n,p)+1))
      }
      if(input$dist_I2=="pois"){
        l=input$l_pois
        esp=l
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rpois(n,l)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rpois(n,l)))
      }
      if(input$dist_I2=="norm"){
        esp=input$mu
        de=sqrt(input$sigma2)
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rnorm(n,esp,de)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rnorm(n,esp,de)))
      }
      if(input$dist_I2=="expon"){
        l=input$l_exp
        esp=1/l
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rexp(n,l)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rexp(n,l)))
      }
      if(input$dist_I2=="unif"){
        a=input$a_unif
        b=input$b_unif
        esp=0.5*(a+b)
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(runif(n,a,b)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(runif(n,a,b)))
      }
    }
    if(input$ic=="I3"){
      quant <- qnorm((1+nivel)/2)
      if(input$dist_I3=="binom"){
        n_binom=input$n_binom
        p=input$p_binom
        esp=n_binom*p
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rbinom(n,n_binom,p)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rbinom(n,n_binom,p)))
      }
      if(input$dist_I3=="geom"){
        p=input$p_geom
        esp=1/p
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rgeom(n,p)+1))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rgeom(n,p)+1))
      }
      if(input$dist_I3=="pois"){
        l=input$l_pois
        esp=l
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rpois(n,l)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rpois(n,l)))
      }
      if(input$dist_I3=="norm"){
        esp=input$mu
        de=sqrt(input$sigma2)
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rnorm(n,esp,de)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rnorm(n,esp,de)))
      }
      if(input$dist_I3=="expon"){
        l=input$l_exp
        esp=1/l
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(rexp(n,l)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(rexp(n,l)))
      }
      if(input$dist_I3=="unif"){
        a=input$a_unif
        b=input$b_unif
        esp=0.5*(a+b)
        set.seed(semilla)
        X_n <- replicate(N_rep,mean(runif(n,a,b)))
        set.seed(semilla)
        vza <- replicate(N_rep,var(runif(n,a,b)))
      }
    }
    
    par(mfrow=c(2,1))
    L <- X_n-quant*sqrt(vza/n)
    U <- X_n+quant*sqrt(vza/n)
    colores <- (L<=esp) & (esp<=U)
    frec_abs <- sum(colores)
    colores[colores==TRUE] <-"black" 
    colores[colores==FALSE] <-"red" 
    plotCI(1:N_rep, X_n, ui=U, li=L,col=colores,lwd=2,sfrac=0.003,
           xlab="",ylab="Estimaciones",main=bquote("Intervalos (longitud media ="~.(mean(U-L))~")"))
    abline(h=esp,col="blue",lwd=2)
    text(x=-0.9, y=esp+0.1, "E(X)",col="blue",cex=0.8)
    
    pie (c(frec_abs, N_rep- frec_abs) ,
         label =c(frec_abs, N_rep- frec_abs) ,
         main = bquote("Proporción de intervalos que contienen a "~E(X)) ,
         col=c("green","red"),radius=1)
    legend ("bottomright", c("Contiene", "No contiene") ,
            fill =c("green","red"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
