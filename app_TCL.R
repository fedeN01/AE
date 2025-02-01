library(shiny)

ui <- fluidPage(
  #Un poco de introduccion
  titlePanel(title=div("Simulaciones de",img(src="estandarizado.png",width=150),"con {X\U2081,...,X\U2099}~X")),
  hr(),
  sidebarLayout(
    #######SIDEBARPANEL lo que va al costado
    sidebarPanel(
      numericInput("seed","Semilla",123),
      numericInput("N_rep","Cantidad de medias muestrales simuladas",1000,min=1),
      numericInput("n","n",10,min=1),
      selectInput("distrib","Distribuciones:",c("Bi(n,p)"="binom","Geom(p)"="geom","Pois(\U03BB)"="pois","N(\U03BC,\U03C3\U00B2)"="norm","Exp(\U03BB)"="expon","Unif(a,b)"="unif"),selected="pois"),
      strong("Parámetros:"),      
      conditionalPanel("input.distrib=='binom'",
                       numericInput("n_binom","n",1,min=1), numericInput("p_binom","p",0.5,min=0,max=1,step=0.01)),
      conditionalPanel("input.distrib=='geom'",
                       numericInput("p_geom","p",0.5,min=0,max=1,step=0.01)),
      conditionalPanel("input.distrib=='pois'",
                       numericInput("l_pois","\U03BB",2,min=0)),
      conditionalPanel("input.distrib=='norm'",
                       numericInput("mu","\U03BC",0), numericInput("sigma2","\U03C3\U00B2",1,min=0)),
      conditionalPanel("input.distrib=='expon'",
                       numericInput("l_exp","\U03BB",1,min=0)),
      conditionalPanel("input.distrib=='unif'",
                       numericInput("a_unif","a",0), numericInput("b_unif","b",1)),
      checkboxInput("normal","Densidad de N(0,1)",value=FALSE)
    ),
    ####### MAIN PANEL lo principal que aparece a la derecha
    mainPanel(plotOutput("scatter",height = "700px"),verbatimTextOutput("data")
    )
  )
)


server <- function(input, output) {
  output$scatter <- renderPlot({
    set.seed(input$seed)
    lat=input$lat
    N_rep=input$N_rep
    n=input$n
    par(mfrow=c(2,1))
    
    if(input$distrib=="binom"){
      n_binom=input$n_binom
      p=input$p_binom
      esp=n_binom*p
      de=sqrt(n_binom*p*(1-p))
      
      lim_izq=0
      lim_der=n_binom
      lim_sup=max(dbinom(lim_izq:lim_der,n_binom,p))
      plot(0,0,xlab="x",ylab="p(x)",xlim=c(lim_izq,lim_der),ylim=c(0,lim_sup),type="n",main=paste("Puntual Bi(",n_binom,",",p,")",sep=""))
      grilla <- seq(lim_izq,lim_der)
      points(grilla, dbinom(grilla,n_binom,p) , pch =16 , col=" red")
      
      xs=replicate(N_rep,mean(rbinom(n,n_binom,p)))
      
    }
    if(input$distrib=="geom"){
      p=input$p_geom
      esp=1/p
      de=sqrt((1-p)*esp^2)
      
      lim_izq=1
      lim_der=qgeom(0.999,p)+1
      lim_sup=max(dgeom(lim_izq:lim_der-1,p))
      plot(0,0,xlab="x",ylab="p(x)",xlim=c(lim_izq,lim_der),ylim=c(0,lim_sup),type="n",main=paste("Puntual Geom(",p,")",sep=""))
      grilla <- seq(lim_izq,lim_der)
      points(grilla , dgeom(grilla-1,p) , pch =16 , col="red")
      
      xs <- replicate(N_rep,mean(rgeom(n,p)+1))
    }
    if(input$distrib=="pois"){
      l=input$l_pois
      esp=l
      de=sqrt(l)
      
      lim_izq=0
      lim_der=qpois(0.999,l)
      lim_sup=max(dpois(lim_izq:lim_der,l))
      plot(0,0,xlab="x",ylab="p(x)",xlim=c(lim_izq,lim_der),ylim=c(0,lim_sup),type="n",main=paste("Puntual Pois(",l,")",sep=""))
      grilla <- seq(lim_izq,lim_der)
      points(grilla , dpois(grilla,l) , pch =16 , col="red")
      
      xs <- replicate(N_rep,mean(rpois(n,l)))
    }
    if(input$distrib=="norm"){
      esp=input$mu
      de=sqrt(input$sigma2)
      
      lim_izq=qnorm(0.001,esp,de)
      lim_der=qnorm(0.999,esp,de)
      lim_sup=dnorm(esp,esp,de)
      plot(0,0,xlab="x",ylab="f(x)",xlim=c(lim_izq,lim_der),ylim=c(0,lim_sup),type="n",main=paste("Densidad N(",esp,",",de,")",sep=""))
      curve(dnorm(x, esp,de), add= TRUE , pch =16 , col="red", lwd =2)
      
      xs<-replicate(N_rep,mean(rnorm(n,esp,de))) 
    }
    if(input$distrib=="expon"){
      l=input$l_exp
      esp=1/l
      de=esp
      
      lim_izq=0
      lim_der=qexp(0.999,l)
      lim_sup=l
      plot(0,0,xlab="x",ylab="f(x)",xlim=c(lim_izq,lim_der),ylim=c(0,lim_sup),type="n",main=paste("Densidad Exp(",l,")",sep=""))
      curve(dexp(x, l),from=0, add= TRUE , pch =16 , col="red", lwd =2)
      
      xs <- replicate(N_rep,mean(rexp(n,l)))
    }
    if(input$distrib=="unif"){
      a=input$a_unif
      b=input$b_unif
      esp=0.5*(a+b)
      de=(b-a)/sqrt(12)
      
      plot(0,0,xlab="x",ylab="f(x)",xlim=c(a-0.1,b+0.1),ylim=c(0,1/(b-a)),type="n",main=paste("Densidad Unif(",a,",",b,")",sep=""))
      curve(dunif(x, a,b),from=a, to=b, add= TRUE , pch =16 , col="red", lwd =2)
      
      xs <- replicate(N_rep,mean(runif(n,a,b)))
    }
    
    z=sqrt(n)*(xs-esp)/de
    der=max(c(min(z),max(z)))
    hist(z,ylim=c(0,dnorm(0)+0.1), probability = TRUE ,breaks = seq(-6,6,0.2),xlab="", 
      main =expression(paste("Histograma de simulaciones de ", sqrt(n)*frac(bar(X)[n]-E(X), sqrt(Var(X)))))
      )
    points(z,jitter(rep(0,length(z)),factor=0.2)-0.01)
    if(input$normal==TRUE){
    curve( dnorm(x), add= TRUE , pch =16 , col="red", lwd =1)
    }
    
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
