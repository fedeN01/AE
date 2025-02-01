library(shiny)
library(plotrix)

ui <- fluidPage(
    #Un poco de introduccion
      titlePanel("Tests de muestras normales para \U03BC"),
      #######SIDEBARPANEL lo que va al costado
        sidebarPanel(
          selectInput("test","Test:",c("Unilateral 1"="tu_1","Unilateral 2"="tu_2","Bilateral"="tb")),
          conditionalPanel("input.test=='tu_1'",
            withMathJax("$$H_0: \\mu\\leq\\mu_0 \\quad vs. \\quad H_1:\\mu>\\mu_0$$")),
          conditionalPanel("input.test=='tu_2'",
            withMathJax("$$H_0: \\mu\\geq\\mu_0 \\quad vs. \\quad H_1:\\mu<\\mu_0$$")),
          conditionalPanel("input.test=='tb'",
            withMathJax("$$H_0: \\mu=\\mu_0 \\quad vs. \\quad H_1:\\mu\\neq\\mu_0$$")),
          
          numericInput("mu_cero","\U03BC\U2080",2),
          
          selectInput("caso","Caso:",c("Varianza conocida"="con","Varianza desconocida"="desc")),
          conditionalPanel("input.test=='tu_1'",
            conditionalPanel("input.caso=='con'",
              withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{\\overline{X}_n-\\mu_0}{\\sigma}\\geq z_{1-\\alpha}$$")),
            conditionalPanel("input.caso=='desc'",
              withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{\\overline{X}_n-\\mu_0}{S_n}\\geq t_{1-\\alpha,n-1}$$"))),
          conditionalPanel("input.test=='tu_2'",
            conditionalPanel("input.caso=='con'",
              withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{\\overline{X}_n-\\mu_0}{\\sigma}\\leq -z_{1-\\alpha}$$")),
            conditionalPanel("input.caso=='desc'",
              withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{\\overline{X}_n-\\mu_0}{S_n}\\leq -t_{1-\\alpha,n-1}$$"))),
          conditionalPanel("input.test=='tb'",
            conditionalPanel("input.caso=='con'",
              withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{|\\overline{X}_n-\\mu_0|}{\\sigma}\\geq z_{1-\\frac{\\alpha}{2}}$$")),
            conditionalPanel("input.caso=='desc'",
              withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{|\\overline{X}_n-\\mu_0|}{S_n}\\geq t_{1-\\frac{\\alpha}{2},n-1}$$"))),
          
          numericInput("seed","Semilla",123),
          sliderInput("level","Nivel de significación (\U03B1)",value=0.1,min=0,max=1,step=0.01),
          numericInput("n","n",5),
          strong("Parámetros verdaderos:"),
          numericInput("mu","\U03BC",2,step=0.1),
          numericInput("sigma2","\U03C3\U00B2",1,min=0)
          ),
    ####### MAIN PANEL lo principal que aparece a la derecha
        mainPanel(
          tabsetPanel(type="tab",
            tabPanel("Simulaciones",plotOutput("tests",height= "700px")),
            tabPanel("Potencia",plotOutput("power"))
            )
        )
  )
        #numericInput("mu_cero","\U03BC\U2080",2),
        #withMathJax("$$H_0: \\mu\\geq\\mu_0 \\quad vs. \\quad H_1:\\mu<\\mu_0$$"),
        #radioButtons("tests","Test:",c("Varianza conocida"="T1","Varianza desconocida"="T2")),      
        #conditionalPanel("input.tests=='T1'",
        #  withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{\\overline{X}_n-\\mu_0}{\\sigma}\\leq -z_{1-\\alpha}$$")),
        #conditionalPanel("input.tests=='T2'",
        #  withMathJax("$$\\text{Rechazo: } \\sqrt{n}\\frac{\\overline{X}_n-\\mu_0}{S_n}\\leq- t_{1-\\alpha,n-1}$$")),
        
        ####### MAIN PANEL lo principal que aparece a la derecha
    



server <- function(input, output) {
  output$tests <- renderPlot({
    mu_0=input$mu_cero
    N_rep=100
    semilla=input$seed
    nivel=input$level
    n=input$n
    esp <- input$mu
    varianza <- input$sigma2
    set.seed(semilla)
    X_n <- replicate(N_rep,mean(rnorm(n,esp,sqrt(varianza))))
    
    if(input$caso=="con"){
      quant <- qnorm(1-nivel)
      vza <- varianza
      }
    if(input$caso=="desc"){
      quant <- qt(1-nivel,n-1)
      set.seed(semilla)
      vza <- replicate(N_rep,var(rnorm(n,esp,sqrt(varianza))))
      }
    estim <- sqrt(n)*(X_n-mu_0)/sqrt(vza)
    
    if(input$test=="tu_1"){
      colores <- (estim<quant)
      frec_abs <- sum(colores)
    }
    if(input$test=="tu_2"){
      quant <- -quant
      colores <- (estim>quant)
      frec_abs <- sum(colores)
    }
    if(input$test=="tb"){
      quant <- if(input$caso=="con"){qnorm(1-nivel/2)}else{qt(1-nivel/2,n-1)}
      colores <- (abs(estim)<quant)
      frec_abs <- sum(colores)
    }
  
    #Grafico rechazo
    par(mfrow=c(2,1),mar=c(5,6,4,2)+0.1)
    colores[colores==TRUE] <-"blue" 
    colores[colores==FALSE] <-"red"
    
    plot(estim,ylab=if(input$caso=="con"){expression(sqrt(n)*frac(bar(X[n])-mu[0],sigma))}else{expression(sqrt(n)*frac(bar(X[n])-mu[0],S[n]))},
        xlab="",main="Simulación estadístico de contraste",xlim=c(1,N_rep),ylim=c(-3,3),type="n")
    abline(v=1:N_rep,col="gray",lwd=0.5)
    abline(h=if(input$test=="tb"){c(-quant,quant)}else{quant},col="red",lwd=1)
    points(estim,col=colores,lwd=2)
    
    
    #Torta
    pie(c(frec_abs, N_rep- frec_abs) ,
         label =c(frec_abs, N_rep- frec_abs) ,
         main = bquote("Proporción de muestras que rechazan"~H[0]) ,
         col=c("blue","red"),radius=1)
    legend ("bottomright", c("No rechaza", "Rechaza"),
            fill =c("blue","red"))
  })
  
  output$power <- renderPlot({
    mu=input$mu
    mu_0=input$mu_cero
    alfa=input$level
    n=input$n
    de <- sqrt(input$sigma2)
    
    if(input$caso=="con"){
      quant <- qnorm(1-alfa)
    }else{
      quant <- qt(1-alfa,n-1)
    }
    if(input$test=="tu_1"){
      potencia<-function(x){
        if(input$caso=="con"){
          1-pnorm(quant-sqrt(n)*(x-mu_0)/de)
        }else{
            1-pt(quant,n-1,ncp=sqrt(n)*(x-mu_0)/de)
        }
        }
      curve(potencia,xlim=c(mu_0-2,mu_0+3),ylim=c(0,1),xlab =expression(mu), ylab=expression(pi(mu)),cex.lab=1.5,
            main="Gráfico de potencia", lwd=2)
      abline(v=mu_0,h=alfa,col="red",lty=1)
      text(x=c(mu_0-2,mu_0+0.1), y=c(alfa+0.05,0), c(expression(alpha),expression(mu[0])),col="red",cex=1.5)
      text(x=mu_0-1.9, y=potencia(mu)+0.05,round(potencia(mu),4),cex=1.5)
      abline(v=mu,h=potencia(mu),col="gray",lty=2)
    }
    if(input$test=="tu_2"){
      potencia<-function(x){
        if(input$caso=="con"){
          pnorm(-quant-sqrt(n)*(x-mu_0)/de)
        }else{
            pt(-quant,n-1,ncp=sqrt(n)*(x-mu_0)/de)}
        }
      curve(potencia,xlim=c(mu_0-3,mu_0+2),ylim=c(0,1),xlab =expression(mu), ylab=expression(pi(mu)),
            main="Gráfico de potencia", lwd=2)
      abline(v=mu_0,h=alfa,col="red",lty=1)
      text(x=c(mu_0-3,mu_0+0.1), y=c(alfa+0.05,0), c(expression(alpha),expression(mu[0])),col="red",cex=1.5)
      text(x=mu_0-1.9, y=potencia(mu)+0.05, round(potencia(mu),4),cex=1.5)
      abline(v=mu,h=potencia(mu),col="gray",lty=2)
    }
    if(input$test=="tb"){
      quant <- if(input$caso=="con"){qnorm(1-alfa/2)}else{qt(1-alfa/2,n-1)}
      potencia<-function(x){
        if(input$caso=="con"){
          1-pnorm(quant-sqrt(n)*(x-mu_0)/de)+pnorm(-quant-sqrt(n)*(x-mu_0)/de)
        }else{
          1-pt(quant,n-1,ncp=sqrt(n)*(x-mu_0)/de)+pt(-quant,n-1,ncp=sqrt(n)*(x-mu_0)/de)
          }}
      curve(potencia,xlim=c(mu_0-3,mu_0+3),ylim=c(0,1),xlab =expression(mu), ylab=expression(pi(mu)),
            main="Gráfico de potencia", lwd=2)
      abline(v=mu_0,h=alfa,col="red",lty=1)
      text(x=c(mu_0-3,mu_0+0.1), y=c(alfa+0.05,0), c(expression(alpha),expression(mu[0])),col="red",cex=1.5)
      text(x=mu_0-1.9, y=potencia(mu)+0.05, round(potencia(mu),4),cex=1.5)
      abline(v=mu,h=potencia(mu),col="gray",lty=2)
      }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           