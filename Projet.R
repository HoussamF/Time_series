library(shiny)
library(forecast)
library(ggplot2)
library(astsa)
library(caTools)
ui<-fluidPage(
  tabsetPanel(
    tabPanel(title = "Graphiques",
             
             uiOutput("imp"),
             uiOutput("choix"),
             uiOutput("texte"),
             uiOutput("texte1"),
             plotOutput("graphique1"),
             uiOutput("saison"),
             plotOutput("graphique_saison"),
             plotOutput("graphique_saison1")
             
             
    ),
    
    tabPanel(title = "Analyse",
             actionButton(inputId = "gen_corr",label = "Matrice des corrélations"),
             plotOutput("graphique_corr"),
             actionButton(inputId = "gen_val_decal",label = "Graphique des valeurs décalées"),
             plotOutput("graphique_val_decal"),
             actionButton(inputId = "gen_correlogram",label = "Autocorrélations"),
             plotOutput("graphique_correlogram"),
             actionButton(inputId = "gen_dec_ad",label = "Décompotition additive"),
             plotOutput("graphique_dec_ad"),
             actionButton(inputId = "gen_dec_m",label = "Décompotition multiplicative"),
             plotOutput("graphique_dec_m"),
             uiOutput("lambdaa"),
             plotOutput("graphique_lambda")
             
             
             
             
             
    ),
    tabPanel(title = "Prévision",
             wellPanel(
               fluidRow(
                 column(2,textInput(inputId = "h",label="Nombre des valeurs du futur"))
               )
             ),
             
             actionButton(inputId = "gen_moyenne",label = "Méthode de moyenne"),
             fluidRow(
               column(6,plotOutput("graphique_moyenne")),
               column(6, plotOutput("residus_moyenne")),
             ) , 
             actionButton(inputId = "gen_naive",label = "Méthode naïve"),
             fluidRow(
               column(6,plotOutput("graphique_naive")),
               column(6, plotOutput("residus_naive")),
             ) , 
             actionButton(inputId = "gen_naive_s",label = "Méthode naïve saisonnière"),
             fluidRow(
               column(6,plotOutput("graphique_naive_s")),
               column(6, plotOutput("residus_naive_s")),
             ) , 
             actionButton(inputId = "gen_deviation",label = "Méthode de déviation"),
             fluidRow(
               column(6,plotOutput("graphique_deviation")),
               column(6, plotOutput("residus_deviation")),
             ) , 
             actionButton(inputId = "gen_nn",label = "Méthode de réseaux de neurones"),
             fluidRow(
               column(6,plotOutput("graphique_nn")),
               column(6, plotOutput("residus_nn")),
             ) , 
             actionButton(inputId = "gen_ses",label = "Lissage exponentiel simple"),
             fluidRow(
               column(6,plotOutput("graphique_ses")),
               column(6, plotOutput("residus_ses")),
             ) , 
             actionButton(inputId = "gen_td",label = "Méthode de tendance linéaire de Holt"),
             fluidRow(
               column(6,plotOutput("graphique_td")),
               column(6, plotOutput("residus_td")),
             ) , 
             actionButton(inputId = "gen_ta",label = "Méthodes de tendance amortie"),
             fluidRow(
               column(6,plotOutput("graphique_ta")),
               column(6, plotOutput("residus_ta")),
             ) , 
             actionButton(inputId = "gen_s_a",label = "Méthode additive de Holt-Winters"),
             fluidRow(
               column(6,plotOutput("graphique_s_a")),
               column(6, plotOutput("residus_s_a")),
             ) , 
             actionButton(inputId = "gen_s_m",label = "Méthode multiplicative de Holt-Winters"),
             fluidRow(
               column(6,plotOutput("graphique_s_m")),
               column(6, plotOutput("residus_s_m")),
             ) , 
             actionButton(inputId = "gen_arima",label = "Méthode ARIMA"),
             fluidRow(
               column(6,plotOutput("graphique_arima")),
               column(6, plotOutput("residus_arima")),
             ) , 
             actionButton(inputId = "gen_ets",label = "Méthode ETS"),
             fluidRow(
               column(6,plotOutput("graphique_ets")),
               column(6, plotOutput("residus_ets")),
             ) , 
             
             
             
             
    ),
    tabPanel(title = "Appprentisage/Test",
             wellPanel(
               fluidRow(
                 column(2,textInput(inputId = "donn_app",label="Nombre d'observations en données de test"))
               ),
               actionButton(inputId = "valider_app",label = "Valider")
             ),
             uiOutput("liste_methodes"),
             plotOutput("train_test_plot")
             
             
             #    actionButton(inputId = "gen_corr",label = "Matrice des corrélations"),
             #   plotOutput("graphique_corr"),
             #  actionButton(inputId = "gen_val_decal",label = "Graphique des valeurs décalées"),
             # plotOutput("graphique_val_decal"),
             #       actionButton(inputId = "gen_correlogram",label = "Autocorrélations"),
             #      plotOutput("graphique_correlogram"),
             #     actionButton(inputId = "gen_dec_ad",label = "Décompotition additive"),
             #    plotOutput("graphique_dec_ad"),
             #   actionButton(inputId = "gen_dec_m",label = "Décompotition multiplicative"),
             #       plotOutput("graphique_dec_m"),
             #      uiOutput("lambdaa"),
             #     plotOutput("graphique_lambda")
    )
    
  )
  
)



server<-function(input,output)
{
  output$imp <- renderUI({
    wellPanel(fileInput(inputId="fichier_csv", label="Importer votre fichier CSV", multiple = FALSE, buttonLabel = "Parcourir...",
                        placeholder = "Aucun fichier sélectionné"))
    
    
  })
  output$choix <- renderUI({
    fichier <- input$fichier_csv
    if(is.null(fichier))
    {
      return(NULL)
    }
    else
    {
      donnees <- read.csv(fichier$datapath,header = TRUE)
      columnNames = colnames(donnees)
      wellPanel(selectInput("data", "Colonne des données:", 
                            choices=columnNames),actionButton(inputId = "valider_donnees",label = "Valider"))
    }
  })
  observeEvent(input$valider_donnees,{
    output$texte <- renderUI({
      wellPanel(
        fluidRow(
          column(6,textInput(inputId = "debut",label="Date début")),
          column(6,textInput(inputId = "frequence",label="Fréquence")),
        ),
        actionButton(inputId = "valider_dates",label = "Valider")
      )
      
    })
  })
  
  observeEvent(input$valider_dates,{
    output$texte1 <- renderUI({
      wellPanel(
        fluidRow(
          column(3,textInput(inputId = "x",label="Nom de l'axe des x")),
          column(3,textInput(inputId = "y",label="Nom del l'axe des y")),
          column(6,textInput(inputId = "titre_graphique",label="Titre du graphique")),
        ),
        actionButton(inputId = "valider_noms",label = "Valider")
      )
      
    })
  })
  
  
  
  observeEvent(input$valider_noms,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique1<-renderPlot({autoplot(donnees,xlab=input$x,ylab=input$y,main=input$titre_graphique)})
    output$saison<-renderUI({
      wellPanel(
        "Vos données sont-elles saisonnières ?",
        actionButton(inputId = "oui",label = "Oui")
      )
    })
    
  })
  observeEvent(input$oui,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_saison<-renderPlot({ggseasonplot(donnees,year.labels = TRUE,main="Graphique saisonnier")})
    output$graphique_saison1<-renderPlot({ggseasonplot(donnees,year.labels = TRUE,polar=TRUE,main="Graphique saisonnier polaire")})
  })
  
  
  observeEvent(input$gen_corr,{
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_corr<-renderPlot({
      GGally::ggpairs(as.data.frame(donnees[,1:2]))})
  })
  observeEvent(input$gen_val_decal,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_val_decal<-renderPlot({gglagplot(donnees)})
  })
  observeEvent(input$gen_correlogram,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_correlogram<-renderPlot({ggAcf(donnees)})
  })
  observeEvent(input$gen_dec_ad,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_dec_ad<-renderPlot({autoplot(decompose(donnees,type = "additive"),main="Décomposition additive")})
  })
  observeEvent(input$gen_dec_m,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_dec_m<-renderPlot({autoplot(decompose(donnees,type = "multiplicative"),main="Décomposition multiplicative")})
    output$lambdaa<-renderUI({
      wellPanel(
        sliderInput(inputId = "lambda",step = 0.1,label = "Choisissez la valeur de lambda pour transformer la série",min = -1,max = 2,value = 1),
        actionButton(inputId = "valider_lambda",label = "Valider")
        
        
      )
    })
    observeEvent(input$valider_lambda,{
      output$graphique_lambda<-renderPlot({autoplot(BoxCox(donnees,input$lambda))}) 
      
      
      
    })
  })
  
  
  
  
  
  observeEvent(input$gen_moyenne,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_moyenne<-renderPlot({autoplot(meanf(donnees,as.numeric(input$h)))})
    output$residus_moyenne<-renderPlot({checkresiduals(meanf(donnees,as.numeric(input$h)))})
  })
  observeEvent(input$gen_naive,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_naive<-renderPlot({autoplot(naive(donnees,as.numeric(input$h)))})
    output$residus_naive<-renderPlot({checkresiduals(naive(donnees,as.numeric(input$h)))})
  })
  observeEvent(input$gen_naive_s,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_naive_s<-renderPlot({autoplot(snaive(donnees,as.numeric(input$h)))})
    output$residus_naive_s<-renderPlot({checkresiduals(snaive(donnees,as.numeric(input$h)))})
  })
  observeEvent(input$gen_deviation,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_deviation<-renderPlot({autoplot(rwf(donnees,as.numeric(input$h),drift=TRUE))})
    output$residus_deviation<-renderPlot({checkresiduals(rwf(donnees,as.numeric(input$h),drift=TRUE))})
  })
  observeEvent(input$gen_nn,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_nn<-renderPlot({autoplot(forecast(nnetar(donnees, lambda=0),h=as.numeric(input$h)))})
    output$residus_nn<-renderPlot({checkresiduals(forecast(nnetar(donnees, lambda=0),h=as.numeric(input$h)))})
  })
  observeEvent(input$gen_ses,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_ses<-renderPlot({autoplot(ses(donnees,h=as.numeric(input$h)))})
    output$residus_ses<-renderPlot({checkresiduals(ses(donnees,h=as.numeric(input$h)))})
  })
  observeEvent(input$gen_td,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_td<-renderPlot({autoplot(holt(donnees,h=as.numeric(input$h)))})
    output$residus_td<-renderPlot({checkresiduals(holt(donnees,h=as.numeric(input$h)))})
  })
  observeEvent(input$gen_ta,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_ta<-renderPlot({autoplot(holt(donnees,h=as.numeric(input$h),damped=TRUE))})
    output$residus_ta<-renderPlot({checkresiduals(holt(donnees,h=as.numeric(input$h),damped=TRUE))})
  })
  observeEvent(input$gen_s_a,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_s_a<-renderPlot({autoplot(hw(donnees,seasonal="additive",h=as.numeric(input$h)))})
    output$residus_s_a<-renderPlot({checkresiduals(hw(donnees,seasonal="additive",h=as.numeric(input$h)))})
  })
  observeEvent(input$gen_s_m,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_s_m<-renderPlot({autoplot(hw(donnees,seasonal="multiplicative",h=as.numeric(input$h)))})
    output$residus_s_m<-renderPlot({checkresiduals(hw(donnees,seasonal="multiplicative",h=as.numeric(input$h)))})
  })
  observeEvent(input$gen_arima,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_arima<-renderPlot({autoplot(forecast(auto.arima(donnees),h=as.numeric(input$h)))})
    output$residus_arima<-renderPlot({checkresiduals(forecast(auto.arima(donnees),h=as.numeric(input$h)))})
  })
  observeEvent(input$gen_ets,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    output$graphique_ets<-renderPlot({autoplot(forecast(ets(donnees),h=as.numeric(input$h)))})
    output$residus_ets<-renderPlot({checkresiduals(forecast(ets(donnees),h=as.numeric(input$h)))})
  })
  #Apprentissage :
  observeEvent(input$valider_app,{
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    test <- subset(donnees, start=length(donnees)-(as.numeric(input$donn_app)))
    train <- subset(donnees, end=length(donnees)-(as.numeric(input$donn_app)))
    output$liste_methodes<-renderUI({
      wellPanel(selectInput("choix_methode", "Choisir la méthode:", 
                            choices=c("ARIMA","ETS","Réseax de neurones")),actionButton(inputId = "valider_methode",label = "Valider"))
      
    })
  })
  observeEvent(input$valider_methode,{
    
    dt <- input$data
    fichier <- input$fichier_csv
    donnees <- read.csv(fichier$datapath,header = TRUE)
    donnees <- donnees[,dt]
    donnees <- ts(donnees,start = as.numeric(input$debut),frequency = as.numeric(input$frequence))
    test <- subset(donnees, start=length(donnees)-(as.numeric(input$donn_app)))
    train <- subset(donnees, end=length(donnees)-(as.numeric(input$donn_app)))
    methode<-input$choix_methode
    print(methode)
    if(methode=="Réseax de neurones")
    {
      output$train_test_plot<-renderPlot({autoplot(forecast(nnetar(train, lambda=0),h=as.numeric(input$donn_app)))+autolayer(test)})
    }
    else
    {
      if(methode=="ETS")
      {
        output$train_test_plot<-renderPlot({autoplot(forecast(ets(train),h=as.numeric(input$donn_app)))+autolayer(test)})
      }
      else
      {
        if(methode=="ARIMA")
        {
          output$train_test_plot<-renderPlot({autoplot(forecast(auto.arima(train),h=as.numeric(input$donn_app)))+autolayer(test)})
        }
      }
    }
    
  })
  
  
  
}
shinyApp(ui,server)
