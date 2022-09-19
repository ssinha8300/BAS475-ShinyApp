server <- function(input, output) {
  
  data <- eventReactive(input$create, {
    req(input$index)
    req(input$key1)
    req(input$key2)
    
    Tennis %>% 
      as_tibble(index = input$index,
                key = c(input$key1, input$key2))
    
  })
  
  output$tsibble <- renderDT(options = list(scrollX = TRUE), {
    data()
  })
  
  data1 <- eventReactive(input$compare, {
    req(input$player)
    req(input$careeryear)
    req(input$yearstat)
    
    SUB <- subset(Tennis, Player %in% c(input$player) & Year %in% c(input$careeryear))
    SUB[,c("Year","Calendar Year","Player",input$yearstat)]
  })
  
  output$yearstattable <- renderDataTable(options = list(scrollX = TRUE), {
    data1()
  })
  
  data2 <- eventReactive(input$compare1, {
    req(input$player1)
    req(input$careerstat)
    
    Tennis1 <- Tennis
    suppressWarnings(for (i in 1:nrow(Tennis1)) {
      if (Tennis1[i,4:21]==0) {
        Tennis1[i,4:21] <- NA 
      }
    })
    
    
    CareerTennis <- Tennis1 %>% 
      group_by(Player) %>% 
      summarise(across(c(3,4,8,10,15,17), sum, na.rm = T),
                round(across(c(5,6,7,9,11,12,13,14,16,18,19,20),
                             mean, na.rm = T)),2)
    
    SUB <- subset(CareerTennis, Player %in% c(input$player1))
    SUB[,c("Player",input$careerstat)]
  })
  
  output$careerstattable <- renderDataTable(options = list(scrollX = TRUE), {
    data2()
  })
  
  output$graphoptions <- renderUI({
    if (input$graphselect == "Yearly Stats") {
      tabItem(tabname = "graph",
              #This is a dummy box, Shiny isn't showing the first box in this tab
              box(title = "Select the Players You Want to See",
                  multiInput(inputId = "player2",
                             label = NULL,
                             choiceNames = unique(Tennis[,3]),
                             choiceValues = unique(Tennis$Player))),
              box(title = "Select the Players You Want to See",
                  multiInput(inputId = "player2",
                             label = NULL,
                             choiceNames = unique(Tennis[,3]),
                             choiceValues = unique(Tennis$Player)),
                  width = 4),
              box(title = "Select the Years You Want to See",
                  multiInput(inputId = "careeryear1",
                             label = NULL,
                             choiceNames = unique(Tennis[,1]),
                             choiceValues = unique(Tennis$Year)),
                  width = 4),
              box(title = "Which Stat Do You Want to Compare?",
                  selectInput(inputId = "selectstat",
                              label = NULL,
                              choices = c("",names(Tennis)[4:21])),
                  width = 4),
              actionButton(inputId = "compare2", label = "Compare"),
              br(), br(), br(), br(), br(), br(), br(), br(), br())
    } else {
      tabItem(tabName = "graph",
              box(title = "Select the Players You Want to See",
                  multiInput(inputId = "player3",
                             label = NULL,
                             choiceNames = unique(Tennis[,3]),
                             choiceValues = unique(Tennis$Player)),
                  width = 4),
              box(title = "Select the Specific Career Stats You Want to See",
                  selectInput(inputId = "careerstat1",
                              label = NULL,
                              choices = c("",names(Tennis)[4:21])),
                  width = 4),
              actionButton(inputId = "compare3", label = "Compare"),
              br(), br(), br(), br(), br(), br(), br(), br(), br(),
              br(), br(), br(), br(), br(), br(), br(), br(), br())
    }
  })
  
  TennisYear <- eventReactive(input$compare2, {
    if (input$graphselect == "Yearly Stats") {
      req(input$player2)
      req(input$careeryear1)
      req(input$selectstat)
      
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player %in% c(input$player2), Year %in% c(input$careeryear1)) %>% 
        ggplot(aes(x = Year,
                   y = get(input$selectstat),
                   group = Player,
                   colour = Player,
                   text = paste(" Year:", Year, "\n",
                                "Number:", get(input$selectstat), "\n",
                                "Player:", Player))) +
        geom_line() +
        labs(x = "Year",
             y = "Number",
             title = paste(input$selectstat,
                           "Won in Each Career Year for Each Player")) -> TennisTS
      ggplotly(TennisTS, tooltip = "text")
      
      
      # Tennis %>% 
      #   as_tsibble(index = Year,
      #              key = c(Player)) %>% 
      #   filter(Player %in% c("Andre Agassi","Jim Courier"), Year %in% c(1:5)) %>% 
      #   autoplot(Aces)
    }
  })
  
  output$yeargraph <- renderPlotly(
    TennisYear()
  )
  
  TennisCareer <- eventReactive(input$compare3, {
    if (input$graphselect == "Career Stats") {
      req(input$player3)
      req(input$careerstat1)
      
      Tennis2 <- Tennis
      suppressWarnings(for (i in 1:nrow(Tennis2)) {
        if (Tennis2[i,4:21]==0) {
          Tennis2[i,4:21] <- NA 
        }
      })
      
      CareerTennis1 <- Tennis2 %>% 
        group_by(Player) %>% 
        summarise(across(c(3,4,8,10,15,17), sum, na.rm = T),
                  round(across(c(5,6,7,9,11,12,13,14,16,18,19,20),
                               mean, na.rm = T)),2)
      
      SUB1 <- subset(CareerTennis1, Player %in% c(input$player3))
      SUB1 %>% 
        ggplot(aes(x=input$careerstat1,
                   y=get(input$careerstat1),
                   fill=input$player3,
                   text = paste(" Player:", input$player3, "\n",
                                "Career #:", get(input$careerstat1)))) +
        geom_bar(stat='identity', position='dodge') +
        labs(x = "Statistic",
             y = "Career #",
             fill = "Player",
             title = paste(input$careerstat1,
                           "by Player in Their Career"))-> CareerBarplot
      
      ggplotly(CareerBarplot, tooltip = "text")
    }
  })
  
  output$careergraph <- renderPlotly(
    TennisCareer()
  )
  
  Transformation <- eventReactive(input$create2, {
    req(input$transformplayer)
    req(input$transformstat)
    
    if (input$transformmodel == "Square Root") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$transformplayer)) -> TennisFilter
      
      TennisFilter %>% 
        ggplot(aes(x = Year,
                   y = sqrt(get(input$transformstat)),
                   group = Player,
                   colour = Player,
                   text = paste(" Year:", Year, "\n",
                                "Number:", sqrt(get(input$transformstat))))) +
        geom_line() +
        labs(x = "Career Year",
             y = paste("Square Root of", input$transformstat),
             title = paste("Square Root Transformation of",
                           input$transformstat)) -> TransformGraph
      ggplotly(TransformGraph, tooltip = "text")
    } else if (input$transformmodel == "Cube Root") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$transformplayer)) -> TennisFilter
      
      TennisFilter %>% 
        ggplot(aes(x = Year,
                   y = (get(input$transformstat)^(1/3)),
                   group = Player,
                   colour = Player,
                   text = paste(" Year:", Year, "\n",
                                "Number:", (get(input$transformstat)^(1/3))))) +
        geom_line() +
        labs(x = "Career Year",
             y = paste("Cube Root of", input$transformstat),
             title = paste("Cube Root Transformation of",
                           input$transformstat)) -> TransformGraph
      ggplotly(TransformGraph, tooltip = "text")
    } else if (input$transformmodel == "Logarithm") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$transformplayer)) -> TennisFilter
      
      TennisFilter %>% 
        ggplot(aes(x = Year,
                   y = log(get(input$transformstat)),
                   group = Player,
                   colour = Player,
                   text = paste(" Year:", Year, "\n",
                                "Number:", log(get(input$transformstat))))) +
        geom_line() +
        labs(x = "Career Year",
             y = paste("Logarithm of", input$transformstat),
             title = paste("Logarithm Transformation of",
                           input$transformstat)) -> TransformGraph
      ggplotly(TransformGraph, tooltip = "text")
    } else if (input$transformmodel == "Inverse") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$transformplayer)) -> TennisFilter
      
      TennisFilter %>% 
        ggplot(aes(x = Year,
                   y = (-1/get(input$transformstat)),
                   group = Player,
                   colour = Player,
                   text = paste(" Year:", Year, "\n",
                                "Number:", (-1/get(input$transformstat))))) +
        geom_line() +
        labs(x = "Career Year",
             y = paste("Inverse of", input$transformstat),
             title = paste("Inverse Transformation of",
                           input$transformstat)) -> TransformGraph
      ggplotly(TransformGraph, tooltip = "text")
    } else if (input$transformmodel == "Box-Cox") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$transformplayer)) -> TennisFilter
      
      TennisFilter %>%
        features(input$transformstat, features = guerrero) %>% 
        pull(lambda_guerrero) -> lambda
      
      TennisFilter %>% 
        ggplot(aes(x = Year,
                   y = box_cox(get(input$transformstat),lambda),
                   group = Player,
                   colour = Player,
                   text = paste(" Year:", Year, "\n",
                                "Number:", box_cox(get(input$transformstat),lambda)))) +
        geom_line() +
        labs(x = "Career Year",
             y = paste("Box-Cox of", input$transformstat),
             title = paste("Box-Cox Transformation of",
                           input$transformstat)) -> TransformGraph
      ggplotly(TransformGraph, tooltip = "text")
    }
  })
  
  output$transform <- renderPlotly(
    Transformation()
  )
  
  SimpleModel <- eventReactive(input$create1, {
    req(input$simpleplayer)
    
    if (input$simplestat == "Aces") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(Aces))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(Aces, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(Aces))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(Aces, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(Aces~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(Aces, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      }
    } else if (input$simplestat == "Double Faults") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Double Faults`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Double Faults`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Double Faults`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Double Faults`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Double Faults`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Double Faults`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } 
    } else if (input$simplestat == "1st Serve %") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`1st Serve %`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`1st Serve %`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`1st Serve %`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`1st Serve %`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`1st Serve %`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`1st Serve %`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } 
    } else if (input$simplestat == "% 1st Serve Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 1st Serve Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 1st Serve Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 1st Serve Points Won`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% 2nd Serve Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 2nd Serve Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 2nd Serve Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 2nd Serve Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } 
    } else if (input$simplestat == "Break Points Faced") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Break Points Faced`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Break Points Faced`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Break Points Faced`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Break Points Faced`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Break Points Faced`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Break Points Faced`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      }
    } else if (input$simplestat == "% Break Points Saved") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Break Points Saved`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Break Points Saved`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Break Points Saved`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Break Points Saved`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Break Points Saved`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Break Points Saved`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "Service Games Played") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Service Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Service Games Played`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Service Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Service Games Played`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Service Games Played`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Service Games Played`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% Service Games Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Service Games Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Service Games Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Service Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Service Games Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Service Games Won`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Service Games Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% Total Service Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Total Service Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Total Service Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Total Service Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% 1st Serve Return Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 1st Serve Return Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 1st Serve Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 1st Serve Return Points Won`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } 
    } else if (input$simplestat == "% 2nd Serve Return Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 2nd Serve Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        } 
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 2nd Serve Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 2nd Serve Return Points Won`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "Break Points Opportunities") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Break Points Opportunities`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Break Points Opportunities`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Break Points Opportunities`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      }
    } else if (input$simplestat == "% Break Points Converted") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Break Points Converted`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Break Points Converted`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Break Points Converted`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Break Points Converted`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Break Points Converted`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Break Points Converted`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "Return Games Played") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Return Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Return Games Played`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Return Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Return Games Played`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Return Games Played`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`Return Games Played`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% Return Games Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Return Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Return Games Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Return Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Return Games Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Return Games Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Return Games Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% Return Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        }  else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Return Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        }  else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Return Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Return Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    } else if (input$simplestat == "% Total Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Total Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Total Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Total Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Total Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Total Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>%
            forecast(h = period) -> fc
          
          names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
          fc[,c("Player","Model","Year","Value")]
        } else if (input$simplegraph == "Residuals") {
          augment(fit) %>% features(`% Total Points Won`, ljung_box)
        } else if (input$simplegraph == "Accuracy") {
          accuracy(fit)
        }
      } 
    }
  })
  
  SimpleGraph <- eventReactive(input$create1, {
    req(input$simpleplayer)
    
    if (input$simplestat == "Aces") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(Aces))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, Aces) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = Aces, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(Aces))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, Aces) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = Aces, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(Aces~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, Aces) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = Aces, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        } 
      }
    } else if (input$simplestat == "Double Faults") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Double Faults`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Double Faults`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Double Faults`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Double Faults`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Double Faults`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Double Faults`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      }
    } else if (input$simplestat == "1st Serve %") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`1st Serve %`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `1st Serve %`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`1st Serve %`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `1st Serve %`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`1st Serve %`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `1st Serve %`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% 1st Serve Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 1st Serve Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 1st Serve Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 1st Serve Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% 2nd Serve Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 2nd Serve Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 2nd Serve Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 2nd Serve Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "Break Points Faced") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Break Points Faced`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Break Points Faced`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Break Points Faced`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% Break Points Saved") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Break Points Saved`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Break Points Saved`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Break Points Saved`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "Service Games Played") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Service Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Service Games Played`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Service Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Service Games Played`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Service Games Played`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Service Games Played`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% Service Games Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Service Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Service Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Service Games Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% Total Service Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Total Service Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Total Service Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Total Service Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% 1st Serve Return Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 1st Serve Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 1st Serve Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 1st Serve Return Points Won`~drift()))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% 2nd Serve Return Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% 2nd Serve Return Points Won`))
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% 2nd Serve Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% 2nd Serve Return Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "Break Points Opportunities") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Break Points Opportunities`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Break Points Opportunities`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Break Points Opportunities`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% Break Points Converted") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Break Points Converted`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Break Points Converted`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Break Points Converted`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "Return Games Played") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`Return Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Return Games Played`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`Return Games Played`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Return Games Played`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`Return Games Played`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `Return Games Played`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% Return Games Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Return Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Return Games Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Return Games Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    } else if (input$simplestat == "% Return Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Return Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Return Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      }
    } else if (input$simplestat == "% Total Points Won") {
      if (input$simplemodel == "Mean") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(MEAN(`% Total Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Naive"){
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(NAIVE(`% Total Points Won`)) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } else if (input$simplemodel == "Drift") {
        Tennis %>% 
          as_tsibble(index = Year,
                     key = c(Player)) %>% 
          filter(Player == c(input$simpleplayer)) -> TennisFilter
        
        fit <- TennisFilter %>% 
          model(RW(`% Total Points Won`~drift())) 
        
        if (input$simplegraph == "Forecast") {
          req(input$forecastnum)
          period <- as.numeric(input$forecastnum)
          
          fit %>% 
            forecast(h = period) -> fc
          
          autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
            labs(title = paste(input$simplemodel,"Forecast for",input$simpleplayer))
        } else if (input$simplegraph == "Residuals") {
          gg_tsresiduals(fit)
        } else if (input$simplegraph == "Accuracy") {
          augment(fit) %>% 
            ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
            geom_point() +
            labs(y = "Fitted Value", x = "Actual Value",
                 title = paste("Fitted vs Actual Values of", input$simplestat)) +
            geom_abline(intercept = 0, slope = 1)
        }
      } 
    }
  })
  
  output$simpletable <- renderDataTable(options = list(scrollX = TRUE), {
    SimpleModel()
  })
  
  output$simple1graph <- renderPlot(
    SimpleGraph()
  )
  
  ETSModel <- eventReactive(input$create3, {
    req(input$etsplayer)
    
    if (input$etsstat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(Aces))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(Aces, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Double Faults`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`Double Faults`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`1st Serve %`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`1st Serve %`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 1st Serve Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 2nd Serve Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Break Points Faced`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`Break Points Faced`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Break Points Saved`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Saved`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Service Games Played`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`Service Games Played`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Service Games Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Service Games Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Total Service Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 1st Serve Return Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 2nd Serve Return Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Break Points Opportunities`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Break Points Converted`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Converted`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Return Games Played`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`Return Games Played`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Return Games Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Return Games Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Return Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Return Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$etsstat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Total Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$etsgraph == "Residuals") {
        augment(fit) %>% features(`% Total Points Won`, ljung_box)
      } else if (input$etsgraph == "Accuracy") {
        accuracy(fit)
      }
    }
  })
  
  ETSGraph <- eventReactive(input$create3, {
    req(input$etsplayer)
    
    if (input$etsstat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(Aces))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, Aces) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = Aces, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Double Faults`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Double Faults`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`1st Serve %`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `1st Serve %`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 1st Serve Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 2nd Serve Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Break Points Faced`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Break Points Saved`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Service Games Played`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Service Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Service Games Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Total Service Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 1st Serve Return Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% 2nd Serve Return Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Break Points Opportunities`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Break Points Converted`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`Return Games Played`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Return Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Return Games Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Return Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$etsstat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$etsplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ETS(`% Total Points Won`))
      
      if (input$etsgraph == "Forecast") {
        req(input$etsnum)
        period <- as.numeric(input$etsnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
          labs(title = paste("ETS Forecast for",input$etsplayer))
      } else if (input$etsgraph == "Components") {
        components(fit) %>% autoplot()
      } else if (input$etsgraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$etsgraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$etsstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    }
  })
  
  output$etstable <- renderDataTable(options = list(scrollX = TRUE), {
    ETSModel()
  })
  
  output$ets1graph <- renderPlot(
    ETSGraph()
  )
  
  ARIMAModel <- eventReactive(input$create4, {
    req(input$arimaplayer)
    
    if (input$arimastat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(Aces, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(Aces, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      } 
    } else if (input$arimastat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Double Faults`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`Double Faults`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      } 
    } else if (input$arimastat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`1st Serve %`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`1st Serve %`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Faced`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`Break Points Faced`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Saved`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Saved`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Service Games Played`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`Service Games Played`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Service Games Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Service Games Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Service Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Opportunities`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Converted`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Converted`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Return Games Played`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`Return Games Played`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Games Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Return Games Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Return Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$arimastat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$arimagraph == "Residuals") {
        augment(fit) %>% features(`% Total Points Won`, ljung_box)
      } else if (input$arimagraph == "Accuracy") {
        accuracy(fit)
      }
    }
  })
  
  ARIMAGraph <- eventReactive(input$create4, {
    req(input$arimaplayer)
    
    if (input$arimastat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(Aces, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, Aces) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      } else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = Aces, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Double Faults`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Double Faults`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`1st Serve %`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `1st Serve %`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Faced`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Saved`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Service Games Played`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Service Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Service Games Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Service Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Opportunities`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Converted`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Return Games Played`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Return Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Games Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$arimastat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Forecast") {
        req(input$arimanum)
        period <- as.numeric(input$arimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$arimaplayer))
      }  else if (input$arimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$arimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$arimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    }
  })
  
  ARIMAReport <- eventReactive(input$create4, {
    req(input$arimaplayer)
    
    if (input$arimastat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(Aces, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Double Faults`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      } 
    } else if (input$arimastat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`1st Serve %`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Faced`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Saved`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Service Games Played`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Service Games Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Service Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Opportunities`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Converted`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Return Games Played`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Games Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    } else if (input$arimastat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$arimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Points Won`, stepwise = FALSE))
      
      if (input$arimagraph == "Report") {
        report(fit)
      }
    }
  })
  
  output$arimatable <- renderDataTable(options = list(scrollX = TRUE), {
    ARIMAModel()
  })
  
  output$arima1graph <- renderPlot(
    ARIMAGraph()
  )
  
  output$arimareport <- renderPrint(
    ARIMAReport()
  )
  
  CustARIMAModel <- eventReactive(input$create7, {
    req(input$custarimaplayer)
    req(input$arimap)
    req(input$arimad)
    req(input$arimaq)
    
    p <- as.numeric(input$arimap)
    d <- as.numeric(input$arimad)
    q <- as.numeric(input$arimaq)
    
    if (input$custarimastat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(Aces~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(Aces, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Double Faults`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`Double Faults`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      } 
    } else if (input$custarimastat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`1st Serve %`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`1st Serve %`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Faced`~pdq(p,d,q)))
      
      if (input$arimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`Break Points Faced`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Saved`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Saved`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Service Games Played`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`Service Games Played`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Service Games Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Service Games Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Service Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Opportunities`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Converted`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Converted`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Return Games Played`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`Return Games Played`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Games Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Return Games Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Return Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$custarimastat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$custarimagraph == "Residuals") {
        augment(fit) %>% features(`% Total Points Won`, ljung_box)
      } else if (input$custarimagraph == "Accuracy") {
        accuracy(fit)
      }
    }
  })
  
  CustARIMAGraph <- eventReactive(input$create7, {
    req(input$custarimaplayer)
    req(input$arimap)
    req(input$arimad)
    req(input$arimaq)
    
    p <- as.numeric(input$arimap)
    d <- as.numeric(input$arimad)
    q <- as.numeric(input$arimaq)
    
    if (input$custarimastat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(Aces~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, Aces) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      } else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = Aces, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Double Faults`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Double Faults`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`1st Serve %`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `1st Serve %`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Faced`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Saved`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Service Games Played`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Service Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Service Games Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Service Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Opportunities`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Converted`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Return Games Played`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Return Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Games Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$custarimastat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Forecast") {
        req(input$custarimanum)
        period <- as.numeric(input$custarimanum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
          labs(title = paste("ARIMA Forecast for",input$custarimaplayer))
      }  else if (input$custarimagraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$custarimagraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$custarimastat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    }
  })
  
  CustARIMAReport <- eventReactive(input$create7, {
    req(input$custarimaplayer)
    req(input$arimap)
    req(input$arimad)
    req(input$arimaq)
    
    p <- as.numeric(input$arimap)
    d <- as.numeric(input$arimad)
    q <- as.numeric(input$arimaq)
    
    if (input$custarimastat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(Aces~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Double Faults`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      } 
    } else if (input$custarimastat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`1st Serve %`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Faced`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Saved`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Service Games Played`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Service Games Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Service Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 1st Serve Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% 2nd Serve Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Break Points Opportunities`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Break Points Converted`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`Return Games Played`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Games Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Return Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    } else if (input$custarimastat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$custarimaplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(ARIMA(`% Total Points Won`~pdq(p,d,q)))
      
      if (input$custarimagraph == "Report") {
        report(fit)
      }
    }
  })
  
  output$custarimatable <- renderDataTable(options = list(scrollX = TRUE), {
    CustARIMAModel()
  })
  
  output$custarima1graph <- renderPlot(
    CustARIMAGraph()
  )
  
  output$custarimareport <- renderPrint(
    CustARIMAReport()
  )
  
  LinearModel <- eventReactive(input$create5, {
    req(input$linearplayer)
    
    if (input$linearstat == "% Total Points Won ~ % Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Points Won` ~ `% Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Total Points Won",
                       "Value", "% Return Points Won")
        fc[,c("Player","Model","Year","Value", "% Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Total Points Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Total Points Won ~ % Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Points Won` ~ `% Total Service Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% Total Service Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Total Points Won",
                       "Value", "% Total Service Points Won")
        fc[,c("Player","Model","Year","Value", "% Total Service Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Total Points Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Service Games Won ~ % 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Service Games Won` ~ `% 1st Serve Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Service Games Won",
                       "Value", "% 1st Serve Points Won")
        fc[,c("Player","Model","Year","Value", "% 1st Serve Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Service Games Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Service Games Won ~ % 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Service Games Won` ~ `% 2nd Serve Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Service Games Won",
                       "Value", "% 2nd Serve Points Won")
        fc[,c("Player","Model","Year","Value", "% 2nd Serve Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Service Games Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "Break Points Opportunities ~ % 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Opportunities` ~ `% 1st Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "Break Points Opportunities",
                       "Value", "% 1st Serve Return Points Won")
        fc[,c("Player","Model","Year","Value", "% 1st Serve Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "Break Points Opportunities ~ % 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Opportunities` ~ `% 2nd Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "Break Points Opportunities",
                       "Value", "% 2nd Serve Return Points Won")
        fc[,c("Player","Model","Year","Value", "% 2nd Serve Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Return Games Won ~ % 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Games Won` ~ `% 1st Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Return Games Won",
                       "Value", "% 1st Serve Return Points Won")
        fc[,c("Player","Model","Year","Value", "% 1st Serve Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Return Games Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Return Games Won ~ % 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Games Won` ~ `% 2nd Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Return Games Won",
                       "Value", "% 2nd Serve Return Points Won")
        fc[,c("Player","Model","Year","Value", "% 2nd Serve Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Return Games Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Return Points Won ~ % 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Points Won` ~ `% 1st Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Return Points Won",
                       "Value", "% 1st Serve Return Points Won")
        fc[,c("Player","Model","Year","Value", "% 1st Serve Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Return Points Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$linearstat == "% Return Points Won ~ % 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Points Won` ~ `% 2nd Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        names(fc) <- c("Scenario" ,"Player", "Model", "Year", "% Return Points Won",
                       "Value", "% 2nd Serve Return Points Won")
        fc[,c("Player","Model","Year","Value", "% 2nd Serve Return Points Won")]
      } else if (input$lineargraph == "Residuals") {
        augment(fit) %>% features(`% Return Points Won`, ljung_box)
      } else if (input$lineargraph == "Accuracy") {
        accuracy(fit)
      }
    }
  })
  
  LinearGraph <- eventReactive(input$create5, {
    req(input$linearplayer)
    
    if (input$linearstat == "% Total Points Won ~ % Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Points Won` ~ `% Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Total Points Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Total Points Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Total Points Won ~ % Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Points Won` ~ `% Total Service Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% Total Service Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% Total Service Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Total Points Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Total Points Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Service Games Won ~ % 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Service Games Won` ~ `% 1st Serve Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 1st Serve Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Service Games Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Service Games Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Service Games Won ~ % 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Service Games Won` ~ `% 2nd Serve Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 2nd Serve Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Service Games Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Service Games Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "Break Points Opportunities ~ % 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Opportunities` ~ `% 1st Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 1st Serve Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
          labs(title = "TSLM Forecast of Break Points Opportunities")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of Break Points Opportunities") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "Break Points Opportunities ~ % 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Opportunities` ~ `% 2nd Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 2nd Serve Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
          labs(title = "TSLM Forecast of Break Points Opportunities")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of Break Points Opportunities") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Return Games Won ~ % 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Games Won` ~ `% 1st Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 1st Serve Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Return Games Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Return Games Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Return Games Won ~ % 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Games Won` ~ `% 2nd Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 2nd Serve Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Return Games Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Return Games Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Return Points Won ~ % 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Points Won` ~ `% 1st Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 1st Serve Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 1st Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Return Points Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Return Points Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$linearstat == "% Return Points Won ~ % 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$linearplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Points Won` ~ `% 2nd Serve Return Points Won`))
      
      if (input$lineargraph == "Forecast") {
        req(input$linearnum)
        x <- as.numeric(input$linearnum)
        
        xfc <- scenarios(
          `% 2nd Serve Return Points Won` = new_data(TennisFilter, 1) %>%
            mutate(`% 2nd Serve Return Points Won` = x))
        
        fit %>% 
          forecast(new_data = xfc) -> fc
        
        autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
          labs(title = "TSLM Forecast of % Return Points Won")
      } else if (input$lineargraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$lineargraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = "Fitted vs Actual Value of % Return Points Won") +
          geom_abline(intercept = 0, slope = 1)
      }
    }
  })
  
  output$lineartable <- renderDataTable(options = list(scrollX = TRUE), {
    LinearModel()
  })
  
  output$linear1graph <- renderPlot(
    LinearGraph()
  )
  
  RegModel <- eventReactive(input$create6, {
    req(input$regplayer)
    
    if (input$regstat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(Aces ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Aces", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(Aces, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Double Faults` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Double Faults", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`Double Faults`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      } 
    } else if (input$regstat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`1st Serve %` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "1st Serve %", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`1st Serve %`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 1st Serve Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 2nd Serve Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Faced` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Faced", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`Break Points Faced`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Break Points Saved` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Saved", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Saved`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Service Games Played` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Service Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`Service Games Played`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Service Games Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Service Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Service Games Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Service Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Service Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Total Service Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 1st Serve Return Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 1st Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% 1st Serve Return Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 2nd Serve Return Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% 2nd Serve Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% 2nd Serve Return Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Opportunities` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Break Points Opportunities", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`Break Points Opportunities`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Break Points Converted` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Break Points Converted", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Break Points Converted`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Return Games Played` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "Return Games Played", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`Return Games Played`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Games Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Games Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Return Games Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Return Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Return Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    } else if (input$regstat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        names(fc) <- c("Player", "Model", "Year", "% Total Points Won", "Value")
        fc[,c("Player","Model","Year","Value")]
      } else if (input$reggraph == "Residuals") {
        augment(fit) %>% features(`% Total Points Won`, ljung_box)
      } else if (input$reggraph == "Accuracy") {
        accuracy(fit)
      }
    }
  })
  
  RegGraph <- eventReactive(input$create6, {
    req(input$regplayer)
    
    if (input$regstat == "Aces") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(Aces ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, Aces) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      } else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = Aces, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "Double Faults") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Double Faults` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Double Faults`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Double Faults`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "1st Serve %") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`1st Serve %` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `1st Serve %`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `1st Serve %`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% 1st Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 1st Serve Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% 2nd Serve Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 2nd Serve Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "Break Points Faced") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Faced` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Faced`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Faced`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Break Points Saved") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Break Points Saved` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Saved`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Saved`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "Service Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Service Games Played` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Service Games Played`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Service Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Service Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Service Games Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Service Games Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Service Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Total Service Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Service Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Service Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Service Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% 1st Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 1st Serve Return Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 1st Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 1st Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% 2nd Serve Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% 2nd Serve Return Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% 2nd Serve Return Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% 2nd Serve Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "Break Points Opportunities") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Break Points Opportunities` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Break Points Opportunities`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Break Points Opportunities`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Break Points Converted") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Break Points Converted` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Break Points Converted`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Break Points Converted`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "Return Games Played") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`Return Games Played` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `Return Games Played`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `Return Games Played`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Return Games Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Games Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Games Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Games Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Return Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Return Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Return Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Return Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    } else if (input$regstat == "% Total Points Won") {
      Tennis %>% 
        as_tsibble(index = Year,
                   key = c(Player)) %>% 
        filter(Player == c(input$regplayer)) -> TennisFilter
      
      fit <- TennisFilter %>% 
        model(TSLM(`% Total Points Won` ~ trend()))
      
      if (input$reggraph == "Forecast") {
        req(input$regnum)
        period <- as.numeric(input$regnum)
        
        fit %>% 
          forecast(h = period) -> fc
        
        autoplot(TennisFilter, `% Total Points Won`) + autolayer(fc) +
          labs(title = paste("Linear Regression Forecast for",input$regplayer))
      }  else if (input$reggraph == "Residuals") {
        gg_tsresiduals(fit)
      } else if (input$reggraph == "Accuracy") {
        augment(fit) %>% 
          ggplot(aes(x = `% Total Points Won`, y = .fitted)) +
          geom_point() +
          labs(y = "Fitted Value", x = "Actual Value",
               title = paste("Fitted vs Actual Values of", input$regstat)) +
          geom_abline(intercept = 0, slope = 1)
      }
    }
  })
  
  output$regtable <- renderDataTable(options = list(scrollX = TRUE), {
    RegModel()
  })
  
  output$reg1graph <- renderPlot(
    RegGraph()
  )
}

shinyApp(ui, server)
