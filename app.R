library(shiny)

# import pokemon stats file
data_stats <- read.csv('pokemon_stats.csv')

# import table with level and correct cp
lvl_cpm <- read.csv('level_cpm.csv')

# import the highest product
high_pr <- read.csv('highest_product.csv')

ui <- fluidPage(
    h1("PvP IV Spread Analysis"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "source",
                label = "Analysis type",
                choices = c(
                    "One Pokemon" = "one",
                    "Many Pokemons (Text)" = "mtext",
                    "Many Pokemons (File)" = "mfile"
                )
            ),
            conditionalPanel(
                condition = "input.source == 'one'",
                selectInput('name', 'Pokemon', choices = levels(data_stats$pokemon), selected = 'Heracross'),
                selectInput('league', 'League', choices = c('Great', 'Ultra', 'Master'), selected = 'Great'),
                numericInput("att", "Attack IV", value = 0, min = 0, max = 15),
                numericInput("def", "Defense IV", value = 15, min = 0, max = 15),
                numericInput("sta", "Stamina IV", value = 15, min = 0, max = 15)
            ),
            conditionalPanel(
                condition = "input.source == 'mtext'",
                textAreaInput("text", "Enter text", placeholder = 'Pokemon, League, Attack IV, Defense IV, Stamina IV\nFor example:\nMuk, Great, 2, 3, 4', rows = 7)
            ),
            conditionalPanel(
                condition = "input.source == 'mfile'",
                fileInput("file", "Select a file")
            ),
            actionButton(inputId = 'analyze', label = 'Analyze')
        ),
        mainPanel(
            DT::dataTableOutput("summary_iv")
        )
    )
)

server <- function(input, output) {
    # all possible iv's combinations
    iva <- rep(0:15, each = 256)                    #attach
    ivd <- rep(rep(0:15, each = 16), times = 16)    #defense
    ivs <- rep(rep(0:15, 16), times = 16)           #stamina
    iv_table <- data.frame(cbind(iva, ivd, ivs))
    
    # decide which source will be analysed
    data_source <- reactive({
        if(input$source == "mtext"){
            data <- input$text
        }else if(input$source == "mfile"){
            data <- input_file()
        }
        return(data)
    })
    
    input_file <- reactive({
        if(is.null(input$file)) {
            return("")
        }
        readLines(input$file$datapath)
    })
    
    analysis <- function(pokemon){
        # check maximum level
        if(input$league == 'Great'){
            leag_value <- 1500
        }else if(input$league == 'Ultra'){
            leag_value <- 2500
        }else if(input$league == 'Master'){
            leag_value <- 4431 #max cp from Slaking lvl40
        }
        
        iv_table$cpm <- sqrt((leag_value*10)/((pokemon$attack + iv_table$iva)*sqrt(pokemon$defense + iv_table$ivd)*sqrt(pokemon$stamina + iv_table$ivs)))
        
        lvl_func <- function(x){
            data_temp <- x - lvl_cpm$cpm
            data_temp[data_temp < 0] <- NA
            lvl_cpm[which.min(data_temp), "level"]
        }
        iv_table$lvl <- sapply(iv_table$cpm, lvl_func)
        iv_table$cp1 <- apply(iv_table, 1, function(x){trunc(((pokemon$attack + x[[1]])*sqrt(pokemon$defense + x[[2]])*
                                                                  sqrt(pokemon$stamina + x[[3]])*lvl_cpm[lvl_cpm$level == min(x[[5]]+0.5, 40), "cpm"]^2)/10)})
        
        iv_table$clv <- apply(iv_table, 1, function(x){if(x[[6]] == leag_value) min(x[[5]]+0.5, 40) else x[[5]]})
        
        # maximum CP
        iv_table$mcp <- apply(iv_table, 1, function(x){trunc(((pokemon$attack + x[[1]])*sqrt(pokemon$defense + x[[2]])*
                                                                  sqrt(pokemon$stamina + x[[3]])*lvl_cpm[lvl_cpm$level == x[[7]], "cpm"]^2)/10)})
        
        # updating cpm values
        iv_table$cpm <- apply(iv_table, 1, function(x){lvl_cpm[lvl_cpm$level == x[[7]], "cpm"]})
        
        # calculate effective stats
        iv_table$sta <- (pokemon$attack +iv_table$iva)*iv_table$cpm
        iv_table$std <- (pokemon$defense+iv_table$ivd)*iv_table$cpm
        iv_table$sts <- floor((pokemon$stamina+iv_table$ivs)*iv_table$cpm)
        iv_table$pro <- iv_table$sta*iv_table$std*iv_table$sts
        
        # calculate the precentage of highest
        if(input$name %in% high_pr$pokemon){
            if(input$league == 'Great'){
                highest_product <- high_pr[high_pr$pokemon == input$name, "great"]
            }else if(input$league == 'Ultra'){
                highest_product <- high_pr[high_pr$pokemon == input$name, "ultra"]
            }else if(input$league == 'Master'){
                highest_product <- high_pr[high_pr$pokemon == input$name, "master"]
            }
            
            iv_table$phi <- apply(iv_table, 1, function(x){round((x[[12]]/highest_product)*100, 2)})
        }else{
            iv_table$phi <- NA
        }
        
        iv_table <- iv_table[, c('iva', 'ivd', 'ivs', 'clv', 'mcp', 'phi')]
        
        test_temp <- iv_table$iva == input$att & iv_table$ivd == input$def & iv_table$ivs == input$sta
        first_row <- iv_table[test_temp, ]
        iv_table <- iv_table[!test_temp, ]
        iv_table <- iv_table[order(-iv_table$phi, -iv_table$mcp), ]
        
        iv_table <- rbind(first_row, iv_table)
        
        colnames(iv_table) <- c('Attack', 'Defense', 'Stamina', 'Level', 'CP', '% Max Stats')
        iv_table
    }
    
    # show the final result
    output$summary_iv <- DT::renderDataTable({
        input$analyze
        isolate({
            if(input$source == "one") {
                poke <- data_stats[data_stats$pokemon == input$name, 2:4]
                final_table <- analysis(poke)
            }else {
                'not show'
            }
        })
    }, rownames = FALSE)
}

shinyApp(ui = ui, server = server)
