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
                textAreaInput("text", "Enter text", placeholder = 'Pokemon League Attack-IV Defense-IV Stamina-IV\nFor example:\nMuk Great 2 3 4', rows = 7)
            ),
            conditionalPanel(
                condition = "input.source == 'mfile'",
                fileInput("file", "Select a file")
            ),
            actionButton(inputId = 'analyze', label = 'Analyze')
        ),
        mainPanel(
            tableOutput('table'),
            DT::dataTableOutput("summary_iv")
        )
    )
)

server <- function(input, output) {
    input_file <- reactive({
        if(is.null(input$file)) {
            return("")
        }
        read.csv(input$file$datapath, header = FALSE)
    })
    
    lvl_func <- function(x){
        data_temp <- x - lvl_cpm$cpm
        data_temp[data_temp < 0] <- NA
        lvl_cpm[which.min(data_temp), "level"]
    }
    
    # calculate the percentage of highest
    highest_perc <- function(name, league, product){
        if(name %in% high_pr$pokemon){
            if(league == 'Great'){
                high_product <- high_pr[high_pr$pokemon == name, "great"]
            }else if(league == 'Ultra'){
                high_product <- high_pr[high_pr$pokemon == name, "ultra"]
            }else if(league == 'Master'){
                high_product <- high_pr[high_pr$pokemon == name, "master"]
            }
            
            phi <- round((product/high_product)*100, 2)
        }else{
            phi <- NA
        }
        return(phi)
    }
    
    one_analysis <- function(pokemon, league){
        # check maximum level
        if(league == 'Great'){
            leag_value <- 1500
        }else if(league == 'Ultra'){
            leag_value <- 2500
        }else if(league == 'Master'){
            leag_value <- 4431 #max cp from Slaking lvl40
        }
        
        
        # all possible iv's combinations
        iva <- rep(0:15, each = 256)                    #attach
        ivd <- rep(rep(0:15, each = 16), times = 16)    #defense
        ivs <- rep(rep(0:15, 16), times = 16)           #stamina
        sheet <- data.frame(cbind(iva, ivd, ivs))
        
        
        sheet$cpm <- sqrt((leag_value*10)/((pokemon$attack + sheet$iva)*sqrt(pokemon$defense + sheet$ivd)*sqrt(pokemon$stamina + sheet$ivs)))
        
        sheet$lvl <- sapply(sheet$cpm, lvl_func)
        sheet$cp1 <- apply(sheet, 1, function(x){trunc(((pokemon$attack + x[[1]])*sqrt(pokemon$defense + x[[2]])*
                     sqrt(pokemon$stamina + x[[3]])*lvl_cpm[lvl_cpm$level == min(x[[5]]+0.5, 40), "cpm"]^2)/10)})
        
        sheet$clv <- apply(sheet, 1, function(x){if(x[[6]] == leag_value) min(x[[5]]+0.5, 40) else x[[5]]})
        
        # maximum CP
        sheet$mcp <- apply(sheet, 1, function(x){trunc(((pokemon$attack + x[[1]])*sqrt(pokemon$defense + x[[2]])*
                     sqrt(pokemon$stamina + x[[3]])*lvl_cpm[lvl_cpm$level == x[[7]], "cpm"]^2)/10)})
        
        # updating cpm values
        sheet$cpm <- apply(sheet, 1, function(x){lvl_cpm[lvl_cpm$level == x[[7]], "cpm"]})
        
        # calculate effective stats
        sheet$sta <- (pokemon$attack +sheet$iva)*sheet$cpm
        sheet$std <- (pokemon$defense+sheet$ivd)*sheet$cpm
        sheet$sts <- floor((pokemon$stamina+sheet$ivs)*sheet$cpm)
        sheet$pro <- sheet$sta*sheet$std*sheet$sts
        
        # calculate the precentage of highest
        if(input$name %in% high_pr$pokemon){
            if(league == 'Great'){
                highest_product <- high_pr[high_pr$pokemon == input$name, "great"]
            }else if(league == 'Ultra'){
                highest_product <- high_pr[high_pr$pokemon == input$name, "ultra"]
            }else if(league == 'Master'){
                highest_product <- high_pr[high_pr$pokemon == input$name, "master"]
            }
            
            sheet$phi <- apply(sheet, 1, function(x){round((x[[12]]/highest_product)*100, 2)})
        }else{
            sheet$phi <- NA
        }
        
        sheet <- sheet[, c('iva', 'ivd', 'ivs', 'clv', 'mcp', 'phi')]
        
        test_temp <- sheet$iva == input$att & sheet$ivd == input$def & sheet$ivs == input$sta
        first_row <- sheet[test_temp, ]
        sheet <- sheet[!test_temp, ]
        sheet <- sheet[order(-sheet$phi, -sheet$mcp), ]
        
        sheet <- rbind(first_row, sheet)
        
        colnames(sheet) <- c('Attack', 'Defense', 'Stamina', 'Level', 'CP', '% Max Stats')
        sheet
    }
    
    many_analysis <- function(sheet){
        temp <- data.frame()
        for(i in 1:dim(sheet)[1]){
            # check maximum level
            if(sheet$league[i] == 'Great'){
                leag_value <- 1500
            }else if(sheet$league[i] == 'Ultra'){
                leag_value <- 2500
            }else if(sheet$league[i] == 'Master'){
                leag_value <- 4431 #max cp from Slaking lvl40
            }
            pokemon <- data_stats[data_stats$pokemon == sheet$name[i], 2:4]
            cpm<- sqrt((leag_value*10)/((pokemon$attack + sheet$iva[i])*sqrt(pokemon$defense + sheet$ivd[i])*sqrt(pokemon$stamina + sheet$ivs[i])))
            
            lvl <- sapply(cpm, lvl_func)
            cp1 <- trunc(((pokemon$attack + sheet$iva[i])*sqrt(pokemon$defense + sheet$ivd[i])*
                   sqrt(pokemon$stamina + sheet$ivs[i])*lvl_cpm[lvl_cpm$level == min(lvl+0.5, 40), "cpm"]^2)/10)
            
            clv <- ifelse(cp1 == leag_value, min(lvl+0.5, 40), lvl)
            
            # maximum CP
            mcp <- trunc(((pokemon$attack + sheet$iva[i])*sqrt(pokemon$defense + sheet$ivd[i])*
                   sqrt(pokemon$stamina + sheet$ivs[i])*lvl_cpm[lvl_cpm$level == clv, "cpm"]^2)/10)
            
            # updating cpm values
            cpm <- lvl_cpm[lvl_cpm$level == clv, "cpm"]
            
            # calculate effective stats
            sta <- (pokemon$attack + sheet$iva[i])*cpm
            std <- (pokemon$defense+ sheet$ivd[i])*cpm
            sts <- floor((pokemon$stamina+ sheet$ivs[i])*cpm)
            pro <- round(sta*std*sts)
            
            phi <- highest_perc(sheet$name[i], sheet$league[i], pro)
            
            temp <- rbind(temp, c(clv, pro, phi))
        }
        
        colnames(temp) <- c('clv', 'pro', 'phi')
        sheet <- cbind(sheet, temp)
        sheet
    }
    # show the final result
    output$summary_iv <- DT::renderDataTable({
        input$analyze
        isolate({
            # decide which source will be analysed
            if(input$source == "one"){
                poke <- data_stats[data_stats$pokemon == input$name, 2:4]
                final_table <- one_analysis(poke, input$league)
                final_table
            }else if(input$source == "mtext"){
                data <- input$text
                data <- read.table(text=gsub("(?<=[a-z])\\s+\n", "\n", data, perl=TRUE), 
                                   header=FALSE, col.names = c("name", "league", 'iva', 'ivd', 'ivs'))
                data <- transform(data, name = as.character(name), league = as.character(league))
                
                final_table <- many_analysis(data)
                final_table
            }else if(input$source == "mfile"){
                data <- input_file()
                colnames(data) <- c('name', 'league', 'iva', 'ivd', 'ivs')
                
                final_table <- many_analysis(data)
                final_table
            }
        })
    }, rownames = FALSE)
    
    # output$table <- renderTable({
    #     if(input$source == "mtext"){
    #         data <- input$text
    #         data <- read.table(text=gsub("(?<=[a-z])\\s+\n", "\n", data, perl=TRUE), 
    #                 header=FALSE, col.names = c("name", "league", 'iva', 'ivd', 'ivs'))
    #         data <- transform(data, name = as.character(name), league = as.character(league))
    #         
    #         data
    #     }
    # })
}

shinyApp(ui = ui, server = server)
