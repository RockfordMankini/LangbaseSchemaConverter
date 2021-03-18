#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(rjson)
library(dplyr)
library(openssl)

options(shiny.maxRequestSize = 100 * 1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Langbase JSON to CSV Converter"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Upload a .json file of the fully exported Langbase database you're working with. \n
                     When a table appears, you can download the files."),
            textOutput("status"),
            fileInput("upload", "Upload a file"),
            downloadButton("download_sentences", "Download Sentences"),
            downloadButton("download_words", "Download Words"),
            downloadButton("download_sources", "Download Sources"),
            downloadButton("download_users", "Download Users")
        ),
        
        mainPanel(
            tabsetPanel(type="tabs",
                tabPanel("Sentences", dataTableOutput("sentences")),
                tabPanel("Words", dataTableOutput("words")),
                tabPanel("Sources", dataTableOutput("sources")),
                tabPanel("Users", dataTableOutput("users")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # get the ID of the language from the language name
    getLangId <- function(jsonfile, langname) {
        result <- jsonfile
        
        for(i in 2:length(result)) {
            
            tablename <- (result[[i]]$name)
            lang_table <- NULL
            
            if(grepl("adm_groups", tablename, fixed = TRUE)) {
                lang_table <- result[[i]]$data
                break
            }
            
        }
        
        for(i in 1:length(lang_table)) {
            
            if(lang_table[[i]]$name == langname) {
                return(lang_table[[i]]$ID)
            }
        }
        
        return(NA)
        
    }
    
    # grabs the sentences from ONE language.
    sentences_to_df <- function(jsonfile, index) {
        
        columnnames <- c("ID", "user", "date", "page", "num", 
                         "english", "original", "comments", "temp", "language", "lang_id")
        
        result <- jsonfile
        tablename <- result[[index]][[2]]
        tablename <- strsplit(tablename, "_")[[1]][1]
        
        langid <- getLangId(jsonfile, tablename)
        
        obs <- result[[index]][[4]]
        rows <- length(obs)
        
        df <- data.frame(ID=character(), user=character(), date=character(), page=character(),
                         num=character(), english=character(), original=character(),
                         comments=character(), temp=character(), language=character(), lang_id=character(), stringsAsFactors = FALSE)
        
        row <- character(11)
        row[10] <- tablename
        row[11] <- langid
        for(i in 1:rows) {
            if(length(obs) == 0) {
                break
            }
            ob <- obs[[i]]
            
            for(j in 1:9) {
                
                val <- ob[[j]]
                
                if(is.null(val)) {
                    val <- NA
                }
                row[j] <- as.character(val)
            }
            df[i,] <- row
        }
        df
    }
    
    # get table of users
    getUserTable <- function(jsonfile, index) {
        result <- jsonfile
        
        adm_users <- result[[index]]$data
        
        user_table <- data.frame(ID=character(), name=character(), password=character(), stringsAsFactors = FALSE)
        
        for(i in 1:length(adm_users)) {
            
            user_info <- adm_users[[i]]
            
            row <- c(user_info$ID, user_info$name, user_info$password)
            user_table[i,] <- row
            
            
        }
        
        # get rid of users who's ids appear multiple times
        user_table <- distinct(user_table, name, .keep_all = TRUE)
        
        # hash passwords
        user_table$password <- md5(user_table$password)
        user_table$username <- user_table$name
        user_table$first_name <- user_table$name
        user_table$last_name <- NA
        user_table$role <- 0
        user_table$email <- NA
        user_table %>% select(id=ID, username, email, first_name, last_name, password, role)
        
    }
    
    # run sentences_to_df for every language.
    make_sentences_df <- function(full_db) {
        
        full_db_json <- full_db
        indices <- c()
        
        for(i in 2:length(full_db)) {
            tablename <- (full_db[[i]]$name)
            
            if(grepl("sentence", tablename, fixed = TRUE)) {
                indices <- append(indices, i)
            }
            
        }
        
        full_db <- 0
        
        for(i in 1:length(indices)) {
            
            print(i)
            
            if(i == 1) {
                full_db <- sentences_to_df(full_db_json, indices[i])
            }
            
            else {
                full_db <- rbind(full_db, sentences_to_df(full_db_json, indices[i]))
            }
            
        }
        
        # give each sentence an ID
        full_db[,1] <- 1:nrow(full_db)
        
        full_db$date <- ifelse(full_db$date == "0000-00-00", NA, full_db$date)
        
        # turn date into date_created and date_modified
        full_db$date_created <- full_db$date
        full_db$date_modified <- full_db$date
        full_db <- full_db %>% select(-date)
        
        return(full_db)
    }
    
    # clean up sentences df
    clean_up_sentences <- function(full_db, user_table) {
        # merge sentences and users, clean it up.
        merged <- (merge(x=user_table, y=full_db, by.x = "username", by.y = "user", all.y=TRUE))
        print(colnames(merged))
        names(merged)[2] <- "user_id"
        names(merged)[8] <- "id"
        names(merged)[16] <- "language_id"
        merged <- merged[,-c(3,4,5,6,7,15)]
        merged <- merged %>% arrange(id)
        merged$user_id <- ifelse(is.na(merged$user_id), NA, merged$user_id)
        
        full_db <- merged
        full_db %>% select(id, original, english, comments, page, temp, num, 
                           user_id, language_id, date_created, date_modified)
    }
    
    # grabs the sentences from ONE language.
    words_to_df <- function(jsonfile, index) {
        
        columnnames <- c("id", "word", "gloss", "part_of_speech", "definition", "comments",
                         "user_id", "language_id", "date_created", "date_modified")
        
        result <- jsonfile
        tablename <- result[[index]][[2]]
        tablename <- strsplit(tablename, "_")[[1]][1]
        
        langid <- getLangId(jsonfile, tablename)
        
        
        obs <- result[[index]][[4]]
        rows <- length(obs)
        
        df <- data.frame(id=character(), word=character(), gloss=character(), part_of_speech=character(), definition=character(),
                         comments=character(), user_id=character(), language_id=character(),
                         date_created=character(), date_modified=character(), stringsAsFactors = FALSE)
        
        row <- character(10)
        row[8] <- langid
        row[c(7,9:10)] <- NA
        
        for(i in 1:rows) {
            if(length(obs) == 0) {
                break
            }
            ob <- obs[[i]]
            
            if(length(ob) == 6) {
                
                for(j in 1:6) {
                    
                    val <- ob[[j]]
                    
                    if(is.null(val) || as.character(val) == "NULL") {
                        val <- NA
                    }
                    row[j] <- as.character(val)
                }
                
            }
            
            else {
                
                row[6] <- NA
                k <- 1
                for(j in c(1,2,3,5,6)) {
                    val <- ob[[k]]
                    k <- k + 1
                    
                    if(is.null(val) || as.character(val) == "NULL") {
                        val <- NA
                    }
                    row[j] <- as.character(val)
                }
                
            }
            
            df[i,] <- row
        }
        df
    }
    
    make_words_df <- function(full_db) {
        
        full_db_json <- full_db
        indices <- c()
        
        for(i in 2:length(full_db)) {
            tablename <- (full_db[[i]]$name)
            
            if(grepl("words", tablename, fixed = TRUE)) {
                indices <- append(indices, i)
            }
            
        }
        
        full_db <- 0
        
        for(i in 1:length(indices)) {
            
            print(i)
            
            if(i == 1) {
                full_db <- words_to_df(full_db_json, indices[i])
            }
            
            else {
                full_db <- rbind(full_db, words_to_df(full_db_json, indices[i]))
            }
            
        }
        
        # give each sentence an ID
        full_db[,1] <- 1:nrow(full_db)
        
        return(full_db)
    }
    
    # grabs the sentences from ONE language.
    sources_to_df <- function(jsonfile, index) {
        
        columnnames <- c("id", "source", "user_id", "language_id", "date_created", "date_modified")
        
        result <- jsonfile
        tablename <- result[[index]][[2]]
        tablename <- strsplit(tablename, "_")[[1]][1]
        
        langid <- getLangId(jsonfile, tablename)
        
        obs <- result[[index]][[4]]
        rows <- length(obs)
        
        df <- data.frame(id=character(), source=character(), user_id=character(), language_id=character(),
                         date_created=character(), date_modified=character(), stringsAsFactors = FALSE)
        
        row <- character(6)
        row[4] <- langid
        row[c(3, 5, 6)] <- NA
        
        for(i in 1:rows) {
            
            if(length(obs) == 0) {
                break
            }
            
            ob <- obs[[i]]
            
            row[1] <- ob$ID
            row[2] <- ob$source
            
            df[i,] <- row
        }
        df
    }
    
    make_sources_df <- function(full_db) {
        
        full_db_json <- full_db
        indices <- c()
        
        for(i in 2:length(full_db)) {
            tablename <- (full_db[[i]]$name)
            
            if(grepl("sources", tablename, fixed = TRUE)) {
                indices <- append(indices, i)
            }
            
        }
        
        full_db <- 0
        
        for(i in 1:length(indices)) {
            
            print(i)
            
            if(i == 1) {
                full_db <- sources_to_df(full_db_json, indices[i])
            }
            
            else {
                full_db <- rbind(full_db, sources_to_df(full_db_json, indices[i]))
            }
            
        }
        
        # give each sentence an ID
        full_db[,1] <- 1:nrow(full_db)
        
        return(full_db)
    }
    
    data <- reactive({
        library(rjson)
        req(input$upload)
        
        full_db_json <- fromJSON(file=input$upload$datapath)
        sentences_table <- make_sentences_df(full_db_json)
        
        words_table <- make_words_df(full_db_json)
        
        sources_table <- make_sources_df(full_db_json)
        
        user_info_index <- 5
        user_table <- getUserTable(full_db_json, user_info_index)
        
        sentences_table <- clean_up_sentences(sentences_table, user_table)
        list(sentences_table, words_table, sources_table, user_table)
        
    })
    
    output$sentences <- DT::renderDataTable({
        data()[[1]]
    })
    
    output$words <- DT::renderDataTable({
        data()[[2]]
    })
    
    output$sources <- DT::renderDataTable({
        data()[[3]]
    })
    
    output$users <- DT::renderDataTable({
        data()[[4]]
    })
    
    output$download_sentences <- downloadHandler(
        filename = function() {
            paste0("sentences.csv")
        },
        content = function(file) {
            write.csv(data()[[1]], file, fileEncoding = "UTF-8", na='NULL', row.names = FALSE)
        }
    )
    
    output$download_words <- downloadHandler(
        filename = function() {
            paste0("words.csv")
        },
        content = function(file) {
            write.csv(data()[[2]], file, fileEncoding = "UTF-8", na='NULL', row.names = FALSE)
        }
    )
    
    output$download_sources <- downloadHandler(
        filename = function() {
            paste0("sources.csv")
        },
        content = function(file) {
            write.csv(data()[[3]], file, fileEncoding = "UTF-8", na='NULL', row.names = FALSE)
        }
    )
    
    output$download_users <- downloadHandler(
        filename = function() {
            paste0("users.csv")
        },
        content = function(file) {
            write.csv(data()[[4]], file, fileEncoding = "UTF-8", na='NULL', row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
