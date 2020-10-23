library(shiny)
library(dplyr)
library(stringr)
library(quanteda)
library(textclean)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    i <- readRDS("Unigram.RDS")
    j <- readRDS("Unigram1.RDS")
    uni <- rbind(i, j)
    k <- readRDS("Bigram.RDS")
    l <- readRDS("Bigram1.RDS")
    bi <- rbind(k, l)
    m <- readRDS("Trigram.RDS")
    n <- readRDS("Trigram.RDS")
    tri <- rbind(m, n)
    
    # function to return highly probable previous word given a word
    biF <- function(w1, n = 20) {
        pwords <- bi[w1]
        if (any(is.na(pwords)))
            return(uniW(n))
        if (nrow(pwords) > n)
            return(pwords[1:n, word2])
        count <- nrow(pwords)
        unW <- uniW(n)[1:(n - count)]
        return(c(pwords[, word2], unW))
    }
    
    # The prediction app
    prediction <- function(str){
        if (str == ""){
            list_word2 <- list(first = "",
                              second = "",
                              third = "",
                              fourth = "",
                              fifth = "",
                              sixth = "")
        }
        else{
            strip(str, apostrophe.remove = TRUE)
            str <- str_trim(str)
            str <- word(str, -1)
            len = str_count(str,"\\w+")
            if(len == 1){
                tokens <- tokens(x = char_tolower(str))
                tokens <- tokenize_sentence(tokens[[1]])
                tokens <- as.character(tokens)
                predicted <- bi %>% filter(word1 == tokens[1])
                predicted2 <- tri %>% filter(word1 == tokens[1])
                predicted3 <- tri %>% filter(word2 == tokens[1])
                list_word2 <- list(first = predicted[1, 2],
                              second = predicted[2, 2],
                              third = predicted[3, 2],
                              fourth = predicted2[1, 2],
                              fifth = predicted2[2, 2],
                              sixth = predicted3[1, 3])
            }
            else if(len > 1){
                str2 <- word(str, -2:-1)
                tokens <- tokens(x = char_tolower(str2))
                tokens <- tokenize_sentence(tokens)
                tokens <- as.character(tokens)
                predicted <- bi %>% filter(word1 == tokens[2])
                predicted2 <- tri %>% filter(word1 == tokens[2])
                predicted3 <- tri %>% filter(word2 == tokens[2])
                list_word2 <- list(first = predicted3[1, 3],
                                  second = predicted3[2, 3],
                                  third = predicted3[3, 3],
                                  fourth = predicted2[1, 2],
                                  fifth = predicted2[1, 2],
                                  sixth = predicted[1, 2])
            }
        }
        return(list_word2)
    }
    
    pbutton <- reactive({
        pb1 = prediction(input$inputString)
        pb1
    })
    
   
    
    output$buttons <- renderUI( {
        div(class="btn-group btn-group-justified",
            actionLink(inputId = "predict1", class="btn btn-default", label = pbutton()$first),
            actionLink(inputId = "predict2", class="btn btn-default", label = pbutton()$second),
            actionLink(inputId = "predict3", class="btn btn-default", label = pbutton()$third),
            actionLink(inputId = "predict4", class="btn btn-default", label = pbutton()$fourth),
            actionLink(inputId = "predict5", class="btn btn-default", label = pbutton()$fifth),
            actionLink(inputId = "predict6", class="btn btn-default", label = pbutton()$sixth)
        )
    })
    
    observeEvent(input$predict1, {
        updateTextInput(session, "inputString", value = paste(input$inputString, pbutton()$first))
    }) 
    observeEvent(input$predict2, {
        updateTextInput(session, "inputString", value = paste(input$inputString, pbutton()$second))
    }) 
    observeEvent(input$predict3, {
        updateTextInput(session, "inputString", value = paste(input$inputString, pbutton()$third))
    }) 
    observeEvent(input$predict4, {
        updateTextInput(session, "inputString", value = paste(input$inputString, pbutton()$fourth))
    })
    observeEvent(input$predict5, {
        updateTextInput(session, "inputString", value = paste(input$inputString, pbutton()$fifth))
    })
    observeEvent(input$predict6, {
        updateTextInput(session, "inputString", value = paste(input$inputString, pbutton()$sixth))
    })
    
})