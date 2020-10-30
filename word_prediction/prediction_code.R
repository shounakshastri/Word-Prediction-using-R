# Load training data

bi_words <- readRDS("./biwords.rds")
tri_words  <- readRDS("./triwords.rds")
quad_words <- readRDS("./quadwords.rds")
penta_words <- readRDS("./pentawords.rds")

bigram <- function(input_words){
    len <- length(input_words)
    filter(bi_words, 
           word1==input_words[len]) %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 2)) %>%
        as.character() -> predicted_word
    ifelse(predicted_word =="character(0)", "...thinking...", return(predicted_word))
}

trigram <- function(input_words){
    len <- length(input_words)
    filter(tri_words, 
           word1==input_words[len-1], 
           word2==input_words[len])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 3)) %>%
        as.character() -> predicted_word
    ifelse(predicted_word=="character(0)", bigram(input_words), return(predicted_word))
}

quadgram <- function(input_words){
    len <- length(input_words)
    filter(quad_words, 
           word1==input_words[len-2], 
           word2==input_words[len-1], 
           word3==input_words[len])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 4)) %>%
        as.character() -> predicted_word
    ifelse(predicted_word=="character(0)", trigram(input_words), return(predicted_word))
}

pentagram <- function(input_words){
    len <- length(input_words)
    filter(penta_words, 
           word1==input_words[len-3],
           word2==input_words[len-2], 
           word3==input_words[len-1], 
           word4==input_words[len])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 5)) %>%
        as.character() -> predicted_word
    ifelse(predicted_word=="character(0)", quadgram(input_words), return(predicted_word))
}

prediction <- function(input){
    input <- tibble(text = input)
    replace_reg <- "[^[:alpha:][:space:]]*"
    input <- input %>%
        mutate(text = str_replace_all(text, replace_reg, ""))
    # Find word count, separate words, lower case
    input_count <- str_count(input, boundary("word"))
    input_words <- unlist(str_split(input, boundary("word")))
    input_words <- tolower(input_words)
    # Call the matching functions
    predicted_word <- ifelse(input_count == 1, bigram(input_words),
                             ifelse (input_count == 2, trigram(input_words),
                                     ifelse(input_count == 3, quadgram(input_words), pentagram(input_words))))
    # Output
    return(predicted_word)
}