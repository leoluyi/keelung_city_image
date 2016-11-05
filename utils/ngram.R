ngram <- function(sentence, n) {
  gram_list <- lapply(1 : (nchar(sentence)-n+1), function(i){
    substr(sentence, i, i+n-1)
  })
  unlist(gram_list, use.names = FALSE)
} 

# ngram("統一三春", 2)

ngram_split <- function(sentence, n) {
  do.call(rbind,lapply(1 : (nchar(sentence)-n+1), function(i){
    unlist(strsplit(substr(sentence, i, i+n-1), split=""))
  }))
}

ngram_split("統一三春", 2)
