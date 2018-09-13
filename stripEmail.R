stripEmail <- function(x){sapply(strsplit(tolower(x),'@'),'[[',1)}
