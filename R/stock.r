  getquote <- function(symbol='tsla'){
    
    query  = paste( "https://www.marketwatch.com/investing/stock/", symbol,"?mod=search_symbol",sep="")
    query
    
    #install.packages("curl")
    rawdata  = curl::curl_fetch_memory(query)
    
    htmldata = rawToChar( rawdata$content)
    
    idx     = regexpr("(?<=\"price\" content\\=\"\\$)[0-9\\.]{1,}(?=\" />)", htmldata,  perl=TRUE)
    matched = regmatches(htmldata, idx)
    price  = as.numeric((matched))
    return(price)
  }