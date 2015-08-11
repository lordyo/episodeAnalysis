
url <- "http://en.wikipedia.org/wiki/List_of_The_X-Files_episodes"

###################################
# The function evaluates every child of an HTML table:
# If it contains an anchor a, if returns its href attribute
# (preceded by the protocol and the base url) plus the link text.
# If not, it just returns the childs text. –
# via lukeA on http://stackoverflow.com/questions/31924546/rvest-table-scraping-including-links

library(XML)
library(httr)

getHrefs <- function(node, encoding) {  
        
        x <- xmlChildren(node)$a 
        
        if (!is.null(x)) 
                
                paste0("http://",
                       parseURI(url)$server,
                       xmlGetAttr(x, "href"),
                       " | ",
                       xmlValue(x) )
        
        else xmlValue(xmlChildren(node)$text) 
}

####################################################
# function to get the table from HTML and apply getHrefs 

getTable <- function(url, tabnum) {
        
        doc <- content(GET(url))
        tab <- readHTMLTable(doc, which = tabnum, elFun = getHrefs)
        
        tab
}


