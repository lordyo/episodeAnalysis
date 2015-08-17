
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
                
                paste0(xmlValue(x),
                       "|",
                       "http://",
                       parseURI(url)$server,
                       xmlGetAttr(x, "href"))
        
        else xmlValue(xmlChildren(node)$text) 
}

########################################################
# function to get the table from HTML and apply getHrefs 


getTable <- function(url, tabnum) {
        
        doc <- content(GET(url))
        tab <- readHTMLTable(doc, which = tabnum, elFun = getHrefs)
        
        tab
}

########################################################
# function to extract the plot from an episode page
# xpath code from http://stackoverflow.com/questions/18167279/trying-to-get-all-p-tag-text-between-two-h2-tags


getPlot <- function(url) {
        
        library(rvest)
        library(magrittr)
        
        eplot <- html(url) %>% 
                # get the nodes following the h2 "Plot" until the next h2
                html_nodes(xpath = "//p[preceding-sibling::h2[1][span='Plot']]") %>% 
                # strip tags
                html_text() %>%
                # concatenate vector of texts into one string
                paste(collapse = "")
        
        eplot
}


#####################################################
# main script:
# 1) construct data frame of all episodes based on series tables
# 2) get all plot summaries based on episode URLs


url <- "http://en.wikipedia.org/wiki/List_of_The_X-Files_episodes"

xf <- getTable(url, c(2:6,8:11)) # tables 2-6, 8-11 (table 7 relates to the first movie)

# consolidate column names
cnames <- c("NrInSeries","NrInSeason","Title","Director","Writer","AirDate","ProdCode","Viewers")
xf <- lapply(xf, setNames, cnames)

# collapse table list into one data frame
xf <- do.call(rbind, xf)

# data cleaning
library(dplyr)
library(tidyr)
xf2 <- tbl_df(xf)

xf2 <- xf2 %>% 
        #separate Title, Director, Writer into text and URL columns
        separate(Title, into = c("Title","epURL"), sep = "\\|", extra = "merge") %>% 
        separate(Director, into = c("Director","DirURL"), sep = "\\|", extra = "merge") %>% 
        separate(Writer, into = c("Writer","WritURL"), sep = "\\|", extra = "merge") %>% 
        #rearrange and drop unnecessary columns
        select(ProdCode, Title, epURL, Director, Writer, AirDate, Viewers)

# get plot summaries
xf2$content <- unlist(lapply(xf2$epURL, getPlot))

