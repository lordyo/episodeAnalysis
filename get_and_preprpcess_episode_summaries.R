
# library calls

library(XML)
library(httr)
library(rvest)
library(magrittr)
library(lubridate)
library(tm)
library(dplyr)
library(tidyr)

############## FUNCTIONS #####################

getHrefs <- function(node, encoding) {  
        # The function evaluates every child of an HTML table:
        # If it contains an anchor a, if returns its href attribute
        # (preceded by the protocol and the base url) plus the link text.
        # If not, it just returns the childs text. –
        # via lukeA on http://stackoverflow.com/questions/31924546/rvest-table-scraping-including-links
        # The function also evaluates if a cell contains a footnote and marks it with a superscript 1 (¹)
        # For X-Files, a double dagger footnotes a "mythology arc" episode
        #
        # Args:
        #  node: an html node
        #  encoding: text encoding
        #
        # Return:
        #  if the node is a link:
        #   the link text, a "¹" sign if it contains an image, and the link URL
        # 
        #  if the node is not a lin:
        #   the node text
        
        x <- xmlChildren(node)$a
        y <- ifelse(is.null(xmlChildren(node)$img), "", "¹")
        
        
        if (!is.null(x)) 
                
                paste0(xmlValue(x),
                       y,
                       "|",
                       "http://",
                       parseURI(url)$server,
                       xmlGetAttr(x, "href"))
        
        else xmlValue(xmlChildren(node)$text) 
}

########################################################

getTable <- function(url, tabnum) {
        # function to get the table from HTML and apply getHrefs 
        # 
        # Args:
        #  url: The URL
        #  tabnum: the number of the table in the overall HTML code
        #
        # Return:
        #  a table
        
        
        doc <- content(GET(url))
        tab <- readHTMLTable(doc, which = tabnum, elFun = getHrefs)
        
        tab
}

########################################################

getPlot <- function(url, t = "Plot", h = "h2") {
        # function to extract the plot from an episode page
        # xpath code from http://stackoverflow.com/questions/18167279/trying-to-get-all-p-tag-text-between-two-h2-tags
        # t refers to span ID, h refers to heading tag level
        #
        # Args:
        #  url: the URL of a Wikipedia page containing
        #  t:   a heading name (defaults to "Plot")
        #  h:   the heading formating (defaults to "h2")
        #
        # Return:
        #  the text of the paragraphs after the specified headings
        #  until the next heading, as character string
        
                
        xp = paste0("//p[preceding-sibling::", h, "[1][span='", t, "']]")
        
        eplot <- read_html(url) %>% 
                # get the nodes following the h2 "Plot" until the next h2
                html_nodes(xpath = xp) %>% 
                # strip tags
                html_text() %>%
                # concatenate vector of texts into one string
                paste(collapse = "")
        
        eplot
}

CreateDatedFilename <- function(filename, extension = "csv") {
        # Creates a string for use as file name, including current date + time
        #
        # Args:
        #  filename: fixed filename to which the date and extension will be added
        #  extension: file extension without preceding period, default CSV
        # 
        # Return:
        #  full_filename; string consisting of filename, date, time, extension
         
        now_time <- as.character(now())
        now_time <- gsub("[ :]", "_", now_time) # replace spaces and colons
        
        full_filename <- paste0(filename, "_", now_time, ".", extension)
        
        full_filename
        
}

getcastcorpus <- function(df) {
        # Creates a frequent term matrix from a data frame of text
        #
        # Args:
        #  df: data frame containing the corpus texts
        #
        # Return:
        #  ffq: frequent term matrix
        
        corp <- Corpus(DataframeSource(df), readerControl = list(language = "en"))
        
        corp <- tm_map(corp, stripWhitespace) #white spaces
        corp <- tm_map(corp, content_transformer(tolower))  #lower case
        corp <- tm_map(corp, removePunctuation, preserve_intra_word_dashes = FALSE) #regular punctuation
        corp <- tm_map(corp, removeNumbers) # numbers
        corp <- tm_map(corp, stemDocument) # stemming
        
        dtm <- DocumentTermMatrix(corp)
        ffq <- findFreqTerms(dtm)
        
        ffq
        
}


#####################################################
# main script:
# 1) construct data frame of all episodes based on series tables
# 2) get all plot summaries based on episode URLs


url <- "http://en.wikipedia.org/wiki/List_of_The_X-Files_episodes"

xf <- getTable(url, c(2:6,8:11)) # tables 2-6, 8-11 (table 7 relates to the first movie)

# save raw data
save(xf, file = paste0("raw_data/", CreateDatedFilename("raw_episodelist", "list"))) 

# consolidate column names
cnames <- c("NrInSeries","NrInSeason","Title","Director","Writer","AirDate","ProdCode","Viewers")
xf <- lapply(xf, setNames, cnames)

# collapse table list into one data frame
xf <- do.call(rbind, xf)


# data cleaning
xf2 <- tbl_df(xf)

xf2 <- xf2 %>% 
        #separate Title, Director, Writer into text and URL columns
        separate(Title, into = c("Title","epURL"), sep = "\\|", extra = "merge") %>% 
        separate(Director, into = c("Director","DirURL"), sep = "\\|", extra = "merge") %>% 
        separate(Writer, into = c("Writer","WritURL"), sep = "\\|", extra = "merge") %>% 
        #differentiate between Monster and Mythology episodes
        mutate(Mythology = grepl("¹", Title)) %>% 
        mutate(Title = sub("¹", "", Title)) %>% 
        #rearrange and drop unnecessary columns
        select(ProdCode, Title, epURL, Director, Writer, AirDate, Viewers, Mythology) %>% 
        # get rid of factors
        mutate(ProdCode = as.character(ProdCode), 
               AirDate = as.character(AirDate),
               Viewers = as.numeric(as.character(Viewers))
        ) %>% 
        # add title without spaces and punctuation for use in variable names
        mutate(VarTitle = paste(ProdCode, gsub("[^[:alnum:]]", "", Title, perl = TRUE), sep = "_"))

# get plot summaries
xf2$content <- unlist(lapply(xf2$epURL, getPlot))

# get missing plot summary from episode 7ABX04
# (plot is nested in "Synopsis" under h3 tag)
xf2$content[xf2$ProdCode == "7ABX04"] <- getPlot(
        "https://en.wikipedia.org/wiki/The_Sixth_Extinction_II:_Amor_Fati",
        t = "Plot", h = "h3") 

# save processed data
write.csv(xf2, paste0("processed_data/", CreateDatedFilename("episodes_summaries_")))

# clean up variables and functions
rm(cnames, url, getHrefs, getPlot, getTable, xf)

########################################
# get cast and character names from IMDB

casturl <- "http://www.imdb.com/title/tt0106179/fullcredits/"

# get third table (full cast)
xcastchar <- content(GET(casturl))
xcastchar <- readHTMLTable(xcastchar, which = 3)

# save raw data
save(xcastchar, file = paste0("raw_data/", CreateDatedFilename("cast_characters_", "list"))) 

# get cast and character names in separate tables
xcast <- data.frame(as.character(xcastchar$V2), stringsAsFactors = FALSE)
xchar <- data.frame(as.character(xcastchar$V4), stringsAsFactors = FALSE)

#get cast and character names as separate vectors
xfcastterms <- getcastcorpus(xcast)
xfcharterms <- getcastcorpus(xchar)

# manual list of non-names found in character description (e.g. "airplane pilot")
remcast <- read.csv("remcast.csv")

# remove all non-name terms from cast and character vectors
xfcastterms <- setdiff(xfcastterms, remcast)
xfcharterms <- setdiff(xfcharterms, remcast)

# remove "scully" from cast vector (it's both a cast an character name)
xfcastterms <- setdiff(xfcastterms, "sculli")

# save processed data
write.csv(xcast, paste0("processed_data/", CreateDatedFilename("cast")))
write.csv(xcast, paste0("processed_data/", CreateDatedFilename("characters")))
write.csv(xfcastterms, paste0("processed_data/", CreateDatedFilename("cast_terms", "vector")))
write.csv(xfcharterms, paste0("processed_data/", CreateDatedFilename("character_terms", "vector")))

# cleanup
rm(casturl, xcast, xchar, xcastchar, getcastcorpus, remcast)

#######################################
# Corpus construction and text cleanup
# http://stackoverflow.com/questions/19850638/tm-reading-in-data-frame-and-keep-texts-id


# create Corpus
m <- list(id = "VarTitle", content = "content")
myReader <- readTabular(mapping = m)
xfcorpus <- Corpus(DataframeSource(xf2), readerControl = list(reader = myReader, language = "en"))

# manually attach the product code for later identificaiton
for (i in 1:length(xfcorpus)) {
        attr(xfcorpus[[i]], "ID") <- xf2$VarTitle[i]
}

# clean up for NLP
xfcorpus <- tm_map(xfcorpus, stripWhitespace) #white spaces
xfcorpus <- tm_map(xfcorpus, content_transformer(tolower))  #lower case
xfcorpus <- tm_map(xfcorpus, removeWords, stopwords("english")) #stop words
xfcorpus <- tm_map(xfcorpus, removePunctuation, preserve_intra_word_dashes = FALSE) #regular punctuation
xfcorpus <- tm_map(xfcorpus, content_transformer(function(row) iconv(row, "latin1", "ASCII", sub=""))) # non-ascii chars
xfcorpus <- tm_map(xfcorpus, removeNumbers) # numbers
xfcorpus <- tm_map(xfcorpus, stemDocument) # stemming
xfcorpus <- tm_map(xfcorpus, removeWords, xfcastterms) #remove names from cast list

# Create Document Term Matrix as data frame
xfDTMfull <- DocumentTermMatrix(xfcorpus, control = list(wordLengths = c(3,15)))
xfDTM <- data.frame(as.matrix(xfDTMfull), stringsAsFactors = TRUE)

# Merge DTM with episode data => full data set including character names
xfmerged <- merge(xf2, xfDTM, by.x = "VarTitle", by.y = "row.names" )

# save processed data
save(xfcorpus, file = paste0("processed_data/", CreateDatedFilename("episode_summaries_incl_characters","corpus")))
save(xfDTMfull, file = paste0("processed_data/", CreateDatedFilename("DTM_incl_characters","dtm")))
write.csv(xfDTM, paste0("processed_data/", CreateDatedFilename("DTM_incl_characters")))
write.csv(xfmerged, paste0("processed_data/", CreateDatedFilename("DTM_incl_characters_and_episode_info")))

# construct a similar data set without character names
xfcorpus <- tm_map(xfcorpus, removeWords, xfcharterms) #remove names from character list 
xfDTMnnfull <- DocumentTermMatrix(xfcorpus, control = list(wordLengths = c(3,15)))
xfDTMnn <- data.frame(as.matrix(xfDTMnnfull), as.is = TRUE)
xfmergednn <- merge(xf2, xfDTMnn, by.x = "VarTitle", by.y = "row.names" )

# save processed data
save(xfcorpus, file = paste0("processed_data/", CreateDatedFilename("episode_summaries_excl_characters","corpus")))
save(xfDTMnnfull, file = paste0("processed_data/", CreateDatedFilename("DTM_excl_characters","dtm")))
write.csv(xfDTMnn, paste0("processed_data/", CreateDatedFilename("DTM_excl_characters")))
write.csv(xfmergednn, paste0("processed_data/", CreateDatedFilename("DTM_excl_characters_and_episode_info")))

# cleanup
rm(i, m, xfcastterms, xfcharterms, myReader)

