
###################################
# The function evaluates every child of an HTML table:
# If it contains an anchor a, if returns its href attribute
# (preceded by the protocol and the base url) plus the link text.
# If not, it just returns the childs text. –
# via lukeA on http://stackoverflow.com/questions/31924546/rvest-table-scraping-including-links
# The function also evaluates if a cell contains a footnote and marks it with a superscript 1 (¹)
# For X-Files, a double dagger footnotes a "mythology arc" episode

library(XML)
library(httr)

getHrefs <- function(node, encoding) {  
        
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
# function to get the table from HTML and apply getHrefs 


getTable <- function(url, tabnum) {
        
        doc <- content(GET(url))
        tab <- readHTMLTable(doc, which = tabnum, elFun = getHrefs)
        
        tab
}

########################################################
# function to extract the plot from an episode page
# xpath code from http://stackoverflow.com/questions/18167279/trying-to-get-all-p-tag-text-between-two-h2-tags
# t refers to span ID, h refers to heading tag level

getPlot <- function(url, t = "Plot", h = "h2") {
        
        library(rvest)
        library(magrittr)
        
        xp = paste0("//p[preceding-sibling::", h, "[1][span='", t, "']]")
        
        eplot <- html(url) %>% 
                # get the nodes following the h2 "Plot" until the next h2
                html_nodes(xpath = xp) %>% 
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
castnames <- c("david", "duchovny",
               "gillian", "anderson",
               "robert","patrick",
               "annabeth", "gish",
               "mitch", "pileggi",
               "william", "davis",
               "nicholas", "lea",
               "mimi","rogers",
               "chris","owens",
               "james","pickens")

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

# clean up variables
rm(cnames)

# get missing plot summary from episode 7ABX04
# (plot is nested in "Synopsis" under h3 tag)
xf2$content[xf2$ProdCode == "7ABX04"] <- getPlot(
        "https://en.wikipedia.org/wiki/The_Sixth_Extinction_II:_Amor_Fati",
        t = "Plot", h = "h3") 

#######################################
# Corpus construction and text cleanup
# http://stackoverflow.com/questions/19850638/tm-reading-in-data-frame-and-keep-texts-id

library(tm)

# create Corpus
m <- list(id = "VarTitle", content = "content")
myReader <- readTabular(mapping = m)
xfcorpus <- Corpus(DataframeSource(xf2), readerControl = list(reader = myReader, language = "en"))

# manually attach the product code for later identificaiton
for (i in 1:length(xfcorpus)) {
        attr(xfcorpus[[i]], "ID") <- xf2$VarTitle[i]
}

## alternative with locally saved files (unnecessary)
# write.csv(x = xf2$content, file = "plot/")
# 
# sapply(row.names(xf2), 
#        function (x) write.table(xf2[[x,8]], file=paste("plot\\", xf2[[x,1]], ".txt", sep=""),
#                               quote = FALSE, row.names = FALSE, col.names = FALSE
#                                 )
# )
# 
# xfcorpus <- VCorpus(DirSource("plot", encoding = ""),
#                        readerControl = list(language = "en"))


# clean up for NLP
xfcorpus <- tm_map(xfcorpus, stripWhitespace) #white spaces
xfcorpus <- tm_map(xfcorpus, content_transformer(tolower))  #lower case
xfcorpus <- tm_map(xfcorpus, removeWords, stopwords("english")) #stop words
xfcorpus <- tm_map(xfcorpus, removeWords, castnames) #stop words
xfcorpus <- tm_map(xfcorpus, removePunctuation, preserve_intra_word_dashes = FALSE) #regular punctuation
xfcorpus <- tm_map(xfcorpus, content_transformer(function(row) iconv(row, "latin1", "ASCII", sub=""))) # non-ascii chars
xfcorpus <- tm_map(xfcorpus, removeNumbers) # numbers
xfcorpus <- tm_map(xfcorpus, stemDocument) # stemming

# Create Document Term Matrix as data frame
xfDTM <- DocumentTermMatrix(xfcorpus, control = list(wordLengths = c(3,15)))
xfDTM <- data.frame(as.matrix(xfDTM), as.is = TRUE)

# Merge DTM with episode data (unsure if this df will be used later)
xfmerged <- merge(xf2, xfDTM, by.x = "VarTitle", by.y = "row.names" )


#################################################
# Exploratory analysis
# Word counts incl. comparison between Monster and Mythology episodes

library(dplyr)

# function to set counts to boolean values
numToBool <- function(x) {
        ifelse(is.numeric(x), x > 0, NA)
}

xfmergedTF <- xfmerged

# change word counts to Boolean values ()
xfmergedTF[ , -c(1:10)] <- as.data.frame(lapply(
        xfmergedTF[ , -c(1:10)],FUN = function(x) {sapply(x, FUN=numToBool)}))

nxf <- nrow(xfmergedTF)

# get total Boolean counts (all episodes)
xfcountAll <- xfmergedTF[ , -c(1:10)] %>% 
        colSums() %>% 
        sort(decreasing = TRUE) %>% 
        `/`(nxf)  #divide by number of episodes

head(xfcountAll, 50)

# get Boolean counts for Monster and Mythology episodes
xfcountTFMonster <- filter(xfmergedTF, Mythology == FALSE)
xfcountTFMyth <- filter(xfmergedTF, Mythology == TRUE)

# get number of episodes
nmyth = nrow(xfcountTFMyth)
nmon = nrow(xfcountTFMonster)

# function to create a ranked list of Boolean counts
prepCountList <- function(df, n) {
        
        df <- df[ , -c(1:10)] %>% 
                colSums() %>% 
                sort(decreasing = TRUE) %>% 
                `/`(n) %>% 
                as.data.frame()
        
        df <- mutate(df, term = rownames(df))
        df$rank <- 1:nrow(df)
        
        colnames(df) <- c("part", "term", "rank")
        
        df
}

# create ranked lists for Monster and Mythology episodes
xfcountTFMonster <- prepCountList(xfcountTFMonster, nmon)
xfcountTFMyth <- prepCountList(xfcountTFMyth, nmyth)

# join the two ranked lists
xfcountmerged <- full_join(xfcountTFMyth, xfcountTFMonster, by = "term")

# create data for slopegraph (top 30 terms for both episode types)
# exclude difference below 10%
xfcountmerged <- xfcountmerged %>% 
        filter(rank.x <= 30 | rank.y <= 30) %>% 
        select(myth = part.x, monster = part.y, term) %>%
        filter(abs(myth-monster) > .1) %>% 
        mutate(myth = round(myth*100,0), monster = round(monster*100,0))

# create slopegraph
source("slopegraph.R")

with(xfcountmerged, slopegraph(myth, monster, term))

###########################################################
# Hierarchical clustering
# Ward Hierarchical Clustering
# results not very interesting

# rownames(xfmerged) <- xfmerged$VarTitle
# 
# dep <- dist(xfmerged[ , -c(1:10)], method = "euclidean") # distance matrix
# fit <- hclust(dep, method="ward") 
# par(cex = 0.6)
# plot(fit, nodePar = list(cex = 0.1)) # display dendogram
# groups <- cutree(fit, k=3) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit, k=3, border="red")
# par(cex = 1)
# 
# xfmergedFlip <- t(xfmerged[ , -c(1:10)])
# xfmergedFlip <- as.data.frame(xfmergedFlip)
# rownames(xfmergedFlip) <- colnames(xfmerged[ , -c(1:10)])
# xfmergedFlip$tot <- rowSums(xfmergedFlip)
# xfmergedFlip$VarTitle <- rownames(xfmergedFlip)
# xfmergedFlip <- arrange(xfmergedFlip, desc(tot))
# rownames(xfmergedFlip) <- xfmergedFlip$VarTitle
# xfmergedFlip$VarTitle <- NULL
# xfmergedFlip <- xfmergedFlip[1:100,]
# 
# 
# dword <- dist(xfmergedFlip[ , -c(1:10)], method = "euclidean") # distance matrix
# fit <- hclust(dword, method="ward") 
# plot(fit) # display dendogram
# groups <- cutree(fit, k=5) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit, k=5, border="red")


#################################################
# Prepare data for Latent Semantic Analysis (LSA)
# Reference: http://nm.wu-wien.ac.at/research/publications/b675.pdf

# remove temrs with less than 5 global counts
xfsums <- xfDTM[colSums(xfDTM) > 5]
#xfsums <- xfDTM  #to use all words

library(lsa)

# weigh DTM - TF-IDF 
xfDTMweigh <- lw_tf(xfsums) * gw_idf(xfsums)

# alternative: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.69.9244&rep=rep1&type=pdf
# xfDTMweigh <- lw_logtf(xfsums) * gw_entropy(xfsums)

#################################################
# exploratory analysis: create heat map of weighted DTM

library(pheatmap)

pheatmap(xfDTMweigh)
pheatmap(xfsums, kmeans_k = 10)

#missing values in 7ABX04




xfspace <- lsa(xfDTMweigh, dims = dimcalc_share())
xfmatrix <- as.textmatrix(xfspace)

# get singular values
# xfsvd <- xfspace[1][1]
# xfsvd <- t(xfspace$tk)

# word distance matrix
# lsaMatrix <- diag(xfspace$sk) %*% t(xfspace$dk)
# distMatrix <- 1 - cosine(lsaMatrix)

# episode distance matrix
# from http://stackoverflow.com/questions/15229584/compute-cosine-similarities-between-documents-in-semantic-space-using-r-lsa-pac
lsaMatrix <- diag(xfspace$sk) %*% t(xfspace$tk)
distMatrix <- 1 - cosine(lsaMatrix)




