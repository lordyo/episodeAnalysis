
# library calls

library(slam)
library(lsa)
library(topicmodels)


#################################################
# Exploratory analysis
# Word counts incl. comparison between Monster and Mythology episodes


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
source("slopegraph.r")

with(xfcountmerged, slopegraph(myth, monster, term))


# cleanup
rm(xfcountmerged, xfcountTFMonster, xfcountTFMyth, xfmergedTF,
   nmon, nmyth, nxf, xfcountAll, numToBool, prepCountList, slopegraph)


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

# remove terms with less than 5 global counts
#xfsums <- xfDTM[colSums(xfDTM) > 5]

#xfsums <- xfDTM  #to use all words

summary(col_sums(xfDTMnnfull))

# NOT RUN: input must be real DTM, not matrixed verion --- correct this

xfterm_tfidf <- tapply(xfDTMnnfull$v/row_sums(xfDTMnnfull)[xfDTMnnfull$i], xfDTMnnfull$j, mean) * 
        log2(nDocs(xfDTMnnfull)/col_sums(xfDTMnnfull > 0))

summary(xfterm_tfidf)

xfsums <- xfDTMnnfull[,xfterm_tfidf >= 0.036]

xfsums <- data.frame(as.matrix(xfsums), stringsAsFactors = FALSE)



# weigh DTM - TF-IDF 
xfDTMweigh <- lw_tf(xfsums) * gw_idf(xfsums)

# alternative: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.69.9244&rep=rep1&type=pdf
# xfDTMweigh <- lw_logtf(xfsums) * gw_entropy(xfsums)

#################################################
# exploratory analysis: create heat map of weighted DTM
# not useful

# library(pheatmap)
# 
# pheatmap(xfDTMweigh)
# pheatmap(xfsums, kmeans_k = 10)

#################################################
# 

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

# determine clusters
wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(distMatrix, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(distMatrix, 7) # 7 cluster solution
# get cluster means 
aggregate(distMatrix,by=list(fit$cluster),FUN=mean)
# append cluster assignment
distMatrix <- data.frame(distMatrix, fit$cluster)

cluslist <- data.frame(VarTitle = rownames(distMatrix), Cluster = distMatrix$fit.cluster)
cluslist <- cluslist %>%
        inner_join(xfmerged[ , c(1,9)], by = "VarTitle") %>% 
        arrange(Cluster, VarTitle)

table(cluslist$Mythology, cluslist$Cluster)

##################################################
# LDA 
# reference: https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

xfDTM2 <- DocumentTermMatrix(xfcorpus, control = list(wordLengths = c(3,15)))
dim(xfDTM2)

summary(col_sums(xfDTM2))

xfterm_tfidf <- tapply(xfDTM2$v/row_sums(xfDTM2)[xfDTM2$i], xfDTM2$j, mean) * 
        log2(nDocs(xfDTM2)/col_sums(xfDTM2 > 0))
summary(xfterm_tfidf)

xfDTM2 <- xfDTM2[,xfterm_tfidf >= 0.036]
xfDTM2 <- xfDTM2[row_sums(xfDTM2) > 0,]
summary(col_sums(xfDTM2))
dim(xfDTM2)


k <- 16
SEED <- 873
xf_tm <- list(VEM = LDA(xfDTM2, k = k, control = list(seed = SEED)))

xfTopic <- topics(xf_tm[["VEM"]], 1)
xfTerms <- terms(xf_tm[["VEM"]], 20)
xfTerms

cluslist2 <- data.frame(VarTitle = names(xfTopic), Cluster = xfTopic, stringsAsFactors = FALSE)

cluslist2 <- cluslist2 %>%
        inner_join(xfmerged[ , c(1,9)], by = "VarTitle") %>% 
        arrange(Cluster, VarTitle)

table(cluslist2$Mythology, cluslist2$Cluster)

#################################################################
# Names by season

xfnames <- c("mulder",
             "sculli", #written this way for stemming reasons
             "skinner",
             "smoke",
             "krycek",
             "doggett",
             "rey",
             "fowley",
             "kersh",
             "spender",
             "byer",
             "frohik",
             "lang",
             "covarrubia",
             "throat",
             "elder",
             "rohrer")

xftimeline <- select_(xfmerged, .dots = c("ProdCode", xfnames))
xftimeline <- xftimeline %>% 
        mutate(Season = as.numeric(substr(ProdCode,1,1))) %>% 
        select(-ProdCode) %>% 
        group_by(Season) %>% 
        summarise_each(funs(sum))


