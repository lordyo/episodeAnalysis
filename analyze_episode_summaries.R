
# library calls

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


prepCountList <- function(df, n) {
        # function to create a ranked list of relative Boolean counts
        #
        # Args:
        #  df: data frame based on the data format in xfmerged
        #  n:  count of episodes
        #
        # Return:
        #  df: a data frame of relative occurence (part),
        #      term and rank
        
        df <- df[ , -c(1:10)] %>%               # get rid of metadata
                colSums() %>%                   # get overall term counts
                sort(decreasing = TRUE) %>%     # sort high to low
                `/`(n) %>%                      # divide by episode count
                as.data.frame()                 # make data frame
        
        df <- mutate(df, term = rownames(df))   # get row names as variable
        df$rank <- 1:nrow(df)                   # add ranks
        
        colnames(df) <- c("part", "term", "rank") # rename col names
        
        df
}


numToBool <- function(x) {
        # function to set counts to boolean values
        #
        # Args:
        #  x: an integer
        #
        # Return:
        #  a Boolean value (>=1 TRUE, <1 FALSE) 
        
        ifelse(is.numeric(x), x > 0, NA)
}

Plot_Timeline <- function(df, titletext) {
        # function to plot a line graph of character
        # appearance by season
        # 
        # Args:
        #  df: data frame with cols season, term (name) and count (nr of appearances)
        #
        # Return:
        #  ggplot2 object (line graph)
        
        library(ggthemes)
        
        g <- ggplot(data = df, aes(x = season, y = count, group = term)) +
                geom_line(aes(color = term)) +
                ggtitle(titletext) +
                theme_few() +
                scale_colour_few()
        
        g


#################################################
# Exploratory analysis
# Word counts incl. comparison between Monster and Mythology episodes


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

# show terms with largest counts
head(xfcountAll, 50)


# get Boolean counts for Monster and Mythology episodes
xfcountTFMonster <- filter(xfmergedTF, Mythology == FALSE)
xfcountTFMyth <- filter(xfmergedTF, Mythology == TRUE)

# get number of episodes
nmyth = nrow(xfcountTFMyth)
nmon = nrow(xfcountTFMonster)

# create ranked lists for Monster and Mythology episodes
xfcountTFMonster <- prepCountList(xfcountTFMonster, nmon)
xfcountTFMyth <- prepCountList(xfcountTFMyth, nmyth)

# join the two ranked lists
xfcountmerged <- full_join(xfcountTFMyth, xfcountTFMonster, by = "term")

# save results
write.csv(xfcountAll, paste0("results/", CreateDatedFilename("relative_count_of_terms_by_episode")))
write.csv(xfcountmerged, paste0("results/", CreateDatedFilename("relative_count_of_terms_by_episode_and_type")))

# create data for slopegraph (top 30 terms for both episode types)
# exclude difference below 10%
xfcountmerged <- xfcountmerged %>% 
        filter(rank.x <= 30 | rank.y <= 30) %>% 
        select(myth = part.x, monster = part.y, term) %>%
        filter(abs(myth-monster) > .1) %>% 
        mutate(myth = round(myth*100,0), monster = round(monster*100,0))

# create slopegraph
source("slopegraph.r")

with(xfcountmerged, slopegraph(myth, monster, term, "slopegraph.png"))

# cleanup
rm(xfcountmerged, xfcountTFMonster, xfcountTFMyth, xfmergedTF,
   nmon, nmyth, nxf, xfcountAll, numToBool, prepCountList, slopegraph)


#################################################################
# Names by season

xfnames <- c(
        # X-Files agents
        "mulder",
        "sculli", # written this way for stemming reasons
        "doggett",
        "rey", # agent reyes, stemmed
        
        # the bad guys
        "smoke", # the cigarette smoking man
        "krycek",
        "elder",
        "rohrer",
        
        # informants
        "covarrubia",
        "throat", # deep throat
        "kritschgau",
        # "X", deep throat's successor, can't be distinguished in our texts
        
        # the FBI
        "skinner",
        "fowley",
        "kersh",
        "spender",
        
        # the Lone Gunmen
        "byer",
        "frohik",
        "lang"
)

# construct DTM
xftimeline <- select_(xfmerged, .dots = c("ProdCode", xfnames))

# set counts to Boolean
xftimeline[ , -1][xftimeline[ , -1] > 1] <- 1
xftimeline[ , -1][xftimeline[ , -1] == 0] <- 0

# construct summary by season
xftimeline <- xftimeline %>% 
        mutate(Season = as.numeric(substr(ProdCode,1,1))) %>%  # get season number
        select(-ProdCode) %>% # Production code no longer needed
        group_by(Season) %>%  # group and summarise by season
        summarise_each(funs(sum)) %>% 
        t() %>%  # rotate the data frame (cols to rows)
        as.data.frame() # matrix to df

xftimeline <- xftimeline[-1 ,] # get rid of season numbers
colnames(xftimeline) <- 1:ncol(xftimeline) # add season numbers as col names

# get row character names as own column
xftimeline$term <- rownames(xftimeline)

# create long format 
library(tidyr)
xftimeline <- gather(xftimeline, "season", "count", 1:9)

Plot_Timeline(filter(xftimeline, term %in% c("mulder", "sculli", "doggett", "rey")), "X-Files Agents")

Plot_Timeline(filter(xftimeline, term %in% c("smoke", "krycek", "elder", "rohrer")), "The Syndicate")

Plot_Timeline(filter(xftimeline, term %in% c("throat", "covarrubia", "kritschgau")), "Informants")

Plot_Timeline(filter(xftimeline, term %in% c("skinner", "fowley", "kersh", "spender")), "FBI Agents")

Plot_Timeline(filter(xftimeline, term %in% c("frohik", "byer", "lang")), "Lone Gunmen")

########################################
# occurence of characters

# get DTM  of all terms including cast (strip the metadata)
termvector <- colnames(xfmerged[ , -(1:10)])

# create vector of character names
# make sure all character names are in the list of terms
xfcharacters <- intersect(xfcharterms, termvector)

# get rid off non-names using the remcast list
xfcharacters <- setdiff(xfcharacters, remcast)

# reduce the DTM to character terms only
xfcharacters <- select_(xfmerged, .dots = xfcharacters)

# counts to Boolean
xfcharacters[ , -1][xfcharacters[ , -1] > 1] <- 1
xfcharacters[ , -1][xfcharacters[ , -1] == 0] <- 0

# construct data frame of terms and Boolean counts
xfcharacters <- data.frame(ct = colSums(xfcharacters), name = names(xfcharacters))

# sort descending by count
xfcharacters <- arrange(xfcharacters, desc(ct))
        
        
