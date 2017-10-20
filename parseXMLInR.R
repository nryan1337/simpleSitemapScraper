
#======================================
# Sitemaps for Investigatory Journalism
#
# or just getting free stuff from coffee shops!
#======================================


#---------------------------------------
# install and load the necessary R packages
#---------------------------------------

# install packages
install.packages(c("XML","rvest","tm","wordcloud")) 


# load packages we need for our analysis
library(XML) 
library(rvest)
library(wordcloud)
library(tm)


#---------------------------------------
# parse the XML doc and print all the pages (including hidden pages)
#---------------------------------------

# enter the website sitemap you want to scrape here
# note: you will probably have to alter this script because every site looks a bit
# different, so requires customisation when you extract info
#-------------------------------------------------

xml.url <- "http://www.indulgecafe.com.au/sitemap.xml"  

# parse the XML file so we can analyse it, it has a tree structure which is kinda cool

xmlfile <- xmlTreeParse(xml.url) 

# Let's jump to the top of the XML tree and navigate from there

xmltop = xmlRoot(xmlfile) 

# have a look at the XML-code of the first subnodes:

print(xmltop)[1:2]

siteMapData <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))



print("here are the pages of the site (including hidden pages)")
print(siteMapData)

#=============================================


#---------------------------------------
# run through the list of site pages, 
# extract content and print a wordcloud of the content
#---------------------------------------

# pages and hidden pages from above for us to scrape
sitesToScrape <- as.character(siteMapData)

# loop through each page and extract words inside the p tag
wordList <- list()

for (site in sitesToScrape) {
  print(site)
  
  scraping_wiki <- read_html(site)
  
  t <- scraping_wiki %>%
    html_nodes("p") %>% # extract all the "p" tags from the html doc
    html_text()
  
  wordList <- append(wordList, t)
}


wordList <- unlist(wordList)


# this is a bit complicated, but really it is just code to form a word cloud.

ap.corpus <- Corpus(DataframeSource(data.frame(as.character(wordList))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.corpus <- tm_map(ap.corpus, PlainTextDocument)
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")

# now print the wordcloud we have created

wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)




