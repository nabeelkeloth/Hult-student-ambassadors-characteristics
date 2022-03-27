#' Title:Hult International Business School Student Spotlight 
#' Purpose: To evaluate Hult student ambassadors characteristics
#' Author:Nabeel keloth
#' Date: March 1 2021

# Setting the working directory 
setwd("~/Desktop/Portfolio/NLP_HULT")

# Loading required libraries 
library(tm)
library(wordcloud)
library(qdap)
library(RColorBrewer)
library(ggplot2)
library(plotrix)
library(ggthemes)
library(ggalt)
library(readr)
library(pbapply)

# To limit errors please run this code

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern="\u00A0", replacement = "")
  return(corpus)
}



# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Create custom stop words
# Stopwords
stops <- c(stopwords('SMART'), 'hult', 'university', 
           'things', 'years', 'born', 'student', 'year', 
           'business', 'london', 'campus', 'asia', 'international',
           'boston', 'india', 'school', 'europe', 'master', 
           'degree', 'mba', 'day', 'ambassador',
           'ive', 'hey', 'lets', 'hults', 'hong kong', 'guys', 'bit')

#Importing the data set 
studentdf <- read.csv("final_student_data.csv")

#concatenating bio and interests columns for the analysis
studentdf$allText <- paste(studentdf$bio, studentdf$interests)

# As of tm version 0.7-3 tabular was deprecated
names(studentdf)[1] <-'doc_id' 


# Exploratory Data Analysis

#plotting the students based on the region 
freqRegion <- freq_terms(studentdf$namSorCountry.region, stopwords = stopwords('SMART'))
plot(freqRegion)

# Create the data for the campus chart
student <- c(28, 15, 34, 8)
campus <- c("Boston","Dubai","London","San Francisco")

# Plot the bar chart for campus 
barplot(student,names.arg=campus,xlab="campus",ylab="No of students",col="blue",
        main="Campus chart",border="red")


#Create data for the program chart 
student_p <- c(30, 11, 30, 6,8) 
program <- c("BBA","MBA", "MIB", "MSBA", "Other" )

# Plot the bar chart for program 
barplot(student_p,names.arg=program,xlab="Program",ylab="No of students",col="green",
        main="Program chart")

#Preparing region and campus variables 

#region 
region = studentdf$namSorCountry.region
text =  studentdf$allText
regiondf <- data.frame(region,text)

#campus 
campus = studentdf$campus
text = studentdf$allText
campusdf <- data.frame(campus, text)


#sub setting by London campus and rest fo the campuses 

london_camp <- subset(campusdf, campusdf$campus == 'London')
london_freq <- freq_terms(london_camp,10,  stopwords = stops)
london_freq

rest_camp <- subset(campusdf, campusdf$campus != 'London')
rest_freq <- freq_terms(rest_camp, 10,  stopwords = stops)
rest_freq

#sub setting by the region Asia and rest of the regions 

asia <- subset(regiondf, regiondf$region == 'Asia')
asia_freq <- freq_terms(asia, 10, stopwords = stops)
asia_freq 

rest_r <- subset(regiondf, regiondf$region != 'Asia')
rest_r_freq <- freq_terms(rest_r, 10, stopwords = stops)
rest_r_freq 

#Creating a pyramid plot to identify the common word in  bio and interests 
# Bring in our supporting functions
source('~/Desktop/Portfolio /NLP_HULT/ZZZ_supportingFunctions.R')

# Read in Data, clean & organize.  Wrapped in another function for you!
textA <- cleanMatrix(pth             = 'final_student_data.csv',
                     columnName      = 'bio',
                     collapse        = T, 
                     customStopwords = stops,
                     type = 'TDM', # TDM or DTM
                     wgt = 'weightTf') # weightTfIdf or weightTf

textB <- cleanMatrix(pth        = 'final_student_data.csv',
                     columnName = 'interests',
                     collapse   = T,
                     customStopwords = stops,
                     type = 'TDM', # TDM or DTM
                     wgt = 'weightTf')

df        <- merge(textA, textB, by ='row.names')
names(df) <- c('terms', 'Bio', 'Interests')

# Calculate the absolute differences among in common terms
df$diff <- abs(df$Bio - df$Interests)

# Organize df for plotting
df<- df[order(df$diff, decreasing=TRUE), ]
top20 <- df[1:20, ]

# Pyarmid Plot
pyramid.plot(lx         = top20$Bio, #left
             rx         = top20$Interests,    #right
             labels     = top20$terms,  #terms
             top.labels = c('Bio', 'Terms', 'Interests'), #corpora
             gap        = 10, # space for terms to be read
             main       = 'Words in Common', # title
             unit       = 'wordFreq') 


#Comparison Cloud for London campus vs rest of the campuses 

# Vector Corpus; omit the meta data
rest<- VCorpus(VectorSource(rest_camp$text))
london <- VCorpus(VectorSource(london_camp$text))

# Clean up the data
rest    <- cleanCorpus(rest, stops)
london <- cleanCorpus(london, stops)

# Another way to extract the cleaned text 
rest       <- unlist(pblapply(rest, content))
london <- unlist(pblapply(london, content))

# FYI
length(rest)

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
rest   <- paste(rest, collapse = ' ')
london <- paste(london, collapse = ' ')


# Combine the subject documents into a corpus of *2* documents
allcampus <- c(rest, london)
allcampus <- VCorpus((VectorSource(allcampus)))

# Make TDM with a different control parameter
ctrl      <- list(weighting = weightTfIdf)
campusTDM  <- TermDocumentMatrix(allcampus, control = ctrl)
campusTDMm <- as.matrix(campusTDM)

# naming the comparison cloud
colnames(campusTDMm) <- c('rest', 'london')

# Choose a color from colorbrewer
pal_1 <- brewer.pal(8, "Dark2")
pal_1 <- pal_1[(1:3)]

# Make comparison cloud
comparison.cloud(campusTDMm, 
                 max.words=50, 
                 random.order=FALSE,
                 title.size=1,
                 colors=pal_1,
                 scale=c(1.5, 0.5))

#Comparison Cloud for region Asia vs rest 

# Vector Corpus; omit the meta data
rest <- VCorpus(VectorSource(rest_r$text))
asia <- VCorpus(VectorSource(asia$text))

# Clean up the data
rest    <- cleanCorpus(rest, stops)
asia    <- cleanCorpus(asia, stops)

# Another way to extract the cleaned text 
rest <- unlist(pblapply(rest, content))
asia <- unlist(pblapply(asia, content))


# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
rest  <- paste(rest, collapse = ' ')
asia  <- paste(asia, collapse = ' ')


# Combine the subject documents into a corpus of *2* documents
allregion <- c(rest, asia)
allregion <- VCorpus((VectorSource(allregion)))

# Make TDM with a different control parameter
ctrl      <- list(weighting = weightTfIdf)
regionTDM  <- TermDocumentMatrix(allregion, control = ctrl)
regionTDMm <- as.matrix(regionTDM)

# naming the comparison cloud
colnames(regionTDMm) <- c('rest', 'asia')

# Choose a color from colorbrewer
pal_2 <- brewer.pal(8, "Dark2")
pal_2 <- pal_2[(4:5)]

# Make comparison cloud
comparison.cloud(regionTDMm, 
                 max.words=50, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=pal_2,
                 scale=c(1.5,0.5))

#creating wordcloud 
# make corpus and clean with full data set 

textCorp <- VCorpus(VectorSource(studentdf$allText))
textCorp <- cleanCorpus(textCorp, stops)

# DTM
txtDTM <- DocumentTermMatrix(textCorp)
txtDTM <- as.matrix(txtDTM)

# WFM
txtWFM <- colSums(txtDTM)

# Examine & Organize
txtWFM <- data.frame(word = names(txtWFM), freq = txtWFM)
rownames(txtWFM) <- NULL
txtWFM <- txtWFM[order(txtWFM$freq, decreasing = T),]

# Plot WFM
barplot(txtWFM$freq[1:5], names.arg = txtWFM$word[1:5], las = 2)
dev.off()

# Choose a color & drop light ones
pal_3 <- brewer.pal(8, "Dark2")

# Word cloud
set.seed(1234)
wordcloud(txtWFM$word, 
          txtWFM$freq, 
          max.words    = 50,
          random.order = FALSE,
          colors       = pal_3,
          scale        = c(1.5,0.5))


#making dot plots 

# clean & organize the data
txtCorpus <- VCorpus(VectorSource(studentdf$interests))
txtCorpus <- cleanCorpus(txtCorpus, stops)
studentTDM  <- TermDocumentMatrix(txtCorpus)

# Inspect word associations
associations <- findAssocs(studentTDM, 'people', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 

#bio
# clean & organize the data
txtCorpus <- VCorpus(VectorSource(studentdf$bio))
txtCorpus <- cleanCorpus(txtCorpus, stops)
studentTDM  <- TermDocumentMatrix(txtCorpus)

# Inspect word associations
associations <- findAssocs(studentTDM, 'experience', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 


# End

