#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' 003figures.R
#' Matylde Diouf
#' 26/02/25
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Objectives:
#' 
#' Create all the figures of manuscript (main and supp mat figures).
#' 
#' 
#'  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' Modifications/Notes 
#' 
#' 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





### Stacked barplot of publication and study information ####
# Figure S1
source(paste(codes.folder, "0031fig2_barplots.R", sep = "/"))


### Word cloud ####
# Graphical abstract
# # https://medium.com/towards-data-science/create-a-word-cloud-with-r-bde3e7422e8a

# Import recoded data
text <- df_inclus_all$pathology_recoded

# keep whole phrases ?
text <- gsub(" ", "_", text)

# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  # tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))


dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
dat <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = dat$word, freq = dat$freq, min.freq = 1,           
          max.words = 200, random.order = FALSE, rot.per = 0.35,            
          colors = brewer.pal(8, "Dark2"))
set.seed(1234)
wordcloud2(data=dat, size = 0.7, shape = 'pentagon')




### Gantt chart on arm enrollment timeline and primary endpoint significance ####
# Figure 3
source(paste(codes.folder, "0032_fig3_gantt.R", sep = "/"))



### Sankey diagram of pathology type, pathology and ATC code for interventions ###
# Figure S3
source(paste(codes.folder, "0033_fig_4_sankey.R", sep = "/"))



### World map of study first author country ####
# Figure S2
source(paste(codes.folder, "0034_fig_5_world_maps.R", sep = "/"))



### Stacked br plot of primary endpoint significance over time ####
# Figure 2
source(paste(codes.folder, "0035_fig6_stacked_barplot.R", sep = "/"))



### Radar plots of 5 TTE items ####
# Figure 4
source(paste(codes.folder, "0036_fig7_radarplot.R", sep = "/"))




# Clean env ---------------------------------------------------------------

rm(tmp, tmp_long)