library(RPTT)
library(tm)
library(jiebaR) # word segmentation
library(wordcloud) # word cloud
library(topicmodels)
library(igraph)
library(showtext)

source("./utils/smp.r", encoding = "UTF-8")
source("./utils/clipboard.R")

# utils -------------------------------------------------------------------

get_posts <- function(url) {
  post_data <- getPostData(url)
  if (!is.null(post_data))
    dplyr::as_data_frame(post_data$postData)
  else
    NULL
}


# get posts ---------------------------------------------------------------

# listPageUrls <- getListPageUrls("Keelung")
# postUrls <- unname(unlist(lapply(tail(listPageUrls, 40), getPostUrls))) # get last 40 pages
#
# ## get posts
# ptt_keelung_data <- dplyr::bind_rows(lapply(postUrls, get_posts))
# ## parse date
# parse_date <- function (string) {
#   date <- stringr::str_match(string,
#                              "\\s+([a-zA-z]+)\\s+([0-9]+)\\s+.*\\s+([0-9]+)$")
#   date <- apply(date[, c(2:4), drop = FALSE], 1, paste0, collapse="-")
#   date <- as.Date(date, "%B-%e-%Y")
#   date
# }
# ptt_keelung_data <- ptt_keelung_data %>%
#   mutate(date = parse_date(Time)) %>%
#   filter(!is.na(Title) & Board!="")

# save(ptt_keelung_data, file = "./data/ptt_keelung_data.RData")
load("./data/ptt_keelung_data.RData")
# View(ptt_keelung_data)


# title classes -----------------------------------------------------------

ptt_keelung_data %>%
  filter(date >= as.Date("2015-01-01")) %>%
  mutate(post_class = stringr::str_extract(Title, "^\\[[^]]+\\]")) %>%
  select(post_class) %>%
  group_by(post_class) %>%
  tally(sort=TRUE) %>% copy.tbl()

# text mining --------------------------------------------------------------

post_text <- ptt_keelung_data %>%
  filter(
    grepl(
      sprintf("\\[%s\\]", paste0(c("情報","分享","新聞","問題"), collapse="\\]|\\[")),
      Title)
  ) %>%
  filter(date >= as.Date("2015-01-01")) %>%
  select(Text) %>% `[[`(1)
post_text <- tolower(post_text)
# remove URLs
removeURL <- function(x) {
  x <- gsub("http[^[:space:]]*", "", x)
  x <- gsub("[0-9]{2,3}\\.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}","",x)
}
post_text <- removeURL(post_text)
post_text <- gsub("◆ from:.*\\n", "", post_text)
post_text <- gsub("\\n", "", post_text) # remove "\n"

## 起手式，結巴建立斷詞器
mixseg <- worker(user = "./utils/dict.txt.utf8.txt",symbol = FALSE,
                 encoding = "UTF-8")
# mixseg <= post_text[1]

# self-made filter (built-in perl's regular expression has bug)
cutter <- function (msg) {
  filter_words = c("基隆",
                   "我.?","他.?","你.?",
                   "所以","可以","沒有","不過","因為",
                   "還是","覺得","大家","比較","感覺","時候","現在","時間",
                   "可能","東西","然後","而且","自己","有點",
                   "這邊","那.","發現","雖然","不要","還是",
                   "一樣","知道","看到","真的","今天","就是","這樣","如果",
                   "不會","什麼","後來","問題","之前","只是","或是","的話",
                   "其他","這麼","已經","很多","出來","整個","但是","卻",
                   "偏偏","如果","不過","因此","或","又","也","其實",
                   "希望","結果","怎麼","當然","有些","以上","另外","此外",
                   "以外","裡面","部分","直接","剛好","由於",
                   "原本","標題","時間","日期","作者","這種","表示","看見",
                   "似乎","一半","一堆","反正","常常","幾個","目前","上次",
                   "公告","只好","哪裡","一.","怎麼","好像","結果",
                   "po","xd","應該","最後","有沒有","sent","from","my","Android",
                   "請問","謝謝","台灣","有人",
                   "還.","各位","報導","這.","ntd","提供","最.","不是","記者",
                   "中心","之.","指出","朋友",
                   "了","也","的","在","與","及","等","是","the","and",
                   "in","a","at","he","is","of","He","b")
  pattern <- sprintf("^%s", paste(filter_words, collapse = "|^"))
  filter_seg <- grep(pattern, mixseg <= msg ,value=TRUE, invert = TRUE)
  return(filter_seg)
}

## vectorize
segRes = lapply(post_text, cutter)
tmWordsVec = sapply(segRes, function(ws) paste(ws,collapse = " "))
# tmWordsVec[1]

## build courpus
myCorpus <- Corpus(VectorSource(tmWordsVec)) # build a corpus
myCorpus <- tm_map(myCorpus, stripWhitespace) # remove extra whitespace

## build tdm
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(2, Inf)))
tdm

## 看看一下詞頻分的如何 (因為看到分的不好才弄個過濾器的)
dtm1 <- DocumentTermMatrix(myCorpus,
                           control = list(
                             wordLengths=c(2, Inf), # to allow long words
                             removeNumbers = TRUE,
                             weighting = weightTf,
                             encoding = "UTF-8")
)
# colnames(dtm1)
# findFreqTerms(dtm1, 10) # 看一下高频词, he沒法filter掉

## dtm
m <- as.matrix(dtm1)
head(m)

## wordcloud
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
# plot
pal2 <- brewer.pal(8,"Dark2")
png(paste(getwd(), "/pic/wordcloud100_",  ".png", sep = ''),
    width=10, height=10, units="in", res=700)
par(family='STHeiti')
wordcloud(d$word, d$freq,
          scale=c(5,0.5),
          min.freq=median(d$freq, na.rm = TRUE),
          max.words=100,
          random.order=FALSE,
          rot.per=.01,
          colors=pal2)
dev.off()


# title wordcloud -------------------------------------------------------------

keys = worker("keywords", topn = 5)

keyr <- function(text) {
  keys <= text
}


ptt_keelung_data %>%
  filter(
    grepl(
      sprintf("\\[%s\\]", paste0(c("情報","分享","新聞","問題"), collapse="\\]|\\[")),
      Title)
  ) %>%
  select(Text) %>% `[[`(1) %>%
  removeURL %>%
  # gsub("Re:|\\[情報\\]|\\[分享\\]", "",.) %>%
  lapply(keyr)


# topic models ------------------------------------------------------------

## 利用tf-idf 來處理高頻詞高估，低頻詞低估
dtm = dtm1
term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i],
                    dtm$j,
                    mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
l1= term_tfidf >= quantile(term_tfidf, 0.5) # second quantile, ie. median
summary(col_sums(dtm))
dim(dtm); dtm <- dtm[,l1]
dtm <- dtm[row_sums(dtm)>0, ]; dim(dtm)
summary(col_sums(dtm))


## smp
fold_num = 10
kv_num = seq(2,24)
seed_num = 2015
try_num = 1

sp <- smp(cross=5, n=dtm$nrow, seed=seed_num) # n = nrow(dtm)
system.time(
  (ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp))
)

## 跑個模擬，挑一個好的主題數
m_per <- apply(ctmK[[1]],1,mean)
m_log <- apply(ctmK[[2]],1,mean)

k=c(kv_num)
#df = ctmK[[1]]  # perplexity matrix
logLik = ctmK[[2]]  # perplexity matrix

# matplot(k, df, type = c("b"), xlab = "Number of topics",
#         ylab = "Perplexity", pch=1:try_num,col = 1, main = '')
# legend("topright", legend = paste("fold", 1:try_num), col=1, pch=1:try_num)
matplot(k, logLik, type = c("b"), xlab = "Number of topics",
        ylab = "Log-Likelihood", pch=1:try_num,col = 1, main = '')
legend("topleft", legend = paste("fold", 1:try_num), col=1, pch=1:try_num)

## 現成有四種調法
n_word <- 10 # 要的文字數
n_topic <-  which(logLik == max(logLik))+1
SEED <- 2015
jss_TM2 <- list(
  VEM = LDA(dtm, k = n_topic, control = list(seed = SEED)),
  VEM_fixed = LDA(dtm, k = n_topic, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs = LDA(dtm, k = n_topic, method = "Gibbs",
              control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
  CTM = CTM(dtm, k = n_topic,
            control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
)
# terms(模型, 要的文字數)
termsForSave1<- terms(jss_TM2[["VEM"]], n_word)
termsForSave2<- terms(jss_TM2[["VEM_fixed"]], n_word)
termsForSave3<- terms(jss_TM2[["Gibbs"]], n_word)
termsForSave4<- terms(jss_TM2[["CTM"]], n_word)

tfs <- as.data.frame(termsForSave2, stringsAsFactors = F); tfs

## plot
adjacent_list <- lapply(1:10, function(i) embed(tfs[,i], 2)[, 2:1])
edgelist <- as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
topic <- unlist(lapply(1:10, function(i) rep(i, 9)))
edgelist$topic <- topic
g <- igraph::graph.data.frame(edgelist,directed=T)
l <- igraph::layout.fruchterman.reingold(g)
# edge.color="black"
nodesize <- centralization.degree(g)$res
V(g)$size <- log(centralization.degree(g)$res)

nodeLabel <- V(g)$name
E(g)$color <-  unlist(lapply(sample(colors()[26:137], 10),
                             function(i) rep(i, 9))); # unique(E(g)$color)
# save(g, nodeLabel,l, file="./data/igraph_data.RData")
# load("./data/igraph_data.RData")

## plot
library(Cairo)
CairoPNG("./pic/test_graph_gibbs.png",
    width=720,
    height=720)
igraph_options(label.family='STHeiti')
plot(g,
     vertex.label = nodeLabel,
     edge.curved = TRUE,
     vertex.label.cex = 1.25,
     vertex.label.color="gray48",
     edge.arrow.size = 0.5,
     layout=l)
dev.off()

