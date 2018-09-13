# oBike_facebook_fanpage_analysis
---
title: "OBIKE data analysis report(social media)"
author: "Ching-Chun, Chen"
date: "2017.09.18"
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float:
      collapsed: false
      smooth_scroll: true
      number_sections: true
    theme: united
---

---

# Quick Start

* ##### **data introduction**：
      + source : OBIKE TW Facebook fanpage posts。(https://www.facebook.com/oBikeTW/) </br>
      + data period : 2017/03/23-2017/09/17 。</br>
      + posts count : 281。</br>
      + collected features : from_name, create_time, type, link, likes_count, comments_count,shares_count etc.。</br>      
      
---

```{r defult設定 ,echo=F,warning=FALSE,message=FALSE,results='hide'}
options('width'=100)
knitr::opts_chunk$set(comment="",prompt=T,strip.white=F,warning=F,echo = F,message = F,fig.align = "center")
#資料預處理####
library(httr)
library(Rfacebook)
library(rjson)
library(httpuv)
library(bitops)
library(RCurl)
library(jiebaR)
library(jiebaRD)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(knitr)
library(DT)
page <- read.csv("/Users/chenchingchun/Desktop/obike/obike_post.csv", stringsAsFactors = F)##change the paht to obike_post.csv
page[is.na(page) == T] <- 999
content <- read.csv("/Users/chenchingchun/Desktop/obike/content.csv", stringsAsFactors = F)##change the paht to content.csv

```

* ##### **Sample data(6/281)**：
```{r echo= FALSE, results = "asis"}
kable(head(page[,c(2:3,5:8,10:12)]),format = "markdown")
```


#Which of these posts got maximum likes?
```{r}
summary = page[which.max(page$likes_count),]
datatable(summary[,c(2:3,5:8,10:12)], rownames =  F, options = list(searching = F, pageLength = 1)) %>%
  formatStyle(columns = "likes_count", background = styleEqual(c("6467"),c("#F8766D")))
```

#Which of these posts got maximum comments?
```{r}
summary1 = page[which.max(page$comments_count),]
datatable(summary1[,c(2:3,5:8,10:12)], rownames =  F, options = list(searching = F, pageLength = 1)) %>%
  formatStyle(columns = "comments_count", background = styleEqual(c("1047"),c("#C77CFF")))
```

#Which post was shared the most?
```{r}
summary2 = page[which.max(page$shares_count),]
datatable(summary2[,c(2:3,5:8,10:12)], rownames =  F, options = list(searching = F, pageLength = 1)) %>%
  formatStyle(columns = "shares_count", background = styleEqual(c("468"),c("#00BFC4")))
```

#WordCloud-What did this fanpage talk about in the past half year?
```{r}
#FB發文內容文字雲(名詞)#####
par(family=("Heiti TC Light"))
cutter = worker("tag",symbol = F)
result <- segment(code = page$message,jiebar = cutter)
##抓名詞
get_noun = function(x){
  stopifnot(inherits(x,"character"))
  index = names(x) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")
  x[index]
}
result <- get_noun(result)
#文字雲
tb <- table(result)
wordcloud(names(tb), tb, min.freq = 2,random.order = F,random.color =F,ordered.colors = F,colors = rainbow(length(1:7)) )
```

#type X likes_counts
####Videos and photos type of posts are more popular than statuses and links.
```{r}
page$type <- as.factor(page$type)
select(page,type, likes_count, comments_count, shares_count) %>%
  group_by(type) %>%
  summarise(mean_likes = mean(likes_count),
            mean_comments = mean(comments_count),
            mean_shares = mean(shares_count)) %>% 
  ggplot(aes(x = type, y = mean_likes , fill = type))+
  geom_bar(stat = "identity")+
  ggtitle("post type mean counts\n") 
```

#time X likes_counts
##all-time
```{r}
select(page, created_time, likes_count, comments_count, shares_count) %>%
  mutate(date = substr(created_time, 1,10)) %>%
  group_by(date) %>%
  summarise(sumlikescount = sum(likes_count)) %>%
  ggplot(aes( x = date , y = sumlikescount)) +
  geom_bar( stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

## daily
#### Maybe 8:00-8:59 in the morning is the best period of updating some new post on timeline in every single day.
```{r}
select(page, created_time, likes_count, comments_count, shares_count) %>%
  mutate(hrmin = substr(created_time, 12,13)) %>%
  group_by(hrmin) %>%
  summarise(meanlikescount = mean(likes_count)) %>%
  ggplot(aes( x = hrmin , y = meanlikescount, group = 1)) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

# Get Posts from particular groups
##### In Taiwan, Ubike is the biggest existing public bicycle company, thus, probably we can use "ubike" as the key word to search FB open groups and obtain the articles to get some concept of transfering the customers from UBIKE to OBIKE.</br></br>


   * Sample posts(6/65)
```{r}
colnames(content)[1] <- "NO."
kable(head(content),"markdown")
```

# WordCloud-What did these groups talk about?
```{r}
# FB發文內容文字雲(名詞)#####
par(family=("Heiti TC Light"))
result1 <- segment(code = content$content,jiebar = cutter)
result1 <- get_noun(result1)
# 文字雲
tb1 <- table(result1)
wordcloud(names(tb1), tb1, min.freq = 2,random.order = F,random.color =F,ordered.colors = F,colors = rainbow(length(1:7)) )
```
