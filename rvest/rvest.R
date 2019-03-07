#' ---
#' title: R爬虫 - rvest包的使用
#' date: "`r Sys.Date()`"
#' author: gaoch
#' output: html_document
#' ---

#' 抓取《热心肠日报》的内容。
#' 
#' # 网页URL规律的分析
#' 
#' 《热心肠日报》分三级目录，分别是每月列表（month），每日列表（daily）和单篇论文（article）。
#' 
#' 每月列表的URL规则以年-月为命名方式，
#' 以 http://www.mr-gut.cn/daily/index/1/2019-03?kf=daily_next 为例，
#' 本链接指向2019-03月份文章列表。
#' 
#' 文章列表放在“`<div class="daily-list">`”标签下，每天的日报
#' 在其中的“`<div class="item">`”中，每个“`item`”中含有日报日期，
#' 题图，题目，链接，话题关键词等。
#' 
#' ```{html}
#' <div class="item fl bcb4a1ca">
#'   <div class="time pb5 fs16">2019-03-06</div>
#'   <div class="image"><a href="/daily/show/1279757960?kf=daily_list" target="_blank"><img src="https://pics-xldkp-com.oss-cn-qingdao.aliyuncs.com/dm/190306/cfcb365d8d9cf3f5963f3132966e50c8_zi_640x320_c_640x320.png"></a></div>
#'   <div class="title pt10 fs16 fb"><a href="/daily/show/1279757960?kf=daily_list" target="_blank">精准操控菌群大突破：刘洋彧等算出最小驱动物种</a></div>
#'   <div class="desc pt10 fs14">190306话题：菌群调控，微生态，呼吸道菌群，口腔菌群，粪菌移植，噬菌体治疗，饮食。</div>
#' </div>
#' ```
#' 
#' 进入日报页面后，每天日报文献放在“`<div class="daily-items">`”
#' 下面的“`<div class="item">`”容器中。这里面包含：核心关键词，分享标题，编辑评语等。
#' 其中，分享标题的标签属性为“`<div class="title">`”，其中的链接指向单个文献页面。
#' 
#' 单个文献页面的URL为“`/papers/read/1060776754`”，最后的数字是文章的ID。
#' 
#' 在单个文献页面中，
#' - “`<h1></h1>`”中的内容是分享标题；
#' - “`<dd class="f1">`”中是短科普的内容；
#' - “`<a href="" class="tag f1 c_ec004e">关键字</a>`”是关键字；
#' - 延伸阅读的内容比较杂乱，在若干个“`<p></p>`”标签下展示期刊、摘要、作者等信息
#' 
#' ```{html}
#' <div class="mb20 lin18 fs14">
#' 	<div>
#' 		<a href="/discovery/periodical_tag/8388?kf=read" target="_blank"><img class="logo" src="x.jpg" alt="图片"/></a>
#' 		<span class="ml10"><a href="/discovery/periodical_tag/8388?kf=read" class="c_191919" target="_blank">Nature Communications</a></span>
#' 		<span class="fr c_15862d">[IF:12.353]</span>
#' 	</div>
#' 		<p><a href="doi.org/10.1038/s41467-019-08890-y" target="_blank" class="shake c_558ec4">A theoretical framework for controlling complex microbial communities</a></p>
#' 		<p>一个用于控制复杂微生物群落的理论框架</p>
#' 		<p><span class="mr10">03-06, Article, 10.1038/s41467-019-08890-y</span><a href="javascript:;" class="c_558ec4 show_more">more</a></p>
#' 		<p class="more_read hide"><span>Abstract:</span><br/><span></span>Microbes form complex communities...<br/></p>
#' 		<p class="more_read hide"><span>First Authors:</span><br/>Marco Tulio Angulo</p>
#' 		<p class="more_read hide"><span>Correspondence Authors:</span><br/>Marco Tulio Angulo,Yangyu Liu</p>
#' 		<p class="more_read hide"><span>All Authors:</span><br/>Marco Tulio Angulo,Claude H Moog,Yangyu Liu</p>
#' </div>
#' ```
#' 

library(rvest)
setwd("rvest/")

read_html_with_retry <- function(url, retry=3, wait=3){
  if(retry<1) {
    print(paste(url, ": failed too many times. Please check your connection."))
    return(NULL)
  }
  require(rvest)
  Page.src = try(read_html(url), silent = T)
  
  # test if Page.src is erroneous
  if (class(Page.src)[1] == "try-error") {
    
    error.cond = attr(Page.src, "condition")
    
    # check if error condition contains “Timed out” phrase.  If regexpr cannot find a
    # match it returns -1
    timed.out = regexpr("Timed out", error.cond, ignore.case = T) != -1
    
    # we want to continue only on “timed out” error
    if (timed.out == TRUE) {
      
      # print information in the console
      print(paste(url, ": Timed out. Trying to reconnect in ", wait,"s. Please wait...",sep=""))
      Sys.sleep(wait)
      
      return(read_html_with_retry(url,retry = retry-1))
    }
  }
  
  return(Page.src)
}

#' Fetch daily of a month by its url
#'
#' @param url 
#'
#' @return a dataframe with five columns, which are pdate, title, url, img_url, desc
#' @export
#'
#' @examples
#'   url <- "http://www.mr-gut.cn/daily/index/1/2019-03"
#'   data <- fetch_daily_of_a_month(url)
fetch_daily_of_a_month <- function(url) {
  webpage <- read_html_with_retry(url)
  if(is.null(webpage)) return(NULL)
  #' 对于日报每月清单，提取日报日期、标题、链接、题图URL、描述等5个信息
  daily <- html_nodes(webpage, ".item")
  pdate <- html_nodes(daily, ".time") %>% html_text()
  title <- html_nodes(daily, ".title") %>% html_text()
  url <- html_nodes(daily, ".title a") %>% html_attr("href")
  id <- str_extract(url,"[0-9]+")
  img_url <- html_nodes(daily, ".image img") %>% html_attr("src")
  desc <- html_nodes(daily, ".desc") %>% html_text()
  
  df <-
    data.frame(
      daily_id = id,
      daily_pdate = pdate,
      daily_title = title,
      daily_url = url,
      daily_img_url = img_url,
      daily_desc = desc
    )
  
  return(df)
}



#' Fetch paper links in a GUT daily 
#'
#' @param daily_url 
#'
#' @return a dataframe with the following values
#'         - paper_id
#'         - url
#'         - comment by reviewer
#' @export
#'
#' @examples
fetch_link_to_paper <- function(daily_url, base_url="http://www.mr-gut.cn") {
  webpage <- read_html_with_retry(paste(base_url,daily_url,sep = "/"))
  if(is.null(webpage)) return(NULL)
  item <- html_nodes(webpage, ".item")
  url <- html_nodes(item,".title a") %>% html_attr("href")
  comment <- html_nodes(item,".detail") %>% html_text()
  id <- stringr::str_extract(url,"[0-9]+")
  df <- data.frame(paper_id = id, paper_url=url, paper_comment=comment)
  return(df)
}

#' Get content of a seprate article
#'
#' @param paper_url 
#' @param base_url 
#'
#' @return a dataframe with the following values
#'         - article_id 
#'         - daily_id 
#'         - article_title 
#'         - article_editor_id 
#'         - article_editor_name 
#'         - article_reviewer_id 
#'         - article_reviewer_name 
#'         - article_content 
#'         - article_keyword_id 
#'         - article_keyword 
#'         - journal_id 
#'         - journal_name 
#'         - article_doi 
#'         - article_source_title 
#'         - article_source_title_cn 
#'         - article_source_abstract 
#'         - article_first_author 
#'         - article_corr_author 
#'         - article_all_author 
#'         
#' @export
#'
#' @examples
fetch_content_of_a_paper <- function(paper_url, base_url="http://www.mr-gut.cn"){
  require(stringr)
  article_id <- str_extract(paper_url,"[0-9]+")
  webpage <- read_html_with_retry(paste(base_url,paper_url,sep = "/"))
  if(is.null(webpage)) return(NULL)
  article <- html_nodes(webpage,".article")
  daily_id <- html_nodes(article,".fs14 .fr .shake") %>% html_attr("href") %>% str_extract("[0-9]+")
  editor_ids <- html_nodes(article,".fs14 .mr15 a") %>% html_attr("href") %>% str_extract("[0-9]+")
  editor_names <- html_nodes(article,".fs14 .mr15 a") %>% html_text()
  if (length(editor_ids)==2){
    editor_id = editor_ids[[1]]
    editor_name = editor_names[[1]]
    reviewer_id = editor_ids[[2]]
    reviewer_name = editor_names[[2]]
  }
  else{
    editor_id = NA
    editor_name = ""
    reviewer_id = NA
    reviewer_name = "" 
  }
  
  title <- html_nodes(article,"h1") %>% html_text()
  content <- html_nodes(article,".content dd") %>% html_text() %>% paste(collapse = "")
  keywords_id <- html_nodes(article,".tag") %>% html_attr("href") %>% str_extract("[0-9]+") %>% paste(collapse = ",")
  keywords <- html_nodes(article,".tag") %>% html_text() %>% paste(collapse = ",")
  journal_name <- html_nodes(article,".ml10") %>% html_text()
  journal_id <- html_nodes(article,".ml10 a") %>% html_attr("href") %>% str_extract("[0-9]+")
  doi_url <- html_nodes(article, ".mb20 .shake") %>% html_attr("href") 
  doi <- gsub("^.*doi.org/","",doi_url)
  article_items <- html_nodes(article, ".mb20 p") %>% html_text()
  if (length(article_items)>=2){
    source_title = article_items[[1]]
    source_title_cn = article_items[[2]]
  }
  else{
    source_title = ""
    source_title_cn = ""
  }
  abstract <- article_items[str_detect(article_items,"Abstract")]
  abstract <- gsub("Abstract:","",abstract)
  first_author <- article_items[str_detect(article_items,"First Authors")]
  first_author <- gsub("First Authors:","",first_author)
  corr_author <- article_items[str_detect(article_items,"Correspondence Authors")]
  corr_author <- gsub("Correspondence Authors:","",corr_author)
  all_author <- article_items[str_detect(article_items,"All Authors")]
  all_author <- gsub("All Authors:","",all_author)
  paper <- list(
      article_id = article_id,
      daily_id = daily_id,
      article_title = title,
      article_editor_id = editor_id,
      article_editor_name = editor_name,
      article_reviewer_id = editor_id,
      article_reviewer_name = editor_name,
      article_content = content,
      article_keyword_id = keywords_id,
      article_keyword = keywords,
      journal_id = journal_id,
      journal_name = journal_name,
      article_doi = doi,
      article_source_title = source_title,
      article_source_title_cn = source_title_cn,
      article_source_abstract = abstract,
      article_first_author = first_author,
      article_corr_author = corr_author,
      article_all_author = all_author
  )

  return(paper)
}

#' Title: fetch by type
#'
#' @param x a URL vector
#' @param type one of c("month","paper_link","paper_content")
#'
#' @return list
#' @export
#'
#' @examples
fetch_by_type <- function(x, type=c("month","paper_link","paper_content")){
  list <- vector("list",length(x))
  if(type=="month"){
    list <- lapply(x, fetch_daily_of_a_month)
  }
  else if(type=="paper_link"){
    list <- lapply(x, fetch_link_to_paper)
  }
  else if(type == "paper_content"){
    list <- lapply(x, fetch_content_of_a_paper)
  }
  else{
    warning(paste("Don't know the type:",type,sep = " "))
  }
  return(list)
}


# 热心肠日报从 2016年2月开始编辑，即从“2016-02 到 2019-03”
months <- paste(rep(c(2016,2017,2018),each=12),formatC(1:12,width = 2,flag = 0),sep="-")
months <- c(months[-1],paste("2019",c("01","02"),sep="-"))
months_url <- paste("http://www.mr-gut.cn/daily/index/1/",months,sep="")


daily_list <- fetch_by_type(months_url,type = "month")
daily <- do.call("rbind",daily_list) # 所有日报的清单
#' 所有日报的清单，有几个TRUE则有几个没有成功
table(sapply(daily_list, is.null))

article_link_list <- fetch_by_type(daily$daily_url,type = "paper_link")
article_links <- do.call("rbind",article_link_list) # 所有文章的清单
#' 所有文章的清单，有几个TRUE则有几个没有成功
table(sapply(article_link_list,is.null))


article_list <- fetch_by_type(article_links$paper_url,type = "paper_content")
#' 所有文章的内容，有较多获取失败
table(sapply(article_list,is.null))

write.csv(daily,file = "daily.csv")
saveRDS(article_list,"article_list.rds")

article <- do.call("rbind",article_list)  # 所有文章的内容
article <- as.data.frame(as.character(matrix(article,ncol = length(article_list[[1]]))))
colnames(article) <- names(article_list[[1]])

# column class are list
lapply(article, class)
# change column class
article <- lapply(article, as.character)

write.csv(a, file = "article.csv",fileEncoding = "UTF-8")
