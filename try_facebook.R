# devtools::install_github("pablobarbera/Rfacebook/Rfacebook")
library(Rfacebook)
library(data.table)
library(dplyr)
library(dtplyr)
library(tidyr)
library(httr)
library(XML)
library(ggplot2)
library(showtext)
library(tm)
library(jiebaR)
library(feather)

source("utils/fb_description_parser.R") # parse_description()
source("utils/clipboard.R") # copy.tbl(), paste.tbl()


# get API token -----------------------------------------------------------

# ## r_crawler
# fb_oauth <- fbOAuth(app_id="1667711623447142",
#                     app_secret="******",
#                     extended_permissions = FALSE)
# dir.create("./fb_oauth", showWarnings = FALSE)
# save(fb_oauth, file="./fb_oauth/fb_oauth")
load("./fb_oauth/fb_oauth")

# go to 'https://developers.facebook.com/tools/explorer' to get your access token
access_token <- "CAACEdEose0cBAGgxFyZCdciiGvMJujGxyXzg5z6afF5e8B1vZC8XfhioLdZBuplHksxcoRO7AODBn6swTZAlQZAeoUQNwZA1AKRT1OrouuZAuimN9TZCqZBv6IIaei8ZCxKTrOLlS0MfTzmDg3eBxU0pMyHWhMuKlB87U0rv5r5I9WKPcsZBJPEFVgWIhLfR15stdpZBH0woo2QCb4ZBtGLY2vGZBX5nVoh17APIQZD"

# try ------------------------------------------------------------------

## Search public posts that mention a string
# (deprecated with version 2.0 of the Facebook Graph API)
# searchFacebook("莊旻達", token=fb_oauth)

## getUsers
# After version 2.0 of the Facebook API, only id, name, and picture are
# available through the API. All the remaining fields will be missing.

## Find Facebook ID of a group
# ids <- searchGroup(name="基隆", token=fb_oauth); View(ids)

## Search pages that mention a string (fail on Windows)
# pages <- searchPages(string="基隆", token=fb_oauth, n=100 )

## Extract list of posts from a public Facebook page
# fb_page <- getPage(page="facebook", token=fb_oauth, n = 10, feed = TRUE)

## Extract information about a public Facebook post
# getPost(post=fb_page$id[1], n=2000, token=fb_oauth)



# search FB Pages -----------------------------------------------------------

# keelung_fb_pages <- searchPages(string = "基隆", token=fb_oauth, n=500) %>% as_data_frame(); View(keelung_fb_pages)
# save(keelung_fb_pages, file = "./data/keelung_fb_pages.RData")
load("./data/keelung_fb_pages.RData")


# page analysis ------------------------------------------------------------

## page count
p_keelung_fb_pages <- keelung_fb_pages %>%
  group_by(category) %>%
  tally(sort = T) %>%
  ggplot(aes(x=reorder(category, n), y=n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.2, colour="darkgrey", size=4) +
  xlab("") + ylab("") +
  ggtitle("Keelung related facebook page categories")

# ggsave("./pic/p_keelung_fb_pages.png", p_keelung_fb_pages, height = 10, width = 10)

## talking_about
talking_about <- keelung_fb_pages %>%
  select(name, talking_about_count, likes, link) %>%
  arrange(desc(talking_about_count)) %>%
  top_n(20, likes)
View(talking_about)


# FB page: aboutkeelung -------------------------------------------------------

page_aboutkeelung <- keelung_fb_pages[grep("基隆人踹共", keelung_fb_pages$name),]
page_aboutkeelung$id
page_aboutkeelung$username

# ## Get all posts
posts_aboutkeelung <- getPage(page_aboutkeelung$username,
                              token=fb_oauth, n = 30000,
                   since = as.Date("2014-09-01")) %>%
  as_data_frame() %>%
  mutate(created_date = as.Date(created_time))
## Parse link
posts_aboutkeelung <- posts_aboutkeelung %>%
  mutate(link_description = parse_description(link)) %>%
  mutate(message_link = paste0(message, link_description, collapse = " "))

# write_excel_csv(posts_aboutkeelung, "~/fb_aboutkeelung_post.csv")
# write_feather(posts_aboutkeelung, "./data/fb_aboutkeelung_post.feather")
read_feather("./data/fb_aboutkeelung_post.feather")
View(posts_aboutkeelung)

## Get all shares
shares_aboutkeelung <- posts_aboutkeelung %>%
  select(id) %>% .[[1]] %>%
  purrr::map_df(getShares, token = fb_oauth, n = 10000) %>%
  as_data_frame()
# write_feather(shares_aboutkeelung, "./data/fb_aboutkeelung_shares.feather")

## Get all Comments
comments_aboutkeelung <- posts_aboutkeelung %>%
  # filter(row_number() %in% 1:5) %>%
  select(id) %>% .[[1]] %>%
  sapply(function(x) {
    # x = "173996702663438_1348080615255035"
    getPost(x, n=2000, token=fb_oauth) %>%
      .$comments
  }, simplify = FALSE) %>%
  bind_rows(.id = "post_id") %>%
  tbl_df
# write_feather(comments_aboutkeelung, "data/fb_aboutkeelung_comments.feather")
# write_excel_csv(comments_aboutkeelung, "~/fb_aboutkeelung_comments.csv")

## Post type
posts_aboutkeelung %>%
  group_by(type) %>%
  tally(sort=TRUE)

## Plot: Likes and shares
p_abkl_like_share <- posts_aboutkeelung %>%
  select(created_date, likes_count, shares_count) %>%
  gather(key = like_share, value = n, -created_date) %>%
  group_by(created_date, like_share) %>%
  tally(wt = n, sort=TRUE) %>%
  ggplot(aes(x = created_date, y = n)) +
  geom_bar(aes(fill = like_share),
           stat = "identity",
           position = "dodge") +
  facet_grid(like_share ~ .,scales = "free_y") +
  xlab("") + ylab("") +
  ggtitle("「基隆人踹共」daily like and share count")
showtext::showtext.auto()
ggsave("./pic/p_abkl_like_share.png", p_abkl_like_share)


## Table: top like type
posts_aboutkeelung %>%
  select(likes_count, message,type, link) %>%
  group_by(type) %>%
  tally(likes_count, sort=TRUE)

## Table: top like
abkl_top_like <- posts_aboutkeelung %>%
  select(message,created_date, likes_count, link) %>%
  top_n(20, likes_count) %>%
  arrange(desc(likes_count)) %>%
  mutate(link_description=parse_description(link)) %>%
  select(-link)
copy.tbl(abkl_top_like)


# FB page: keelung_girl ------------------------------------------------------

# page_keelung_girl <- keelung_fb_pages[grep("港灣城姬", keelung_fb_pages$name),]
# page_keelung_girl$id
# page_keelung_girl$username
#
# ## get all posts
# posts_keelung_girl <- getPage(page_keelung_girl$id, token=access_token, n = 3000,
#                              since = as.Date("2014-09-01")) %>%
#   as_data_frame() %>%
#   mutate(created_date = as.Date(created_time))
# save(posts_keelung_girl, file = "./data/posts_keelung_girl.RData")
load("./data/posts_keelung_girl.RData")
View(posts_keelung_girl)

## like_share count
p_klgl_like_share <- posts_keelung_girl %>%
  select(created_date, likes_count, shares_count) %>%
  gather(like_share, n, -created_date) %>%
  group_by(created_date, like_share) %>%
  tally(wt = n, sort=TRUE) %>%
  ggplot(aes(x=created_date, y=n)) +
  geom_bar(aes(fill=like_share),
           stat="identity",
           position = "dodge") +
  facet_grid(like_share ~ .,scales = "free_y") +
  xlab("") + ylab("") +
  ggtitle("「港灣城姬！基隆少女」daily like and share count") +
  theme(text=element_text(family='STHeiti'))
showtext::showtext.auto()
ggsave("./pic/p_klgl_like_share.png", p_klgl_like_share)

## top like
klgl_top_like <- posts_keelung_girl %>%
  select(message,created_date, likes_count, link) %>%
  top_n(20, likes_count) %>%
  arrange(desc(likes_count)) %>%
  mutate(link_description=parse_description(link)) %>%
  select(-link)
copy.tbl(klgl_top_like)

