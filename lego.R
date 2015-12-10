library(rvest)
library(stringr)
library(gtools)

# theme_url <- "http://brickset.com/browse/sets"
# theme_read <- read_html(theme_url)
# themes <- html_nodes(theme_read, "li") %>%
#           html_nodes(., "a") %>%
#           html_text()

find_theme <- function(x){
  x <- str_to_title(x)
  y <- str_extract(x,Themes)[!is.na(str_extract(x,Themes))]
  if(!invalid(y))
    return(y)
  else
    return("NA")
}

is_new <- function(x){
  if(any(str_detect(x,c("new","NEW","New","Seal","SEAL","seal","MINT","Mint","mint"))))
    return("Yes")
  if(any(str_detect(x,c("USED","Used","used","INCOMPLETE","incomplete","Incomplete"))))
    return("No")
  else
    return("Not Sure")
}

with_box <- function(x){
  if(any(str_detect(x,c("W/ BOX","w/ Box","w/ box","With Box","sealed box","Sealed Box","SEALED BOX"))))
    return("Yes")
  if(any(str_detect(x,c("NO BOX","No Box","no box","NO Box"))))
    return("No")
  else
    return("Not Sure")
}

within_link <- function(url){
  html <- read_html(url)
  item_condition <- html_nodes(html,"#vi-itm-cond") %>%
                    html_text()
  time_left <- html_nodes(html,"#vi-cdown_timeLeft") %>%
                    html_text() %>%
                    str_replace_all(.,"(\r|\n|\t)","")
#   shipping_cost <- html_nodes(html,"span") %>%
#                    #html_nodes(.,"span") %>%
#                    html_nodes(.,"span") %>%
#                    html_text()
  item_location <- html_nodes(html,"#itemLocation .iti-eu-bld-gry") %>%
                   html_text()
  theme <- html_nodes(html,"tr:nth-child(4) td~ .attrLabels+ td span") %>%
           html_text()

  set_number <- html_nodes(html,"tr:nth-child(2) h2") %>%
                html_text()
#   description <-  html_nodes(html," #desc_ifr") %>%
#     html_text()
   
  
  return(c(item_condition, time_left, item_location, theme, set_number))
}

base_url = "http://www.ebay.com/"
listing_part1 = "sch/i.html?LH_Auction=1&_from=R40&_sacat=0&_nkw=lego&_pgn="
listing_part2="&_skc=200&rt=nc"
Themes <- c("Harry Potter", "Pirates Of The Caribbean", "Lord Of The Rings","Ninjago","Monster Fighters",
            "Hobbits","Star Wars","Creator","Prince of Persia","Simpsons","Bionicle",
            "Monsters","Batman","Indiana Jones","Superheros","Atlantis","Vikings",
            "Jurassic World","Lego Movie","Minecraft","Mixels","Town City","Friends",
            "Super Heros")
pages <- 40
rest <- NULL

df <- function(l){
  titles = html_nodes(listings, "h3") %>%
    html_nodes(.,"a") %>%
    html_text() %>% 
    lapply(.,function(x) str_replace(x, "New listing\r\n\t\t","")) %>%
    unlist()
  
  lego<- data.frame(title = titles)
  
  lego$theme <- unlist(lapply(titles, find_theme))
  
  lego$cur_price = html_nodes(listings,"li") %>%
    html_nodes(.,"span.bold") %>%
    html_text() %>%
    str_replace_all(.,"\\$","") %>%
    as.numeric()
  
  lego$bid = html_nodes(listings,"li.lvformat") %>%
    html_nodes(.,"span") %>%
    html_text() %>%
    str_extract(.,"[0-9]") %>%
    as.numeric()
  
  lego$is_new = unlist(lapply(titles, is_new))
  
  lego$box = unlist(lapply(titles, with_box))
  
  links = html_nodes(listings,".vip") %>%
              html_attr("href")
  
  lego$link = links
  
  other = sapply(links,within_link) %>%
          t() %>%
          do.call(rbind,.)
  colnames(other) <- c("condition","time_left","item_location","themes","set_number")
  
  cat(dim(lego)[1], "\n")
  return(cbind(lego,other))
}

res <- NULL

for(i in 1:pages){
  cat("parsing", i, "'th page\n")
  listings = read_html(paste0(base_url, listing_part1, i,listing_part2))
  res = rbind(res,df(listings))
}

#lego$minifigure

##lego = do.call(cbind,list(title,theme,cur_price,bid))

# 
# f <- function(x){
#   return(c(x+1,x*2,x^3))
# }
#t(sapply(c(1,2,3),f))