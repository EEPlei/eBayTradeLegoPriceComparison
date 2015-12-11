library(shiny)
library(magrittr)
library(httr)
library(dplyr)
library(stringr)
library(rvest)
library(xml2)

sets = c("4184: Black Pearl","4195: Queen Annes Revenge","2507: Fire Temple","10232: Palace Cinema","70810: MetalBeard's Sea Cow","10193: Medieval Market Village")   
numbers = c('4184','4195','2507','10232','70810','10193')
names = c("Black Pearl","Queen Annes Revenge","Fire Temple","Palace Cinema","MetalBeard's Sea Cow","Medieval Market Village") 
Sets = data.frame(sets = sets, numbers = numbers, names = names)

predicates <- c("Auctions","Buy it now","New","Used","Free Shipping","Best Offer","Sold")
query <- c("&LH_Auction=1","&LH_BIN=1","&LH_ItemCondition=11","&LH_ItemCondition=12","&LH_FS=1","&LH_BO=1","&LH_Complete=1&LH_Sold=1")
map <- data.frame(predicates = predicates, query = query)

gq <- function(pred){ #gq = get query
  map[map$predicates == pred,]$query
} 
scrape <- function(url){
  html = read_html(url)
  web_pages = html_nodes(html, ".vip") %>%
              html_attr("href")
  
  shipping_cost = lapply(web_pages, 
                         function(x){
                           link = read_html(x)
                           shipping_cost = html_nodes(link, "#fshippingCost") %>%
                                           html_text() %>%
                                           str_trim()
                         })
  
  title = html_nodes(html, ".lvtitle") %>%
          html_text() %>%
          str_trim()
  
  price = html_nodes(html, ".prc .bold") %>%
          html_text() %>%
          str_trim() %>%
          str_extract("[$0-9.]*") 
  
  ending_time = html_nodes(html, ".tme") %>%
                html_text() %>%
                str_trim() 
  
  t = cbind(title, price, ending_time, shipping_cost, web_pages)  
  final_df = as.data.frame(t)
  final_df$shipping_cost[final_df$shipping_cost=="character(0)"] = "Calculate"
  return(final_df)
}
total <- function(df){
  z <- df
  z$price <- as.numeric(substring(z$price, 2))
  free.rep <- function(row){
    ifelse(row$shipping_cost == "FREE", yes = 0, no = row$shipping_cost)
  }
  z$shipping_cost <- apply(z, 1, free.rep)
  dollar.rep <- function(row){
    ifelse(row$shipping_cost != "Calculate" & row$shipping_cost != 0, 
           yes = substring(row$shipping_cost, 2), no = row$shipping_cost)
  }
  z$shipping_cost <- apply(z, 1, dollar.rep)
  calc.rep <- function(row){
    calc <- filter(z, shipping_cost != "Calculate")
    calc.mean <- mean(as.numeric(calc$shipping_cost))
    ifelse(row['shipping_cost'] == "Calculate", 
           yes = calc.mean + as.numeric(row['price']), 
           no = (as.numeric(row['price']) + as.numeric(row['shipping_cost'])))
  }
  z$total <- apply(z, 1, calc.rep)
  calc.est <- function(row){
    ifelse(row['shipping_cost'] == "Calculate", yes = "estimated", 
           no = "exact")
  }
  z$est <- apply(z, 1, calc.est)
  z$price <- df$price
  add_dollar <- function(row){
    ifelse(row['shipping_cost'] != "Calculate", 
           yes = paste0("$", row['shipping_cost']), 
           no = row['shipping_cost'])
  }
  z$shipping_cost <- apply(z, 1, add_dollar)
  z$total <- paste0("$", z$total)
  return(z)
}

filter_best <- function(active, hist){
  active <- total(active)
  hist <- total(active)
  active$total <- as.numeric(substring(active$total, 2))
  x <- active[order(active$total, decreasing = FALSE),]
  hist$total <- as.numeric(substring(hist$total, 2))
  y <- hist[order(hist$total, decreasing = FALSE),]
  x$savings <- median(x$total) - x$total
  cutoff <- quantile(y$total, probs = .25)
  best <- filter(x, total <= cutoff)
  best$savings <- ifelse(best$savings >= 0, 
                         yes = paste0('$', best$savings), 
                         no = paste0('-$',substring(best$savings,2)))
  best$total <- paste0("$", best$total)
  return(list(cutoff = cutoff,best = best))
}
filter_brute <- function(df){
  df$price <- as.numeric(substring(df$price, 2))
  x <- df[order(df$price, decreasing = FALSE),]
  lb <- median(x$price)*0.5
  hb <- median(x$price)*1.5
  x <- filter(x, price <= hb)
  x <- filter(x, price >= lb)
  return(x)
}
filter_kmeans <- function(df){
  df$price <- as.numeric(substring(df$price, 2))
  clu <- kmeans(df$price,3)$cluster
  df <- cbind(df,clu)
  x <- filter(df, clu == 2)
  return(x)
}


base_url <- "http://www.ebay.com/sch/i.html?_from=R40"
#http://www.ebay.com/sch/i.html?_from=R40&_nkw=Lego%20black%20pearl&LH_BIN=1&LH_ItemCondition=12&_ipg=200&_sop=10


shinyServer(function(input, output,session) {
  set = reactive({
    input$set
  })
  
  type = reactive({
    input$type
  })
  
  condition = reactive({
    input$condition
  })
  
  best_offer = reactive({
    if(type() == "Buy it now")
      input$best_offer
  })
  
  free_shipping = reactive({
    input$freeshipping
  })
  
  clustering = reactive({
    input$clustering
  })
  
  sortBy = reactive({
    input$sortBy
  })
  
  url = reactive({
    url = paste0(base_url,"&_nkw=Lego ",set(), gq(type), gq(condition))
    if(best_offer())
      url = paste0(url, gq("Best Offer"))
    if(free_shipping())
      url = paste0(url, gq("Free Shipping"))
    url = paste0(url, "&_ipg=200")  #200 listings per page
    if(type() == 'Auctions')
      url = paste0(url, "&_sop=1")  #if auctions, get the ending soonest listings
    else if(type() == 'Buy it now')
      url = paste0(url, "&_sop=10")  #if BIN, get the newly listed listings
    url
  })
  
  active = reactive({
    if(clustering() == "Brute Force")
      return(filter_brute(scrape(url)))
    else if(clustering() == "Kmeans")
      return(filter_kmeans(scrape(url)))
  })
  
  historical = reactive({
    url_his = paste0(url,gq("Sold"))  #order will be ending recent, no matter sop = 1 or 10
    if(clustering() == "Brute Force")
      return(filter_brute(scrape(url_his)))
    else if(clustering() == "Kmeans")
      return(filter_kmeans(scrape(url_his)))
  })
  
  
  results = reactive({
    res <- filter_best(active(), hist())$best
    if(sortBy() == "lowest total cost"){
      y <- res[order(res$total, decreasing = FALSE),]
    }
    else if(sortBy() == "time ending soonest"){
      y <- res[order(as.numeric(res$time), decreasing = FALSE),]
    }
    y
  })
  output$cutoff_price <- renderText({
    #naive approach: get the 25% quantile of historical()
    filter_best(active(), hist())$cutoff
  })
  
  output$table <- renderTable({
    results()
  })
  
  
})
