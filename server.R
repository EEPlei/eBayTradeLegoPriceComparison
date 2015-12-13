library(shiny)
library(magrittr)
library(stringr)
library(httr)
library(dplyr)
library(rvest)
library(xml2)


sets = c("4184: Black Pearl","4195: Queen Annes Revenge","2507: Fire Temple","10232: Palace Cinema","10193: Medieval Market Village")   
numbers = c('4184','4195','2507','10232','10193')
names = c("Black Pearl","Queen Annes Revenge","Fire Temple","Palace Cinema","Medieval Market Village") 
Sets = data.frame(sets = sets, numbers = numbers, names = names)

predicates <- c("Auctions","Buy it now","New","Used","Free Shipping","Best Offer","Sold")
query <- c("&LH_Auction=1","&LH_BIN=1","&LH_ItemCondition=11","&LH_ItemCondition=12","&LH_FS=1","&LH_BO=1","&LH_Complete=1&LH_Sold=1")
map <- data.frame(predicates = predicates, query = query)

gq <- function(pred){ #gq = get query
  map[map$predicates == pred,]$query
} 
scrape <- function(url){
  html = read_html(url,verbose = TRUE)
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
  
  
  t = cbind(title, price,  shipping_cost, web_pages)  
  final_df = as.data.frame(t)
  final_df$shipping_cost[final_df$shipping_cost=="character(0)"] = "Calculate"
  return(final_df)
}
total <- function(df){
  z <- df
  if(nrow(z) == 0){
    return(z)
  }
  free.rep <- function(row){
    ifelse(row$shipping_cost == "FREE", yes = 0, no = row$shipping_cost)
  }
  z$shipping_cost <- apply(z, 1, free.rep)
  dollar.rep <- function(row){
    ifelse(row$shipping_cost != "Calculate" & row$shipping_cost != 0, 
           yes = substring(row$shipping_cost, 2), no = row$shipping_cost)
  }
  z$shipping_cost <- apply(z, 1, dollar.rep)
  
  
  calc <- filter(z, shipping_cost != "Calculate")
  if(nrow(calc) == 0){
    calc.mean = 10
  }else{
    calc.mean <- mean(as.numeric(calc$shipping_cost))
  }
   
  calc.rep <- function(row){
    #print(row["price"])
    print(as.numeric(row["price"]))
    ifelse(row['shipping_cost'] == "Calculate", 
           yes = calc.mean + as.numeric(row["price"]), 
           no = (as.numeric(row["price"]) + as.numeric(row['shipping_cost'])))
    
  }
  
  z$total <- apply(z, 1, calc.rep)
  
  
  
  calc.est <- function(row){
    ifelse(row['shipping_cost'] == "Calculate", yes = "estimated", 
           no = "exact")
  }
  z$est <- apply(z, 1, calc.est)
  #z$price <- df$price
  # add_dollar <- function(row){
  #   ifelse(row$shipping_cost != "Calculate", 
  #          yes = paste0("$", row$shipping_cost), 
  #          no = row$shipping_cost)
  # }
  #z$shipping_cost <- apply(z, 1, add_dollar)
  #z$total <- paste0("$", z$total)
  return(z)
}

filter_best <- function(active, histo){
  if(nrow(active) == 0 | nrow(histo) == 0)
    return("No cheap lego set detected :(")
  active <- total(active)
  histo <- total(histo)
  #active$total <- as.numeric(substring(active$total, 2))
  x <- active[order(active$total, decreasing = FALSE),]
  #histo$total <- as.numeric(substring(histo$total, 2))
  y <- histo[order(histo$total, decreasing = FALSE),]
  x$savings <- median(x$total) - x$total
  cutoff <- quantile(y$total, probs = .25)
  best <- filter(x, total <= cutoff)
  # best$savings <- ifelse(best$savings >= 0, 
  #                        yes = paste0('$', best$savings), 
  #                        no = paste0('-$',substring(best$savings,2)))
  if(nrow(best) == 0)
    return("No cheap lego set detected :(")
  #best$total <- paste0("$", best$total)
  return(best)
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
# filter_kmeans <- function(df){
#   df$price <- as.numeric(substring(df$price, 2))
#   clu <- kmeans(df$price,3)$cluster
#   df <- cbind(df,clu)
#   x <- filter(df, clu == 2)
#   return(x)
# }


base_url <- "http://www.ebay.com/sch/i.html?_from=R40"
#http://www.ebay.com/sch/i.html?_from=R40&_nkw=Lego%20black%20pearl&LH_BIN=1&LH_ItemCondition=12&_ipg=200&_sop=10


shinyServer(function(input, output,session) {
  set = eventReactive(input$go,{
    Sets[Sets$sets == input$set,]$names
  })
  
  type = eventReactive(input$go,{
    input$type
  })
  
  condition = eventReactive(input$go,{
    input$condition
  })
  
  best_offer = eventReactive(input$go,{
    if(type() == "Buy it now")
      input$best_offer
  })
  
  free_shipping = eventReactive(input$go,{
    input$freeshipping
  })
  
  # clustering = eventReactive(input$go,{
  #   input$clustering
  # })
  # 
  # sortBy = eventReactive(input$go,{
  #   input$sortBy
  # })
  
  url = eventReactive(input$go,{
    setname <- str_replace_all(set()," ","%20")
    url = paste0(base_url,"&_nkw=Lego%20",setname, gq(type()), gq(condition()))
    #print(url)
    print(best_offer())
    if(!is.null(best_offer()))
      if(best_offer())
        url = paste0(url, gq("Best Offer"))
    if(!is.null(free_shipping()))
      if(free_shipping())
        url = paste0(url, gq("Free Shipping"))
    url = paste0(url, "&_ipg=200")  #200 listings per page
    if(type() == 'Auctions')
      url = paste0(url, "&_sop=1")  #if auctions, get the ending soonest listings
    else if(type() == 'Buy it now')
      url = paste0(url, "&_sop=10")  #if BIN, get the newly listed listings
    print(url)
    url
  })
  
  active = eventReactive(input$go,{
    # if(clustering() == "Brute Force")
      return(filter_brute(scrape(url())))
    # else if(clustering() == "Kmeans")
    #   return(filter_kmeans(scrape(url())))
  })
  
  historical = eventReactive(input$go,{
    url_his = paste0(url(),gq("Sold"))  
    url_his = str_replace(url_his, "&_ipg=200","")
    url_his = str_replace(url_his, "&_sop=1","") #order will be ending recent
    url_his = str_replace(url_his, "&_sop=10","") #order will be ending recent
    # if(clustering() == "Brute Force")
      return(filter_brute(scrape(url_his)))
    # else if(clustering() == "Kmeans")
    #   return(filter_kmeans(scrape(url_his)))
  })
  
  
  results = eventReactive(input$go,{
    res <- filter_best(active(), historical())
    if(res == "No cheap lego set detected :(")
      return(data.frame(Empty = "No cheap lego set detected :("))
    y <- res[order(res$total, decreasing = FALSE),]

    
    y
  })
  
  output$table <- renderTable({
    results()
  })
})
