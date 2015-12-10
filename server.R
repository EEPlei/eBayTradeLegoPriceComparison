library(shiny)
library(magrittr)
library(httr)

sets = c("4184: Black Pearl","4195: Queen Annes Revenge","2507: Fire Temple","10232: Palace Cinema","70810: MetalBeard's Sea Cow","10193: Medieval Market Village")   
numbers = c('4184','4195','2507','10232','70810','10193')
names = c("Black Pearl","Queen Annes Revenge","Fire Temple","Palace Cinema","MetalBeard's Sea Cow","Medieval Market Village") 
Sets = data.frame(sets = sets, numbers = numbers, names = names)

predicates <- c("Auctions","Buy it now","New","Used","Free Shipping","Best Offer","Sold")
query <- c("&LH_Auction=1","&LH_BIN=1","&LH_ItemCondition=11","&LH_ItemCondition=12","&LH_FS=1","&LH_BO=1","&LH_Sold=1")
map <- data.frame(predicates = predicates, query = query)

gq <- function(pred){ #gq = get query
  map[map$predicates == pred,]$query
} 

scrape <- function(url){
  
  return(df)
}

filter <- function(df){
  
  return(df)
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
  
  url = reactive({
    url = paste0(base_url,"&_nkw=Lego ",set(), gq(type), gq(condition))
    if(best_offer())
      url = paste0(url, gq("Best Offer"))
    if(free_shipping)
      url = paste0(url, gq("Free Shipping"))
    url = paste0(url, "&_ipg=200")  #200 listings per page
    if(type == 'Auctions')
      url = paste0(url, "&_sop=1")  #if auctions, get the ending soonest listings
    else if(type == 'Buy it now')
      url = paste0(url, "&_sop=10")  #if BIN, get the newly listed listings
    url
  })
  
  active = reactive({
    filter(scrape(url))
  })
  
  historical = reactive({
    url_his = paste0(url,gq("Sold"))
    filter(scrape(url_his))
  })
  
  output$cutoff_price <- renderText({
    #naive approach: get the 25% quantile of historical()
    print()
  })
  
  output$table <- renderTable({
    result <- active()
    active() <- active()[c("title","price","time to ending", "shipping cost","link")] %>%
                active()[active()$price <= output$cutoff_price]
    result
  })
  
  
})
