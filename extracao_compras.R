library(webdriver)
library(rvest)
library(stringr)

extrai.compras = function(limit.stop = 10){
  limit = 0
  
  #CONFIGURAÇÕES
  pjs = run_phantomjs()
  ses <- Session$new(port = pjs$port)
  
  ses$go("https://pulsesms.app/login")
  
  Sys.sleep(.5)
  
  if(ses$getUrl() == "https://pulsesms.app/login"){
    #SOLICITA SENHA E LOGIN DO PULSE
    em = readline("Login: ")
    senha = rstudioapi::askForPassword("Senha")
    
    ses$findElement(xpath = '//*[@id="username"]')$sendKeys(em)
    
    ses$findElement(xpath = '//*[@id="password"]')$sendKeys(senha)
    
    ses$findElement(xpath = '//*[@id="login"]/span')$click()
    
    verif = NA
    
    while(is.na(verif)){
      verif = ses$getSource() %>% 
        read_html() %>% 
        html_node(xpath = '//p/span[1]/text()') %>% 
        html_text() %>% 
        as.numeric()
      
      limit = limit + 1
      
      if(limit == limit.stop){
        stop("ERROR LOGIN")
      }
    }
  }
  
  ses$findElement(xpath = '//*[@id="search-button"]')$click()
  ses$findElement(xpath = '//*[@id="search-bar"]')$sendKeys("27888")
  
  verif = NA
  
  while(verif != "27888" | is.na(verif)){ # MELHORAR ESSA PARTE
    verif = ses$getSource() %>% 
      read_html() %>% 
      html_node(xpath = '//p/span[1]/text()') %>% 
      html_text()
    
    limit = limit + 1
    
    if(limit == limit.stop){
      stop("ERROR AO LOCALIZAR NUMERO")
    }
  }
  
  l = ses$getUrl()
  
  ses$findElement(xpath = '//*[@id="conversation-list"]/div[2]/div[2]')$click()
  
  while(ses$getUrl() == l){}
  
  Sys.sleep(.5)
  
  compras = ses$getSource() %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="message-list"]/div/div/div[1]/div') %>% 
    html_text()
  
  while(compras[1]==""){
    compras = ses$getSource() %>% 
      read_html() %>% 
      html_nodes(xpath = '//*[@id="message-list"]/div/div/div[1]/div') %>% 
      html_text()
  }

  
  invisible(ses$delete())
  invisible(pjs$process$kill())

  final.cartao = str_sub(compras,51,54) %>% as.numeric()
  
  data.compra = as.POSIXct(str_sub(compras,59,74),
                           format = '%d/%m/%Y %H:%M')
  
  
  aux = str_locate(compras,"VALOR DE R")
  
  valor.estab = str_sub(compras,aux[,2]+3)
  
  sep = str_locate(valor.estab," ")
  
  valor = str_sub(valor.estab,end=sep[,2]-1) %>% 
    str_replace_all(",",".") %>% 
    as.numeric()
  
  estab.aux = str_sub(valor.estab,sep[,2]+1)
  
  estab = estab.aux %>% 
    str_sub(end = estab.aux %>% str_locate("Rio|RIO|rio|SAO|Sao|sao") %>% .[,1] -1) %>% 
    str_squish()
  
  fim = tibble(final.cartao = final.cartao,
         data.compra = data.compra,
         valor = valor,
         estab = estab)
  
  tags = read.csv("tags.csv")
  
  fim = sqldf::sqldf("select fim.*, 
                             tags.tag 
                      from fim 
                      left join tags on instr(upper(fim.estab),  upper(tags.estab))")
  
  return(fim)
}
