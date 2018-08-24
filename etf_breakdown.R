library(rvest)

sector_bd <- function(v) {
  theurl <- paste("https://www.etf.com/",v,sep = "")
  file <- read_html(theurl)
  tables <- html_nodes(file, "div")

  h <- read_html(theurl)
  h <- h %>% 
  html_nodes(xpath = '//*[@id="fit"]')

  s <- unlist(strsplit(html_text(h),"\n"))
  s <- s[nchar(s) > 0]

  a <- grep("segment benchmark",s,ignore.case = T)[1]
  b1 <- grep("top",s,ignore.case = T)
  b2 <-  grep("holdings",s,ignore.case = T)
  b <- intersect(b1,b2)[1]

  s <-s[(a+1):(b-1)]
  s <- s[!s == " "]

  s <- gsub("%","",s)
  ds <- data.frame(matrix(s,,3,byrow = T))
  ds[,-1] <- ds[,-1] <- apply(ds[,-1],2,as.numeric)
  names(ds) <- c("Sector","Percent1","Percent2")
  ds$Tic <- v
  return(ds)
}

v.list <- c("SPY","VTI","AGG","VHT","XHB","TBLU","ROBO","PXLG")
ds.list <- list()
for(v in v.list) { 
  ds.list <- c(ds.list,list(try(sector_bd(v))))
}

ds.list <- ds.list[sapply(ds.list, class) == "data.frame"]
DS <- Reduce(rbind,ds.list)

write.csv(DS,"etf_bd.csv",row.names = F)




