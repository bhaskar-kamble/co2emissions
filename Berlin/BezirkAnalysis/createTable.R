createTable <- function(obj,
                        fontSize = 5,
                        isHeader = FALSE,
                        headerName = NULL,
                        columnNames,
                        columnWidths,
                        columnsToRound = NULL,
                        roundOffTo = 2,
                        addRowTotals = FALSE,
                        addColTotals = FALSE,
                        parseHeader = FALSE) {
  require(gridExtra)
  require(grid)
  
  names(obj) <- columnNames
  obj[, columnsToRound] <- data.frame(sapply(obj[, columnsToRound] , function(x) round(x,roundOffTo)))
  
  obj <- obj[ , c("Jahr" , names(obj)[ names(obj) != "Jahr" ])   ] 
  
  if (addColTotals) {
    objtemp <- data.frame(Jahr="Gesamt")
    for (var in names(obj)[ names(obj) != "Jahr" ]) {
      objtemp[[var]] <- sum(obj[[var]])
    }
    obj <- rbind(obj,objtemp)
  }
  if (addRowTotals) {
    obj$Gesamt <- rowSums(obj[ , names(obj)[ names(obj) != "Jahr" ]])  
  }
  
  
  t0 <- ttheme_default()
  tab01 <- tableGrob(obj , theme=t0 , rows=NULL)
  
  if (isHeader) {
    if (!parseHeader) {
      header <- tableGrob(obj[1 , 1] , theme=t0, rows=NULL, cols=headerName)
    }
    if (parseHeader) {
      t1 <- ttheme_default(colhead=list(fg_params = list(parse = TRUE)))
      header <- tableGrob(obj[1 , 1] , theme=t1, rows=NULL, cols=headerName)
    }
    
    jn <- gtable_combine(header[1,],tab01,along=2)
    jn$widths <- columnWidths
    #jn$widths <- rep(max(jn$widths),length(jn$widths))
    jn$layout[1:2,c("l","r")] <- list(1,ncol(obj))
    
  }
  
  if (!isHeader) {
    jn <- tab01
    jn$widths <- columnWidths
  }
  
  
  grid.newpage()
  grid.draw(jn)
}