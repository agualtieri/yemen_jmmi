# Multiple response question analysis

multiple.response <- function(data, question.prefix) {
  
  
  z <- length(question.prefix)
  temp = vector("list", z)
  
  for (i in 1:z) {
    a = grep(question.prefix[i], names(data))
    b = sum(data[, a] != 0, na.rm = T)
    d = colSums(data[, a] != 0, na.rm = T)
    e = sum(rowSums(data[,a]) !=0, na.rm = T)
    f = as.numeric(c(d, b))
    temp[[i]] = data.frame(question = c(sub(question.prefix[i], 
                                            "", names(d)), "Total"),
                           freq = f,
                           percent = (f/b)*100,
                           percentofcases = (f/e)*100 )
    names(temp)[i] = question.prefix[i]
  }
  temp
}


test <- multiple.response(current.month.analysis, "mrk_supply_issues/")
