dbscan2 <- 
function (data, eps, MinPts = 5, scale = FALSE, method = c("hybrid", 
                                                           "raw", "dist"), seeds = TRUE, showplot = FALSE, countmode = NULL) 
{
  distcomb <- function(x, data) {
    data <- t(data)
    temp <- apply(x, 1, function(x) {
      sqrt(colSums((data - x)^2))
    })
    if (is.null(dim(temp))) 
      matrix(temp, nrow(x), ncol(data))
    else t(temp)
  }
  method <- match.arg(method)
  data <- as.matrix(data)
  n <- nrow(data)
  if (scale) 
    data <- scale(data)
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)
  for (i in 1:n) {
    if (i %in% countmode) 
      cat("Processing point ", i, " of ", n, ".\n")
    unclass <- (1:n)[cv < 1]
    if (cv[i] == 0) {
      if (method == "dist") {
        reachables <- unclass[data[i, unclass] <= eps]
      }
      else {
        reachables <- unclass[as.vector(distcomb(data[i, 
                                                      , drop = FALSE], data[unclass, , drop = FALSE])) <= 
                                eps]
      }
      if (length(reachables) + classn[i] < MinPts) 
        cv[i] <- (-1)
      else {
        cn <- cn + 1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          if (showplot) {
            fileout <- "dbscan"
           fileout2 <- paste(fileout,i,".jpg", sep="")
          jpeg(filename=fileout2)
            plot(data, col = 1 + cv, pch = 1 + isseed)
          dev.off()
          }
                    cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()
          if (method == "hybrid") {
            tempdist <- distcomb(data[ap, , drop = FALSE], 
                                 data[unclass, , drop = FALSE])
            frozen.unclass <- unclass
          }
          for (i2 in seq(along = ap)) {
            j <- ap[i2]
              if (showplot) {
                fileout <- "dbscan"
                fileout2 <- paste(fileout,i,".jpg", sep="")
                jpeg(filename=fileout2)
                plot(data, col = 1 + cv, pch = 1 + isseed)
                dev.off()
            }

            if (method == "dist") {
              jreachables <- unclass[data[j, unclass] <= 
                                       eps]
            }
            else if (method == "hybrid") {
              jreachables <- unclass[tempdist[i2, match(unclass, 
                                                        frozen.unclass)] <= eps]
            }
            else {
              jreachables <- unclass[as.vector(distcomb(data[j, 
                                                             , drop = FALSE], data[unclass, , drop = FALSE])) <= 
                                       eps]
            }
            if (length(jreachables) + classn[j] >= MinPts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 
                                                            0])
            }
            classn[jreachables] <- classn[jreachables] + 
              1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass)) 
      break
  }
  rm(classn)
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  if (showplot) {
    fileout <- "dbscan"
    if(n < 10){numOut <- paste("0",n)}
      else{numOut <- n}
    jpeg(paste(c(fileout,numOut,".jpg"),sep = ""))
    plot(data, col = 1 + cv, pch = 1 + isseed) 
    dev.off()}
  out <- list(cluster = cv, eps = eps, MinPts = MinPts)
  if (seeds && cn > 0) {
    out$isseed <- isseed
  }
  class(out) <- "dbscan"
  out
}