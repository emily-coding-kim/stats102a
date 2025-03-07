# How to import various functions from various packages etc
# can import in one line or different lines

#' @importFrom stats ave na.omit dgamma optim
#' @importFrom stats optimise
#' @importFrom stats setNames
#' @importFrom  utils head tail modifyList







drop_zero <- function(p, q, nums, neg = FALSE){
  nums <- unlist(nums)
  if(anyNA(nums) || all(nums==0))
    return(list(p = 0, q=0, nums = sum(nums)))
  p <-  max(which(nums > 0) - q - 1)
  if(!neg) p <- max(0, p)
  nums <- head(nums, p + q + 1)
  q <- q - min(which(nums > 0)) + 1
  if(!neg) q <- max(0, q)
  nums <- tail(nums, p + q + 1)
  list(p = p, q = q, nums = nums)
}



#' pqnumber
#' A S3 constructor  for a class that deals with large number
#' @usage pqnumber(sgn, p, q, nums)
#'
#' @param sgn gives the sgn of the number
#' @param p An integer vector indicating the number of decimal points
#' @param q An integer vector indicating the exponent for base 10
#' @param nums A list of integer vectors where each vector is the literal
#' digits of a number. The length of each vector must be equal to the corresponding
#' p + q + 1
#' @examples
#' pqnumber(c(-1,1), c(2,3), c(0,1), list(c(3, 1, 4), c(1, 2, 3, 4, 5)))
#' @returns a pqnumber
#' @export

pqnumber <- function(sgn, p, q, nums){
  a <- unlist(nums)
  if(any( a< 0 , a != floor(a), a > 9, na.rm = TRUE))
    stop("nums must contain integers[0-9]", call. = FALSE)

  if(!is.list(nums)) stop('nums must be a list')
  n <- length(nums)

  if(length(p) == 1) p <- rep(p, n)
  if(length(q) == 1) q <- rep(q, n)
  if(length(sgn) == 1) sgn <- rep(sgn, n)

  if(any(p + q + 1 != lengths(nums), na.rm = TRUE))
    stop("p + q + 1 must equal length(nums)", call. = FALSE)

  if(length(p) != n) stop("length(p) must be 1 or ", n, call. = FALSE)
  if(length(q) != n) stop("length(q) must be 1 or ", n, call. = FALSE)
  if(length(sgn)!= n) stop("length(sgn) must be 1 or ", n, call. = FALSE)

  if(any(p!=floor(p), p < 0, na.rm = TRUE))
    stop("p must contain non-negative integers", call. = FALSE)

  if(any(q!=floor(q), q < 0, na.rm = TRUE))
    stop("q must contain non-negative integers", call. = FALSE)

  if(any(sgn!=floor(sgn), !sgn %in% c(-1:1, NA), na.rm = TRUE))
    stop("sgn must be either -1, 0 or 1", call. = FALSE)

  vals <- do.call(mapply, list(drop_zero, p, q, nums))
  p <- unlist(vals["p", ])
  q <- unlist(vals["q", ])
  nums <- setNames(vals["nums", ], names(nums))

  is.na(sgn) <- sapply(nums, anyNA) | is.na(p) | is.na(q)
  NAS <- is.na(sgn)
  is.na(nums) <- NAS
  is.na(p) <- NAS
  is.na(q) <- NAS
  sgn[sapply(nums, \(x)all(x==0))] <- 0
  zeros <- sgn == 0
  nums[zeros] <- 0
  p[zeros] <- q[zeros] <- 0

  structure(list(sgn = as.integer(sgn),
                 p = as.integer(p),
                 q = as.integer(q),
                 nums = lapply(nums, as.integer)),
            class = "pqnumber")
}


#' @export
length.pqnumber <- function(x) max(lengths(unclass(x)))

#' @export
names.pqnumber <- function(x) names(x$nums)

#' @export
`names<-.pqnumber` <- function (x, value) {
  names(x$nums) <- value
  x
}

#' @export
`[.pqnumber` <- function(x, i) {
  if(missing(i)) return(x)
  if(is.character(i)) i <- match(i, names(x))
  as_pqnumber(lapply(x, \(v)unclass(v)[i]))
}

#' @export
`[<-.pqnumber` <- function(x, i, value){
  if (!length(value)) return(x)
  if (missing(i)) i <- seq_along(x)
  if(is.character(i)) i <- match(i, names(x))
  value <- as_pqnumber(value)
  for(n in names(unclass(x)))   x[[n]][i] <- value[[n]]
  x
}

#' @export
is.na.pqnumber <- function(x) is.na(x$sgn)

#' @export
na.omit.pqnumber <- function(object, ...) object[!is.na(object)]

#' @export
format.pqnumber <- function(x, digits = 5, scientific = FALSE, ...){
  if(any(x$p>7, x$q>=6, na.rm = TRUE)) scientific <- TRUE
  dec_must <-  length(x) > 1 && any(x$p > 0, na.rm = TRUE)
  digs <- min(digits, max(x$p, digits, na.rm = TRUE))
  f <- function(sgn, p, q, nums){
    if(is.na(sgn)) return("NA")
    x <-  drop_zero(p, q, nums, FALSE)
    v <- drop_zero(p, q, nums, TRUE)
    sn <- if(sgn<0) "-" else ""

    if(abs(v$q)>=6 || scientific){
      dec <- paste(head(c(v$nums[-1], integer(digs)), digs), collapse = "")
      pnt <- if(nzchar(dec)) "." else ""
      s <- sprintf("%s%d%s%se%+03d", sn, v$nums[1], pnt, dec, v$q)
    }
    else if(v$q < 0){
      dec <- paste0(head(nums[-1], digs + 3), collapse = "")
      s <- sprintf("%s%d.%s", sn, nums[1], dec)
    }
    else {
      int <- paste0(head(x$nums, x$q+1), collapse = "")
      dec <- paste(head(tail(x$nums, -x$q-1), digits - x$q+3), collapse = "")
      if(!nzchar(dec) && dec_must) dec <- paste0(rep(0,digs), collapse = "")
      pnt <- if(nzchar(dec)) "." else ""
      s <- sprintf("%s%s%s%s", sn, int, pnt, dec)
    }
    s
  }
  format(do.call(mapply, c(f, x)), justify = "right", ...)|>
      setNames(names(x$nums))
}

#' @export
print.pqnumber <- function (x, digits=5, max = 20, quote=FALSE, scientific = FALSE, ...)
{
  if (is.null(max))  max <- getOption("max.print", 10L)

  if (max < length(x)) {
    print(format(x[seq_len(max)], digits, scientific), max = max + 1, quote = quote, ...)
    cat(" [ reached 'max' / getOption(\"max.print\") -- omitted",
        length(x) - max, "entries ]\n")
  }
  else if (length(x))  print(format(x, digits, scientific), max = max, quote=quote, ...)
  else cat(class(x)[1L], "of length 0\n")
  invisible(x)
}

#' A coercion function
#' Coerces a integer, logical, valid character, valid list into a pqnumber
#' @export
#' @examples
#' as_pqnumber(c(0, NA, 3.142, 1234567898765, -123))
#' as_pqnumber("123e1000")
#' as_pqnumber(3.14, p = 3, q = 4)
#' @param x a valid numeric, character, list or logical to be coerced to as_pqnumber
#' @param ... extra arguments passed. ie what p and q should be
#' @return pqnumber
as_pqnumber <- function(x, ...) UseMethod('as_pqnumber')

#' @exportS3Method stats102a123456789::as_pqnumber
as_pqnumber.list <- function(x, ...) do.call(pqnumber,  x)



#' @exportS3Method stats102a123456789::as_pqnumber
as_pqnumber.logical <- function(x, ...){
  pqnumber(sgn = x, p = 0, q = 0, nums = as.list(x))
}


#' @exportS3Method stats102a123456789::as_pqnumber
as_pqnumber.numeric <- function(x, ...){
  if(any(grepl("e+", x))) stop("x cannot be in scientific notation", call. = FALSE)
  if(any(is.infinite(x))) stop("x must be finite", call. = FALSE)
  sgn <- sign(x)
  x <- abs(x)
  x[is.na(x)] <- 0
  qc <- pmax(floor(log10(x)), 0)
  x <- format(x, scientific = FALSE)
  nums <- lapply(strsplit(gsub("[. ]", "", x), ""), as.integer)
  pc <- lengths(nums) - qc - 1
  pqnumber(sgn, pc, qc, nums)
}

#' @exportS3Method stats102a123456789::as_pqnumber
as_pqnumber.pqnumber <- function(x, p=NULL, q=NULL, ...){
  p <- p %||% x$p
  q <- q %||% x$q
  add <- function(sgn, pin, qin, num, p, q){
    #browser()
    if(anyNA(c(sgn, p, q, num, pin, qin))) return(NA)
    c(integer(q - qin), unlist(num), integer(p - pin))
  }
  modifyList(x, list(p = p, q = q,
                     nums = Map(add, x$sgn, x$p, x$q, x$nums, p, q)))
}

#' @exportS3Method stats102a123456789::as_pqnumber
as_pqnumber.character <- function(x, ...){
  x <- trimws(x)
  y <- grepl("^[-+]?\\d*(\\.\\d*)?([eE][-+]?\\d+)?$", x)
  valid <- x[y]
  valid <- sub("(\\D*)[.]", "\\10.", valid)
  n <- length(x)
  p_out <- q_out <- sn_out <- rep(NA, n)
  nums_out <- vector("list", n)
  sn_out[y] <- ifelse(grepl("^-", valid), -1, 1)
  s <- regexec("^[-+]?(\\d*)(?:\\.(\\d*))?(?:[eE]([-+]?\\d+))?$",
               valid, perl = TRUE)
  u <- do.call(rbind, regmatches(valid, s))
  e <- as.integer(u[,4])
  e[is.na(e)] <- 0
  q_out[y] <- qc <- pmax(nchar(u[,2]) - 1 + pmax(e, 0), 0)
  p_out[y] <- pc <- nchar(u[,3]) - pmin(e, 0)
  explode <- function(p, q, num, e){
    n <- length(num)
    if(p+q+1 == n) return(num)
    if(e < 0) c(integer(-e), num)
    else c(num, integer(e))
  }
  nums_out[y] <- gsub("[.-]|e.*", "", valid)|>
    strsplit("")|>
    lapply(as.integer)|>
    Map(explode, pc, qc, num = _, e)
    pqnumber(sn_out, p_out, q_out, nums_out)
}



carry <- function(x){
  for(i in seq(length(x), 2)){
    if(x[i] > 9){
      x[i - 1] <- x[i - 1] + x[i] %/% 10
      x[i] <- x[i] %% 10
    }
    if(x[i] < 0){
      x[i] <- x[i] + 10
      x[i - 1] <- x[i - 1] -1
    }
  }
  x
}



plus <- function(sn1,num1,sn2,num2){
  if(is.na(sn1) | is.na(sn2)) return(list(NA, NA))
  if(sn1 == 0) return(list(sn2, num2))
  if(sn2 == 0) return(list(sn1, num1))
  nums <- carry(c(0, sn1*num1 + sn2*num2))
  sn <- 1
  if(nums[1] < 0) {
    sn <- -1
    nums <- carry(-nums)
  }
  list(sn, nums)
}


mult <- function(x_nums, y_nums){
  if(anyNA(c(x_nums, y_nums))) return(NA)
  if(all(x_nums ==0)||all(y_nums==0)) return(0)
  m <- x_nums > 0; n <- y_nums > 0
  ml <- length(x_nums); nl <- length(y_nums)
  n_zerosx <- ml - sum(m); n_zerosy <- ml - sum(m)
  if((n_zerosx < n_zerosy || sum(m) > sum(n))){
    x_use <- y_nums
    y_use <- x_nums
  }
  else{
    x_use <- x_nums
    y_use <- y_nums
  }
  res <- 0
  n <- length(x_use)
  for(i in which(x_use > 0)){
      res <- res + c(numeric(i - 1), x_use[i]*y_use, numeric(n - i))
  }
  carry(c(0,res))
}


#' @export
Ops.pqnumber <- function (e1, e2 = NULL)
{
  ok <- switch(.Generic, `==` = , `!=` = , `+`= , `-` = ,
               `>` =, `>=` =, `<` = , `<=` = , `*`=, `^`= TRUE, FALSE)
  if (!ok) {
    stop(gettextf("%s not meaningful for pqnumbers", sQuote(.Generic)))
  }
  switch(.Generic,
         "+" =  {
           if( missing(e2)) return(e1)
           e2 <- as_pqnumber(e2)
           p <- pmax(e1$p, e2$p)
           q <- pmax(e1$q, e2$q)
           e1 <- as_pqnumber(e1, p, q)
           e2 <- as_pqnumber(e2, p, q)
           nums <- mapply(plus, e1$sgn, e1$nums, e2$sgn, e2$nums)
           sgn <- unlist(nums[1, ])
           pqnumber(sgn, p, q + (e1$sgn*e2$sgn != 0), nums[2, ])
         },
         "-" =  {
           if(missing(e2)) return(modifyList(e1, list(sgn=-e1$sgn)))
           e1 + - e2
          },
         "*" =   {
           e2 <- as_pqnumber(e2)

           nums <- Map(mult, e1$nums, e2$nums)
           sgn <- e1$sgn*e2$sgn
           p <- e1$p + e2$p
           p[sgn==0] <- 0
           pqnumber(sgn, p, lengths(nums) - p - 1, nums)},
         "^" = {
           if(floor(e2)!=e2) stop("Only implemented where the exponenet is integer")

           Reduce(\(x, i)e1*x, seq_len(e2), rep(1, length(e1)))
         },
         "==" = (e1-e2)$sgn == 0,
         "!=" = (e1-e2)$sgn != 0,
         ">" = (e1-e2)$sgn > 0,
         ">=" = (e1-e2)$sgn >= 0,
         "<" = (e1-e2)$sgn < 0,
         "<=" = (e1-e2)$sgn < 0,
         stop("Operator not implemented yet", call. = FALSE))
}

#' @export
Math.pqnumber <- function(x, ...){

  fm <- function(x, op){
    for(i in seq_len(length(x) - 1)){
      x[i+1] <- match.fun(op)(x[i], x[i + 1])
    }
    x
  }
  switch(.Generic,
         cumsum = fm(x, "+"),
         cumprod = fm(x, "*"),
         cummax = fm(x, "max"),
         cummin = fm(x, "min"),
         abs = modifyList(x, list(sgn=abs(x$sgn))),
         stop("Not implemented"))
}

#' @export
c.pqnumber <- function(...){
  do.call(Map, c(c, lapply(list(...), \(x)unclass(as_pqnumber(x)))))|>
    as_pqnumber()
}



#' @export
Summary.pqnumber <- function(x, ..., na.rm = FALSE){
  x <- c(x, ...)
  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(as_pqnumber(NA))
  switch(.Generic,
         sum = Reduce(\(i,j)i+x[j], seq_along(x), as_pqnumber(0)),
         prod = Reduce(\(i,j)i*x[j], seq_along(x), as_pqnumber(1)),
         min = x[Reduce(\(i, j) if(x[i] < x[j]) i else j, seq_along(x))],
         max = x[Reduce(\(i, j) if(x[i] > x[j]) i else j, seq_along(x))],
         stop("Not implemented"))
}



