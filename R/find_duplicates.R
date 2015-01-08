#' returns duplicated rows
#'
#' @param x a data.table
#' @param ... Variables to group by. Default is the key, or everything is the data.table is not keyed.
#' @param gen A character that specifies  the name of a new variable with the number of duplicates. Default to "N".
#' @param vars Used to work around non-standard evaluation.
#' @return a data.table with groups that have duplicates. 
#' @examples
#' library(data.table)
#' DT <- data.table(a = rep(1:2, each = 3), b = 1:6)
#' find_duplicates(DT, a)
#' @export
find_duplicates <- function(x, ..., gen = "N"){
  find_duplicates_(x, vars = lazyeval::lazy_dots(...), gen = gen)
}

#' @export
#' @rdname find_duplicates
find_duplicates_ <- function(x, vars, gen = "N"){
  stopifnot(is.data.table(x))
  names <- names(x)
  if (gen %in% names)   stop(paste("A variable named", gen, "already exists."))
  if (anyDuplicated(names))  stop("x has duplicate column names")
  dots <- lazyeval::all_dots(vars)
  vars <- names(select_vars_(names(x), dots))
  if (!length(vars)){
    if (length(key(x))){
      vars <- key(x)
    } else{
      vars <- names(x)
    }
  }
  ans <- x[, .I[.N>1L], by=c(vars)]
  ans <- ans[[length(ans)]]
  ans <- x[ans]
  n_groups <- nrow(unique(ans, by = vars))
  message(paste(n_groups,"groups have duplicates"))
  if (nrow(ans)){
    ans[, c(gen) := .N, by = c(vars)]
    setkeyv(ans, c(gen, vars))
    setcolorder(ans, c(gen, vars, setdiff(names(ans), c(vars, gen))))
  }
  ans[]
}

