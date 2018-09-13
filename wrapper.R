wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
