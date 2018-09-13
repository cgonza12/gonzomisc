myBuilder <- function (object, n_features = 5)
{
  if (!class(object) == "sel_obj")
    stop(paste0("This function only works with objects of type ",
                sQuote("sel_obj"), "!"))
  if (n_features > length(pull(object@stability[, "feature"]))) {
    n_features <- length(pull(object@stability[, "feature"]))
    warning(paste0("Seems like you chose too many features. Try to reduce ",
                   sQuote("n_features"), ". Using all features: ",
                   n_features))
  }
  form <- as.formula(paste(object@setup[["target"]],
                           "~", paste(pull(object@stability[, "feature"][1:n_features,
                                                                         ]), collapse = "+"), sep = ""))
  return(form)
}