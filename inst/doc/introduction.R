## ---- message = FALSE----------------------------------------------------
# Subclass "Labelled_df" for R6Frame.
library(R6Frame)
is.labelled_df <- function(x) inherits(x, "Labelled_df")
Labelled_df <- R6::R6Class("Labelled_df",
  inherit = R6Frame::R6Frame,
  
  # Private methods
  private = list(.label = NULL),
  
  # Public methods
  public = list(
    set_label = function(..., list = NULL) {
      # Set label for a variable.
      new <- merge_vectors(..., list, private$.label, default = self$names())
      private$.label <- new
      invisible(self)
    },
    get_label = function(which = NULL) {
      # Get label for a specified variable. (NULL = all labels.)
      res <- private$.label
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, names(res))]
        if (!length(res)) res <- NULL
      }
      res
    }
  )
  
)

# Utility function that merges named vectors for private field (.label) in Labelled_df.
# Duplicates are dropped from the end of the named vector (after unlisting).
merge_vectors <- function(..., default = NULL) {
  dots <- list(...)
  if (!length(dots)) stop("No vectors supplied.")

  # Use unnamed default's as names for a vector of NA's.
  # (So we can pass colummnames as a default.)
  if (!is.null(default) && is.null(names(default))) {
    default <- setNames(rep(NA, length(default)), default)
  }

  res <- c(unlist(dots), default)
  nms <- names(res)
  if (is.null(nms) && any(is.na(nms)) && any(nms == ""))
    stop("All elements must be named.")

  # Return should be ordered by default if it exists
  res <- res[!duplicated(names(res), fromLast = FALSE)]
  if (!is.null(default)) {
    res[names(default)]
  } else {
    res
  }

}

## ------------------------------------------------------------------------
Labelled_df$set(
  "public",
  "do_merge",
  function(f, dots, env) {
      # Get existing labels from data
      lab <- lapply(dots, function(x) { if (is.labelled_df(x)) x$get_label() })
      # Assign to private field (self$do and in turn self$update will remove duplicates)
      private$.label <- merge_vectors(private$.label, lab)
      # Call the default R6Frame method "do_merge" to complete the operation.
      super$do_merge(f, dots, env)
  }
)

## ------------------------------------------------------------------------
Labelled_df$set(
  "public",
  "update",
  function(renamed = NULL) {
      # If renamed is not NULL, it is a vector with the same length as the data
      # and contains the new names for the variables. (Usually from dplyr methods.)
      if (!is.null(renamed)) {
        private$.label <- setNames(private$.label, renamed)
      }
      self$set_label()
      self
  }
)

## ------------------------------------------------------------------------
org <- data.frame("A" = c("Yes","No"), "B" = c(1, 2), stringsAsFactors = FALSE)

# Initialize
df <- Labelled_df$new(org)

# Set label
df$set_label(A = "Yes/No", B = "Numbers")
df$get_label()

# Add a column
df[, "test"] <- "test"

# Split and set label
df2 <- df[, "test", drop = FALSE]
df[["test"]] <- NULL

df2$set_label(test = "Test label")

# Join again
df3 <- cbind(df2, df)
df3$get_label()

