#' Split train and test data
#'
#' Splits a data matrix into two subsets, returning train and test subsets
#' along with some basic summary statistics. Users may specify preferred
#' sampling proportions, and may provide an integer vector of seed values which
#' will be used in selecting the split that most closely approaches the desired
#' proportions. By default, split_train_and_test splits into 50% TRAIN and 50% TEST.
#'
#' @param matrix An N x M matrix, containing N rows (observations) and M columns (data features)
#' @param obs An integer value specifying how many rows from the original data set should be preserved. (See `size` parameter to \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample}{sample} for details)
#' @param train.prop A real number between 0 and 1, describing the preferred proportion of training data
#' @param test.prop A real number between 0 and 1, describing the preferred proportion of test data
#' @param seeds An integer vector of seed values, for use in randomly sampling observations into train and test subsets
#'
#' @return A **list** containing the following results:
#' \item{`counts`}{a table containing raw counts of train and test observations}
#' \item{`contingency.table`}{a contingency table, describing the proportion of observations assigned to train and test sets}
#' \item{`test`}{a matrix holding the test data subset, with the same number of columns as the input matrix, and one row per sampled observation}
#' \item{`train`}{a matrix holding the train data subset, with the same number of columns as the input matrix, and one row per sampled observation}
#'
#' @export
#'
#' @examples
#' # By default, split_train_and_test attempts to split into 50% TRAIN and 50% TEST, and iterates once.
#' default <- split_train_and_test(iris)
#' default$contingency.table
#'
#' # Increasing the number of seeds increases the likelihood of an optimal split
#' many.seeds <- split_train_and_test(iris, seeds=1:113)
#' many.seeds$contingency.table
#'
#' # Pass proportions if you prefer a different split
#' eighty.twenty <- split_train_and_test(iris, train.prop=0.8, test.prop=0.2)
#'
#' # Use `obs` to subset your data while sampling
#' subset <- split_train_and_test(iris, obs=100)  ## produces ~50 TRAIN and ~50 TEST observations
#' subset$counts
#'
split_train_and_test <- function(matrix, obs=NROW(matrix), train.prop=0.5,
                                 test.prop=0.5, seeds=1){
  result.list <- list()
  for(i in 1:length(seeds)){
    set.seed(seeds[i])
    proportions <- c(train=train.prop, test=test.prop)
    sets <- sample(names(proportions), obs, replace=TRUE, prob=proportions)
    contingency.tbl <- (table(sets)/obs)
    deviation <- abs(train.prop - contingency.tbl["train"])/2

    # Capture the sampling that best approximates the requested proportions
    if(i == 1 || deviation < last.deviation){
      last.deviation <- deviation
      result.list[["counts"]] <- table(sets)
      result.list[["contingency.table"]] <- contingency.tbl
      result.list[["test"]] <- matrix[sets=="test",]
      result.list[["train"]] <- matrix[sets=="train",]
    }
  }
  result.list
}
