#' KMEANS
#'
#' Generates a K-means clustering of a given feature matrix, with K cluster centers.
#'   K-means is a common non-probabilistic clustering method, useful when data can be
#'   fit to circular clusters.
#'
#' @seealso \href{https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kmeans}{stats::kmeans} for a more robust implementation.
#'
#' @param data_matrix An N x M matrix, containing N rows (observations) and M columns (data features)
#' @param K A positive integer describing the number of clusters the clustering should produce.
#'
#' @return A **list** containing the following results:
#'
#' \item{`cluster`}{a vector of length N containing the cluster ids of each observation. Cluster ids are positive integers from 1:K inclusive.}
#' \item{`centers`}{a matrix of cluster centers, with K rows, and M columns.}
#' \item{`withinss`}{a vector of length K, containing within-cluster sums of squares for each cluster.}
#' \item{`tot.withinss`}{the total within-cluster sum of squares}
#' \item{`size`}{a vector of length K, containing the number of points in each cluster.}
#'
#' @export
#'
#' @examples
#' # Cluster the iris data set into three clusters:
#' KMEANS(iris[1:4], K=3)
KMEANS <- function(data_matrix, K){
  withinss <- numeric(length=K)

  # select K points from the elements in the data for initial cluster centers
  observation.indices <- 1:NROW(data_matrix)
  initial.point.indices <- sample(observation.indices, K)
  centers <- data_matrix[initial.point.indices, ,drop=FALSE]

  last.tot.withinss <- NULL
  iteration <- 1
  while (iteration < 11) {
    cluster <- NULL
    size <- numeric(length=K)

    # Assign each point to the nearest cluster
    for (observation in 1:NROW(data_matrix))
    {
      min.norm <- NULL
      for (center in 1:NROW(centers))
      {
        diff = data_matrix[observation, ] - centers[center, ]
        local.norm <- norm(diff, type="2")
        if (is.null(min.norm) || (local.norm < min.norm)){
          arg.min <- center
          min.norm <- local.norm
        }
      }
      cluster <- c(cluster, arg.min)
      # calculate withinss and size while we're already looping
      obs.local.ss <- sum((data_matrix[observation, ] - centers[arg.min, ])^2)
      withinss[arg.min] <- withinss[arg.min] + obs.local.ss
      size[arg.min] <- size[arg.min] + 1
    }
    tot.withinss <- sum(withinss)

    # terminate main loop if tot.withinss has converged, unless first iteration
    if (!is.null(last.tot.withinss) && (tot.withinss == last.tot.withinss)) {
      break
    }

    # find new cluster means
    for (k in 1:NROW(centers)){
      mask <- which(cluster == k)
      centers[k, ]<- colMeans(data_matrix[mask, ])
    }

    # increment counter and repeat as needed
    iteration = iteration + 1
  }
  # return stored results
  return (list(cluster=cluster,
               centers=centers,
               withinss=withinss,
               tot.withinss=tot.withinss,
               size=size))
}
