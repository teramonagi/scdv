gmm <- function(wv, k, args = list())
{
  x <- ClusterR::center_scale(wv, mean_center = T, sd_scale = T)
  gmm <-  ClusterR::GMM(x, k, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10)
  # predict centroids, covariance matrix and weights
  ClusterR::predict_GMM(x, gmm$centroids, gmm$covariance_matrices, gmm$weights)$cluster_proba
}

calc_idf <- function(doc, word)
{
  D <- length(doc)
  denominator <- colSums(matrix(unlist(purrr::map(doc, ~ word %in% .x)), nrow = D, byrow = TRUE))
  1 + log(D / denominator)
}


#' Get embedding expression by word2vec
#'
#' Get embedding expression by word2vec
#'
#' @inheritParams doc
#' @inheritParams dimension
#' @inheritParams word2vec_args
#' @export
word2vec <- function(doc, dimension, args = list())
{
  output_file <- tempfile(fileext = ".bin")
  document_file <- tempfile()
  # Save doc into temp file
  writeLines(stringr::str_c(unlist(doc), collapse = " "), document_file)
  # Train a model by word2vec
  model <- wordVectors::train_word2vec(document_file, output_file = output_file, vectors = dimension, force = TRUE)
  as.matrix(model@.Data)
}

#' Calculate Word-Topic Vector
#'
#' Calculate Word-Topic Vector
#'
#' @inheritParams doc
#' @inheritParams k
#' @inheritParams dimension
#' @inheritParams p
#' @param word2vec_args Parameters for wrod2vec model ( parameters of wordVectors::train_word2vec )
#' @param gmm_args Parameters for GMM model ( parameters of ClusterR::center_scale and ClusterR::GMM )
#' @export
word_topic_vector <- function(doc, k, dimension, word2vec_args = list(), gmm_args = list())
{
  wv <- word2vec(doc, dimension, word2vec_args)
  prob <- gmm(wv, k, gmm_args)
  wcv <- purrr::map(seq_len(ncol(prob)), ~ prob[,.x]*wv)
  word <- rownames(wv)
  idf <- calc_idf(doc, word)
  idf * purrr::reduce(wcv, ~ cbind(.x, .y))
}

#' Calculate Document Vector (SCDV)
#'
#' Calculate Document Vector (SCDV)
#'
#' @inheritParams doc
#' @inheritParams wtv
#' @export
document_vector <- function(doc, wtv)
{
  word <- rownames(wtv)
  purrr::map(doc, ~ colSums(wtv[.x[.x %in% word], ]))
}

#' Calculate Sparse Composite Document Vector (SCDV)
#'
#' Calculate Sparse Composite Document Vector (SCDV)
#'
#' @inheritParams doc
#' @inheritParams k
#' @inheritParams dimension
#' @inheritParams p
#' @inheritParams word2vec_args
#' @inheritParams gmm_args
#' @export
scdv <- function(doc, k, dimension, p = 0.01, word2vec_args = list(), gmm_args = list()){
  wtv <- word_topic_vector(doc, k, dimension, word2vec_args, gmm_args)
  dv <- document_vector(doc, wtv)
  purrr::map(dv, ~ make_sparse(.x, p))
}

#' @keywords internal
make_sparse <- function(dv, p)
{
  t <- mean(abs(range(dv)))
  ifelse(abs(dv) > p * t, dv, 0)
}

#' List of document. Each document consists of a vector of words (tokenized-word)
#'
#' List of document. Each document consists of a vector of words (tokenized-word)
#'
#' @name doc
#' @title doc
#' @keywords internal
#' @param doc List of document. each document
NULL

#' The number of clusters
#'
#' The number of clusters
#'
#' @name k
#' @title k
#' @keywords internal
#' @param k The number of clusters
NULL

#' The dimensions of word vector representations for every word
#'
#' The dimensions of word vector representations for every word
#'
#' @name dimension
#' @title dimension
#' @keywords internal
#' @param dimension The dimensions of word vector representations for every word
NULL

#' The sparsity threshold for SCDV
#'
#' The sparsity threshold for SCDV
#'
#' @name p
#' @title p
#' @keywords internal
#' @param p The sparsity threshold for SCDV
NULL

#' Word-topic vectors
#'
#' Word-topic vectors
#'
#' @name wtv
#' @title wtv
#' @keywords internal
#' @param wtv Word-topic vectors
NULL

#' Parameters for wrod2vec model ( parameters of wordVectors::train_word2vec )
#'
#' Parameters for wrod2vec model ( parameters of wordVectors::train_word2vec )
#'
#' @name word2vec_args
#' @title word2vec_args
#' @keywords internal
#' @param word2vec_args Parameters for wrod2vec model ( parameters of wordVectors::train_word2vec )
#' @param args Parameters for wrod2vec model ( parameters of wordVectors::train_word2vec )
NULL

#' Parameters for GMM model ( parameters of ClusterR::center_scale and ClusterR::GMM )
#'
#' @name gmm_args
#' @title gmm_args
#' @keywords internal
#' @param gmm_args Parameters for GMM model ( parameters of ClusterR::center_scale and ClusterR::GMM )
#' @param args Parameters for GMM model ( parameters of ClusterR::center_scale and ClusterR::GMM )
NULL
