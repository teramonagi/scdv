
word2vec <- function(doc, output_file = NULL){
  document_file <- tempfile()
  if(is.null(output_file)){
    output_file <- tempfile(fileext = ".bin")
  }
  # Save doc into temp file
  writeLines(stringr::str_c(unlist(doc), collapse = " "), document_file)
  # Train a model by word2vec
  model <- wordVectors::train_word2vec(document_file, output_file, force=TRUE)
  as.matrix(model@.Data)
}

gmm <- function(word_vector, k){
  x <- ClusterR::center_scale(word_vector, mean_center = T, sd_scale = T)
  gmm <-  ClusterR::GMM(x, k, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10)
  # predict centroids, covariance matrix and weights
  ClusterR::predict_GMM(x, gmm$centroids, gmm$covariance_matrices, gmm$weights)$cluster_proba
}

calc_idf <- function(word, doc){
  D <- length(doc)
  denominator <- colSums(matrix(unlist(purrr::map(doc, ~ word %in% .x)), nrow = D, byrow = TRUE))
  1 + log(D / denominator)
}

word_topics <- function(doc, w2v_args = list(use_), gmm_args){
  wv <- word2vec(doc, output_file = NULL)
  prob <- gmm(wv, k)
  wcv <- purrr::map(seq_len(k), ~ prob[,.x]*wv)
  idf <- calc_idf(dimnames(wv)[[1]], doc)
  idf * purrr::reduce(wcv, ~ cbind(.x, .y))
}

make_sparse <- function(dv){
  t <- mean(abs(range(dv)))
  ifelse(abs(dv) > p/100 * t, dv, 0)
}

scdv <- function(doc, wtv){
  wv <- word2vec(doc)
  prob <- gmm(wv, k)
  wcv <- purrr::map(seq_len(k), ~ prob[,.x]*wv)
  word <- rownames(wv)
  idf <- calc_idf(word, doc)
  wtv <- idf * purrr::reduce(wcv, ~ cbind(.x, .y))
  dv <- purrr::map(doc, ~ colSums(wtv[.x[.x %in% word], ]))
  purrr::map(dv, ~ make_sparse(.x))
}
