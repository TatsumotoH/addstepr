# add new step functions for recipes and tunes


#  step_dbscan_fpc ----
#

step_dbscan_fpc_new <- function(terms, role, trained, skip, id, eps,  minPts, retain, model, data) {
  step(
    subclass = "dbscan_fpc",
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id,
    eps = eps,
    minPts = minPts,
    retain = retain,
    model = model,
    data = data
  )
}


#' step_dbscan: a recipe step for clustering by dbscan.
#' fpcパッケージのdbscan関数を用いたstep関数
#'
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?
#' @param trained A logical to indicate if the quantities for preprocessing
#'  have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'  by [recipes::bake.recipe()]?
#' @param eps The parameter eps defines radius of neighborhood around a point x.
#' @param minPts The parameter MinPts is the minimum number of neighbors within “eps” radius.
#' @param retain Alogical to specify whether the original predictors should be retained along with the new embedding 
#'  variables.
#' @param id A character string that is unique to this step to identify it.
#' @export
step_dbscan_fpc = function(recipe, ..., role = "predictor", trained = FALSE, skip = FALSE,
                        eps = NULL, minPts = NULL, retain = FALSE, id = rand_id("dbscan_fpc")) {
  if (is.null(eps)) stop("eps value is not defined")
  add_step(recipe,
           step_dbscan_fpc_new(terms = recipes::ellipse_check(...),
                           role = role,
                           trained = trained,
                           skip = skip,
                           id = id,
                           eps = eps,
                           minPts = minPts,
                           retain = retain,
                           model = NULL,
                           data = NULL
           )
  )
}

#' prep for step_dbscan_fpc
#' @export

prep.step_dbscan_fpc = function(x, training, info = NULL, ...) {
  dat = training[, recipes::terms_select(x$terms, info = info), drop = FALSE]

  if (sum(is.na(dat)) > 0) {
    warning("Missing values were present")
    dat <- na.omit(dat)
    if (nrow(dat) == 0) stop("No rows remain in dataset after missing values rows omitted")
  }
  if (ncol(dat) == 0) stop("Clusters not created as no numeric columns were found")
  if (is.null(x$minPts)) {
    minPts <- ncol(dat) + 1
  } else {
    minPts <- x$minPts
  }


  mod = fpc::dbscan(data = dat, eps = x$eps, MinPts = minPts, scale = FALSE, method = "hybrid", seeds = TRUE)

  if (is.null(mod$isseed)) {
    stop(paste("No clusters can be detected using MinPts = ", minPts, "and eps = ", x$eps))
  }


  step_dbscan_fpc_new(terms = x$terms,
                  role = x$role,
                  trained = TRUE,
                  skip = x$skip,
                  id = x$id,
                  eps = x$eps,
                  MinPts = x$minPts,
                  retain = x$retain,
                  model = mod,
                  data = dat)
}

#' bake(juice) for step_dbscan_fpc
#' @export
bake.step_dbscan_fpc = function(object, new_data, ...) {


  new_data_predictors = new_data[,names(object$data)]


  clus <- predict(object$model, data = object$data, newdata = new_data_predictors)

  new_data <- cbind(new_data, cluster = as.factor(paste0("C",clus)))

  if (!object$retain) {
    new_data[, names(object$data)] = NULL
  }

  as_tibble(new_data)
}

#' print for step_dbscan
#' @export
print.step_dbscan_fpc = function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    cat(paste0("dbscan step found ", length(unique(x$mod$cluster)) - 1," clusters using "))
    cat(recipes::format_selectors(x$terms, width = width))
    cat(" [trained]\n")
  } else {
    cat("dbscan step for ")
    cat(recipes::format_selectors(x$terms, width = width))
    cat("\n")
  }
  invisible(x)
}

#' tidy for step_dbscan_fpc
#' @export
tidy.step_dbscan_fpc = function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}

# step_dbscan_fをtunableにするためのコード  ここから
# 参考
# https://qiita.com/takechanman1228/items/c7f23873c087630bab18


#パラメータepsにレンジを与える関数
#' @export
eps <- function(range = c(0.1, 3), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(eps = "eps"),
    finalize = NULL
  )
}

#パラメータMinPtsにレンジを与える関数
#' @export
minPts <- function(range = c(1L, 20L), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(minPts = "minPts"),
    finalize = NULL  #データ確定時(=finalize)に呼び出されるhook関数.データ依存のパラメータレンジ設定に使用
  )
}


#tunable関数にstep_dbscan関数を登録する
#' @export
tunable.step_dbscan_fpc = function(x, ...) {
  tibble::tibble(
    name = c("eps", "minPts"),
    call_info = list(
      list(pkg = "addstepr", fun = "eps", range=c(0.1,3)), ##一番はじめのnameのパラメータepsに対応するパラメータ範囲
      list(pkg = "addstepr", fun = "minPts", range=c(1,20))  ##一番はじめのnameのパラメータMinPtsに対応するパラメータ範囲
    ),
    source = "recipe",    #recipe or model_spec
    component = "step_dbscan_fpc",
    component_id = x$id
  )
}

# step_dbscanをtunableにするためのコード   ここまで


# step_dbscan ----
# originalのdbscanアルゴリズムによるdbscanの実装
# dbscanパッケージのdbscan関数を利用
#
step_dbscan_new = function(terms, role, trained, skip, id, eps,  minPts, retain, model, data) {
  step(
    subclass = "dbscan",
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id,
    eps = eps,
    minPts = minPts,
    retain = retain,
    model = model,
    data = data
  )
}

#' step_dbscan: a recipe step for clustering by dbscan (original algorithm).
#' dbscanパッケージのdbscan関数を利用
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?
#' @param trained A logical to indicate if the quantities for preprocessing
#'  have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'  by [recipes::bake.recipe()]?
#' @param eps The parameter eps defines radius of neighborhood around a point x.
#' @param minPts The parameter minPts is the minimum number of neighbors within “eps” radius.
#' @param retain Alogical to specify whether the original predictors should be retained along with the new embedding 
#'  variables.
#' @param id A character string that is unique to this step to identify it.
#' @export
step_dbscan = function(recipe, ..., role = "predictor", trained = FALSE, skip = FALSE,
                        eps = NULL, minPts = NULL, retain = FALSE, id = rand_id("dbscan")) {
  if (is.null(eps)) stop("eps value is not defined")
  add_step(recipe,
           step_dbscan_new(terms = recipes::ellipse_check(...),
                           role = role,
                           trained = trained,
                           skip = skip,
                           id = id,
                           eps = eps,
                           minPts = minPts,
                           retain = retain,
                           model = NULL,
                           data = NULL
           )
  )
}


#' prep for step_dbscan
#' @export
prep.step_dbscan = function(x, training, info = NULL, ...) {
  dat = training[, recipes::terms_select(x$terms, info = info), drop = FALSE]

  if (sum(is.na(dat)) > 0) {
    warning("Missing values were present")
    dat <- na.omit(dat)
    if (nrow(dat) == 0) stop("No rows remain in dataset after missing values rows omitted")
  }
  if (ncol(dat) == 0) stop("Clusters not created as no numeric columns were found")
  if (is.null(x$minPts)) {
    minPts <- ncol(dat) + 1
  } else {
    minPts <- x$minPts
  }

  mod = dbscan::dbscan(x = dat, eps = x$eps, minPts = minPts, weights = NULL, borderPoints = TRUE)

  if (0 == sum(mod$cluster)) {
    stop(paste("No clusters can be detected using minPts = ", minPts, "and eps = ", x$eps))
  }


  step_dbscan_new(terms = x$terms,
                  role = x$role,
                  trained = TRUE,
                  skip = x$skip,
                  id = x$id,
                  eps = x$eps,
                  minPts = x$minPts,
                  retain = x$retain,
                  model = mod,
                  data = dat)
}


#' bake(juice) for step_dbscan
#' @export
bake.step_dbscan = function(object, new_data, ...) {


  new_data_predictors = new_data[,names(object$data)]


  clus <- predict(object$model, data = object$data, newdata = new_data_predictors)

  new_data <- cbind(new_data, cluster = as.factor(paste0("C",clus)))

  if (!object$retain) {
    new_data[, names(object$data)] <- NULL
  }

  as_tibble(new_data)
}


#' print for step_dbscan
#' @export
print.step_dbscan = function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    cat(paste0("dbscan step found ", length(unique(x$mod$cluster)) - 1," clusters using "))
    cat(recipes::format_selectors(x$terms, width = width))
    cat(" [trained]\n")
  } else {
    cat("dbscan step for ")
    cat(recipes::format_selectors(x$terms, width = width))
    cat("\n")
  }
  invisible(x)
}


#' print for step_dbscan
#' @export
tidy.step_dbscan = function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}

# step_dbscanのパラメータをtunableにする関数

#' パラメータepsにレンジを与える関数
#' @export
#' eps2 = function(range = c(0.1, 3), trans = NULL) {
#'   new_quant_param(
#'     type = "double",
#'     range = range,
#'     inclusive = c(TRUE, TRUE),
#'     trans = trans,
#'     label = c(eps2 = "eps"),
#'     finalize = NULL
#'   )
#' }
#' 
#' #' パラメータminPtsにレンジを与える関数
#' #' @export
#' minPts2 = function(range = c(1L, 20L), trans = NULL) {
#'   new_quant_param(
#'     type = "integer",
#'     range = range,
#'     inclusive = c(TRUE, TRUE),
#'     trans = trans,
#'     label = c(minPts2 = "minPts"),
#'     finalize = NULL  #データ確定時(=finalize)に呼び出されるhook関数.データ依存のパラメータレンジ設定に使用
#'   )
#' }

#' tunable関数にstep_dbscan関数を登録するを
#' @export
tunable.step_dbscan = function(x, ...) {
  tibble::tibble(
    name = c("eps", "minPts"),
    call_info = list(
      list(pkg = "addstepr", fun = "eps", range=c(0.1,3)), ##一番はじめのnameのパラメータepsに対応するパラメータ範囲
      list(pkg = "addstepr", fun = "minPts", range=c(1,20))  ##一番はじめのnameのパラメータminPtsに対応するパラメータ範囲
    ),
    source = "recipe",    #recipe or model_spec
    component = "step_dbscan",
    component_id = x$id
  )
}

# -----ここまで step_dbscan

# step_kmeans ----
#
step_kmeans_new = function(terms, role, trained, skip, id, num_k, retain, model, data) {
  step(
    subclass = "kmeans",
    terms = terms,
    role = role,
    trained = trained,
    skip = skip,
    id = id,
    num_k = num_k,
    retain = retain,
    model = model,
    data = data
  )
}

#' step_kmeans: a recipe step for clustering by kmeans.
#'
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?
#' @param trained A logical to indicate if the quantities for preprocessing
#'  have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'  by [recipes::bake.recipe()]?
#' @param num_k The parameter num_k is passed to the parameter centers in kmeans function.
#' @param retain A logical to specify whether the original predictors should be retained along with the new embedding 
#'  variables.
#' @param id A character string that is unique to this step to identify it.
#' @export
step_kmeans = function(recipe, ..., role = "predictor", trained = FALSE, skip = FALSE,
                       num_k = NULL, retain = FALSE, id = rand_id("kmeans")) {
  if (is.null(num_k)) stop("num_k value is not defined")
  add_step(recipe,
           step_kmeans_new(terms = recipes::ellipse_check(...),
                           role = role,
                           trained = trained,
                           skip = skip,
                           id = id,
                           num_k = num_k,
                           retain = retain,
                           model = NULL,
                           data = NULL
           )
  )
}


#' prep for step_dkmeans
#' @export
prep.step_kmeans = function(x, training, info = NULL, ...) {
  dat = training[, recipes::terms_select(x$terms, info = info), drop = FALSE]
  
  if (sum(is.na(dat)) > 0) {
    warning("Missing values were present")
    dat <- na.omit(dat)
    if (nrow(dat) == 0) stop("No rows remain in dataset after missing values rows omitted")
  }
  
  if (ncol(dat) == 0) stop("Clusters not created as no numeric columns were found")
  
  if (is.null(x$num_k)) {
    num_k <- ncol(dat) - 1
  } else {
    num_k <- x$num_k
  }
  
  mod = kmeans(x = dat, centers  = x$num_k)
  
  if (0 == sum(mod$cluster)) {
    stop(paste("No clusters can be detected using minPts = ", minPts, "and eps = ", x$eps))
  }
  
  
  step_kmeans_new(terms = x$terms,
                  role = x$role,
                  trained = TRUE,
                  skip = x$skip,
                  id = x$id,
                  num_k = x$num_k,
                  retain = x$retain,
                  model = mod,
                  data = dat)
}


#' bake for kmeans
#' @export
bake.step_kmeans = function(object, new_data, ...) {
  
  # predict function for stats::kmeans
  predict.kmeans <- function(object,
                             newdata,
                             method = c("centers", "classes")) {
    method <- match.arg(method)
    
    centers <- object$centers
    ss_by_center <- apply(centers, 1, function(x) {
      colSums((t(newdata) - x) ^ 2)
    })
    best_clusters <- apply(ss_by_center, 1, which.min)
    
    if (method == "centers") {
      centers[best_clusters, ]
    } else {
      best_clusters
    }
  }  
  
  
  new_data_predictors = new_data[,names(object$data)]
  
  
  clus <- predict.kmeans(object$model, newdata = new_data_predictors, method = "classes")
  
  new_data <- cbind(new_data, cluster = as.factor(paste0("C",clus)))
  
  if (!object$retain) {
    new_data[, names(object$data)] <- NULL
  }
  
  as_tibble(new_data)
}


#' print for step_kmeans
#' @export
print.step_kmeans = function(x,  width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    cat(paste0("kmeans step found ", length(unique(x$mod$cluster)) - 1," clusters using "))
    cat(recipes::format_selectors(x$terms, width = width))
    cat(" [trained]\n")
  } else {
    cat("kmeans step for ")
    cat(recipes::format_selectors(x$terms, width = width))
    cat("\n")
  }
  invisible(x)
}


#' print for step_kmeans
#' @export
tidy.step_kmeans = function(x, ...) {
  res <- recipes:::simple_terms(x, ...)
  res$id <- x$id
  res
}

# tunable function for step_kmeans

#' パラメータepsにレンジを与える関数
#' @export
#' eps2 = function(range = c(0.1, 3), trans = NULL) {
#'   new_quant_param(
#'     type = "double",
#'     range = range,
#'     inclusive = c(TRUE, TRUE),
#'     trans = trans,
#'     label = c(eps2 = "eps"),
#'     finalize = NULL
#'   )
#' }
#' 
#' #' パラメータminPtsにレンジを与える関数
#' #' @export
#' minPts2 = function(range = c(1L, 20L), trans = NULL) {
#'   new_quant_param(
#'     type = "integer",
#'     range = range,
#'     inclusive = c(TRUE, TRUE),
#'     trans = trans,
#'     label = c(minPts2 = "minPts"),
#'     finalize = NULL  #データ確定時(=finalize)に呼び出されるhook関数.データ依存のパラメータレンジ設定に使用
#'   )
#' }

#パラメータMinPtsにレンジを与える関数
#' @export
num_k <- function(range = c(1L, 10L), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_k = "num_k"),
    finalize = NULL  #データ確定時(=finalize)に呼び出されるhook関数.データ依存のパラメータレンジ設定に使用
  )
}



#' tunable関数にstep_dbscan関数を登録するを
#' @export
tunable.step_kmeans = function(x, ...) {
  tibble::tibble(
    name = c("num_k"),
    call_info = list(
      list(pkg = "addstepr", fun = "num_k", range=c(1L,10L)) ##一番はじめのnameのパラメータnum_kに対応するパラメータ範囲
    ),
    source = "recipe",    #recipe or model_spec
    component = "step_kmeans",
    component_id = x$id
  )
}
