
test_that("keras_es for multiclass classification works", {

  skip("heavy")
  skip_if_not(keras::is_keras_available())
  ## tensorflow::tf$random$set_seed(seed)

  model_keras_es$fit_params$add(
    default_mc = list(keras_model = keras_model(), verbose = 0)
  )

  keys_cv["model"] <- "keras_es"
  keys_cv["fit_param"] <- "default_mc"
  cv <- test_model(options_mc, keys_cv, "keras.engine.training.Model",
                   pred_mc_col_names)

})

test_that("keras_es for binary classification works", {

  skip("heavy")
  skip_if_not(keras::is_keras_available())
  ## tensorflow::tf$random$set_seed(seed)

  model_keras_es$fit_params$add(
    default_bin = list(keras_model = keras_model(), verbose = 0)
  )

  keys_cv["model"] <- "keras_es"
  keys_cv["fit_param"] <- "default_bin"
  cv <- test_model(options_bin, keys_cv, "keras.engine.training.Model",
                   pred_bin_col_names)

})

test_that("keras_es for regression works", {

  skip("heavy")
  skip_if_not(keras::is_keras_available())
  ## tensorflow::tf$random$set_seed(seed)

  model_keras_es$fit_params$add(
    default_reg = list(keras_model = keras_model(), verbose = 0)
  )

  keys_cv["model"] <- "keras_es"
  keys_cv["fit_param"] <- "default_reg"
  cv <- test_model(options_reg, keys_cv, "keras.engine.training.Model",
                   pred_reg_col_names)

})

test_that("keras_es for poisson works", {

  skip("heavy")
  skip_if_not(keras::is_keras_available())
  ## tensorflow::tf$random$set_seed(seed)

  model_keras_es$fit_params$add(
    default_pois = list(keras_model = keras_model(), verbose = 0)
  )

  keys_cv["model"] <- "keras_es"
  keys_cv["fit_param"] <- "default_pois"
  cv <- test_model(options_pois, keys_cv, "keras.engine.training.Model",
                   pred_reg_col_names)

})
