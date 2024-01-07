remotes::install_github(c("rstudio/reticulate",
                          "rstudio/tensorflow",
                          "rstudio/keras"))
# reticulate::install_python("3.10:latest")                          
reticulate::virtualenv_create("./.venv", version = "3.10:latest")
tensorflow::install_tensorflow(envname = "./.venv", version = "release",
                               extra_packages = "tensorflow-text")


#

library(purrr)
library(envir)

library(tensorflow)
library(tfautograph)
library(keras)

use_virtualenv("./.venv")
options(tensorflow.extract.warn_tensors_passed_asis = FALSE)

attach_eval({
  import_from(glue, glue)
  import_from(jsonlite, read_json)
  import_from(withr, with_dir, with_options)
  import_from(keras$layers, Dense)
  np <- reticulate::import("numpy", convert = FALSE)
  
  seq_len0 <- function(x) seq.int(from = 0L, length.out = x)
})



# reticulate::py_install("torch", pip = TRUE)
torch <- reticulate::import("torch", convert = FALSE)
with_dir("~/github/facebookresearch/llama/weights/LLaMA/7B", {
  pretrained_weights <- torch$load("consolidated.00.pth",
                                   map_location = "cpu")
  for (name in names(pretrained_weights)) {
    filename <- sprintf("%s.npy", name)
    array <- pretrained_weights[[name]]$numpy()
    np$save(filename, array)
    message(glue(
      "wrote: '{basename(filename)}' with shape: {array$shape}"))
  }
})


weights_path <- function(filename) normalizePath(file.path(
  "~/github/facebookresearch/llama/weights/LLaMA/",
  glue(filename, .envir = parent.frame())), mustWork = TRUE)


params <- read_json(weights_path("7B/params.json"))
str(params)



tf_text <- reticulate::import("tensorflow_text")
tokenizer_path <- weights_path("tokenizer.model")
tokenizer <- tf_text$SentencepieceTokenizer(
  tf$io$gfile$GFile(tokenizer_path, "rb")$read(),
  add_bos = TRUE, add_eos = FALSE,
)

prompt <- "The best way to attract bees"
tokenizer$tokenize(prompt)

prompt |> tokenizer$tokenize() |> tokenizer$detokenize()


show_tokens <- function(what) {
  if(is.character(what))
    token_ids <- what |> tokenizer$tokenize() |> as.integer()
  else
    token_ids <- as.integer(what)
  tokens <- token_ids |>
    map_chr(function(id) {
      id |>
        as_tensor(shape = c(1)) |>
        tokenizer$detokenize() |>
        as.character()
    })
  
  names(tokens) <- token_ids
  tokens
}

show_tokens(prompt)


RMSNorm(keras$layers$Layer) %py_class% {
  initialize <-
    function(eps = 1e-6, ..., block_id = NULL, feeds_into = NULL) {
      super$initialize(...)
      self$eps <- eps
      self$block_id <- block_id
      self$feeds_into <- feeds_into
    }
  
  build <- function(input_shape) {
    # input_shape == (batch_size, seqlen, params$dim)
    # self$w will broadcast over batch_size and seqlen dims.
    # w_shape == (1, 1, params$dim)
    w_shape <- rep(1L, length(input_shape))
    w_shape[length(input_shape)] <- as.integer(input_shape) |> tail(1L)
    
    # define a local function that will load
    # the pretrained-weights if we supplied `block_id` and `feeds_into`
    import_from({self}, block_id, feeds_into)
    initializer <-if (is.null(block_id))
      "ones"
    else if (block_id >=0) {
      \(...) weights_path("7B/layers.{block_id}.{feeds_into}_norm.weight.npy") |>
        np$load() |> np$expand_dims(0:1)
    } else if(block_id == -1)
      # load weights for the final output normalization layer, which is not
      # part of a TransformerBlock
      \(...) weights_path("7B/norm.weight.npy") |>
      np$load() |> np$expand_dims(0:1)
    
    self$w <- self$add_weight(shape = w_shape,
                              initializer = initializer,
                              trainable = TRUE)
  }
  
  rrms <- function(x) {
    # reciprocal root mean square along the last axis
    x %>% # (batch_size, seqlen, n_features)
      tf$math$square() %>%
      tf$reduce_mean(axis = -1L, keepdims = TRUE) %>% # (batch_size, seqlen, 1)
      tf$math$add(self$eps) %>% # for numerical stability
      tf$math$rsqrt()
  }
  
  call <- function(x) {
    x * self$rrms(x) * self$w
  }
}


precomputed_rotation_matrix <- compute_rotation_matrix(
  seqlen = 2048L, # LLaMA max seqlen
  feature_dim = with(params, dim %/% n_heads)  # head_size
)

apply_rotary_embedding_faster <- function(x) {
  
  rotate_every_two <- function(x) {
    x1 <- x[all_dims(), `::2`]
    x2 <- x[all_dims(), `2::2`]
    x_ <- tf$stack(list(-x2, x1), axis = -1L)
    tf$reshape(x_, tf$shape(x))
  }
  
  repeat_each_twice <- function(x) {
    tf$`repeat`(x, 2L, axis = -1L)
  }
  
  seqlen <- tf$shape(x)[2]
  rot <- precomputed_rotation_matrix[, NA:seqlen, , ]
  
  cos <- Re(rot) |> repeat_each_twice()
  sin <- Im(rot) |> repeat_each_twice()
  
  (x * cos) + (rotate_every_two(x) * sin)
}
rand <- tf$random$uniform(shape(3, 8, params$n_heads, 128))
all(apply_rotary_embedding(rand) ==
      apply_rotary_embedding_faster(rand))