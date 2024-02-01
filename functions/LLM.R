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



TransformerBlock(keras$layers$Layer) %py_class% {
  
  initialize <- function(attn_head_size, attn_n_heads,
                         norm_eps = k_epsilon(), ...,
                         block_id = NULL) {
    super$initialize(...)
    
    self$attention <- Attention(attn_head_size, attn_n_heads,
                                block_id = block_id)
    
    self$feed_forward <- FeedForward(
      hidden_dim = 4 * attn_head_size * attn_n_heads,
      block_id = block_id)
    
    self$attention_norm <- RMSNorm(eps = norm_eps, block_id = block_id,
                                   feeds_into = "attention")
    self$feed_forward_norm <- RMSNorm(eps = norm_eps, block_id = block_id,
                                      feeds_into = "ffn")
  }
  
  call <- function(x, ..., cache = NULL) {
    
    # norm and attention
    x2 <- x |>
      self$attention_norm() |>
      self$attention(..., cache = cache)
    
    # maybe unpack cache returned by Attention
    if(!is.null(cache))
      c(x2, cache) %<-% x2
    
    x <- x + x2 # add residual
    
    # norm and swiglu projection
    x2 <- x %>%
      self$feed_forward_norm() %>%
      self$feed_forward()
    
    x <- x + x2 # residual again
    
    if(is.null(cache)) x else list(x, cache)
  }
}


#adding layers

layer_transformer_block <- create_layer_wrapper(TransformerBlock)
layer_rms_norm <- create_layer_wrapper(RMSNorm)

# input to the model will be output from the tokenizer
input <- layer_input(shape(NA)) #, dtype = "int32")

x <- input |>
  tok_embeddings()  # instantiated earlier in the blog-post

for(block_id in seq_len0(params$n_layers)) {
  x <- x |>
    layer_transformer_block(attn_head_size = params$dim %/% params$n_heads,
                            attn_n_heads = params$n_heads,
                            norm_eps = params$norm_eps,
                            block_id = block_id)
}

# final output projection into logits of output tokens
x <- x |>
  layer_rms_norm(block_id = -1, eps = params$norm_eps) |>
  layer_dense(
    tokenizer$vocab_size(), use_bias = FALSE,
    kernel_initializer = \(...) np$load(weights_path("7B/output.weight.npy"))$`T`
  )

# slice out the logits for the last token
with_options(c(tensorflow.extract.warn_negatives_pythonic = FALSE), {
  output <- x[, -1, ]
})

llama <- keras_model(input, output) %>%
  compile(jit_compile = TRUE)


next_token_probs <- prompt %>%
  tokenizer$tokenize() %>%
  llama()

next_token_probs


for (i in 1:20) {
  
  next_token_probs <- prompt_tokens |> llama()
  next_token <- sampler(next_token_probs)
  
  prompt_tokens %<>% { tf$concat(c(., next_token), axis = -1L) }
  
  # end of sentence
  if (as.logical(next_token == tokenizer$string_to_id(".")))
    break
}

prompt_tokens |>
  tokenizer$detokenize() |>
  as.character() |>
  strwrap(60) |> writeLines()


llama <- TransformerDecoder(vocab_size = tokenizer$vocab_size(),
                            n_blocks = params$n_layers,
                            n_heads = params$n_heads,
                            head_size = params$dim %/% params$n_heads,
                            norm_eps = params$norm_eps)

prompt <- "The best way to attract bees"

test_generate <- function() {
  prompt |>
    tokenizer$tokenize() |>
    llama$generate(as_tensor(17L)) |>
    tokenizer$detokenize() |>
    as.character() |>
    strwrap(60) |> writeLines()
}
