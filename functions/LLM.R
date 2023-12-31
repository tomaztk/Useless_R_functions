remotes::install_github(c("rstudio/reticulate",
                          "rstudio/tensorflow",
                          "rstudio/keras"))
# reticulate::install_python("3.10:latest")                          
reticulate::virtualenv_create("./.venv", version = "3.10:latest")
tensorflow::install_tensorflow(envname = "./.venv", version = "release",
                               extra_packages = "tensorflow-text")




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
