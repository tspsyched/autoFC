fit_TIRT_mplus_new <- function(data, ...)
{
  file_name <- glue::glue_collapse(sample(0:9, 10, TRUE))
  mplus_data <- make_sem_data(data)
  mplus_model <- make_mplus_code_new(data, eta_file = paste0(file_name,
                                                         ".csv"), ...)
  mplus_object <- suppressMessages(do.call(MplusAutomation::mplusObject,
                                           c(mplus_model, list(rdata = mplus_data))))
  inp_file <- paste0(file_name, ".inp")
  out_file <- paste0(file_name, ".out")
  fit <- MplusAutomation::mplusModeler(mplus_object, modelout = inp_file,
                                       run = 1L, writeData = "always", quiet = FALSE)
  fit$model_code <- readChar(inp_file, file.info(inp_file)$size)
  # unlink(inp_file)
  # unlink(paste0(file_name, ".out"))
  # unlink(gsub("\"", "", fit$results$input$data$file, fixed = TRUE))
  # unlink(fit$results$savedata_info$fileName)
  npersons <- attr(data, "npersons")
  traits <- attr(data, "traits")
  savedata <- fit$results[["savedata"]]
  fit$results[["savedata"]] <- NULL
  ncol_save <- ncol(savedata)
  trait_scores <- trait_scores_se <- matrix(NA, ncol = length(traits),
                                            nrow = npersons)
  if (is.numeric(ncol_save) && length(ncol_save) > 0) {
    cnames <- colnames(savedata)
    tnames <- cnames[grepl("^TRAIT[[:digit:]]+$", cnames)]
    if (length(tnames)) {
      trait_scores <- savedata[, tnames, drop = FALSE]
    }
    tnames_se <- cnames[grepl("^TRAIT[[:digit:]]+_SE$", cnames)]
    if (length(tnames)) {
      trait_scores_se <- savedata[, tnames_se, drop = FALSE]
    }
  }
  colnames(trait_scores) <- colnames(trait_scores_se) <- traits
  fit$results$trait_scores <- trait_scores
  fit$results$trait_scores_se <- trait_scores_se
  class(fit) <- c("mplusObjectTIRT", class(fit))
  TIRTfit(fit, data)
}
