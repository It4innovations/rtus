#' @docType class
#' @title TusClient
#' @description TusClient Class
#' @field url URL
#' @field headers HTTP headers
#' @importFrom R6 R6Class
#' @export
TusClient <- R6::R6Class(
  "TusClient",
  public = list(
    url = "http://localhost/files",
    headers = NULL,
    #' @description initialization method
    #' @param url URL
    #' @param headers Headers
    initialize = function(url = NULL, headers = NULL) {
      if (!is.null(url)) {
        self$url <- url
      }

      if (!is.null(headers)) {
        self$headers <- headers
      }
    },
    #' @description Set headers
    #' @param headers Headers to set
    set_headers = function(headers = list()) {
      self$headers <- c(self$headers, headers)
    },
    #' @description Get the uploader
    #' @param file_path File path
    #' @param file_stream File stream
    #' @param url URL
    #' @param client TusClient
    #' @param chunk_size Chunk size
    #' @param metadata Metadata
    #' @param retries Retry limit
    #' @param retry_delay Retry delay
    #' @param store_url Store URL
    #' @param url_storage URL storage
    #' @param fingerprinter Fingerprinter
    #' @param upload_checksum Upload checksum
    uploader = function(file_path = NULL,
                        file_stream = NULL,
                        url = NULL,
                        client = NULL,
                        chunk_size = NULL,
                        metadata = NULL,
                        retries = 10,
                        retry_delay = 15,
                        store_url = FALSE,
                        url_storage = NULL,
                        fingerprinter = NULL,
                        upload_checksum = TRUE) {
      return(
        TusUploader$new(
          client = self,
          file_stream = file_stream,
          file_path = file_path,
          chunk_size = chunk_size,
          retries = retries,
          retry_delay = retry_delay,
          url = url,
          metadata = metadata,
          url_storage = url_storage,
          fingerprinter = fingerprinter,
          upload_checksum = upload_checksum
        )
      )
    }
  )
)
