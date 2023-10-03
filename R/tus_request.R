#' @docType class
#' @title TusBaseRequest
#' @description TusBaseRequest Class
#' @field url The request URL
#' @field response_headers Response headers
#' @field status_code HTTP response status code
#' @field response_content Response content
#' @field file File to upload
#' @field request_headers Request headers
#' @field content_length Content length
#' @field upload_checksum Upload checksum
#' @field checksum_algorithm Checksum algorithm
#' @field checksum_algorithm_name Checksum algorithm name
#' @importFrom base64enc base64encode
#' @importFrom digest digest
#' @importFrom R6 R6Class
#' @export
TusBaseRequest <- R6::R6Class(
  "TusBaseRequest",
  public = list(
    url = NULL,
    response_headers = list(),
    status_code = NULL,
    response_content = NULL,
    file = NULL,
    request_headers = NULL,
    content_length = NULL,
    upload_checksum = NULL,
    checksum_algorithm = NULL,
    checksum_algorithm_name = NULL,
    #' @description initialization method
    #' @param uploader The TusUploader instance
    initialize = function(uploader) {
      self$url <- uploader$url
      self$response_headers <- list()
      self$file <- uploader$get_file_stream()
      seek(self$file, where = uploader$offset, origin = "start")
      self$request_headers <- c(
        "upload-offset" = toString(uploader$offset),
        "Content-Type" = "application/offset+octet-stream",
        uploader$get_headers()
      )
      self$content_length <- uploader$get_request_length()
      self$upload_checksum <- uploader$upload_checksum
      self$checksum_algorithm <- uploader$checksum_algorithm
      self$checksum_algorithm_name <-
        uploader$checksum_algorithm_name
    },
    #' @description add_checksum Add the upload checksum header for the chunk
    #' @param chunk The chunk to digest.
    add_checksum = function(chunk) {
      if (self$upload_checksum) {
        self$request_headers$`upload-checksum` <-
          paste(self$checksum_algorithm_name,
                iconv(base64enc::base64encode(charToRaw(
                  digest::digest(chunk,
                                 self$checksum_algorithm_name,
                                 serialize = FALSE)
                )), to = "ascii"))
      }
    }
  )
)



#' @docType class
#' @title TusRequest
#' @description TusRequest Class
#' @importFrom R6 R6Class
#' @importFrom httr2 request req_method req_headers req_body_raw req_perform
#' @export
TusRequest <- R6::R6Class("TusRequest",
                          inherit = TusBaseRequest,
                          public = list(
                            #' @description Perform the tus request
                            perform = function() {
                              tryCatch({
                                chunk <- readBin(self$file,
                                                 what = "raw",
                                                 n = self$content_length)
                                close(self$file)
                                self$add_checksum(chunk)

                                resp <-
                                  httr2::request(self$url) |>
                                  httr2::req_method("PATCH") |>
                                  httr2::req_headers(!!!self$request_headers) |>
                                  httr2::req_body_raw(chunk) |>
                                  httr2::req_perform()
                                self$status_code <- resp$status_code
                                self$response_content <-
                                  resp$content
                                self$response_headers <-
                                  resp$headers
                              }, error = function(e) {
                                stop(paste("Error performing request:", e))
                              })
                            }
                          ))
