# plumber.R

#* Echo the parameter that was sent in
#* @get /echo
function(){
  list(msg = 'Hello World')
}

#* Model
#* @param upload File to upload
#* @post /uploadfile
function(req, res){
  fileInfo <- list(formContents = Rook::Multipart$parse(req))
  ## The file is downloaded in a temporary folder
  tmpfile <- fileInfo$formContents$upload$tempfile
  ## Copy the file to a new folder, with its original name
  fn <- file.path(fileInfo$formContents$upload$filename)
  file.copy(tmpfile, fn)
  ## Send a message with the location of the file
  res$body <- paste0("Your file is now stored in ", fn, "\n")
  res
}

# curl -v -F upload=@"D:\projects\notes1.txt" http://localhost:4535/uploadfile

# plumber::plumb('model.R') %>% plumber::pr_run()