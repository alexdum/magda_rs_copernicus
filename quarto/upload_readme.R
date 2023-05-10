

ctry_long <- c("France", "Italy", "Romania")

for (i in 1:length(ctry_long)) {

  print(ctry_long[i])
  RCurl::ftpUpload(
    "quarto/readme.pdf",
    paste0("ftp://213.127.133.58/MAGDA/input/",ctry_long[i],"/Dynamic/RS/readme.pdf"),
    userpwd = "MAGDA:FW@MAGDA_RS"
  )
}
