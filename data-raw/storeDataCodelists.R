require(geometa)

standards = c("19139", "19139-3")
codelists <- lapply(standards, function(standard){
  #ISO 19139
  cl = geometa::parseISOCodelists(version = standard)
  cls = lapply(cl, function(x){
    list(
      identifier = x$identifier$value,
      data = x$getCodeEntries(pretty = TRUE),
      codeSpace = x$codeSpace,
      description = x$description,
      id = x$id,
      refFile = x$refFile
    )
  })
  names(cls) = names(cl)
  return(cls)
})
names(codelists) = standards

usethis::use_data(codelists, overwrite = TRUE)