#compute coverage
require(geometa, quietly = TRUE)
cov_1 <- geometa_coverage(version = "19139")
cov_2 <- geometa_coverage(version = "19115-3")
cov <- unique(rbind(cov_1, cov_2))

#coverage summary
cov$id = paste(cov$specification, " - ", cov$schema, " - ", cov$namespace)
stds <- unique(cov$id)
stds <- stds[order(stds)]
cov_summary <- do.call("rbind", lapply(stds, function(std){
  cov_std <- cov[cov$id == std,]
  cov_std_per <- round(nrow(cov_std[cov_std$in_geometa,]) / nrow(cov_std) * 100,2)
  cov_std_in <- nrow(cov_std[cov_std$in_geometa,])
  cov_std_out <- nrow(cov_std[!cov_std$in_geometa,])
  cov_std_refactored <- nrow(cov_std[cov_std$refactored,])
  cov_std_torefactor <- nrow(cov_std[!cov_std$refactored,])
  cov_out <- data.frame(
    id = std,
    Specification = cov_std$specification[1],
    Schema = cov_std$schema[1],
    Title = cov_std$title[1],
    Namespace = cov_std$namespace[1],
    Supported = cov_std_in,
    Missing = cov_std_out,
    Refactored = cov_std_refactored,
    Torefactor = cov_std_torefactor,
    Coverage = cov_std_per,
    stringsAsFactors = FALSE
  )
  print(cov_out)
  return(cov_out)
}))
cov_summary$coverage_class <- floor(cov_summary$Coverage/20)
row.names(cov_summary) <- cov_summary$id
cov_summary$id = NULL

cov_summary <- rbind(
  cov_summary[startsWith(cov_summary$Specification, "ISO"),],
  cov_summary[startsWith(cov_summary$Specification, "GML"),],
  cov_summary[startsWith(cov_summary$Specification, "SWE"),]
)

#cov_summary_md (for README)
cov_summary_md <- cov_summary
cov_summary_md$Coverage <- sapply(1:nrow(cov_summary_md), function(i){
  cov <- cov_summary_md[i,]
  sprintf("[![%1$s](%2$s)](%3$s)",
          row.names(cov),
          URLencode(gsub("PERCENT","%",sprintf("https://img.shields.io/badge/%1$s-%2$sPERCENT-%3$s.svg",
                                               "",round(cov$Coverage),
                                               switch(as.character(cov$coverage_class),
                                                      "0" = "ad0f0f",
                                                      "1" = "ff0c0c",
                                                      "2" = "f9ae2c",
                                                      "3" = "f2eb24",
                                                      "4" = "33cc7a",
                                                      "5" = "4a4ea8"
                                               )))
          ),
          "https://github.com/eblondel/geometa"
  )
})
cov_summary_md$coverage_class <- NULL
cov_summary_md <- cov_summary_md[,c("Specification", "Schema", "Title","Namespace","Coverage","Supported","Missing", "Refactored", "Torefactor")]
cov_summary_md[cov_summary_md$Specification == "ISO/TS 19115-3:2023",]$Specification = paste(cov_summary_md[cov_summary_md$Specification == "ISO/TS 19115-3:2023",]$Specification, emo::ji("new"))
row.names(cov_summary_md) <- NULL
cov_kable <- kableExtra::kable(cov_summary_md, format = "markdown")
kableExtra::save_kable(cov_kable, file = file.path(getwd(), "inst/extdata/coverage/geometa_coverage_summary.md"))

#coverage summary
cov_summary$coverage_class <- NULL
write.csv(cov_summary, file = file.path(getwd(), "inst/extdata/coverage/geometa_coverage_summary.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

#coverage inventory
cov_inventory <- cov
cov_inventory[is.na(cov_inventory$geometa_class),"geometa_class"] <- "<missing>"
write.csv(cov_inventory, file = file.path(getwd(), "inst/extdata/coverage/geometa_coverage_inventory.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")