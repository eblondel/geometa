#compute coverage
require(geometa, quietly = TRUE)
cov <- geometa_coverage()

#coverage summary
stds <- unique(cov$specification)
stds <- stds[order(stds)]
cov_summary <- do.call("rbind", lapply(stds, function(std){
  cov_std <- cov[cov$specification == std,]
  cov_std_per <- round(nrow(cov_std[cov_std$in_geometa,]) / nrow(cov_std) * 100,2)
  cov_std_in <- nrow(cov_std[cov_std$in_geometa,])
  cov_std_out <- nrow(cov_std[!cov_std$in_geometa,])
  cov_out <- data.frame(
    Standard = std,
    Title = cov_std$title[1],
    Namespace = cov_std$ns_prefix[1],
    Supported = cov_std_in,
    Missing = cov_std_out,
    Coverage = cov_std_per,
    stringsAsFactors = FALSE
  )
  return(cov_out)
}))
cov_summary$coverage_class <- floor(cov_summary$Coverage/20)
row.names(cov_summary) <- cov_summary$Standard

cov_summary <- rbind(
  cov_summary[startsWith(cov_summary$Standard, "ISO"),],
  cov_summary[startsWith(cov_summary$Standard, "GML"),],
  cov_summary[startsWith(cov_summary$Standard, "SWE"),]
)
cov_summary$coverage_class <- NULL
write.csv(cov_summary, file = file.path(getwd(), "inst/extdata/coverage/geometa_coverage_summary.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

#coverage inventory
cov_inventory <- cov
cov_inventory[is.na(cov_inventory$geometa_class),"geometa_class"] <- "<missing>"
write.csv(cov_inventory, file = file.path(getwd(), "inst/extdata/coverage/geometa_coverage_inventory.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")