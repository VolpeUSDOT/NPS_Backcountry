# Compare input files

orig <- read.csv(file.path("Data", "ATMP2011_CompleteDoseVars_dprime.csv"))

orig = orig[-1]

new <- read.csv(file.path("Data", "COMPILED_FROM_INPUT_ATMP2011_CompleteDoseVars_dprime.csv"))

dim(orig)
dim(new)

identical(names(new), names(orig))

new_rowID <- apply(new, MARGIN = 1, FUN = function(x) paste(x, collapse = "_"))

orig_rowID <- apply(orig, MARGIN = 1, FUN = function(x) paste(x, collapse = "_"))

new_not_in_orig <- new_rowID[!new_rowID %in% orig_rowID]

length(new_not_in_orig)

new_not_in_df <- new[!new_rowID %in% orig_rowID,]

# See if the issues is with HikeBeginAfterMidnt = NA
# Yes, these are the rows which were removed.

new2 <- new[!is.na(new$HikeBeginMinAfterMidnt),]

dim(new2)

new2_rowID <- apply(new2, MARGIN = 1, FUN = function(x) paste(x, collapse = "_"))

new2_not_in_orig <- new2_rowID[!new2_rowID %in% orig_rowID]
