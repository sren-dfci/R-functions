status_clean <- function(v) {
  vs <- v
  vs[grepl("Negative", vs, ignore.case = TRUE)] <- "Negative"
  vs[grepl("Positive", vs, ignore.case = TRUE)] <- "Positive"
  vs[grepl("Equivocal", vs, ignore.case = TRUE)] <- "Equivocal"
  print(table(v, vs, useNA = "ifany"))
  return(vs)
}


her2_status <- function(fish, ihc) {
  # check length, unique values
  if (length(fish) != length(ihc)) {
    stop("fish and ihc need to be in equal length")
  }
  # convert factor to character if possible
  fish <- as.character(fish)
  ihc <- as.character(ihc)
  # # vector cleaning
  # fish <- status_clean(fish)
  # ihc <- status_clean(ihc)
  # check options
  s <- c("Negative", "Equivocal", "Positive", "Not Done", "Unknown")
  if (sum(!fish %in% s)) {
    stop("fish has some values unrecognized")
  }
  if (sum(!ihc %in% s)) {
    stop("ihc has some values unrecognized")
  }
  # define
  her2 <- ifelse(
    fish %in% c("Negative", "Positive"),
    fish,
    ifelse(
      fish %in% c("Not Done", "Unknown"),
      ihc,
      ifelse(
        ihc %in% c("Equivocal", "Not Done", "Unknown"),
        ihc,
        ifelse(ihc == "Negative", "Query", "Positive; Query")
      )
    )
  )
  tmp <- data.frame(fish = fish, ihc = ihc, her2 = her2)
  tmp <- unique(tmp)
  tmp <- tmp[order(tmp$her2, tmp$fish, tmp$ihc), ]
  print(tmp)
  return(her2)
}


hr_status <- function(er, pr) {
  # check vector length and unique values
  if (length(er) != length(pr)) {
    stop("er and pr need to be in equal length")
  }
  # convert factor to characater if needed
  er <- as.character(er)
  pr <- as.character(pr)
  # # vector cleaning
  # er <- status_clean(er)
  # pr <- status_clean(pr)
  # check options
  s <- c("Negative", "Low Positive", "Positive", "Not Done", "Unknown")
  if (sum(!er %in% s)) {
    stop("er has some values unrecognized")
  }
  if (sum(!pr %in% s)) {
    stop("pr has some values unrecognized")
  }
  # define HR
  hr <- ifelse(
    er == "Negative" & pr == "Negative",
    "Negative",
    ifelse(
      er == "Positive" & pr == "Positive",
      "Positive",
      "Not Done/Unknown"
    )
  )
  return(hr)
}


cancer_type <- function(her2, hr) {
  # check vector length and unique values
  if (length(her2) != length(hr)) {
    stop("her2 and hr need to be in equal length")
  }
  her2_status <- c(
    "Positive", "Negative", "Equivocal", "Not Done", "Unknown",
    "Query", "Positive; Query"
  )
  hr_status <- c(
    "Positive", "Negative", "Not Done/Unknown"
  )
  if (sum(!her2 %in% her2_status)) {
    stop("her2 has some values unrecognized")
  }
  if (sum(!hr %in% hr_status)) {
    stop("hr has some values unrecognized")
  }
  cancer_type <- ifelse(
    her2 %in% c("Positive", "Positive; Query") & hr == "Negative",
    "her2",
    ifelse(
      her2 == "Negative" & hr == "Positive",
      "hr",
      ifelse(
        her2 == "Negative" & hr == "Negative",
        "tnbc",
        "query"
      )
    )
  )
  return(cancer_type)
}