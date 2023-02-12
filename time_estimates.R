library(lubridate)

get_duration <- function(filepath) {
	gsub(
		system(
			paste0(
				"ffprobe -i ", normalizePath(filepath),
				" 2>&1 | grep Duration"
			),
			intern = TRUE
		),
		pattern = "\\s*Duration:\\s+(\\d+:\\d+:\\d+).*",
		replacement = "\\1"
	)
}

allep <- fs::dir_ls("content/episode/", regexp = ".*\\/\\d+[_-]")
nonsepcep <- allep[!grepl("Special",allep)]

specep <- allep[grepl("Special",allep)]
length(nonsepcep)
alldurations <- purrr::map_chr(nonsepcep, ~{
	lns <- readLines(.x);
	gsub(".*(\\d+:\\d+:\\d+).*", "\\1", lns[grepl("podcast_duration", lns)])
})
alldurations
alldurations_LD <- purrr::map(
	alldurations, ~lubridate::as.duration(lubridate::hms(.x))
)

total_time <- purrr::reduce(alldurations_LD, `+`)
dur <- lubridate::as.period(total_time)
sprintf('%02d %02d:%02d:%02d', day(dur), hour(dur), minute(dur), second(dur))


allspecdurations <- purrr::map_chr(specep, ~{
	lns <- readLines(.x);
	gsub(".*(\\d+:\\d+:\\d+).*", "\\1", lns[grepl("podcast_duration", lns)])
})
allspecdurations_LD <- purrr::map(
	allspecdurations, ~lubridate::as.duration(lubridate::hms(.x))
)
total_spec_time <- purrr::reduce(allspecdurations_LD, `+`)
sdur <- lubridate::as.period(total_spec_time)
sprintf('%02d %02d:%02d:%02d', day(sdur), hour(sdur), minute(sdur), second(sdur))


gttime <- total_spec_time + total_time
gdur <- lubridate::as.period(gttime)
sprintf('%02d %02d:%02d:%02d', day(gdur), hour(gdur), minute(gdur), second(gdur))


#
audiobooks <- fs::dir_ls("../../../audiobooks/")

bl <- purrr::map(
	audiobooks,
	~lubridate::as.duration(lubridate::hms(get_duration(.x)))
)
tbl <- purrr::reduce(bl, `+`)
tbdur <- lubridate::as.period(tbl)
sprintf('%02d %02d:%02d:%02d', day(tbdur), hour(tbdur), minute(tbdur), second(tbdur))
