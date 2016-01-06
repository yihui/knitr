rm(list = ls(all.names = TRUE))
options(stringsAsFactors = FALSE)

FILES = c(
  list.files('.', '.pdf$'),
  '../../../knitr-book2/DDR-Yihui-Xie-Chap1-3.pdf'
)
i = file.exists(FILES)
if (!all(i)) stop('These files do not exist: ', paste(FILES[!i], collapse = ', '))

counts = data.frame(file = basename(FILES), count = 0)
if (file.exists('download_count.csv')) {
  counts = rbind(counts, read.csv('download_count.csv'))
  counts = counts[order(counts$file, counts$count, decreasing = TRUE), ]
  counts = counts[!duplicated(counts$file), ]
}
rownames(counts) = FILES[match(basename(FILES), counts$file)]

library(httr)
token = Sys.getenv('GH_TOKEN', NA)
if (is.na(token)) stop('GH_TOKEN not found')
a = authenticate(token, "x-oauth-basic", "basic")
GET = function(...) {
  r = httr::GET(..., a)
  stop_for_status(r)
  r
}
DELETE = function(...) {
  r = httr::DELETE(..., a)
  stop_for_status(r)
  r
}
POST = function(...) {
  r = httr::POST(..., a)
  stop_for_status(r)
  r
}

r = GET('https://api.github.com/repos/yihui/knitr/releases')
x = content(r, 'parsed')
for (i in x) {
  if (i$tag_name == 'doc') {
    r_url = i$url
    r_upload = i$upload_url
    break
  }
}
r = GET(r_url)
x = content(r, 'parsed')
# delete assets that already exist
for (i in x$assets) {
  j = match(i$name, counts$file)
  if (is.na(j)) next
  if (i$size == file.info(rownames(counts)[j])[, 'size']) {
    FILES = setdiff(FILES, rownames(counts)[j])
    message(i$name, ' probably not changed')
    next
  }
  counts[j, 2] = counts[j, 2] + i$download_count
  message('deleting ', i$name)
  DELETE(i$url)
  Sys.sleep(1)
}

# update download counts
counts = counts[order(counts$file), , drop = FALSE]
write.csv(counts, 'download_count.csv', row.names = FALSE)

# upload files
for (f in FILES) {
  message('uploading ', f)
  POST(
    sub('{?name,label}', '', r_upload, fixed = TRUE),
    query = list(name = basename(f)), body = upload_file(f, mime::guess_type(f))
  )
  Sys.sleep(1)
}

q('no')
