### ----------------------------------------- ##
### STAT509 site: copy built quizzes + write index.html (GitHub Pages, repo rllob/stat509)
### Usage (from this folder): Rscript build_site.R
###   reads site.yaml; for each quiz: copies <bank>.html (built by quiz-deck/build_quiz.R) to
###   quizzes/<file>, reads the bank for title / question count / topics; writes index.html
### ----------------------------------------- ##

suppressPackageStartupMessages({ library(yaml); library(jsonlite) })
`%||%` <- function(a, b) if (is.null(a)) b else a
fa   <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", fa, value = TRUE)[1])))
setwd(here)
S <- read_yaml("site.yaml")
dir.create("quizzes", showWarnings = FALSE)

esc <- function(s) { s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE); gsub(">", "&gt;", s, fixed = TRUE) }
cards <- list(); meta <- list()
for (sec in S$sections) {
  items <- character(0)
  for (q in sec$quizzes) {
    bank <- read_yaml(q$bank)
    nq <- length(bank$questions) + sum(vapply(bank$include %||% list(), function(f)
            length(read_yaml(file.path(dirname(q$bank), f))), 1))
    html_src <- sub("\\.ya?ml$", ".html", q$bank)
    if (!file.exists(html_src)) stop("built quiz not found: ", html_src, " (run build_quiz.R first)", call. = FALSE)
    if (file.mtime(html_src) < file.mtime(q$bank)) warning("quiz HTML older than its bank: ", html_src, call. = FALSE)
    file.copy(html_src, file.path("quizzes", q$file), overwrite = TRUE, copy.date = TRUE)
    topics <- vapply(bank$quiz$topics, function(t) t$label, "")
    meta[[length(meta) + 1]] <- list(id = bank$quiz$id, n = nq)
    items <- c(items, sprintf(
      '<a class="quiz" href="quizzes/%s"><div class="qt">%s</div><div class="qs">%s</div><div class="qn">%d questions &middot; %s</div><div class="prog" data-id="%s" data-n="%d"></div></a>',
      q$file, esc(bank$quiz$title), esc(bank$quiz$subtitle %||% ""), nq, paste(esc(topics), collapse = " &middot; "),
      bank$quiz$id, nq))
    cat(sprintf("  %-14s %3d questions  <- %s\n", q$file, nq, html_src))
  }
  cards[[length(cards) + 1]] <- paste0("<h2>", esc(sec$heading), "</h2>", paste(items, collapse = ""))
}

html <- paste0('<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<meta name="robots" content="noindex, nofollow">
<meta name="theme-color" content="#0f1115">
<title>', esc(S$title), '</title>
<style>
:root{--bg:#f6f7f9;--card:#fff;--fg:#1b1d22;--muted:#5f6672;--line:#dfe3e8;--accent:#2f6fd6;--ok:#1f8a4c;--chip:#eef1f5}
@media (prefers-color-scheme:dark){:root{--bg:#0f1115;--card:#181b21;--fg:#e8eaee;--muted:#9aa2ae;--line:#2a2f38;--accent:#6ea3ff;--ok:#4cc27d;--chip:#222730}}
*{box-sizing:border-box}html,body{margin:0;background:var(--bg);color:var(--fg)}
body{font:17px/1.5 -apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;padding:env(safe-area-inset-top) 16px calc(env(safe-area-inset-bottom) + 24px)}
.wrap{max-width:720px;margin:0 auto}
h1{font-size:30px;margin:22px 0 2px;letter-spacing:.01em}
.sub{color:var(--muted);margin-bottom:6px}.note{color:var(--muted);font-size:14px;margin-bottom:8px}
h2{font-size:15px;text-transform:uppercase;letter-spacing:.05em;color:var(--muted);margin:26px 0 10px}
a.quiz{display:block;text-decoration:none;color:inherit;background:var(--card);border:1px solid var(--line);border-radius:16px;padding:16px 18px;margin-bottom:12px}
a.quiz:active{transform:scale(.995)}
.qt{font-weight:650;font-size:18px}.qs{color:var(--muted);font-size:14.5px;margin-top:2px}
.qn{font-size:13px;color:var(--muted);margin-top:8px}
.prog{margin-top:10px;font-size:13px;color:var(--ok)}
.bar{height:6px;background:var(--chip);border-radius:4px;overflow:hidden;margin-top:4px}.bar>div{height:100%;background:var(--ok)}
footer{color:var(--muted);font-size:12.5px;margin-top:28px}
</style></head><body><div class="wrap">
<h1>', esc(S$title), '</h1><div class="sub">', esc(S$subtitle %||% ""), '</div><div class="note">', esc(S$note %||% ""), '</div>',
paste(unlist(cards), collapse = "\n"), '
<footer>Tip: on a phone, use the browser&#39;s &ldquo;Add to Home Screen&rdquo; to open this page like an app.</footer>
</div>
<script>
document.querySelectorAll(".prog").forEach(function (el) {
  var s = null; try { s = JSON.parse(localStorage.getItem("quizdeck:" + el.dataset.id + ":v1")); } catch (e) {}
  var n = +el.dataset.n, q = (s && s.q) || {}, seen = 0, m = 0;
  Object.keys(q).forEach(function (k) { if (q[k].seen > 0) seen++; if (q[k].box >= 4) m++; });
  if (!seen) { el.textContent = "Not started"; el.style.color = "var(--muted)"; return; }
  el.innerHTML = "Seen " + seen + "/" + n + " &middot; mastered " + m + "/" + n + "<div class=\\"bar\\"><div style=\\"width:" + (100 * m / n).toFixed(1) + "%\\"></div></div>";
});
</script></body></html>')
con <- file("index.html", open = "w", encoding = "UTF-8"); writeLines(html, con); close(con)
if (!file.exists(".nojekyll")) file.create(".nojekyll")      # serve files as-is (no Jekyll processing)
cat("OK  index.html with", length(meta), "quizzes\n")
