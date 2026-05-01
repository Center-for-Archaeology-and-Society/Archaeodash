auth_email_enabled <- function() {
  identical(tolower(Sys.getenv("ARCHAEODASH_AUTH_EMAIL_ENABLED", "1")), "1")
}

auth_email_sender <- function() {
  trimws(Sys.getenv("ARCHAEODASH_SMTP_FROM", ""))
}

auth_email_reply_to <- function() {
  trimws(Sys.getenv("ARCHAEODASH_SMTP_REPLY_TO", ""))
}

auth_email_mode <- function() {
  mode <- tolower(trimws(Sys.getenv("ARCHAEODASH_AUTH_EMAIL_MODE", "smtp")))
  if (!(mode %in% c("smtp", "log"))) "smtp" else mode
}

auth_email_default_base_url <- function() {
  trimws(Sys.getenv("ARCHAEODASH_BASE_URL", ""))
}

build_auth_absolute_url <- function(session = NULL, path = "/", query = list()) {
  query <- query[!vapply(query, function(x) is.null(x) || identical(x, ""), logical(1))]
  base_url <- auth_email_default_base_url()

  if (!nzchar(base_url) && !is.null(session)) {
    req <- tryCatch(session$request, error = function(e) NULL)
    scheme <- ""
    host <- ""
    port <- ""
    script_name <- ""
    if (!is.null(req)) {
      scheme <- tryCatch(as.character(req$HTTP_X_FORWARDED_PROTO), error = function(e) "")
      if (!nzchar(scheme)) {
        https_flag <- tryCatch(as.character(req$HTTPS), error = function(e) "")
        scheme <- if (identical(tolower(https_flag), "on")) "https" else "http"
      }
      host <- tryCatch(as.character(req$HTTP_X_FORWARDED_HOST), error = function(e) "")
      if (!nzchar(host)) host <- tryCatch(as.character(req$HTTP_HOST), error = function(e) "")
      if (!nzchar(host)) host <- tryCatch(as.character(req$SERVER_NAME), error = function(e) "")
      port <- tryCatch(as.character(req$SERVER_PORT), error = function(e) "")
      script_name <- tryCatch(as.character(req$SCRIPT_NAME), error = function(e) "")
      if (!nzchar(script_name)) script_name <- "/"
    }
    if (nzchar(host)) {
      has_port <- grepl(":[0-9]+$", host)
      include_port <- nzchar(port) &&
        !has_port &&
        !((identical(scheme, "http") && identical(port, "80")) ||
            (identical(scheme, "https") && identical(port, "443")))
      base_url <- paste0(scheme, "://", host, if (include_port) paste0(":", port) else "", script_name)
    }
  }

  if (!nzchar(base_url)) return("")

  if (!grepl("^https?://", base_url, ignore.case = TRUE)) {
    base_url <- paste0("https://", base_url)
  }
  if (!grepl("/$", base_url)) {
    base_url <- paste0(base_url, "/")
  }

  rel_path <- if (nzchar(path)) sub("^/+", "", as.character(path)) else ""
  url <- paste0(base_url, rel_path)
  if (length(query) > 0) {
    encoded_query <- paste(
      stats::setNames(vapply(query, utils::URLencode, character(1), reserved = TRUE), names(query)),
      collapse = "&"
    )
    url <- paste0(url, if (grepl("\\?", url, fixed = TRUE)) "&" else "?", encoded_query)
  }
  url
}

auth_email_subject <- function(kind = c("verify", "reset")) {
  kind <- match.arg(kind)
  if (identical(kind, "verify")) {
    "Verify your ArchaeoDash account"
  } else {
    "Reset your ArchaeoDash password"
  }
}

compose_auth_email_message <- function(to_email, subject, html_body, text_body = html_body) {
  from_email <- auth_email_sender()
  if (!nzchar(from_email)) return("")
  reply_to <- auth_email_reply_to()
  boundary <- paste0("archaeodash_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  headers <- c(
    paste0("From: ArchaeoDash <", from_email, ">"),
    paste0("To: <", tolower(trimws(as.character(to_email))), ">"),
    paste0("Subject: ", subject),
    "MIME-Version: 1.0",
    paste0("Content-Type: multipart/alternative; boundary=\"", boundary, "\"")
  )
  if (nzchar(reply_to)) {
    headers <- c(headers, paste0("Reply-To: ", reply_to))
  }
  parts <- c(
    headers,
    "",
    paste0("--", boundary),
    "Content-Type: text/plain; charset=UTF-8",
    "Content-Transfer-Encoding: 8bit",
    "",
    text_body,
    "",
    paste0("--", boundary),
    "Content-Type: text/html; charset=UTF-8",
    "Content-Transfer-Encoding: 8bit",
    "",
    html_body,
    "",
    paste0("--", boundary, "--"),
    ""
  )
  paste(parts, collapse = "\r\n")
}

send_auth_email <- function(to_email, subject, html_body, text_body = html_body) {
  if (!auth_email_enabled()) {
    app_log(paste("Auth email disabled for:", tolower(trimws(as.character(to_email)))))
    return(TRUE)
  }
  if (!app_require_packages("curl", feature = "Authentication emails")) return(FALSE)

  to_email <- tolower(trimws(as.character(to_email)))
  from_email <- auth_email_sender()
  smtp_server <- trimws(Sys.getenv("ARCHAEODASH_SMTP_SERVER", ""))
  smtp_username <- trimws(Sys.getenv("ARCHAEODASH_SMTP_USERNAME", ""))
  smtp_password <- Sys.getenv("ARCHAEODASH_SMTP_PASSWORD", "")
  use_ssl <- tolower(trimws(Sys.getenv("ARCHAEODASH_SMTP_USE_SSL", "try")))
  if (!(use_ssl %in% c("try", "no", "force"))) use_ssl <- "try"

  if (!nzchar(to_email) || !nzchar(from_email)) {
    app_log("Authentication email not sent: sender or recipient email missing.")
    return(FALSE)
  }

  message <- compose_auth_email_message(
    to_email = to_email,
    subject = subject,
    html_body = html_body,
    text_body = text_body
  )
  if (!nzchar(message)) return(FALSE)

  if (identical(auth_email_mode(), "log")) {
    app_log(paste0("AUTH EMAIL [", subject, "] to ", to_email))
    app_log(text_body)
    return(TRUE)
  }

  if (!nzchar(smtp_server) || !nzchar(smtp_username) || !nzchar(smtp_password)) {
    app_log("Authentication email not sent: SMTP configuration is incomplete.")
    return(FALSE)
  }

  tryCatch({
    curl::send_mail(
      mail_from = from_email,
      mail_rcpt = to_email,
      message = message,
      smtp_server = smtp_server,
      use_ssl = use_ssl,
      username = smtp_username,
      password = smtp_password,
      verbose = FALSE
    )
    TRUE
  }, error = function(e) {
    app_log(paste0("Authentication email send failed: ", conditionMessage(e)))
    FALSE
  })
}
