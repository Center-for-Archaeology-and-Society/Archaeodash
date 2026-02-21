#!/usr/bin/env bash
set -euo pipefail

URL_DEFAULT="https://cas.rc.asu.edu/app/Archaeodash/"
URL="${1:-${ARCHAEODASH_LIVE_URL:-$URL_DEFAULT}}"
TIMEOUT="${ARCHAEODASH_HEALTHCHECK_TIMEOUT:-20}"

TMP_BODY="$(mktemp)"
cleanup() {
  rm -f "$TMP_BODY"
}
trap cleanup EXIT

HTTP_CODE="$(curl -sS -L -m "$TIMEOUT" -o "$TMP_BODY" -w "%{http_code}" "$URL" || true)"
if [[ "$HTTP_CODE" != "200" ]]; then
  echo "$(date -Is) status=FAIL reason=http_code code=${HTTP_CODE:-curl_error} url=$URL"
  exit 1
fi

if ! grep -Eqi "ArchaeoDash|CAS|Login|Sign[[:space:]]*In|shiny" "$TMP_BODY"; then
  echo "$(date -Is) status=FAIL reason=content_check url=$URL"
  exit 1
fi

echo "$(date -Is) status=OK code=$HTTP_CODE url=$URL"
