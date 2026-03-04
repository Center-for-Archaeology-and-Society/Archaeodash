#!/usr/bin/env bash
set -euo pipefail

URL_DEFAULT="http://127.0.0.1:23838/inst/app/"
URL="${1:-${ARCHAEODASH_BETA_URL:-$URL_DEFAULT}}"
CONTAINER="${ARCHAEODASH_BETA_CONTAINER:-archaeodashbeta}"
TIMEOUT="${ARCHAEODASH_WATCHDOG_TIMEOUT:-8}"
CPU_THRESHOLD="${ARCHAEODASH_WATCHDOG_CPU_THRESHOLD:-85}"
FAIL_LIMIT="${ARCHAEODASH_WATCHDOG_FAIL_LIMIT:-3}"
STATE_DIR="${ARCHAEODASH_WATCHDOG_STATE_DIR:-/var/lib/archaeodash}"
STATE_FILE="${STATE_DIR}/beta_watchdog.state"
LOCK_FILE="${STATE_DIR}/beta_watchdog.lock"

mkdir -p "$STATE_DIR"
touch "$STATE_FILE"
exec 9>"$LOCK_FILE"
if ! flock -n 9; then
  echo "$(date -Is) status=SKIP reason=lock_held container=$CONTAINER"
  exit 0
fi

consecutive_failures=0
last_restart_epoch=0
last_reason=""
if [[ -s "$STATE_FILE" ]]; then
  # shellcheck disable=SC1090
  source "$STATE_FILE" || true
fi

write_state() {
  cat >"$STATE_FILE" <<STATE
consecutive_failures=${consecutive_failures}
last_restart_epoch=${last_restart_epoch}
last_reason='${last_reason}'
STATE
}

timestamp="$(date -Is)"

if ! docker ps --filter "name=^${CONTAINER}$" --filter "status=running" --format '{{.Names}}' | grep -qx "$CONTAINER"; then
  echo "${timestamp} status=WARN reason=container_not_running container=${CONTAINER}"
  consecutive_failures=0
  write_state
  exit 0
fi

cpu_raw="$(docker stats --no-stream --format '{{.CPUPerc}}' "$CONTAINER" 2>/dev/null || true)"
cpu_val="$(echo "${cpu_raw:-0%}" | tr -d '%' | awk '{print ($1+0)}')"
high_cpu=0
if awk -v cpu="$cpu_val" -v threshold="$CPU_THRESHOLD" 'BEGIN { exit !(cpu >= threshold) }'; then
  high_cpu=1
fi

tmp_body="$(mktemp)"
cleanup() {
  rm -f "$tmp_body"
}
trap cleanup EXIT

http_code="$(curl -sS -L -m "$TIMEOUT" -o "$tmp_body" -w '%{http_code}' "$URL" || true)"
unhealthy=1
if [[ "$http_code" == "200" ]] && grep -Eqi 'ArchaeoDash|shiny|login|logout|Data Manager' "$tmp_body"; then
  unhealthy=0
fi

if [[ "$high_cpu" -eq 1 && "$unhealthy" -eq 1 ]]; then
  consecutive_failures=$((consecutive_failures + 1))
  check_reason="high_cpu_and_unhealthy"
else
  consecutive_failures=0
  check_reason="healthy_or_low_cpu"
fi

if [[ "$consecutive_failures" -ge "$FAIL_LIMIT" ]]; then
  if docker restart "$CONTAINER" >/dev/null 2>&1; then
    last_restart_epoch="$(date +%s)"
    last_reason="$check_reason"
    echo "${timestamp} status=RESTART container=${CONTAINER} cpu=${cpu_val} code=${http_code:-curl_error} failures=${consecutive_failures} reason=${check_reason} url=${URL}"
  else
    echo "${timestamp} status=FAIL reason=restart_failed container=${CONTAINER} cpu=${cpu_val} code=${http_code:-curl_error} failures=${consecutive_failures} url=${URL}"
  fi
  consecutive_failures=0
else
  echo "${timestamp} status=OK container=${CONTAINER} cpu=${cpu_val} code=${http_code:-curl_error} high_cpu=${high_cpu} unhealthy=${unhealthy} failures=${consecutive_failures}/${FAIL_LIMIT} url=${URL}"
fi

write_state
