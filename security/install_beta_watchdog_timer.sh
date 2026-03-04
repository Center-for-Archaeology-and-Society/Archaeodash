#!/usr/bin/env bash
set -euo pipefail

if [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
  echo "Run as root (sudo)."
  exit 1
fi

SCRIPT_SOURCE_DEFAULT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/beta_runtime_watchdog.sh"
SCRIPT_SOURCE="${1:-$SCRIPT_SOURCE_DEFAULT}"
URL="${2:-http://127.0.0.1:23838/inst/app/}"
CONTAINER="${3:-archaeodashbeta}"

if [[ ! -f "$SCRIPT_SOURCE" ]]; then
  echo "Watchdog script not found: $SCRIPT_SOURCE"
  exit 1
fi

install -d -m 0755 /usr/local/bin
install -m 0755 "$SCRIPT_SOURCE" /usr/local/bin/archaeodash_beta_watchdog.sh

install -d -m 0755 /var/log/archaeodash
touch /var/log/archaeodash/beta_watchdog.log
chmod 0644 /var/log/archaeodash/beta_watchdog.log

install -d -m 0755 /var/lib/archaeodash
touch /var/lib/archaeodash/beta_watchdog.state
chmod 0644 /var/lib/archaeodash/beta_watchdog.state

cat >/etc/systemd/system/archaeodash-beta-watchdog.service <<SERVICE
[Unit]
Description=ArchaeoDash beta runtime watchdog
After=network-online.target docker.service
Wants=network-online.target

[Service]
Type=oneshot
Environment=ARCHAEODASH_BETA_URL=${URL}
Environment=ARCHAEODASH_BETA_CONTAINER=${CONTAINER}
Environment=ARCHAEODASH_WATCHDOG_TIMEOUT=8
Environment=ARCHAEODASH_WATCHDOG_CPU_THRESHOLD=85
Environment=ARCHAEODASH_WATCHDOG_FAIL_LIMIT=3
Environment=ARCHAEODASH_WATCHDOG_STATE_DIR=/var/lib/archaeodash
ExecStart=/bin/bash -lc '/usr/local/bin/archaeodash_beta_watchdog.sh >> /var/log/archaeodash/beta_watchdog.log 2>&1'
SERVICE

cat >/etc/systemd/system/archaeodash-beta-watchdog.timer <<'TIMER'
[Unit]
Description=Run ArchaeoDash beta watchdog every 2 minutes

[Timer]
OnCalendar=*:0/2
Persistent=true
Unit=archaeodash-beta-watchdog.service

[Install]
WantedBy=timers.target
TIMER

systemctl daemon-reload
systemctl enable --now archaeodash-beta-watchdog.timer
systemctl start archaeodash-beta-watchdog.service

echo "Installed beta watchdog timer and service."
echo "Status: systemctl status archaeodash-beta-watchdog.timer --no-pager"
echo "Recent checks: tail -n 20 /var/log/archaeodash/beta_watchdog.log"
