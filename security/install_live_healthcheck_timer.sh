#!/usr/bin/env bash
set -euo pipefail

if [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
  echo "Run as root (sudo)."
  exit 1
fi

SCRIPT_SOURCE_DEFAULT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/live_healthcheck.sh"
SCRIPT_SOURCE="${1:-$SCRIPT_SOURCE_DEFAULT}"
URL="${2:-https://cas.rc.asu.edu/app/Archaeodash/}"

if [[ ! -f "$SCRIPT_SOURCE" ]]; then
  echo "Healthcheck script not found: $SCRIPT_SOURCE"
  exit 1
fi

install -d -m 0755 /usr/local/bin
install -m 0755 "$SCRIPT_SOURCE" /usr/local/bin/archaeodash_live_healthcheck.sh

install -d -m 0755 /var/log/archaeodash
touch /var/log/archaeodash/live_healthcheck.log
chmod 0644 /var/log/archaeodash/live_healthcheck.log

cat >/etc/systemd/system/archaeodash-live-healthcheck.service <<SERVICE
[Unit]
Description=ArchaeoDash live app health check
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
Environment=ARCHAEODASH_LIVE_URL=${URL}
Environment=ARCHAEODASH_HEALTHCHECK_TIMEOUT=20
ExecStart=/bin/bash -lc '/usr/local/bin/archaeodash_live_healthcheck.sh >> /var/log/archaeodash/live_healthcheck.log 2>&1'
SERVICE

cat >/etc/systemd/system/archaeodash-live-healthcheck.timer <<'TIMER'
[Unit]
Description=Run ArchaeoDash live app health check every 30 minutes

[Timer]
OnCalendar=*:0/30
Persistent=true
Unit=archaeodash-live-healthcheck.service

[Install]
WantedBy=timers.target
TIMER

systemctl daemon-reload
systemctl enable --now archaeodash-live-healthcheck.timer
systemctl start archaeodash-live-healthcheck.service

echo "Installed timer and service."
echo "Status: systemctl status archaeodash-live-healthcheck.timer --no-pager"
echo "Recent checks: tail -n 20 /var/log/archaeodash/live_healthcheck.log"
