# Live Healthcheck Timer Setup 2026-02-21

## Summary
- Added a live-app health check script and a root installer that configures a `systemd` timer to run every 30 minutes.
- Health check verifies live URL reachability (`HTTP 200`) and expected app/login content markers.
- Check results are appended to `/var/log/archaeodash/live_healthcheck.log`.

## Files
- `security/live_healthcheck.sh`
- `security/install_live_healthcheck_timer.sh`
- `security/README.md`

## Usage
From repository root:

```bash
sudo ./security/install_live_healthcheck_timer.sh
```

Optional custom URL:

```bash
sudo ./security/install_live_healthcheck_timer.sh ./security/live_healthcheck.sh https://cas.rc.asu.edu/app/Archaeodash/
```

## Validation Commands
```bash
systemctl status archaeodash-live-healthcheck.timer --no-pager
systemctl status archaeodash-live-healthcheck.service --no-pager
tail -n 20 /var/log/archaeodash/live_healthcheck.log
```

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-21]]
