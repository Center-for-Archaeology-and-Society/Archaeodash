# Interaction Log 2026-07-01

- Requested latest ArchaeoDash beta release be pushed to production; synchronized `Archaeodashbeta` into `../Archaeodash`, rebuilt/recreated and restarted the `archaeodash` Docker Compose service, installed package version `2026.05.01.0501` inside the live container, and verified HTTP `200` on the live endpoint.
- Requested current ArchaeoDash user database emails; queried only the `users.email` column from MySQL and reported the sorted email list without credentials or password data.
- Requested password reset for the ArchaeoDash account associated with `fergusonje@missouri.edu`; updated the stored password hash for the single matching user and verified the new credential without logging the password.
- Requested crash diagnosis for ArchaeoDash beta; checked Docker/Shiny logs and found the beta Shiny R worker was OOM-killed, with the service currently returning HTTP `200`.
