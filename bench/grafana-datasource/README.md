# Cardano Timeseries Grafana Datasource

A Grafana datasource plugin that connects Grafana to the Cardano Timeseries HTTP server
(exposed by `cardano-tracer`). It allows querying node metrics, node info, startup
parameters, and sync state directly from Grafana dashboards.

## Prerequisites

- [Node.js](https://nodejs.org/) (v18 or later)
- [Docker](https://www.docker.com/) with the Compose plugin

## Build

Install dependencies (first time only):

```sh
npm install
```

Build the plugin:

```sh
npm run build
```

This produces the compiled plugin under `dist/`.

## Run

Start Grafana with the plugin loaded:

```sh
docker compose up -d
```

Grafana will be available at http://localhost:3001 (default credentials: `admin` / `admin`).

The datasource is pre-provisioned and points at `http://host.docker.internal:3400` — the
default address of the timeseries server when running locally. To change it, edit
`provisioning/datasources/cardano-timeseries.yaml` and restart the container.

## Development

To rebuild automatically on file changes:

```sh
npm run dev
```

Then restart the container to pick up the new `dist/`:

```sh
docker compose down && docker compose up -d
```

To type-check without building:

```sh
npm run typecheck
```
