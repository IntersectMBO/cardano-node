import React from 'react';
import { DataSourcePluginOptionsEditorProps } from '@grafana/data';

import { CardanoTimeseriesOptions } from './types';

type Props = DataSourcePluginOptionsEditorProps<CardanoTimeseriesOptions>;

// The server URL is configured via Grafana's built-in URL field on the datasource config page.
// Grafana proxies all requests from its backend (the container) to that URL, so use
// http://host.docker.internal:<port> when running Grafana in Docker.
export function ConfigEditor(_props: Props) {
  return null;
}
