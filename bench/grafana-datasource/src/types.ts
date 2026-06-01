import { DataSourceJsonData, DataQuery } from '@grafana/data';

// Haskell: Set (Label, Text), serialised as a JSON object
export type SeriesIdentifier = Record<string, string>;

export interface TimeseriesItem {
  metric: SeriesIdentifier;
  // Each element is a 2-element array [timestamp_s, value_str] — Prometheus wire format
  values: [number, string][];
}

export interface InstantItem {
  metric: SeriesIdentifier;
  // [timestamp_s, value_str] — Prometheus wire format
  value: [number, string];
}

// Tagged union mirroring the Haskell Value GADT
export type Value =
  | { resultType: 'scalar';    result: number }
  | { resultType: 'matrix';    result: TimeseriesItem[] }
  | { resultType: 'vector';    result: InstantItem[] }
  | { resultType: 'pair';      fst: Value; snd: Value }
  | { resultType: 'truth' }
  | { resultType: 'falsity' }
  | { resultType: 'duration';  result: number } // seconds
  | { resultType: 'timestamp'; result: number } // seconds since epoch
  | { resultType: 'text';      result: string }
  | { resultType: 'unit' }
  | { resultType: 'nil' }
  | { resultType: 'cons'; head: Value; tail: Value }
  | { resultType: 'function' };

export interface QueryResponse {
  status: 'success';
  data: Value;
}

// NodeInfo JSON shape — field names match Haskell record selectors
export interface NodeInfoResponse {
  niName: string;
  niProtocol: string;
  niVersion: string;
  niCommit: string;
  niStartTime: string;       // ISO8601
  niSystemStartTime: string; // ISO8601
  uptimeSeconds: number;
}

// NodeStartupInfo JSON shape
export interface NodeStartupInfoResponse {
  suiEra: string;
  suiSlotLength: number;       // seconds
  suiEpochLength: number;
  suiSlotsPerKESPeriod: number;
}

// /timeseries/node/{id}/state response
export interface NodeStateResponse {
  syncProgress: number; // percentage 0–100
}

// /timeseries/nodes array element
export interface NodeEntry {
  nodeName: string;
  slug: string;
}

export type QueryType = 'timeseries' | 'nodes' | 'node-info' | 'node-startup' | 'node-sync-progress';

export interface CardanoTimeseriesQuery extends DataQuery {
  queryType: QueryType;
  queryText: string;
  nodeNameSlug: string;
  legendFormat: string;
}

export const defaultQuery: Partial<CardanoTimeseriesQuery> = {
  queryType: 'timeseries',
  queryText: '',
  nodeNameSlug: '',
  legendFormat: '',
};

// No custom jsonData fields — the server URL is the standard Grafana datasource URL field.
export type CardanoTimeseriesOptions = DataSourceJsonData;
