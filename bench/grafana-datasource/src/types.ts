import { DataSourceJsonData, DataQuery } from '@grafana/data';

// Haskell: Set (Label, Text), serialised as a JSON object
export type SeriesIdentifier = Record<string, string>;

export interface TimeseriesItem {
  labels: SeriesIdentifier;
  // Each element is a 2-element JSON array [timestamp_ms, Value]
  data: [number, Value][];
}

export interface InstantItem {
  labels: SeriesIdentifier;
  timestamp: number; // ms since epoch
  value: Value;
}

// Tagged union mirroring the Haskell Value GADT
export type Value =
  | { tag: 'Scalar'; value: number }
  | { tag: 'RangeVector'; value: TimeseriesItem[] }
  | { tag: 'InstantVector'; value: InstantItem[] }
  | { tag: 'Pair'; fst: Value; snd: Value }
  | { tag: 'Truth' }
  | { tag: 'Falsity' }
  | { tag: 'Duration'; value: number } // milliseconds
  | { tag: 'Timestamp'; value: number } // ms since epoch
  | { tag: 'Text'; value: string }
  | { tag: 'Function' };

export interface CardanoTimeseriesQuery extends DataQuery {
  queryText: string;
}

export const defaultQuery: Partial<CardanoTimeseriesQuery> = {
  queryText: '',
};

// No custom jsonData fields — the server URL is the standard Grafana datasource URL field.
export type CardanoTimeseriesOptions = DataSourceJsonData;
