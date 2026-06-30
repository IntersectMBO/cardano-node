import { DataSourcePlugin } from '@grafana/data';

import { CardanoTimeseriesDatasource } from './datasource';
import { QueryEditor } from './QueryEditor';
import { CardanoTimeseriesQuery, CardanoTimeseriesOptions } from './types';

export const plugin = new DataSourcePlugin<
  CardanoTimeseriesDatasource,
  CardanoTimeseriesQuery,
  CardanoTimeseriesOptions
>(CardanoTimeseriesDatasource)
  .setQueryEditor(QueryEditor);
