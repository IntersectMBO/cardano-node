import { DataSourcePlugin } from '@grafana/data';

import { CardanoTimeseriesDatasource } from './datasource';
import { ConfigEditor } from './ConfigEditor';
import { QueryEditor } from './QueryEditor';
import { CardanoTimeseriesQuery, CardanoTimeseriesOptions } from './types';

export const plugin = new DataSourcePlugin<
  CardanoTimeseriesDatasource,
  CardanoTimeseriesQuery,
  CardanoTimeseriesOptions
>(CardanoTimeseriesDatasource)
  .setConfigEditor(ConfigEditor)
  .setQueryEditor(QueryEditor);
