import React, { ChangeEvent } from 'react';
import { InlineField, InlineFieldRow, Input, Select, TextArea } from '@grafana/ui';
import { QueryEditorProps, SelectableValue } from '@grafana/data';

import { CardanoTimeseriesDatasource } from './datasource';
import { CardanoTimeseriesOptions, CardanoTimeseriesQuery, QueryType, defaultQuery } from './types';

type Props = QueryEditorProps<
  CardanoTimeseriesDatasource,
  CardanoTimeseriesQuery,
  CardanoTimeseriesOptions
>;

const QUERY_TYPES: Array<SelectableValue<QueryType>> = [
  { label: 'Timeseries Query',   value: 'timeseries',         description: 'Run a timeseries expression' },
  { label: 'Node List',          value: 'nodes',              description: 'List connected node IDs' },
  { label: 'Node Info',          value: 'node-info',          description: 'Name, version, protocol, start times' },
  { label: 'Node Startup',       value: 'node-startup',       description: 'Era, slot length, epoch length, KES period' },
  { label: 'Node Sync Progress', value: 'node-sync-progress', description: 'Sync progress percentage' },
];

export function QueryEditor({ query, onChange, onRunQuery }: Props) {
  const queryType    = query.queryType ?? 'timeseries';
  const queryText    = query.queryText ?? defaultQuery.queryText ?? '';
  const nodeId       = query.nodeId ?? '';
  const legendFormat = query.legendFormat ?? '';

  const onQueryTypeChange = (v: SelectableValue<QueryType>) => {
    onChange({ ...query, queryType: v.value! });
    onRunQuery();
  };

  const onTextChange = (e: ChangeEvent<HTMLTextAreaElement>) => {
    onChange({ ...query, queryText: e.target.value });
  };

  const onNodeIdChange = (e: ChangeEvent<HTMLInputElement>) => {
    onChange({ ...query, nodeId: e.target.value });
  };

  const onLegendFormatChange = (e: ChangeEvent<HTMLInputElement>) => {
    onChange({ ...query, legendFormat: e.target.value });
  };

  const needsNodeId =
    queryType === 'node-info' || queryType === 'node-startup' || queryType === 'node-sync-progress';

  return (
    <>
      <InlineFieldRow>
        <InlineField label="Type" labelWidth={8}>
          <Select
            width={22}
            options={QUERY_TYPES}
            value={queryType}
            onChange={onQueryTypeChange}
          />
        </InlineField>
        {needsNodeId && (
          <InlineField label="Node ID" labelWidth={10} grow>
            <Input
              value={nodeId}
              placeholder="e.g. 127.0.0.1:3001"
              onChange={onNodeIdChange}
              onBlur={onRunQuery}
            />
          </InlineField>
        )}
      </InlineFieldRow>
      {queryType === 'timeseries' && (
        <>
          <InlineField label="Query" labelWidth={14} grow>
            <TextArea
              rows={5}
              value={queryText}
              placeholder={'Enter timeseries query…\n\nExample:\n  increase cardano_node_metrics_Forge_forged_counter[now - 1h; now]'}
              onChange={onTextChange}
              onBlur={onRunQuery}
            />
          </InlineField>
          <InlineField label="Legend format" labelWidth={14} grow>
            <Input
              value={legendFormat}
              placeholder="{{node_name}}"
              onChange={onLegendFormatChange}
              onBlur={onRunQuery}
            />
          </InlineField>
        </>
      )}
    </>
  );
}
