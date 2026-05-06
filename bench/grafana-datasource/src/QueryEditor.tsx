import React, { ChangeEvent } from 'react';
import { InlineField, TextArea } from '@grafana/ui';
import { QueryEditorProps } from '@grafana/data';

import { CardanoTimeseriesDatasource } from './datasource';
import { CardanoTimeseriesOptions, CardanoTimeseriesQuery, defaultQuery } from './types';

type Props = QueryEditorProps<
  CardanoTimeseriesDatasource,
  CardanoTimeseriesQuery,
  CardanoTimeseriesOptions
>;

export function QueryEditor({ query, onChange, onRunQuery }: Props) {
  const queryText = query.queryText ?? defaultQuery.queryText ?? '';

  const onTextChange = (e: ChangeEvent<HTMLTextAreaElement>) => {
    onChange({ ...query, queryText: e.target.value });
  };

  return (
    <InlineField label="Query" labelWidth={8} grow>
      <TextArea
        rows={5}
        value={queryText}
        placeholder={'Enter timeseries query…\n\nExample:\n  increase cardano_node_metrics_Forge_forged_int[now - 1h; now]'}
        onChange={onTextChange}
        onBlur={onRunQuery}
      />
    </InlineField>
  );
}
