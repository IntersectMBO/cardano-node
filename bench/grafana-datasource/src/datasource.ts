import {
  DataFrame,
  DataQueryError,
  DataQueryRequest,
  DataQueryResponse,
  DataSourceApi,
  DataSourceInstanceSettings,
} from '@grafana/data';
import { FetchResponse, getBackendSrv, getTemplateSrv } from '@grafana/runtime';

import { CardanoTimeseriesQuery, CardanoTimeseriesOptions, QueryResponse } from './types';
import { valueToDataFrames } from './toDataFrames';

export class CardanoTimeseriesDatasource extends DataSourceApi<
  CardanoTimeseriesQuery,
  CardanoTimeseriesOptions
> {
  private readonly baseUrl: string;

  constructor(instanceSettings: DataSourceInstanceSettings<CardanoTimeseriesOptions>) {
    super(instanceSettings);
    // instanceSettings.url is the Grafana server-side proxy path (/api/datasources/proxy/<id>).
    // Requests go from the Grafana container, not the browser, avoiding CORS and host-resolution issues.
    this.baseUrl = (instanceSettings.url ?? '').replace(/\/$/, '');
  }

  async query(options: DataQueryRequest<CardanoTimeseriesQuery>): Promise<DataQueryResponse> {
    const active = options.targets.filter((t) => !t.hide && t.queryText?.trim());

    const frames: DataFrame[] = [];
    let error: DataQueryError | undefined;

    for (const target of active) {
      const interpolated = getTemplateSrv().replace(target.queryText, options.scopedVars);
      try {
        frames.push(...(await this.runQuery(interpolated)));
      } catch (err: any) {
        error = err;
        break;
      }
    }

    return { data: frames, error };
  }

  private async runQuery(queryText: string) {
    try {
      // `.toPromise()` avoids the rxjs-version conflict that arises when
      // @grafana/data bundles its own rxjs alongside the top-level one.
      const response = await (getBackendSrv()
        .fetch<QueryResponse>({
          url: `${this.baseUrl}/timeseries/query`,
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          data: { query: queryText },
        }) as any).toPromise() as FetchResponse<QueryResponse>;
      return valueToDataFrames(response.data.data);
    } catch (err: any) {
      // Server returns { status: "error", errorType: "...", error: "..." } as JSON.
      // Grafana's proxy wraps 4xx bodies as { data: { ... } }.
      const fullText = err?.data?.error ?? err?.data?.message ?? err?.message ?? 'Query failed';
      const firstLine = fullText.split('\n')[0];
      const error: DataQueryError = {
        message: firstLine,
        data: { message: firstLine, error: fullText },
        status: err?.status,
      };
      throw error;
    }
  }

  async testDatasource() {
    try {
      // POST an empty query; any non-network-error response means the server is reachable.
      await (getBackendSrv().fetch({
        url: `${this.baseUrl}/timeseries/query`,
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        data: { query: '' },
      }) as any).toPromise();
      return { status: 'success', message: 'Connected to Cardano Timeseries server' };
    } catch (err: any) {
      // A 400 response means the server is reachable but rejected the empty query — still a success.
      if (err?.status === 400) {
        return { status: 'success', message: 'Connected to Cardano Timeseries server' };
      }
      return { status: 'error', message: `Cannot reach server: ${err?.message ?? err}` };
    }
  }
}
