import {
  DataQueryRequest,
  DataQueryResponse,
  DataSourceApi,
  DataSourceInstanceSettings,
} from '@grafana/data';
import { FetchResponse, getBackendSrv, getTemplateSrv } from '@grafana/runtime';

import { CardanoTimeseriesQuery, CardanoTimeseriesOptions, Value } from './types';
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

    const results = await Promise.all(
      active.map((target) => {
        const interpolated = getTemplateSrv().replace(target.queryText, options.scopedVars);
        return this.runQuery(interpolated);
      })
    );

    return { data: results.flat() };
  }

  private async runQuery(queryText: string) {
    try {
      // `.toPromise()` avoids the rxjs-version conflict that arises when
      // @grafana/data bundles its own rxjs alongside the top-level one.
      const response = await (getBackendSrv()
        .fetch<Value>({
          url: `${this.baseUrl}/timeseries/query`,
          method: 'POST',
          headers: { 'Content-Type': 'text/plain' },
          data: queryText,
        }) as any).toPromise() as FetchResponse<Value>;
      return valueToDataFrames(response.data);
    } catch (err) {
      console.error('CardanoTimeseries: query failed', err);
      return [];
    }
  }

  async testDatasource() {
    try {
      // POST an empty query; any non-network-error response means the server is reachable.
      await (getBackendSrv().fetch({
        url: `${this.baseUrl}/timeseries/query`,
        method: 'POST',
        data: '',
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
