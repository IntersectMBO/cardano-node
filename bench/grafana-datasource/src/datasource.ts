import {
  DataFrame,
  DataQueryError,
  DataQueryRequest,
  DataQueryResponse,
  DataSourceApi,
  DataSourceInstanceSettings,
  FieldType,
  MetricFindValue,
  toDataFrame,
} from '@grafana/data';
import { FetchResponse, getBackendSrv, getTemplateSrv } from '@grafana/runtime';

import {
  CardanoTimeseriesQuery,
  CardanoTimeseriesOptions,
  NodeInfoResponse,
  NodeStartupInfoResponse,
  NodeStateResponse,
  QueryResponse,
} from './types';
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
    const active = options.targets.filter((t) => !t.hide);

    const frames: DataFrame[] = [];
    let error: DataQueryError | undefined;

    // Grafana expands $__from/$__to to raw ms integers (Scalar in our type system, not Timestamp).
    // Replace them ourselves before getTemplateSrv() sees them so they become valid timestamp
    // expressions: epoch + Nms.
    const from = options.range.from.valueOf();
    const to = options.range.to.valueOf();

    for (const target of active) {
      const queryType = target.queryType ?? 'timeseries';
      try {
        switch (queryType) {
          case 'timeseries': {
            if (!target.queryText?.trim()) { continue; }
            const preProcessed = target.queryText
              .replace(/\$__from/g, `epoch + ${from}ms`)
              .replace(/\$__to/g,   `epoch + ${to}ms`);
            const interpolated = getTemplateSrv().replace(preProcessed, options.scopedVars);
            const legendFormat = getTemplateSrv().replace(target.legendFormat ?? '', options.scopedVars);
            frames.push(...(await this.runQuery(interpolated, legendFormat)));
            break;
          }
          case 'nodes':
            frames.push(...(await this.fetchNodes()));
            break;
          case 'node-info': {
            const nm = getTemplateSrv().replace(target.nodeName ?? '', options.scopedVars);
            frames.push(...(await this.fetchNodeInfo(nm)));
            break;
          }
          case 'node-startup': {
            const nm = getTemplateSrv().replace(target.nodeName ?? '', options.scopedVars);
            frames.push(...(await this.fetchNodeStartup(nm)));
            break;
          }
          case 'node-sync-progress': {
            const nm = getTemplateSrv().replace(target.nodeName ?? '', options.scopedVars);
            frames.push(...(await this.fetchNodeSyncProgress(nm)));
            break;
          }
        }
      } catch (err: any) {
        error = err;
        break;
      }
    }

    return { data: frames, error };
  }

  private async runQuery(queryText: string, legendFormat = '') {
    try {
      // `.toPromise()` avoids the rxjs-version conflict that arises when
      // @grafana/data bundles its own rxjs alongside the top-level one.
      const response = await (getBackendSrv()
        .fetch<QueryResponse>({
          url: `${this.baseUrl}/timeseries/query`,
          method: 'POST',
          headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
          data: new URLSearchParams({ query: queryText }).toString(),
        }) as any).toPromise() as FetchResponse<QueryResponse>;
      return valueToDataFrames(response.data.data, legendFormat);
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

  // Generic GET helper. Returns null on 404, throws DataQueryError for other failures.
  private async fetchJson<T>(path: string): Promise<T | null> {
    try {
      const response = await (getBackendSrv()
        .fetch<T>({ url: `${this.baseUrl}${path}`, method: 'GET' }) as any
      ).toPromise() as FetchResponse<T>;
      return response.data;
    } catch (err: any) {
      if (err?.status === 404) { return null; }
      const msg = err?.data?.message ?? err?.message ?? 'Request failed';
      const error: DataQueryError = { message: msg, status: err?.status };
      throw error;
    }
  }

  private async fetchNodes(): Promise<DataFrame[]> {
    const nodeNames = await this.fetchJson<string[]>('/timeseries/nodes') ?? [];
    return [toDataFrame({
      name: 'nodes',
      fields: [{ name: 'Node Name', type: FieldType.string, values: nodeNames }],
    })];
  }

  private async fetchNodeInfo(nodeName: string): Promise<DataFrame[]> {
    const ni = await this.fetchJson<NodeInfoResponse>(
      `/timeseries/node/${encodeURIComponent(nodeName)}/info`
    );
    if (!ni) { return []; }
    return [toDataFrame({
      name: 'node-info',
      fields: [
        { name: 'Name',              type: FieldType.string, values: [ni.niName] },
        { name: 'Protocol',          type: FieldType.string, values: [ni.niProtocol] },
        { name: 'Version',           type: FieldType.string, values: [ni.niVersion] },
        { name: 'Commit',            type: FieldType.string, values: [ni.niCommit.slice(0, 7)] },
        { name: 'Start Time',        type: FieldType.string, values: [ni.niStartTime] },
        { name: 'System Start Time', type: FieldType.string, values: [ni.niSystemStartTime] },
        { name: 'Uptime',            type: FieldType.number, values: [ni.uptimeSeconds] },
      ],
    })];
  }

  private async fetchNodeStartup(nodeName: string): Promise<DataFrame[]> {
    const nsi = await this.fetchJson<NodeStartupInfoResponse>(
      `/timeseries/node/${encodeURIComponent(nodeName)}/startup`
    );
    if (!nsi) { return []; }
    return [toDataFrame({
      name: 'node-startup',
      fields: [
        { name: 'Era',               type: FieldType.string, values: [nsi.suiEra] },
        { name: 'Slot Length (s)',   type: FieldType.number, values: [nsi.suiSlotLength] },
        { name: 'Epoch Length',      type: FieldType.number, values: [nsi.suiEpochLength] },
        { name: 'KES Period Length', type: FieldType.number, values: [nsi.suiSlotsPerKESPeriod] },
      ],
    })];
  }

  private async fetchNodeSyncProgress(nodeName: string): Promise<DataFrame[]> {
    const ns = await this.fetchJson<NodeStateResponse>(
      `/timeseries/node/${encodeURIComponent(nodeName)}/sync-progress`
    );
    if (!ns) { return []; }
    return [toDataFrame({
      name: 'node-sync-progress',
      fields: [
        { name: 'Sync Progress (%)', type: FieldType.number, values: [ns.syncProgress] },
      ],
    })];
  }

  async metricFindQuery(_query: string): Promise<MetricFindValue[]> {
    const nodeNames = await this.fetchJson<string[]>('/timeseries/nodes') ?? [];
    return nodeNames.map((nm) => ({ text: nm, value: nm }));
  }

  async testDatasource() {
    try {
      // POST an empty query; any non-network-error response means the server is reachable.
      await (getBackendSrv().fetch({
        url: `${this.baseUrl}/timeseries/query`,
        method: 'POST',
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        data: 'query=',
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
