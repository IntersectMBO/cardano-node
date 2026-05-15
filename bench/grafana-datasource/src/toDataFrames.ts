import { DataFrame, FieldType, MutableDataFrame } from '@grafana/data';

import { Value, TimeseriesItem, InstantItem, SeriesIdentifier } from './types';

function seriesName(labels: SeriesIdentifier): string {
  const entries = Object.entries(labels);
  return entries.length === 0
    ? '(no labels)'
    : entries.map(([k, v]) => `${k}="${v}"`).join(', ');
}

// Best-effort: extract a numeric value from any Value constructor.
// Returns null for non-scalar values — Grafana renders null as a gap.
function extractScalar(v: Value): number | null {
  switch (v.resultType) {
    case 'scalar':    return v.result;
    case 'duration':  return v.result;
    case 'timestamp': return v.result * 1000;
    case 'truth':     return 1;
    case 'falsity':   return 0;
    default:          return null;
  }
}

function rangeToFrame(ts: TimeseriesItem): DataFrame {
  const frame = new MutableDataFrame({
    name: seriesName(ts.metric),
    fields: [
      { name: 'Time', type: FieldType.time },
      { name: 'Value', type: FieldType.number },
    ],
  });
  for (const [t, v] of ts.values) {
    frame.add({ Time: t * 1000, Value: parseFloat(v) });
  }
  return frame;
}

function instantVectorToFrame(instants: InstantItem[]): DataFrame {
  const frame = new MutableDataFrame({
    name: 'instant_vector',
    fields: [
      { name: 'Time', type: FieldType.time },
      { name: 'Labels', type: FieldType.string },
      { name: 'Value', type: FieldType.number },
    ],
  });
  for (const inst of instants) {
    frame.add({
      Time: inst.value[0] * 1000,
      Labels: seriesName(inst.metric),
      Value: parseFloat(inst.value[1]),
    });
  }
  return frame;
}

export function valueToDataFrames(value: Value): DataFrame[] {
  switch (value.resultType) {
    case 'scalar': {
      const frame = new MutableDataFrame({
        name: 'scalar',
        fields: [{ name: 'Value', type: FieldType.number }],
      });
      frame.add({ Value: value.result });
      return [frame];
    }

    case 'matrix':
      return value.result.map(rangeToFrame);

    case 'vector':
      return [instantVectorToFrame(value.result)];

    case 'pair': {
      const fst = valueToDataFrames(value.fst).map((f, i) => ({ ...f, name: `pair.fst[${i}]` }));
      const snd = valueToDataFrames(value.snd).map((f, i) => ({ ...f, name: `pair.snd[${i}]` }));
      return [...fst, ...snd];
    }

    case 'truth':
    case 'falsity': {
      const frame = new MutableDataFrame({
        name: 'bool',
        fields: [{ name: 'Value', type: FieldType.boolean }],
      });
      frame.add({ Value: value.resultType === 'truth' });
      return [frame];
    }

    case 'duration': {
      const frame = new MutableDataFrame({
        name: 'duration_s',
        fields: [{ name: 'Value', type: FieldType.number }],
      });
      frame.add({ Value: value.result });
      return [frame];
    }

    case 'timestamp': {
      const frame = new MutableDataFrame({
        name: 'timestamp_s',
        fields: [{ name: 'Value', type: FieldType.time }],
      });
      frame.add({ Value: value.result * 1000 });
      return [frame];
    }

    case 'text': {
      const frame = new MutableDataFrame({
        name: 'text',
        fields: [{ name: 'Value', type: FieldType.string }],
      });
      frame.add({ Value: value.result });
      return [frame];
    }

    case 'unit':
      return [];

    case 'nil':
      return [];

    case 'cons': {
      // Collect the spine into an array, then render as a table frame.
      const items: Value[] = [];
      let cur: Value = value;
      while (cur.resultType === 'cons') { items.push(cur.head); cur = cur.tail; }
      // cur is now the tail terminator (expected nil; any other tag is an improper list)
      const frame = new MutableDataFrame({
        name: 'list',
        fields: [{ name: 'Value', type: FieldType.string }],
      });
      for (const item of items) {
        const display = item.resultType === 'text' ? item.result : (extractScalar(item)?.toString() ?? item.resultType);
        frame.add({ Value: display });
      }
      return [frame];
    }

    case 'function':
      throw new Error('Cannot render a Function value — it has no serialisable representation');
  }
}
