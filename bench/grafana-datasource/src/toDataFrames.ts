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
  switch (v.tag) {
    case 'Scalar':    return v.value;
    case 'Duration':  return v.value;
    case 'Timestamp': return v.value;
    case 'Truth':     return 1;
    case 'Falsity':   return 0;
    default:          return null;
  }
}

function rangeToFrame(ts: TimeseriesItem): DataFrame {
  const frame = new MutableDataFrame({
    name: seriesName(ts.labels),
    fields: [
      { name: 'Time', type: FieldType.time },
      { name: 'Value', type: FieldType.number },
    ],
  });
  for (const [t, v] of ts.data) {
    frame.add({ Time: t, Value: extractScalar(v) });
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
      Time: inst.timestamp,
      Labels: seriesName(inst.labels),
      Value: extractScalar(inst.value),
    });
  }
  return frame;
}

export function valueToDataFrames(value: Value): DataFrame[] {
  switch (value.tag) {
    case 'Scalar': {
      const frame = new MutableDataFrame({
        name: 'scalar',
        fields: [{ name: 'Value', type: FieldType.number }],
      });
      frame.add({ Value: value.value });
      return [frame];
    }

    case 'RangeVector':
      return value.value.map(rangeToFrame);

    case 'InstantVector':
      return [instantVectorToFrame(value.value)];

    case 'Pair': {
      const fst = valueToDataFrames(value.fst).map((f, i) => ({ ...f, name: `pair.fst[${i}]` }));
      const snd = valueToDataFrames(value.snd).map((f, i) => ({ ...f, name: `pair.snd[${i}]` }));
      return [...fst, ...snd];
    }

    case 'Truth':
    case 'Falsity': {
      const frame = new MutableDataFrame({
        name: 'bool',
        fields: [{ name: 'Value', type: FieldType.boolean }],
      });
      frame.add({ Value: value.tag === 'Truth' });
      return [frame];
    }

    case 'Duration': {
      const frame = new MutableDataFrame({
        name: 'duration_ms',
        fields: [{ name: 'Value', type: FieldType.number }],
      });
      frame.add({ Value: value.value });
      return [frame];
    }

    case 'Timestamp': {
      const frame = new MutableDataFrame({
        name: 'timestamp_ms',
        fields: [{ name: 'Value', type: FieldType.time }],
      });
      frame.add({ Value: value.value });
      return [frame];
    }

    case 'Text': {
      const frame = new MutableDataFrame({
        name: 'text',
        fields: [{ name: 'Value', type: FieldType.string }],
      });
      frame.add({ Value: value.value });
      return [frame];
    }

    case 'Unit':
      return [];

    case 'Function':
      throw new Error('Cannot render a Function value — it has no serialisable representation');
  }
}
