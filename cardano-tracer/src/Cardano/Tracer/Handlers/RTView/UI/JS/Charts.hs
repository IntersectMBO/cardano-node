module Cardano.RTView.GUI.JS.Charts
    ( prepareChartsJS
    -- Charts JS snippets.
    , memoryUsageChartJS
    , cpuUsageChartJS
    , diskUsageChartJS
    , networkUsageChartJS
    -- Charts updaters.
    , updateMemoryUsageChartJS
    , updateCPUUsageChartJS
    , updateDiskUsageChartJS
    , updateNetworkUsageChartJS
    , resizeChartJS
    ) where

prepareChartsJS :: String
prepareChartsJS =
  "window.charts = new Map();"

memoryUsageChartJS :: String
memoryUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Memory | kernel',"
  , "      backgroundColor: '#FF8000',"
  , "      borderColor: '#FF8000',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Memory | RTS',"
  , "      backgroundColor: '#DC143C',"
  , "      borderColor: '#DC143C',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'MB'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

cpuUsageChartJS :: String
cpuUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'CPU | total',"
  , "      backgroundColor: '#FE2E2E',"
  , "      borderColor: '#FE2E2E',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'CPU | code',"
  , "      backgroundColor: '#008B8B',"
  , "      borderColor: '#008B8B',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'CPU | GC',"
  , "      backgroundColor: '#8B0000',"
  , "      borderColor: '#8B0000',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'Percent'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

diskUsageChartJS :: String
diskUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Disk | RD',"
  , "      backgroundColor: '#0080FF',"
  , "      borderColor: '#0080FF',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Disk | WR',"
  , "      backgroundColor: '#D358F7',"
  , "      borderColor: '#D358F7',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'KB/s'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

networkUsageChartJS :: String
networkUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Network | IN',"
  , "      backgroundColor: '#D7DF01',"
  , "      borderColor: '#D7DF01',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Network | OUT',"
  , "      backgroundColor: '#00FF80',"
  , "      borderColor: '#00FF80',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'KB/s'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

-- Chart updaters.
-- Please note that after 900 data points (which are collected in every 30 minutes)
-- we remove outdated points. It allows to avoid too compressed, narrow charts.
-- This is a temporary solution, it will be improved in the future releases.

updateMemoryUsageChartJS
  , updateCPUUsageChartJS
  , updateDiskUsageChartJS
  , updateNetworkUsageChartJS :: String
updateMemoryUsageChartJS  = updateDoubleDatasetChartJS
updateCPUUsageChartJS     = updateThreeDatasetsChartJS
updateDiskUsageChartJS    = updateDoubleDatasetChartJS
updateNetworkUsageChartJS = updateDoubleDatasetChartJS

{-
updateSingleDatasetChartJS :: String
updateSingleDatasetChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]
-}

updateDoubleDatasetChartJS :: String
updateDoubleDatasetChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[1].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).data.datasets[1].data.push(%4);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

updateThreeDatasetsChartJS :: String
updateThreeDatasetsChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[1].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[2].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).data.datasets[1].data.push(%4);"
  , "window.charts.get(%1).data.datasets[2].data.push(%5);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

-- During changing of panes' size we have to explicitly recise charts.
resizeChartJS :: String
resizeChartJS = "window.charts.get(%1).resize();"
