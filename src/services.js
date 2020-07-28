import "./main.js";

import Chart from 'chart.js';

var ctx1 = document.getElementById('myChart1');

const inputs1 = [0,1,2,3,4,5,6,7,8,9,10];
const inputs2 = [0,1,2,3,4,5,6,7,8,9,10];

const data1 = inputs1.map(x => x**3);

const data2 = inputs2.map(x => x**2);

var myChart1 = new Chart(ctx1, {
  type: 'line',
  data: {
    labels: [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    datasets: [{
      label: 'Traditional',
      data: data1,
      backgroundColor: 'rgba(255, 99, 132, 1)',
      borderColor: 'rgba(255, 99, 132, 1)',
      fill: false
    } , {
      label: 'Type Driven',
      data: data2,
      backgroundColor: 'rgba(153, 102, 255, 1)',
      borderColor: 'rgba(153, 102, 255, 1)',
      fill: false
    }]
  },
  options: {
    elements: {
      point: {
        radius: 0
      }
    },
    responsive: true,
    title: {
      display: true,
      text: 'Keeping Complexity Low',
      fontSize: 40
    },
    tooltips: {
      enabled: false
    },
    hover: {
      mode: 'nearest',
      intersect: true
    },
    scales: {
      xAxes: [{
        display: true,
        scaleLabel: {
          display: true,
          labelString: 'Features',
          fontSize: 30
        },
        ticks:{
          display: false
        }
      }],
      yAxes: [{
        display: true,
        scaleLabel: {
          display: true,
          labelString: 'Complexity',
          fontSize: 30
        },
        ticks:{
          display: false
        }
      }]
    },
    legend: {
      labels: {
        fontSize: 20
      }
    }
  }
});

var ctx2 = document.getElementById('myChart2');
var isWaviLabs = true;

var myChart2 = new Chart(ctx2, {
  type: 'doughnut',
  data: {
    labels: ['Unit Tests', 'Integration Tests', 'Other Tests', 'Property-based Tests', 'Static Type Checker'],
    datasets: [{
      label: 'Type Driven',
      data: [1, 1, 1, 1, 1],
      backgroundColor: ['rgba(172, 146, 235, 1)', 'rgba(79, 193, 232, 1)', 'rgba(237, 85, 100, 1)', 'rgba(160, 213, 104, 1)', 'rgba(255, 206, 84, 1)']
    }
  ]
  },
  options: {
    elements: {
      arc: {
        borderWidth: 0
      }
    },
    responsive: true,
    title: {
      display: true,
      text: 'Wavi Labs Test Coverage',
      fontSize: 40
    },
    legend: {
      labels: {
        fontSize: 20
      }
    },
    tooltips: {
      enabled: false
    }
  }
});

function toggleChart2 () {
  if (isWaviLabs) {
    myChart2 = new Chart(ctx2, {
      type: 'doughnut',
      data: {
        labels: ['Unit Tests', 'Integration Tests', 'Other Tests'],
        datasets: [{
          label: 'Type Driven',
          data: [1, 1, 1, 1, 1],
          backgroundColor: ['rgba(172, 146, 235, 1)', 'rgba(79, 193, 232, 1)', 'rgba(237, 85, 100, 1)', 'rgba(160, 213, 104, 0)', 'rgba(255, 206, 84, 0)']
        }
      ]
      },
      options: {
        elements: {
          arc: {
            borderWidth: 0
          }
        },
        responsive: true,
        title: {
          display: true,
          text: 'Traditional Test Coverage',
          fontSize: 40
        },
        legend: {
          labels: {
            fontSize: 20
          }
        },
        tooltips: {
          enabled: false
        }
      }
    });
    isWaviLabs = false;
  } else {
    myChart2 = new Chart(ctx2, {
      type: 'doughnut',
      data: {
        labels: ['Unit Tests', 'Integration Tests', 'Other Tests', 'Property-based Tests', 'Static Type Checker'],
        datasets: [{
          label: 'Type Driven',
          data: [1, 1, 1, 1, 1],
          backgroundColor: ['rgba(172, 146, 235, 1)', 'rgba(79, 193, 232, 1)', 'rgba(237, 85, 100, 1)', 'rgba(160, 213, 104, 1)', 'rgba(255, 206, 84, 1)']
        }
      ]
      },
      options: {
        elements: {
          arc: {
            borderWidth: 0
          }
        },
        responsive: true,
        title: {
          display: true,
          text: 'Wavi Labs Test Coverage',
          fontSize: 40
        },
        legend: {
          labels: {
            fontSize: 20
          }
        },
        tooltips: {
          enabled: false
        }
      }
    });
    isWaviLabs = true;
  }
}

window.setInterval(toggleChart2, 5000);
