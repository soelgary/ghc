'use strict'

const autocannon = require('autocannon')


function warmup(cb) {
  const instance = autocannon({
    title: "Warmup",
    url: 'http://localhost:3000',
    connections: 5,
    duration: 30,
    reconnectRate: 10000
  }, cb);
  autocannon.track(instance, {
    renderProgressBar: true,
    renderResultTable: false
  });
}

function blast() {
  /*const instance = autocannon({
    title: "Blast",
    url: 'http://localhost:3000',
    connections: 10,
    duration: 5
  }, console.log);
  autocannon.track(instance, {
    renderProgressBar: true,
    renderResultTable: false
  });*/
}

warmup(() => blast());
