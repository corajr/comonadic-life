# comonadic-life

Implementation of Conway's Game of Life using a comonad transformer.

A simple port of https://github.com/gelisam/conway/blob/master/src/Conway.hs

## Installation

```sh
git clone git://github.com/corajr/comonadic-life
cd example
npm install
npm start
```

Visit `http://localhost:3000` in your browser and click Step to evolve.

## Available scripts

### watch

`npm start` or `npm run watch` will start a development server, which
hot-reloads your application when sources changes.

### serve

`npm run serve` serves your application without watching for changes or
hot-reloading.

### build

`npm run build` bundles and minifies your application to run in production mode.
