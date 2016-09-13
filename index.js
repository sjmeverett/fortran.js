require('babel-register');
var FortranParser = require('./src/FortranParser').default;
var parser = new FortranParser();
var fs = require('fs');
var src = fs.readFileSync('./test.f90').toString();

src.split('\n').forEach((line) => parser.addSource(line));
console.log(parser.source);

try {
  console.log(require('util').inspect(parser.parse(), {depth: null}));
} catch (ex) {
  console.error('Line ' + parser.line + ' ' + ex.message);
  console.error(ex.stack.split('\n')[2]);
}
