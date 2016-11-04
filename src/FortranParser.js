
import _ from 'lodash';
import Parser from './Parser';

export default class FortanParser extends Parser {
  constructor() {
    super([
      {pattern: /;\n/gm, fn: (match) => (this.line++, match[0]), type: 'endofstatement'},
      {pattern: /[ \t]+/gm, skip: true},
      {pattern: /[+\-]?[0-9]+(\.[0-9]+)?([DE]-?[0-9]+)?\b/g, type: 'number'},
      {pattern: /'(([^']|'')*)'/g, type: 'string', fn: (match) => match[1]},
      {pattern: /\.(TRUE|FALSE)\./g, type: 'boolean'},
      /\.(LT|LE|EQ|GT|GE|NE|AND|OR|NOT)\./gi,
      {pattern: /(SUBROUTINE|IF|RETURN|END)/gi, fn: (match) => match[0].toUpperCase()},
      {pattern: /(INTEGER|REAL|DOUBLE[ \t]+PRECISION|COMPLEX|LOGICAL|CHARACTER|PARAMETER)/gi, type: 'type', fn: (match) => match[0].toUpperCase()},
      {pattern: /(DIMENSION|ALLOCATABLE|POINTER|TARGET|EXTERNAL|INTRINSIC)/gi, type: 'attribute', fn: (match) => match[0].toUpperCase()},
      /([*/\-+()=,]|\*\*|::?)/g,
      {pattern: /[a-z][a-z0-9]*/gi, type: 'identifier', fn: (match) => match[0].toUpperCase()}
    ]);

    this.line = 1;
  }


  parse() {
    this.source += '\n';
    this.nextToken();
    let statements = [];

    while (this.currentToken !== null) {
      if (!this.has('endofstatement')) {
        statements.push(this.parseProgramStatement());
      }
    }

    return statements;
  }


  parseProgramStatement() {
    if (this.has('SUBROUTINE')) {
      return {type: 'subroutine', spec: this.parseSubroutine()};

    } else {
      throw new Error('Unexpected token ' + this.currentToken.raw);
    }
  }


  parseSubroutine() {
    let name = this.expect('identifier');
    let args = [];
    this.expect('(');

    while (!this.has(')')) {
      args.push(this.expect('identifier'));

      if (this.currentToken.type !== ')') {
        this.expect(',');
      }
    }

    this.expect('endofstatement');

    let variables = this.parseVariableDeclarations();
    let statements = [];
    //
    // while (!this.has('END')) {
    //   statements.push(this.parseStatement());
    // }

    return {name, args, variables, statements};
  }


  parseVariableDeclarations() {
    let variables = {};

    while (this.currentToken.type === 'type') {
      _.assign(variables, this.parseVariableDeclaration());
    }

    return variables;
  }


  parseVariableDeclaration() {
    let type = this.expect('type');

    if (type === 'PARAMETER') {
      return this.parseConstantDeclaration();

    } else {
      let attributes = [];
      let allocatable = false;
      let dimensions;

      if (this.currentToken.type === ',') {
        while (this.has(',')) {
          if (this.has('attribute', 'ALLOCATABLE')) {
            allocatable = true;

          } else if (this.has('attribute', 'DIMENSION')) {
            this.expect('(');
            dimensions = this.parseArraySpec();
          }
        }

        this.expect('::');

      } else {
        this.has('::');
      }

      let variables = {};

      do {
        let variable = {
          name: this.expect('identifier'),
          dimensions,
          type
        };

        if (this.has('(')) {
          variable.dimensions = this.parseArraySpec();
        }

        if (this.currentToken.type !== 'endofstatement') {
          this.expect(',');
        }

        variables[variable.name] = variable;
      } while (!this.has('endofstatement'));

      return variables;
    }
  }


  parseArraySpec() {
    let dimensions = [];

    do {
      if (this.has('*')) {
        dimensions.push({assumedSize: true});

      } else {
        let spec = this.parseSpecificationExpression();
        let upper, lower;

        if (!this.has(':')) {
          lower = spec;
          upper = this.parseSpecificationExpression();
        } else {
          lower = {type: 'assumed'};
          upper = spec;
        }

        dimensions.push({lower, upper});
      }

      if (this.currentToken.type !== ')') {
        this.expect(',');
      }
    } while (!this.has(')'))

    return dimensions;
  }


  parseSpecificationExpression() {
    if (this.currentToken.type === 'number') {
      return {type: 'literal', value: this.expect('number')};
    } else if (this.currentToken.type === 'identifier') {
      return {type: 'variable', value: this.expect('identifier')};
    } else {
      return {type: 'assumed'};
    }
  }


  parseConstantDeclaration() {
    this.expect('(');
    let constants = {};

    do {
      let name = this.expect('identifier');
      this.expect('=');
      let value = this.parseLiteral();
      constants[name] = {type: 'constant', value};

      if (this.currentToken.type === ')') {
        this.expect(',');
      }
    } while (!this.has(')'));

    this.expect('endofstatement');
    return constants;
  }


  parseStatement() {
    if (this.has('if')) {
      this.expect('(')
      let expr = this.parseExpression();
      this.expect(')')
      let body = this.parseStatement();
      return {type: 'if', expr, body};
    }
  }


  parseExpression() {
    let left = this.parseExpression();
    let type;
    let right;

    if (this.has('.AND.')) {
      type = 'andExpression';
      right = this.parseExpression2();

    } else if (this.has('.OR.')) {
      type = 'orExpression';
      right = this.parseExpression2();

    } else {
      return left;
    }

    return {type, left, right};
  }


  parseExpression2() {

  }


  parseLiteral() {
    switch (this.currentToken.type) {
      case 'number': return this.get('number');
      case 'boolean': return this.get('boolean');
      case 'string': return this.get('string');
      default:
        throw new Error('expected literal value');
    }
  }


  addSource(line) {
    // skip comments
    if (line[0] === 'c')
      return;

    let statement = line.substring(6, 73);

    if (line[5] !== ' ') {
      if (!this.source.length)
        throw new Error('Unexpected line continuation on first line');

      this.source = this.source.substring(0, this.source.length - 1)
        + statement + '\n';

    } else {
      let label = parseInt(line.substring(0, 5));

      if (!isNaN(label)) {
        this.source += label + '\n';
      }

      if (!/^continue/i.test(statement))
        this.source += statement + '\n';
    }
  }
};
