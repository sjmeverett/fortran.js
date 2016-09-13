

export default class Parser {
  constructor(rules) {
    this.stack = [];
    this.source = '';
    this.position = 0;
    this.currentToken = null;

    this.rules = rules.map((obj) => {
      let rule = {};

      if (obj instanceof RegExp) {
        rule.pattern = obj;
      } else {
        rule.pattern = obj.pattern;
      }

      rule.skip = obj.skip || false;
      rule.type = obj.type;
      rule.fn = obj.fn || ((match) => match[0]);
      return rule;
    });
  }


  has(type, value) {
    if (this.currentToken.type === type && (typeof value === 'undefined' || this.currentToken.value === value)) {
      let token = this.currentToken;
      this.nextToken();
      return token;

    } else {
      return null;
    }
  }


  expect(type, value) {
    let result = this.has(type, value);

    if (result === null) {
      throw new Error(`Expected ${type} ${(value || '')}, not ${this.currentToken.raw}`);

    } else {
      return result.value;
    }
  }


  expectType(type) {
    let value = this.hasType(type);

    if (value) {
      return value;
    } else {
      throw new Error('Expected ' + type);
    }
  }


  nextToken() {
    while (this.position < this.source.length) {
      for (let rule of this.rules) {
        rule.pattern.lastIndex = this.position;
        let result = rule.pattern.exec(this.source);

        if (result && result.index === this.position) {
          this.position += result[0].length;

          if (rule.skip) {
            break;

          } else {
            let value = rule.fn(result);

            this.currentToken = {
              raw: result[0],
              type: rule.type || value,
              value
            };
            
            return this.currentToken;
          }
        }
      }
    }

    return this.currentToken = null;
  }
};
