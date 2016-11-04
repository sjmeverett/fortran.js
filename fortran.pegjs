

Program
  = statement:ProgramStatement statements:(StatementSeparator ProgramStatement)* StatementSeparator?  {
  	return [statement].concat(statements);
  }

ProgramStatement
  = 'subroutine' _ name:Identifier _  args:ArgumentList StatementSeparator
      variables:VariableDeclarationList
      statements:SubroutineStatements
      _ 'end' _ {
  	return {type: 'subroutine', name, args, variables, statements};
  }

ArgumentList
  = '(' _ ')' { return []; }
  / '(' _ name:Identifier names:(_ ',' _ n:Identifier {return n;})* _ ')' {
  	return [name].concat(names);
  }

VariableDeclarationList
  = variables:(decl:VariableDeclaration _ StatementSeparator {return decl;})* {
    return Object.assign.apply(null, variables);
  }

VariableDeclaration
  = _ type:Type (attributes:( _ ',' _ attr:Attribute _ {return attr;})* '::')? decl:DeclarationList {
  	var vars = {};
  	decl.forEach((v) => { v.type = type; vars[v.name] = v; });
    return vars;
  }

Type
  = 'real'
  / 'integer'

Attribute
  = 'allocatable'
  / 'dimension' _ dimensions:Dimensions {return dimensions;}

DeclarationList
  = decl:Declaration decls:( _ ',' _ d:Declaration { return d; })* {
  	return [decl].concat(decls);
  }

Declaration
  = _ name:Identifier _ dimensions:Dimensions? {
  	return {name, dimensions};
  }

Dimensions
  = '(' _ bound:Bounds bounds:( _ ',' _ b:Bounds {return b;})* _ ')' {
  	return [bound].concat(bounds);
  }

Bounds
  = Identifier
  / ':'
  / '*'

SubroutineStatements
  = (statement:SubroutineStatement _ StatementSeparator { return statment; })*

SubroutineStatement
  = 'if' _ '(' _ expression:Expression _ ')' _ statement:SubroutineStatement {
    return {type: 'if', expression, statement}
  }

Expression
  = head:AndExpression tail:(_ '.OR.' _ t:AndExpression { return t; })* {
    return {type: 'or', expressions: [head].concat(tail)};
  }

OrTerm
  = head:OrExpression tail:(_ '.AND.' _ t:OrExpression { return t; })* {
    return {type: 'and', expressions: [head].concat(tail)};
  }

AndTerm
  =

Identifier
  = [a-z][a-z0-9]* { return text(); }

StatementSeparator "statement separator"
  = [\n;]

_ "whitespace"
  = [ \t]*
