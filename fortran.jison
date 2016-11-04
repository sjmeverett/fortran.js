
%lex
%%

[ \t]+                /* skip whitespace */
[\n;]                 return 'SEP'
[0-9]+                return 'INT'
[+\-]?[0-9]+(\.[0-9]+)?([DE]-?[0-9]+)?\b  return 'NUMBER'
".TRUE."              return 'BOOLEAN'
".FALSE."             return 'BOOLEAN'
".LT."                return 'LT'
".LE."                return 'LE'
".EQ."                return 'EQ'
".GT."                return 'GT'
".GE."                return 'GE'
".NE."                return 'NE'
".AND."               return 'AND'
".OR."                return 'OR'
".NOT."               return 'NOT'
\'[^']*\'             return 'STRING'
"**"                  return '**'
"*"                   return '*'
"/"                   return '/'
"-"                   return '-'
"+"                   return '+'
"("                   return '('
")"                   return ')'
"="                   return '='
","                   return ','
"::"                  return '::'
":"                   return ':'
"subroutine"          return 'SUBROUTINE'
"dimension"           return 'DIMENSION'
"allocatable"         return 'ALLOCATABLE'
"integer"             return 'TYPE'
"real"                return 'TYPE'
"if"                  return 'IF'
"goto"                return 'GOTO'
[a-zA-Z][a-zA-Z0-9]*  return 'IDENT'
<<EOF>>               return 'EOF'
.                     return 'INVALID'

/lex

%left OR
%left AND
%left NOT
%left '+' '-'
%left '*' '/'
%right '**'

%start program

%%

program
    : programStatements EOF
    { return $1 }
    ;

programStatements
    : programStatements SEP programStatement
        { $$ = $1.concat($2); }
    | programStatement
        { $$ = [$1];}
    ;


programStatement
    : SUBROUTINE IDENT '(' argumentList ')' SEP variableDeclarations SEP subroutineStatements END
      { $$ = {type: 'subroutine', name: $2, args: $3, variables: $7, statements: $6}; }
    ;

variableDeclarations
    : %empty
      { $$ = {}; }
    | variableDeclarations SEP variableDeclaration
      { $$ = Object.assign($1, $3); }
    | variableDeclaration
      { $$ = $1; }
    ;

variableDeclaration
    : typeSpec declList
      { var vars = {};
        $2.forEach((v) => { v.type = $1; vars[v.name] = v; });
        $$ = vars; }
    ;

declList
    : decl
      { $$ = [$1]; }
    | declList ',' decl
      { $$ = $1.concat($3); }
    ;

decl
    : IDENT
      { $$ = {name: $1}; }
    | IDENT '(' dimensions ')'
      { $$ = {name: $1, dimensions: $3}; }
    ;

typeSpec
    : TYPE
    { $$ = {type: $1} }
    | TYPE '::'
    { $$ = {type: $1} }
    | TYPE ',' attrList '::'
    { $$ = {type: $1, attibutes: $3, dimensions: $3.dimensions}; }
    ;

attrList
    : attr
    { $$ = $1; }
    | attrList ',' attr
    { $$ = Object.assign($1, $3); }
    ;

attr
    : DIMENSION '(' dimensions ')'
    { $$ = {dimensions: $3}; }
    | ALLOCATABLE
    { $$ = {allocatable: true}; }
    ;

dimensions
    : dimensions ',' bounds
    { $$ = $1.concat($3); }
    | bounds
    { $$ = [$1]; }
    ;

bounds
    : bound
    { $$ = {lower: 'assumed', upper: $1}; }
    | '*'
    { $$ = {autoshape: true}; }
    | ':'
    { $$ = {autosize: true}; }
    | bound ':'
    { $$ = {lower: $1, upper: 'assumed'}; }
    | bound ':' bound
    { $$ = {lower: $1, upper: $3}; }
    ;

bound
    : IDENT
    | NUMBER
    ;

argumentList
    : %empty
      { $$ = []; }
    | argumentList ',' IDENT
      { $$ = $1.concat($3); }
    | IDENT
    ;


subroutineStatements
    : subroutineStatements SEP subroutineStatement
    { $$ = $1.concat($3); }
    | subroutineStatement
    ;

subroutineStatement
    : IF '(' expression ')' subroutineStatement
    { $$ = {type: 'if', condition: $3, statement: $5}; }
    | GOTO INT
    { $$ = {type: 'goto', label: $2}; }
    | ASSIGNABLE '=' expression
    { $$ = {type: 'assignment', variable: $1, expression: $3}; }
    | INT
    { $$ = {type: 'label', name: $1}; }
    | CALL IDENT '(' exprList ')'
    { $$ = {type: 'call', args: $4}; }
    | IDENT '(' exprList ')'
    { $$ = {type: 'call', args: $3}; }
    | RETURN
    { $$ = {type: 'return'}; }
    | ALLOCATE '(' shapeSpec ')'
    { $$ = {type: 'allocate', spec: $3}; }
    | ALLOCATE '(' shapeSpec ',' STAT '=' IDENT ')'
    { $$ = {type: 'allocate', spec: $3, stat: $7}; }
    ;


shapeSpec
    : expression ':'
    { $$ = {lower: $1, upper: 'assumed'}; }
    | expression ':' expression
    { $$ = {lower: $1, upper: $3}; }
    ;


exprList
    : exprList ',' expression
    { $$ = $1.concat($3); }
    | expression
    | %empty
    { $$ = []; }
    ;


expression
    : expression '+' expression
    { $$ = {type: 'add', left: $1, right: $3}; }
    | expression '-' expression
    { $$ = {type: 'subtract', left: $1, right: $3}; }
    | expression '*' expression
    { $$ = {type: 'multiply', left: $1, right: $3}; }
    | expression '/' expression
    { $$ = {type: 'divide', left: $1, right: $3}; }
    | expression '**' expression
    { $$ = {type: 'power', left: $1, right: $3}; }
    | expression AND expression
    { $$ = {type: 'and', left: $1, right: $3}; }
    | expression OR expression
    { $$ = {type: 'or', left: $1, right: $3}; }
    | expression EQ expression
    { $$ = {type: 'equalTo', left: $1, right: $3}; }
    | expression NE expression
    { $$ = {type: 'notEqualTo', left: $1, right: $3}; }
    | expression LT expression
    { $$ = {type: 'lessThan', left: $1, right: $3}; }
    | expression LE expression
    { $$ = {type: 'lessThanOrEqualTo', left: $1, right: $3}; }
    | expression GT expression
    { $$ = {type: 'greaterThan', left: $1, right: $3}; }
    | expression GE expression
    { $$ = {type: 'greaterThanOrEqual', left: $1, right: $3}; }
    | NOT expression
    { $$ = {type: 'not', argument: $2}; }
    | NUMBER
    { $$ = {type: 'value', value: parseFloat($1)}; }
    | BOOLEAN
    { $$ = {type: 'value', value: $1 === 'true'}; }
    | STRING
    { $$ = {type: 'value', value: $1}; }
    | IDENT
    { $$ = {type: 'variable', name: $1}; }
    ;
