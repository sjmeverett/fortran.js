
%lex
%%

\n                    return 'ENDOFLINE'
\s+                   /* skip whitespace */
[+\-]?[0-9]+(\.[0-9]+)?([DE]-?[0-9]+)?\b  return 'NUMBER'
".TRUE."              return 'BOOLEAN'
".FALSE."             return 'BOOLEAN'
".LT."                return '.LT.'
".LE."                return '.LE.'
".EQ."                return '.EQ.'
".GT."                return '.GT.'
".GE."                return '.GE.'
".NE."                return '.NE.'
".AND."               return '.AND.'
".OR."                return '.OR.'
".NOT."               return '.NOT.'
'[^']*'               return 'STRING'
"*"                   return '*'
"/"                   return '/'
"-"                   return '-'
"+"                   return '+'
"**"                  return '**'
"("                   return '('
")"                   return ')'
"="                   return '='
","                   return ','
":"                   return ':'
"SUBROUTINE"          return 'SUBROUTINE'
"subroutine"          return 'SUBROUTINE'
"REAL"                return 'REAL'
"real"                return 'REAL'
[a-zA-Z][a-zA-Z0-9]   return 'IDENTIFIER'
<<EOF>>               return 'EOF'
.                     return 'INVALID'

/lex

%left '.OR.'
%left '.AND.'
%left '.NOT.'
%left '+' '-'
%left '*' '/'
%right '**'

%start statements

%%

statements
  :
