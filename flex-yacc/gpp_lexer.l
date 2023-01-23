%{
    #include <stdio.h>
    #include <string.h>
    #include "y.tab.h"
%}

fraction    ({integer}|{digit})f({integer}|{digit})
integer     [1-9][0-9]*
id          ({alpha}|{underscore})({alpha}|{underscore}|{digit})*
notid       {digit}({alpha}|{underscore}|{digit})*
digit       [0-9]
alpha       [a-zA-Z]
underscore  [_]
plus        [+]
minus       [-]
div         [/]
mult        [*]
op          [(]
cp          [)]  
comma       [,]
ws          [ \t]+
nl          [\n]
comment     [;]{2}.*

  
%%
and         {strcat(yylval.content.string_exp, yytext); return OP_AND;}
or          {strcat(yylval.content.string_exp, yytext); return OP_OR;}
not         {strcat(yylval.content.string_exp, yytext); return OP_NOT;}
eq          {strcat(yylval.content.string_exp, yytext); return OP_EQ;}
gt          {strcat(yylval.content.string_exp, yytext); return OP_GT;}
set         {strcat(yylval.content.string_exp, yytext); return OP_SET;}
defvar      {strcat(yylval.content.string_exp, yytext); return DEFV;}
deffun      {strcat(yylval.content.string_exp, yytext);
             yylval.content.func_flag = 1; return DEFF;}
while       {strcat(yylval.content.string_exp, yytext); return KW_WHILE;}
if          {strcat(yylval.content.string_exp, yytext); return KW_IF;}
true        {strcat(yylval.content.string_exp, yytext); return KW_TRUE;}
false       {strcat(yylval.content.string_exp, yytext); return KW_FALSE;}
exit        {strcat(yylval.content.string_exp, yytext); return KW_EXIT;}


{plus}      {strcat(yylval.content.string_exp, yytext); return OP_PLUS;}
{minus}     {strcat(yylval.content.string_exp, yytext); return OP_MINUS;}
{div}       {strcat(yylval.content.string_exp, yytext); return OP_DIV;}
{mult}      {strcat(yylval.content.string_exp, yytext); return OP_MULT;}
{op}        {strcat(yylval.content.string_exp, yytext); return OP;}
{cp}        {strcat(yylval.content.string_exp, yytext); return CP;}
{comma}     {strcat(yylval.content.string_exp, yytext); return OP_COMMA;}

{comment}   {return COMMENT;}

{fraction}  {strcat(yylval.content.string_exp, yytext);
             strcpy(yylval.content.word, yytext); 
             yylval.content.length = yyleng; 
             return VALUEF;}

{id}        {strcat(yylval.content.string_exp, yytext);
             strcpy(yylval.content.word, yytext); 
             return ID;}


{ws}   {strcat(yylval.content.string_exp, yytext);}

{nl}   {strcat(yylval.content.string_exp, yytext);}


.|{notid}     {printf("LEXICAL ERROR: %s cannot be tokenized!\n", yytext);
                                    return 0;}

%%

  
int yywrap(){return 1;}