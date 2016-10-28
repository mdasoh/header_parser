%{
extern struct newtype_t
{
   struct newtype_t* next;
   char* types;
}* newtype_top
,* newstruct_top
,* newfunc_top;
int newtype_traverse( char* candidate );

#include "stdlib.h"
#include "stdio.h"
#include "y.tab.h"
%}

%%
"#"[^\r\n]*([\r\n]|[\r]|[\n]) {}
[ \t\r\n]* {}
\( { printf( "lp" ); return lparen; }
\) { printf( "rp" ); return rparen; }
\{ { printf( "{" ); return lbrace; }
\} { printf( "}" ); return rbrace; }
\[ { printf( "lb" ); return lbracket; }
\] { printf( "rb" ); return rbracket; }
"\.\.\." { return ellipsis; }
"\." { return dot; }
"\->" { return arrow; }
"\-\-" { return decr; }
"\+\+" { return incr; }
"<<=" { return lshift_assign; }
">>=" { return rshift_assign; }
"<<" { printf( "lsh" ); return lshift; }
">>" { printf( "rsh" ); return rshift; }
"<=" { return le; }
">=" { return ge; }
"==" { return eq; }
"!=" { return ne; }
"&&" { return logical_and; }
"\|\|" { return logical_or; }
"\*=" { return star_assign; }
"/=" { return slash_assign; }
"\%=" { return percent_assign; }
"\+=" { return plus_assign; }
"\-=" { return minus_assign; }
"&=" { return ampersand_assign; }
"\^=" { return caret_assign; }
"\|=" { return pipe_assign; }
[,] { return comma; }
[;] { return semicolon; }
"&" { return ampersand; }
"\*" { printf( "star" ); return star; }
"\+" { return plus; }
"\-" { return minus; }
"\~" { return tilde; }
"\!" { return bang; }
"/" { return slash; }
"\%" { return percent; }
"<" { return lt; }
">" { return gt; }
"\^" { return caret; }
"\|" { printf( "PIPE" ); return pipe_; }
"\?" { return query; }
":" { return colon; }
"=" { return assign; }
"auto" { return auto_; }
"break" { return break_; }
"case" { return case_; }
"const" { return const_; }
"continue" { return continue_; }
"default" { return default_; }
"do" { return do_; }
"else" { return else_; }
"enum" { return enum_; }
"extern" { return extern_; }
"for" { return for_; }
"goto" { return goto_; }
"if" { return if_; }
"register" { return register_; }
"return" { printf( "RET" ); return return_; }
"sizeof" { return sizeof_; }
"static" { return static_; }
"struct" { return struct_; }
"switch" { return switch_; }
"typedef" { return typedef_; }
"union" { return union_; }
"volatile" { return volatile_; }
"while" { return while_; }
"char" { return char_; }
"double" { return double_; }
"float" { return float_; }
"int" { return int_; }
"long" { return long_; }
"short" { return short_; }
"signed"|"__signed__" { return signed_; }
"unsigned" { return unsigned_; }
"void" { printf( "OID" ); return void_; }
"inline"|"__inline"|"__inline__" { return inline_; }
"restrict"|"__restrict" { return restrict_; }
"_Static_assert" { return _Static_assert_; }
"_Thread_local" { return _Thread_local_; }
"_Noreturn" { return _Noreturn_; }
"_Bool" { return _Bool_; }
"_Complex" { return _Complex_; }
"_Atomic" { return _Atomic_; }
"_Alignas" { return _Alignas_; }
"asm" { return asm_; }
"_Generic" { return _Generic_; }
"__extension__" { printf( "EE" ); return __extension___; }
"__thread" { return __thread_; }
"__auto_type" { return __auto_type_; }
"_Decimal32" { return _Decimal32_; }
"_Decimal64" { return _Decimal64_; }
"_Decimal128" { return _Decimal128_; }
"_Fract" { return _Fract_; }
"_Accum" { return _Accum_; }
"_Sat" { return _Sat_; }
"typeof"  { return typeof_; }
"__attribute__" { return __attribute___; }
"__label__" { return __label___; }
"__alignof__" { return __alignof___; }
"__real__" { return __real___; }
"__imag__" { return __imag___; }
"__func__" { return __func___; }
"__FUNCTION__" { return __FUNCTION___; }
"__PRETTY_FUNCTION__" { return __PRETTY_FUNCTION___; }
"__builtin_va_arg" { return __builtin_va_arg_; }
"__builtin_va_list" { return __builtin_va_list_; }
"__builtin_offsetof" { return __builtin_offsetof_; }
"__builtin_choose_expr" { return __builtin_choose_expr_; }
"__builtin_types_compatible_p" { return __builtin_types_compatible_p_; }
"__builtin_complex" { return __builtin_complex_; }
"__builtin_shuffle" { return __builtin_shuffle_; }

([0-9][a-fA-F0-9]*)|(0x[a-fA-F0-9]+) { printf( "." ); return decimal_constant_; }
[a-zA-Z0-9_]+ { int type = newtype_traverse( yytext ); if( 1 == type ) { printf( "identy" ); yylval.identifier = strdup( yytext ); return identifier_; } else if( 0 == type ) { printf( "caught" ); yylval.typedef_name = strdup( yytext ); return typedef_name_; } else if( 2 == type ) { printf( "structype" ); yylval.identifier = strdup( yytext ); return structype_; } else if( 3 == type ) { printf( "func" ); yylval.identifier = strdup( yytext ); return function; } }



%%
//[a-zA-Z0-9_]+_t { yylval.identifier = strdup( yytext ); printf( "Q" ); return typedef_name_; }

//"&&"|"\|\|" { printf( "op" ); return op; }
//"\*const"|[\*&] { printf( "&" ); return ointer; }
//[;,] { printf( ";" ); return semicolon; }
//"struct"|"enum"|"union" { return _struct; }
//"void"|"bool"|"char"|"int"|"float"|"double"|"__builtin_va_list" { return _type; }
//"short"|"long" { return _width; }
//"__extension__ typedef"|"typedef" { return tada; printf( "T" ); }
//"signed"|"unsigned"|"const"|"static"|"extern"|"volatile"|"register"|"__extension__"|"__inline"|"__inline__" { return _spec; }
//"__attribute__" { return _attr; }
//"__declspec" { return watspec; }
//"sizeof" { return _sizeof; }
//"return" { printf( "ret" ); return ret; }
//([0-9][a-fA-F0-9]*)|(0x[a-fA-F0-9]+) { printf( "." ); return digits; }
//[a-zA-Z0-9_]+_t { yylval.identifier = yytext[ 0 ]; printf( "Q" ); return identyt; }
//[a-zA-Z0-9_]+ { if( 1 == newtype_traverse( yytext ) ) { yylval.buffer = strdup( yytext ); return identy; } else { yylval.identifier = yytext[ 0 ]; return identyt; } }
//[\+\-\|\.=\?:]|"<<"|">>" { printf( "_" ); yylval.identifier = '_'; return op; }
//[/\\] { yylval.identifier = '0'; return op; }
//[/\\\'] { yylval.identifier = '0'; return identy; }

//atom identifier_
//atom typedef_name_
//atom address_space_

//atom floating_constant_
//atom decimal_constant_
//atom octal_constant_
//atom hexadecimal_constant_
//atom character_constant_
//atom string_literal_

//\([,a-zA-Z0-9\*_ \t\r\n]*\) { return identy; }  // see flex manual faq questions
//\([,a-zA-Z0-9\*_ \t\r\n]*\)([ \r\n\t])*; { return identy; }

int newtype_traverse( char* candidate )
{
   struct newtype_t* bah = newtype_top;
   printf( "Z" );
   for(; bah != NULL; bah = bah -> next )
   {
      if( 0 == strcmp( candidate, bah -> types ) )
         return 0;
   }
   bah = newstruct_top;
   printf( "z" );
   for(; bah != NULL; bah = bah -> next )
   {
      if( 0 == strcmp( candidate, bah -> types ) )
         return 2;
   }
   bah = newfunc_top;
   printf( "~" );
   for(; bah != NULL; bah = bah -> next )
   {
      if( 0 == strcmp( candidate, bah -> types ) )
         return 3;
   }
   printf( "_" );
   return 1; // identifier
}

int yywrap() // it's in -lfl
{
   return 1;
}

extern int yylex(); // -lfl
//int main( int argc, char* argv ) // it's in the yacc input
//{
//   while( yylex() );
//   return 0;
//}

/*
%%
"#"[^\r\n]*([\r\n]|[\r]|[\n]) {}
[ \t\r\n]* {}
\( { printf( "lp" ); return lparen; }
\) { printf( "rp" ); return rparen; }
\{ { return lbrace; }
\} { return rbrace; }
\[ { return lbracket; }
\] { return rbracket; }
"&&"|"\|\|" { printf( "op" ); return op; }
"\*const"|[\*&] { printf( "&" ); return ointer; }
[;,] { printf( ";" ); return semicolon; }
"struct"|"enum"|"union" { return _struct; }
"void"|"bool"|"char"|"int"|"float"|"double"|"__builtin_va_list" { return _type; }
"short"|"long" { return _width; }
"__extension__ typedef"|"typedef" { return tada; printf( "T" ); }
"signed"|"unsigned"|"const"|"static"|"extern"|"volatile"|"register"|"__extension__"|"__inline"|"__inline__" { return _spec; }
"__attribute__" { return _attr; }
"__declspec" { return watspec; }
"sizeof" { return _sizeof; }
"return" { printf( "ret" ); return ret; }
([0-9][a-fA-F0-9]*)|(0x[a-fA-F0-9]+) { printf( "." ); return digits; }
[a-zA-Z0-9_]+_t { yylval.identifier = yytext[ 0 ]; printf( "Q" ); return identyt; }
[a-zA-Z0-9_]+ { if( 1 == newtype_traverse( yytext ) ) { yylval.buffer = strdup( yytext ); return identy; } else { yylval.identifier = yytext[ 0 ]; return identyt; } }
[\+\-\|\.=\?:]|"<<"|">>" { printf( "_" ); yylval.identifier = '_'; return op; }
[/\\] { yylval.identifier = '0'; return op; }
[/\\\'] { yylval.identifier = '0'; return identy; }
%%
*/
