%{
#define Xstatic_assert_declaration_no_semi 100
#define Xtypeddef_name 101
#define Xaddress_space 102
#define Xstring_literal 103
#define Xprimary_expression 104
#define Xfloating_constant 105
#define Xinteger_constant 106
#define Xcharacter_constant 107
#define Xdecimal_constant 108
#define Xoctal_constant 109
#define Xhexadecimal_constant 110
#define Xtypedef_name 111
extern int yylex();
void yyerror( char* s );
#include <stdio.h>
void newtype( char* sis );
void newstruct( char* sis );
void newfunc( char* sis );
#include <string.h> // strdup()
int sclass = 0;
int declsclasscopy = 0;
%}

%union { char* identifier; char* typedef_name; void* address_space; double floating_constant; int decimal_constant; int octal_constant; int hexadecimal_constant; char character_constant; char* string_literal; }
//%start statements
%start translation_unit
%token <identifier> identifier_
%token <typedef_name> typedef_name_
//%token <typedef_name> structype_
%token <identifier> structype_
%token <identifier> function
%token <address_space> address_space_
%token <floating_constant> floating_constant_
//%token <decimal_constant> decimal_constant_
%token <identifier> decimal_constant_
%token <octal_constant> octal_constant_
%token <hexadecimal_constant> hexadecimal_constant_
%token <character_constant> character_constant_
%token <string_literal> string_literal_

%token <identifier> lbracket
%token <identifier> rbracket
%token <identifier> lparen
%token <identifier> rparen
%token <identifier> lbrace
%token <identifier> rbrace
%token <identifier> dot
%token <identifier> ellipsis
%token <identifier> arrow
%token <identifier> incr
%token <identifier> decr
%token <identifier> ampersand
%token <identifier> star
%token <identifier> plus

%token <identifier> minus
%token <identifier> tilde
%token <identifier> bang
%token <identifier> slash
%token <identifier> percent
%token <identifier> lshift
%token <identifier> rshift
%token <identifier> lt
%token <identifier> gt
%token <identifier> le
%token <identifier> ge
%token <identifier> eq
%token <identifier> ne
%token <identifier> caret
%token <identifier> pipe_

%token <identifier> logical_and
%token <identifier> logical_or
%token <identifier> query
%token <identifier> colon
%token <identifier> assign
%token <identifier> star_assign
%token <identifier> slash_assign
%token <identifier> percent_assign
%token <identifier> plus_assign
%token <identifier> minus_assign
%token <identifier> lshift_assign
%token <identifier> rshift_assign
%token <identifier> ampersand_assign
%token <identifier> caret_assign
%token <identifier> pipe_assign
%token <identifier> comma
%token <identifier> semicolon

//lbracket rbracket lparen rparen
//lbrace rbrace dot ellipsis arrow
//incr decr ampersand star plus
//minus tilde bang slash percent
//lshift rshift lt gt le
//ge eq ne caret pipe
//logical_and logical_or query colon
//assign star_assign slash_assign
//percent_assign plus_assign minus_assign
//lshift_assign rshift_assign ampersand_assign
//caret_assign pipe_assign comma semicolon

%token <identifier> auto_
%token <identifier> break_
%token <identifier> case_
%token <identifier> const_
%token <identifier> continue_
%token <identifier> default_
%token <identifier> do_
%token <identifier> else_
%token <identifier> enum_
%token <identifier> extern_
%token <identifier> for_
%token <identifier> goto_
%token <identifier> if_
%token <identifier> register_
%token <identifier> return_
%token <identifier> sizeof_
%token <identifier> static_
%token <identifier> struct_
%token <identifier> switch_
%token <identifier> typedef_
%token <identifier> union_
%token <identifier> volatile_
%token <identifier> while_
%token <identifier> char_
%token <identifier> double_
%token <identifier> float_
%token <identifier> int_
%token <identifier> long_
%token <identifier> short_
%token <identifier> signed_
%token <identifier> unsigned_
%token <identifier> void_
%token <identifier> inline_
%token <identifier> restrict_
%token <identifier> _Static_assert_
%token <identifier> _Thread_local_
%token <identifier> _Noreturn_
%token <identifier> _Bool_
%token <identifier> _Complex_
%token <identifier> _Atomic_
%token <identifier> _Alignas_
%token <identifier> asm_
%token <identifier> _Generic_
%token <identifier> __extension___
%token <identifier> __thread_
%token <identifier> __auto_type_
%token <identifier> _Decimal32_
%token <identifier> _Decimal64_
%token <identifier> _Decimal128_
%token <identifier> _Fract_
%token <identifier> _Accum_
%token <identifier> _Sat_
%token <identifier> typeof_
%token <identifier> __attribute___
%token <identifier> __label___
%token <identifier> __alignof___
%token <identifier> __real___
%token <identifier> __imag___
%token <identifier> __func___
%token <identifier> __FUNCTION___
%token <identifier> __PRETTY_FUNCTION___
%token <identifier> __builtin_va_list_
%token <identifier> __builtin_va_arg_
%token <identifier> __builtin_offsetof_
%token <identifier> __builtin_choose_expr_
%token <identifier> __builtin_types_compatible_p_
%token <identifier> __builtin_complex_
%token <identifier> __builtin_shuffle_

//%type <identifier> translation_unit
%type <identifier> external_declarations
%type <identifier> external_declaration
%type <identifier> declaration
%type <identifier> function_definition
%type <identifier> declaration_list
%type <identifier> init_declarator_list
%type <identifier> init_declarator
%type <identifier> nested_function_definition
%type <identifier> asm_definition
%type <identifier> static_assert_declaration
%type <identifier> static_assert_declaration_no_semi
%type <identifier> declaration_specifiers
%type <identifier> storage_class_specifier
%type <identifier> function_specifier
%type <identifier> type_specifier
%type <identifier> type_qualifier
%type <identifier> atomic_type_specifier
%type <identifier> enum_specifier
%type <identifier> enumerator_list
%type <identifier> enumerator
%type <identifier> enumeration_constant
%type <identifier> struct_or_union_specifier
%type <identifier> struct_or_union
%type <identifier> struct_contents
%type <identifier> struct_declaration_list
%type <identifier> struct_declaration
%type <identifier> specifier_qualifier_list
%type <identifier> struct_declarator_list
%type <identifier> struct_declarator
%type <identifier> typeof_specifier
%type <identifier> alignment_specifier
%type <identifier> declarator
%type <identifier> direct_declarator
%type <identifier> pointer
%type <identifier> type_qualifier_list
%type <identifier> array_declarator
%type <identifier> parameter_type_list
%type <identifier> parameter_list
%type <identifier> parameter_declaration
%type <identifier> identifier_list
%type <identifier> abstract_declarator
%type <identifier> direct_abstract_declarator
%type <identifier> parameter_forward_declarations
%type <identifier> asm_string_literal
%type <identifier> simple_asm_expr
%type <identifier> attributes
%type <identifier> attribute
%type <identifier> attribute_list
%type <identifier> attrib
%type <identifier> any_word
%type <identifier> type_name
%type <identifier> initializer
%type <identifier> initializer_list
%type <identifier> designation
%type <identifier> designator_list
%type <identifier> designator
%type <identifier> array_designator
%type <identifier> compound_statement
%type <identifier> block_item_list
%type <identifier> block_item
%type <identifier> nested_declaration
%type <identifier> label_declaration
%type <identifier> label
%type <identifier> statement
%type <identifier> labeled_statement
%type <identifier> expression_statement
%type <identifier> selection_statement
%type <identifier> iteration_statement
%type <identifier> jump_statement
%type <identifier> condition
%type <identifier> if_statement
%type <identifier> switch_statement
%type <identifier> while_statement
%type <identifier> do_statement
%type <identifier> for_statement
%type <identifier> asm_statement
%type <identifier> asm_argument
%type <identifier> asm_goto_argument
%type <identifier> asm_operands
%type <identifier> asm_operand
%type <identifier> asm_clobbers
%type <identifier> asm_goto_operands
%type <identifier> assignment_expression
%type <identifier> assignment_operator
%type <identifier> conditional_expression
%type <identifier> multiplicative_expression
%type <identifier> additive_expression
%type <identifier> shift_expression
%type <identifier> relational_expression
%type <identifier> equality_expression
%type <identifier> and_or_expression
%type <identifier> exclusive_or_expression
%type <identifier> inclusive_or_expression
%type <identifier> logical_and_expression
%type <identifier> logical_or_expression
%type <identifier> cast_expression
%type <identifier> unary_expression
%type <identifier> unary_operator
%type <identifier> generic_selection
%type <identifier> generic_assoc_list
%type <identifier> generic_association
%type <identifier> postfix_expression
%type <identifier> argument_expression_list
%type <identifier> primary_expression
%type <identifier> offsetof_member_designator
%type <identifier> expression
%type <identifier> expr_list
%type <identifier> nonempty_expr_list
%type <identifier> constant
%type <identifier> integer_constant
%type <identifier> argument_expression
%type <identifier> constant_expression
%type <identifier> struct_or_union_name
%type <identifier> struct_or_union_type
%type <identifier> dname
//%type <identifier> declaredfun
%type <identifier> init_declarator_list_list
%type <identifier> sis
%type <identifier> boom
%type <identifier> bah

%%
translation_unit :
                 | external_declarations
                 ;

external_declarations : external_declaration
                      | external_declarations external_declaration
                      ;

external_declaration : function_definition { printf( "function" ); }
                     | declaration { printf( "decl" ); } //declaration
                     | asm_definition { printf( "asmdef" ); }
                     | semicolon { printf( "stray;" ); }
		     | __extension___ external_declaration { $$ = $2; printf( "extension" ); }
		     ; //| declarator declaration semicolon { newfunc( $1 ); printf( "initialized" ); }

parameters : lparen rparen
	   | lparen parameter_type_list rparen
	   ;

sis : lparen pointer rparen
    | lparen pointer init_declarator_list rparen
    | lparen pointer init_declarator_list parameters rparen
    | lparen pointer init_declarator_list sis parameters rparen
    ;

boom : declaration_specifiers init_declarator_list parameters
     | declaration_specifiers sis parameters
     | declaration_specifiers init_declarator_list sis parameters
     ;

bah : boom semicolon { sclass = 0; }
    ;

declaration : declaration_specifiers semicolon { printf( "NDEC" ); }
            | declaration_specifiers init_declarator_list_list semicolon { $$ = $2; if( sclass ) { newtype( strdup( $2 ) ); printf( "INITIALIZED%s", $2 ); declsclasscopy = sclass = 0; } else printf( "NOINIT" ); }
            | declaration_specifiers bah { printf( "function" ); }
            | bah { printf( "unction" ); }
            | declaration_specifiers lparen pointer init_declarator_list rparen lparen parameter_type_list rparen semicolon { $$ = $4; { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $4 ); } //sclass?
            | static_assert_declaration { printf( "assert" ); }
            ;

function_definition : declarator compound_statement { newfunc( $1 ); printf( "definition" ); }
		    | declarator declaration_list compound_statement { newfunc( $1 ); printf( "initialized" ); }
                    | declaration_specifiers declarator compound_statement { $$ = $2; newfunc( $2 ); printf( "declaratordef%s", $2 ); }
                    | declaration_specifiers declarator declaration_list compound_statement { $$ = $2; newfunc( $2 ); printf( "declaratordef%s", $2 ); }
                    ;

declaration_list : declaration { printf( "seen" ); }
                 | declaration_list declaration { printf( "(decl)" ); }
                 ;

init_declarator_list_list : init_declarator { declsclasscopy = 1; printf( "COPY1:%s", $1 ); }
                          | init_declarator_list_list comma init_declarator { $$ = $3; declsclasscopy = 1; printf( "COPY2:%s", $3 ); if( sclass ) { newtype( strdup( $1 ) ); printf( "INITIALIZED%s", $1 ); } else printf( "NOINITS" ); }
                          ;

init_declarator_list : init_declarator { if( sclass ) printf( "[adeclarator%s]", $1 ); }
                     | init_declarator_list init_declarator { $$ = $2; if( sclass ) printf( "(declarator)" ); }
                     ;

init_declarator : declarator { $$ = $1; if( sclass ) printf( "[ideclarator%s]", $1 ); }
                | declarator assign initializer { $$ = $1; printf( "assignment" ); sclass = 0; }
                | declarator attributes { $$ = $1; printf( "ideclarator" ); }
                | declarator attributes assign initializer { $$ = $1; printf( "assignment" ); }
                | declarator simple_asm_expr { $$ = $1; printf( "asmdeclarator" ); sclass = 0; }
                | declarator simple_asm_expr assign initializer { $$ = $1; printf( "asmdeclarator" ); sclass = 0; }
                | declarator simple_asm_expr attributes { $$ = $1; printf( "asmdeclarator" ); sclass = 0; }
                | declarator simple_asm_expr attributes assign initializer { $$ = $1; printf( "asmdeclarator" ); sclass = 0; }
                ;

nested_function_definition : declaration_specifiers declarator compound_statement { printf( "nested" ); sclass = 0; }
                           | declaration_specifiers declarator declaration_list compound_statement { printf( "nested" ); sclass = 0; }
                           ;

asm_definition : simple_asm_expr semicolon { printf( "asmdef" ); }
               ;

static_assert_declaration : static_assert_declaration_no_semi semicolon { printf( "assertdecl" ); }
                          ;

static_assert_declaration_no_semi : _Static_assert_ lparen constant_expression comma string_literal_ rparen { printf( "asserting" ); return Xstatic_assert_declaration_no_semi; }
                                  ;

declaration_specifiers : storage_class_specifier declaration_specifiers { $$ = $2; if( sclass ) printf( "SPEC%s", $2 ); else sclass = 0; }
                       | type_specifier declaration_specifiers { $$ = $2; printf( "spec" ); }
                       | type_qualifier declaration_specifiers { $$ = $2; printf( "spec" ); }
                       | function_specifier declaration_specifiers { $$ = $2; printf( "spec" ); }
                       | alignment_specifier declaration_specifiers { $$ = $2; printf( "spec" ); }
                       | attributes declaration_specifiers { $$ = $2; printf( "spec" ); }
                       | storage_class_specifier { if( sclass ) printf( "SPEC" ); else sclass = 0; }
                       | type_specifier { printf( "spec" ); }
                       | type_qualifier { printf( "spec" ); }
                       | function_specifier { printf( "spec" ); }
                       | alignment_specifier { printf( "spec" ); }
                       | attributes { printf( "spec" ); }
                       ;

storage_class_specifier : typedef_ { printf( "s_typedef" ); sclass = 1; }
                        | extern_ { printf( "s_extern" ); sclass = 0; }
                        | static_ { printf( "s_static" ); sclass = 0; }
                        | auto_ { printf( "s_auto" ); sclass = 0; }
                        | register_ { printf( "s_reg" ); sclass = 0; }
                        | _Thread_local_ { printf( "s_thread" ); sclass = 0; }
                        | __thread_ { printf( "s_thread" ); sclass = 0; }
                        ;

function_specifier : inline_ { printf( "s_func" ); }
                   | _Noreturn_ { printf( "s_func" ); }
                   ;

type_specifier : void_ { printf( "t" ); }
               | char_ { printf( "t" ); }
               | short_ { printf( "t" ); }
               | int_ { printf( "t" ); }
	       | __builtin_va_list_ { printf( "t" ); }
               | long_ { printf( "t" ); }
               | float_ { printf( "t" ); }
               | double_ { printf( "t" ); }
               | signed_ { printf( "t" ); }
               | unsigned_ { printf( "t" ); }
               | _Bool_ { printf( "t" ); }
               | _Complex_ { printf( "t" ); }
               | struct_or_union_specifier { printf( "t" ); }
               | enum_specifier { printf( "t" ); }
               | typedef_name_ { printf( "TYPEDEF:%s", $1 ); }
               | atomic_type_specifier { printf( "t" ); }
               | typeof_specifier { printf( "t" ); }
               | __auto_type_ { printf( "t" ); }
               | _Decimal32_ { printf( "t" ); }
               | _Decimal64_ { printf( "t" ); }
               | _Decimal128_ { printf( "t" ); }
               | _Fract_ { printf( "t" ); }
               | _Accum_ { printf( "t" ); }
               | _Sat_ { printf( "t" ); }
               ;

type_qualifier : const_ { printf( "q" ); }
               | restrict_ { printf( "q" ); }
               | volatile_ { printf( "q" ); }
               | _Atomic_ { printf( "q" ); }
               | address_space_ { printf( "q" ); return Xaddress_space; }
               ;

atomic_type_specifier : _Atomic_ lparen type_name rparen { printf( "atomic" ); }
                      ;

enum_specifier : enum_ lbrace enumerator_list rbrace { printf( "enum" ); }
               | enum_ lbrace enumerator_list comma rbrace attributes { printf( "enum" ); }
               | enum_ identifier_ lbrace enumerator_list rbrace { $$ = $2; newstruct( strdup( $2 ) ); printf( "enum" ); }
               | enum_ identifier_ lbrace enumerator_list comma rbrace attributes { $$ = $2; newstruct( strdup( $2 ) ); printf( "enum" ); }
               | enum_ attributes lbrace enumerator_list rbrace { printf( "enum" ); }
               | enum_ attributes lbrace enumerator_list comma rbrace attributes { printf( "enum" ); }
               | enum_ attributes identifier_ lbrace enumerator_list rbrace { $$ = $3; newstruct( strdup( $3 ) ); printf( "enum" ); }
               | enum_ attributes identifier_ lbrace enumerator_list comma rbrace attributes { $$ = $3; newstruct( strdup( $3 ) ); printf( "enum" ); }
               | enum_ lbrace enumerator_list comma rbrace { printf( "enum" ); }
               | enum_ lbrace enumerator_list rbrace attributes { printf( "enum" ); }
               | enum_ identifier_ lbrace enumerator_list comma rbrace { $$ = $2; newstruct( strdup( $2 ) ); printf( "enum" ); }
               | enum_ identifier_ lbrace enumerator_list rbrace attributes { $$ = $2; newstruct( strdup( $2 ) ); printf( "enum" ); }
               | enum_ attributes lbrace enumerator_list comma rbrace { printf( "enum" ); }
               | enum_ attributes lbrace enumerator_list rbrace attributes { printf( "enum" ); }
               | enum_ attributes identifier_ lbrace enumerator_list comma rbrace { $$ = $3; newstruct( strdup( $3 ) ); printf( "enum" ); }
               | enum_ attributes identifier_ lbrace enumerator_list rbrace attributes { $$ = $3; newstruct( strdup( $3 ) ); printf( "enum" ); }
               | enum_ identifier_ { $$ = $2; newstruct( $2 ); }
               | enum_ attributes identifier_ { $$ = $3; newstruct( $2 ); }
               ;

enumerator_list : enumerator
                | enumerator_list comma enumerator { printf( "(enum)" ); }
                ;

enumerator : enumeration_constant
           | enumeration_constant assign constant_expression { printf( "enum=" ); }
           ;

enumeration_constant : identifier_ { printf( "id_enum" ); }
                     ;

struct_or_union_specifier : struct_or_union_name lbrace rbrace
                          | struct_or_union_name lbrace rbrace attributes
                          | struct_or_union_name lbrace struct_contents rbrace
                          | struct_or_union_name lbrace struct_contents rbrace attributes
                          | struct_or_union_type
                          ;

struct_or_union_name : struct_or_union attributes
                     | struct_or_union
                     | struct_or_union_type
                     ;

struct_or_union_type : struct_or_union identifier_ { $$ = $2; printf( "ON" ); newstruct( strdup( $2 ) ); }
                     | struct_or_union dname { $$ = $2; if( sclass != 1 ); } //{ printf( "OFF" ); sclass = 0; } }
                     | struct_or_union typedef_name_ { $$ = $2; if( sclass != 1 ) { printf( "OFF" ); sclass = 0; } }
                     | struct_or_union attributes identifier_ { $$ = $3; newstruct( strdup( $2 ) ); }
                     ;

struct_or_union : struct_ { printf( "struct" ); if( !sclass ) sclass = 2; }
                | union_ { printf( "union" ); if( !sclass ) sclass = 3; }
                ;

struct_contents : struct_declaration_list { printf( "contents1" ); }
                | struct_declaration { printf( "contents2" ); }
                | struct_declaration_list struct_declaration { printf( "contents3" ); }
                ;

struct_declaration_list : struct_declaration semicolon { printf( "s" ); }
                        | struct_declaration_list struct_declaration semicolon { printf( "s" ); }
                        | struct_declaration_list semicolon { printf( "s" ); }
                        | semicolon { printf( "stray;" ); }
                        ;

struct_declaration : specifier_qualifier_list struct_declarator_list { printf( "SDECLCOMPLETE" ); declsclasscopy = 0; }
                   | static_assert_declaration_no_semi
                   | __extension___ struct_declaration { printf( "extensionstruct" ); }
                   | specifier_qualifier_list
                   ;

specifier_qualifier_list : type_specifier specifier_qualifier_list
                         | type_qualifier specifier_qualifier_list
                         | attributes specifier_qualifier_list
                         | type_specifier
                         | type_qualifier
                         | attributes
                         ;

// FIXME: sclass is currently reset after the first one
struct_declarator_list : struct_declarator { $$ = $1; printf( "SDECL1" ); }
                       | struct_declarator_list comma struct_declarator { $$ = $3; printf( "SDECL2" ); }
                       | struct_declarator_list comma attributes struct_declarator { $$ = $4; printf( "SDECL3" ); }
                       ;

struct_declarator : declarator { printf( "STRUCT" ); declsclasscopy = 1; }
                  | declarator attributes { declsclasscopy = 1; }
                  | colon constant_expression { declsclasscopy = 1; }
                  | colon constant_expression attributes { declsclasscopy = 1; }
                  | declarator colon constant_expression { declsclasscopy = 1; }
                  | declarator colon constant_expression attributes { declsclasscopy = 1; }
                  ;

typeof_specifier : typeof_ lparen expression rparen
                 | typeof_ rparen type_name rparen
                 ;

alignment_specifier : _Alignas_ lparen type_name rparen
                    | _Alignas_ lparen constant_expression rparen
                    ;

dname : identifier_
      | structype_ // tilde structype_
      | typedef_name_ // structype_ || typedef_name_
      ;

declarator : direct_declarator { $$ = $1; printf( "DIRECT" ); }
           | pointer direct_declarator { $$ = $2; printf( "INDIRECT" ); }
           ;

direct_declarator : dname { $$ = $1; printf( "IDENTY%s", $1 ); } //| structype_ { $$ = $1; printf( "SIDENTY" ); }// | declaration_specifiers structype_ semicolon { printf( "SIDENTY%s", $1 ); } // moved from direct_declarator
                  | lparen declarator rparen { $$ = $2; printf( "(DECL)" ); }
                  | lparen attributes declarator rparen { $$ = $3; printf( "(ADECL)" ); }
                  | direct_declarator array_declarator { printf( "ARRDECL" ); }
                  | direct_declarator lparen parameter_type_list rparen { printf( "(TYPLIST)" ); }
                  | direct_declarator lparen rparen { printf( "()" ); }
                  | direct_declarator lparen parameter_forward_declarations rparen { printf( "(FDECLS)" ); }
                  | direct_declarator lparen identifier_list rparen { printf( "(IDLIST)" ); }
                  | direct_declarator lparen parameter_forward_declarations parameter_type_list rparen { printf( "(FTYPLIST)" ); }
                  ;

pointer : star
        | star pointer
        | star type_qualifier_list
        | star type_qualifier_list pointer
        ;

type_qualifier_list : type_qualifier
                    | attributes
                    | type_qualifier_list type_qualifier
                    | type_qualifier_list attributes
                    ;

array_declarator : lbracket rbracket
                 | lbracket assignment_expression rbracket
                 | lbracket type_qualifier_list rbracket
                 | lbracket type_qualifier_list assignment_expression rbracket
                 | lbracket static_ assignment_expression rbracket
                 | lbracket static_ type_qualifier_list assignment_expression rbracket
                 | lbracket type_qualifier_list static_ assignment_expression rbracket
                 | lbracket star rbracket
                 | lbracket type_qualifier_list star rbracket
                 ;

parameter_type_list : parameter_list
                    | parameter_list comma ellipsis
                    ;

parameter_list : parameter_declaration
               | parameter_list comma parameter_declaration
               ;

parameter_declaration : declaration_specifiers declarator { printf( "parameter1" ); } // sclass = 0;
                      | declaration_specifiers declarator attributes { printf( "parameter2" ); } //sclass = 0; }
                      | declaration_specifiers { printf( "parameter3" ); } //sclass = 0; }
                      | declaration_specifiers attributes { printf( "parameter4" ); } //sclass = 0; }
                      | declaration_specifiers abstract_declarator { printf( "abstract3" ); } //sclass = 0; }
                      | declaration_specifiers abstract_declarator attributes { printf( "abstract4" ); sclass = 0; }
                      | boom { printf( "abstract5" ); }
                      ;

identifier_list : identifier_
                | identifier_list comma identifier_
                ;

abstract_declarator : pointer { printf( "ABSTRACT" ); }
                    | direct_abstract_declarator
                    | pointer direct_abstract_declarator
                    ;

direct_abstract_declarator : lparen abstract_declarator lparen { printf( "(ABSTRACT)" ); }
                           | lparen attributes abstract_declarator lparen
                           | array_declarator
                           | direct_abstract_declarator array_declarator
                           | lparen rparen { printf( "()" ); }
                           | lparen parameter_type_list rparen { printf( "(PARAM)" ); }
                           | direct_abstract_declarator lparen rparen
                           | direct_abstract_declarator lparen parameter_type_list rparen
                           | lparen parameter_forward_declarations rparen
                           | lparen parameter_forward_declarations parameter_type_list rparen
                           | direct_abstract_declarator lparen parameter_forward_declarations rparen
                           | direct_abstract_declarator lparen parameter_forward_declarations parameter_type_list rparen
                           ;

parameter_forward_declarations : parameter_list semicolon
                               | parameter_forward_declarations parameter_list semicolon
                               ;

asm_string_literal : string_literal_ { return Xstring_literal; }
                   ;

simple_asm_expr : asm_ lparen asm_string_literal rparen
                ;

attributes : attribute
           | attributes attribute
           ;

attribute : __attribute___ lparen lparen attribute_list rparen rparen
          | __attribute___ lparen lparen rparen rparen
          ;

attribute_list : attribute_list comma
               | attrib
               | attribute_list comma attrib
               ;

attrib : any_word { printf( "ATTRANY" ); }
       | any_word lparen identifier_ rparen { printf( "ATTRANY" ); }
       | any_word lparen identifier_ comma nonempty_expr_list rparen { printf( "ATTRANY" ); }
       | any_word lparen expr_list rparen { printf( "ATTRANY" ); }
       ;

any_word : identifier_ { printf( "anys" ); }
         | storage_class_specifier { printf( "anys" ); }
         | type_specifier { printf( "anys" ); }
         | type_qualifier { printf( "anys" ); }
         ;

type_name : specifier_qualifier_list
          | specifier_qualifier_list abstract_declarator
          ;

initializer : assignment_expression { printf( "=INI" ); }
            | lbrace initializer_list rbrace { printf( "INI" ); }
            | lbrace initializer_list comma rbrace
            | lbrace rbrace { printf( "{}" ); }
            ;

initializer_list : initializer
                 | initializer_list comma initializer
                 | designation initializer
                 | initializer_list comma designation initializer
                 ;

designation : designator_list assign
            | array_designator
            | identifier_ colon
            ;

designator_list : designator
                | designator_list designator
                ;

designator : array_designator
           | dot identifier_
           ;

array_designator : lbracket constant_expression rbracket
                 | lbracket constant_expression ellipsis constant_expression rbracket
                 ;

compound_statement : lbrace rbrace
		   | lbrace return_ lparen declarator rshift decimal_constant_ rparen pipe_ lparen declarator lshift decimal_constant_ rparen semicolon rbrace { $$ = $6; printf( "EXTRA1" ); }
		   | lbrace return_ lparen identifier_ rshift decimal_constant_ rparen pipe_ lparen identifier_ lshift decimal_constant_ rparen semicolon rbrace { $$ = $6; printf( "EXTRA2" ); }
		   | lbrace return_ lparen structype_ rshift decimal_constant_ rparen pipe_ lparen structype_ lshift decimal_constant_ rparen semicolon rbrace { $$ = $6; printf( "EXTRA3" ); }
		   | lbrace return_ lparen typedef_name_ rshift decimal_constant_ rparen pipe_ lparen typedef_name_ lshift decimal_constant_ rparen semicolon rbrace { $$ = $6; printf( "EXTRA4" ); }
                   | lbrace block_item_list rbrace { $$ = $2; printf( "BITMLIST" ); }
		   | lbrace label_declarations block_item_list rbrace { $$ = $3; printf( "_BITMIST" ); }
                   ;

block_item_list : block_item { printf( "BITM" ); }
                | block_item_list block_item
                ;

block_item : nested_declaration { printf( "NBITM" ); }
           | statement { printf( "BISTMT" ); }
           ;

nested_declaration : declaration { printf( "NDECL" ); }
                   | __extension___ nested_declaration { printf( "extensionnested" ); }
                   | nested_function_definition
                   ;

label_declarations : label_declaration
                   | label_declarations label_declaration
                   ;

label_declaration : __label___ identifier_list semicolon
                  ;

label : identifier_ colon
      | identifier_ colon attributes
      | case_ constant_expression colon
      | default_ colon
      | case_ constant_expression ellipsis constant_expression colon
      ;

statement : labeled_statement
          | compound_statement
          | expression_statement { printf( "EXPRSTMT" ); }
          | selection_statement
          | iteration_statement
          | jump_statement { printf( "JUMPSTMT" ); }
          | asm_statement
          ;

labeled_statement : label statement
                  ;

expression_statement : semicolon
                     | expression semicolon
                     ;

selection_statement : if_statement
                    | switch_statement
                    ;

iteration_statement : while_statement
                    | do_statement
                    | for_statement
                    ;

jump_statement : return_ expression semicolon { printf( "RETREXPR" ); }
               | goto_ identifier_ semicolon
               | continue_ semicolon
               | break_ semicolon
               | return_ semicolon { printf( "RETRSEMI" ); }
               | goto_ star expression semicolon
               ;

condition : lparen expression rparen { printf( "CONDI" ); }
          ;

if_statement : if_ condition statement
             | if_ condition statement else_ statement
             ;

switch_statement : switch_ lparen expression rparen statement
                 ;

while_statement : while_ condition statement
                ;

do_statement : do_ statement while_ condition semicolon
             ;

for_statement : for_ lparen semicolon semicolon expression rparen statement
              | for_ lparen semicolon semicolon rparen statement
              | for_ lparen semicolon expression semicolon expression rparen statement
              | for_ lparen semicolon expression semicolon rparen statement
              | for_ lparen expression semicolon semicolon expression rparen statement
              | for_ lparen expression semicolon semicolon rparen statement
              | for_ lparen expression semicolon expression semicolon expression rparen statement
              | for_ lparen expression semicolon expression semicolon rparen statement
              | for_ lparen nested_declaration semicolon rparen statement
              | for_ lparen nested_declaration semicolon expression lparen statement
              | for_ lparen nested_declaration expression semicolon rparen statement
              | for_ lparen nested_declaration expression semicolon expression rparen statement
              ;

asm_statement : asm_ lparen asm_argument rparen semicolon
              | asm_ goto_ lparen asm_goto_argument rparen semicolon
asm_statement : asm_ type_qualifier lparen asm_argument rparen semicolon
              | asm_ type_qualifier goto_ lparen asm_goto_argument rparen semicolon
              ;

asm_argument : asm_string_literal
             | asm_string_literal colon
             | asm_string_literal colon colon
             | asm_string_literal colon colon colon
             | asm_string_literal colon colon colon asm_clobbers
             | asm_string_literal colon asm_operands
             | asm_string_literal colon asm_operands colon
             | asm_string_literal colon asm_operands colon colon
             | asm_string_literal colon asm_operands colon colon asm_clobbers
             | asm_string_literal colon colon asm_operands
             | asm_string_literal colon colon asm_operands colon
             | asm_string_literal colon colon asm_operands colon asm_clobbers
             | asm_string_literal colon asm_operands
             | asm_string_literal colon asm_operands colon asm_operands
             | asm_string_literal colon asm_operands colon asm_operands colon
             | asm_string_literal colon asm_operands colon asm_operands colon asm_clobbers
             ;

asm_goto_argument : asm_string_literal colon colon colon colon asm_goto_operands
                  | asm_string_literal colon colon asm_operands colon colon asm_goto_operands
                  | asm_string_literal colon colon colon asm_clobbers colon asm_goto_operands
                  | asm_string_literal colon colon asm_operands colon asm_clobbers colon asm_goto_operands
                  ;

asm_operands : asm_operand
             | asm_operands comma asm_operand
             ;

asm_operand : asm_string_literal lparen expression rparen
            | lbracket identifier_ rbracket asm_string_literal lparen expression rparen
            ;

asm_clobbers : asm_string_literal
             | asm_clobbers comma asm_string_literal
             ;

asm_goto_operands : identifier_
                  | asm_goto_operands comma identifier_
                  ;

assignment_expression : conditional_expression { printf( "ACOND" ); }
                      | unary_expression assignment_operator assignment_expression { printf( "AEQU" ); }
		      ;

assignment_operator : assign
                    | star_assign
                    | slash_assign
                    | percent_assign
                    | plus_assign
                    | minus_assign
                    | lshift_assign
                    | rshift_assign
                    | ampersand_assign
                    | caret_assign
                    | pipe_assign
                    ;

conditional_expression : logical_or_expression { printf( "OCOND" ); }
                       | logical_or_expression query expression colon conditional_expression { printf( "IFCOND" ); }
                       | logical_or_expression query colon conditional_expression { printf( "IECOND" ); }
                       ;

multiplicative_expression : cast_expression
                          | multiplicative_expression star cast_expression
                          | multiplicative_expression slash cast_expression
                          | multiplicative_expression percent cast_expression
                          ;

additive_expression : multiplicative_expression
                    | additive_expression plus multiplicative_expression
                    | additive_expression minus multiplicative_expression
                    ;

shift_expression : additive_expression
                 | shift_expression lshift additive_expression
                 | shift_expression rshift additive_expression
                 ;

relational_expression : shift_expression
                      | relational_expression lt shift_expression
                      | relational_expression gt shift_expression
                      | relational_expression le shift_expression
                      | relational_expression ge shift_expression
                      ;

equality_expression : relational_expression
                    | equality_expression eq relational_expression
                    | equality_expression ne relational_expression
                    ;

and_or_expression : equality_expression
                  | and_or_expression ampersand equality_expression
                  | and_or_expression pipe_ equality_expression
                  | equality_expression ampersand and_or_expression
                  | equality_expression pipe_ and_or_expression
                  ;

exclusive_or_expression : caret and_or_expression
                        | and_or_expression
                        | exclusive_or_expression caret and_or_expression
                        ;

inclusive_or_expression : exclusive_or_expression
                        | inclusive_or_expression bang exclusive_or_expression
                        ;

logical_and_expression : inclusive_or_expression { printf( "OLOGAND" ); }
                       | logical_and_expression logical_and inclusive_or_expression { printf( "LOGAND" ); }
                       ;

logical_or_expression : logical_and_expression { printf( "ANDLOGOR" ); }
                      | logical_or_expression logical_or logical_and_expression { printf( "LOGOR" ); }
                      ;

cast_expression : unary_expression { printf( "CEXPR" ); }
                | lparen type_name rparen unary_expression { printf( "CAST" ); }
                ;

unary_expression : postfix_expression { printf( "PUEXPR" ); }
                 | incr unary_expression
                 | decr unary_expression
                 | unary_operator cast_expression
                 | sizeof_ unary_expression
                 | sizeof_ lparen type_name rparen
                 | __alignof___ unary_expression
                 | __alignof___ lparen type_name rparen
                 | logical_and identifier_
                 ;

unary_operator : ampersand
               | star
               | plus
               | minus
               | tilde
               | bang
               | __extension___ { printf( "extensionunary" ); }
               | __real___
               | __imag___
               ;

generic_selection : _Generic_ lparen assignment_expression comma generic_assoc_list rparen
                  ;

generic_assoc_list : generic_association
                   |  generic_assoc_list comma generic_association
                   ;

generic_association : type_name colon assignment_expression
                    | default_ colon assignment_expression
                    ;

postfix_expression : primary_expression
                   | postfix_expression lbracket expression rbracket
                   | postfix_expression lparen rparen
                   | postfix_expression lparen argument_expression_list rparen
                   | function lparen rparen
                   | function lparen argument_expression_list rparen
                   | function lparen identifier_list rparen { printf( "funcall" ); }
                   | identifier_ lparen identifier_list rparen { printf( "funcall" ); }
                   | postfix_expression dot identifier_
                   | postfix_expression arrow identifier_
                   | postfix_expression incr
                   | postfix_expression decr
                   | lparen type_name rparen lbrace initializer_list rbrace
                   | lparen type_name rparen lbrace initializer_list comma rbrace
                   ;

argument_expression_list : argument_expression
                         | argument_expression_list comma argument_expression
                         ;

primary_expression : lparen expression rparen { printf( "PAREN" ); }
		   | lparen identifier_ rparen { printf( "ick" ); }
		   | identifier_ { printf( "PRIMARY" ); }
                   | structype_
                   | constant
                   | string_literal_ { return Xprimary_expression; }
                   | generic_selection
                   | __func___
                   | __FUNCTION___
                   | __PRETTY_FUNCTION___
                   | lparen compound_statement rparen
                   | __builtin_va_arg_ lparen assignment_expression comma type_name rparen
                   | __builtin_offsetof_ lparen type_name comma offsetof_member_designator rparen
                   | __builtin_choose_expr_ lparen assignment_expression comma assignment_expression comma assignment_expression rparen
                   | __builtin_types_compatible_p_ lparen type_name comma type_name rparen
                   | __builtin_complex_ lparen assignment_expression comma assignment_expression rparen
                   | __builtin_shuffle_ lparen assignment_expression comma assignment_expression rparen
                   | __builtin_shuffle_ lparen assignment_expression comma assignment_expression comma assignment_expression rparen
                   ;

offsetof_member_designator : identifier_
                           | offsetof_member_designator dot identifier_
                           | offsetof_member_designator lbracket expression rbracket
                           ;

expression : assignment_expression { printf( "EXPR" ); }
           | expression comma assignment_expression { printf( "EXPR" ); }
           ;

expr_list : nonempty_expr_list
          ;

nonempty_expr_list : assignment_expression
                   | nonempty_expr_list comma assignment_expression
                   ;

constant : floating_constant_ { return Xfloating_constant; }
         | integer_constant
         | character_constant_ { return Xcharacter_constant; }
         ;

integer_constant : decimal_constant_
                 | octal_constant_ { return Xoctal_constant; }
                 | hexadecimal_constant_ { return Xhexadecimal_constant; }
                 ;

argument_expression : assignment_expression
                    ;

constant_expression : conditional_expression
                    ;

%%

void yyerror( char* s )
{
   printf( "\n * %s * ", s );
}

struct newtype_t
{
   struct newtype_t* next;
   char* types;
}* newtype_top = NULL
,* newstruct_top = NULL
,* newfunc_top = NULL;

void newtype( char* sis )
{
   struct newtype_t* boom = ( struct newtype_t* )malloc( sizeof( struct newtype_t ) );
   boom -> types = sis; //strdup( sis );
   boom -> next = newtype_top;
   newtype_top = boom;
   printf( "newtype:%s", sis );
   printf( "%s,", sis );
}

void newstruct( char* sis )
{
   struct newtype_t* boom = ( struct newtype_t* )malloc( sizeof( struct newtype_t ) );
   boom -> types = sis; //strdup( sis );
   boom -> next = newstruct_top;
   newstruct_top = boom;
   printf( "%s,", sis );
}

void newfunc( char* sis )
{
   struct newtype_t* boom = ( struct newtype_t* )malloc( sizeof( struct newtype_t ) );
   boom -> types = sis; //strdup( sis );
   boom -> next = newfunc_top;
   newfunc_top = boom;
   printf( "%s,", sis );
}

int main( int argc, char* argv[] )
{
   newtype_top = NULL;
   while( yyparse() );
   return 0;
}







/*
declaredfun : lparen pointer init_declarator_list rparen lparen rparen { $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $3 ); }
            | lparen declaration_specifiers pointer rparen lparen rparen { printf( "FUNP" ); } //{ $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $4 ); }
            | lparen pointer rparen lparen rparen { printf( "FUNP" ); } //{ $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $3 ); }
            | lparen declaration_specifiers pointer rparen lparen parameter_type_list rparen { printf( "FUNP" ); } //{ $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $4 ); }
            | lparen pointer rparen lparen parameter_type_list rparen { printf( "FUNP" ); } // { $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $3 ); }
            | lparen declaration_specifiers pointer declaredfun rparen lparen rparen { $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $4 ); }
            | lparen pointer declaredfun rparen lparen rparen { $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $3 ); }
            | lparen declaration_specifiers pointer declaredfun rparen lparen parameter_type_list rparen { $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $4 ); }
            | lparen pointer declaredfun rparen lparen parameter_type_list rparen { $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $3 ); }
            | lparen declaration_specifiers pointer declarator lparen parameter_type_list rparen rparen lparen rparen { $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $4 ); }
            | lparen pointer declarator lparen parameter_type_list rparen rparen lparen rparen { $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $3 ); }
            | lparen declaration_specifiers pointer declarator lparen parameter_type_list rparen rparen lparen parameter_type_list rparen { $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $4 ); }
            | lparen pointer declarator lparen parameter_type_list rparen rparen lparen parameter_type_list rparen { $$ = $3; if( sclass ) { newtype( strdup( $3 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FUNP%s", $3 ); }
            | init_declarator_list lparen parameter_type_list rparen { $$ = $1; if( sclass ) { newtype( strdup( $1 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $2 ); }
            | init_declarator_list lparen rparen { $$ = $2; if( sclass ) { newtype( strdup( $1 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $1 ); }
            | declarator declaredfun { $$ = $1; printf( "FUNDECL:%s", $1 ); }
            ;
function_declaration : declarator declaration { newfunc( $1 ); printf( "initialized" ); }
                     | declaration_specifiers declarator declaration { $$ = $2; newfunc( $2 ); printf( "declaratordef%s", $2 ); }
                     ;

            | declaration_specifiers init_declarator_list lparen parameter_type_list rparen semicolon { $$ = $2; if( sclass ) { newtype( strdup( $2 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $2 ); }
            | declaration_specifiers init_declarator_list lparen rparen semicolon { $$ = $2; if( sclass ) { newtype( strdup( $2 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $2 ); }
            | declaration_specifiers lparen pointer init_declarator_list rparen lparen rparen semicolon { $$ = $4; if( sclass ) { newtype( strdup( $4 ) ); if( !declsclasscopy ) sclass = 0; } printf( "INITIAL_FP%s", $4 ); }

                          | struct_or_union_name identifier_ lbrace rbrace { $$ = $2; newtype( strdup( $2 ) ); }
                          | struct_or_union_name identifier_ lbrace rbrace attributes { $$ = $2; newtype( strdup( $2 ) ); }
                          | struct_or_union_name identifier_ lbrace struct_contents rbrace { $$ = $2; newtype( strdup( $2 ) ); }
                          | struct_or_union_name identifier_ lbrace struct_contents rbrace attributes { $$ = $2; newtype( strdup( $2 ) ); }
                          | struct_or_union attributes lbrace rbrace
                          | struct_or_union attributes lbrace rbrace attributes
                          | struct_or_union attributes lbrace struct_contents rbrace
                          | struct_or_union attributes lbrace struct_contents rbrace attributes
                          | struct_or_union attributes identifier_ lbrace rbrace { $$ = $3; newtype( strdup( $3 ) ); }
                          | struct_or_union attributes identifier_ lbrace rbrace attributes { $$ = $3; newtype( strdup( $3 ) ); }
                          | struct_or_union attributes identifier_ lbrace struct_contents rbrace { $$ = $3; newtype( strdup( $3 ) ); }
                          | struct_or_union attributes identifier_ lbrace struct_contents rbrace attributes { $$ = $3; newtype( strdup( $3 ) ); }
                          | struct_or_union attributes identifier_ { $$ = $3; newtype( strdup( $3 ) ); }
 { return Xinteger_constant; }
 { return Xdecimal_constant; }
return Xtypedef_name; }
%type <identifier> statements
statements : statement
           | statements statement
           ;





%{
extern struct newtype_t
{
   struct newtype_t* next;
   char* types;
}* newtype_top;
int newtype_traverse( char* candidate );

#include "stdlib.h"
#include "stdio.h"
#include "y.tab.h"
%}

//symbol [ lbracket symbol ] rbracket symbol ( lparen symbol ) rparen
//symbol { lbrace symbol } rbrace symbol . dot symbol ... ellipsis symbol -> arrow
//symbol ++ incr symbol -- decr symbol & ampersand symbol * star symbol + plus
//symbol - minus symbol ~ tilde symbol ! bang symbol / slash symbol % percent
//symbol << lshift symbol >> rshift symbol < lt symbol > gt symbol <= le
//symbol >= ge symbol == eq symbol != ne symbol ^ caret symbol | pipe
//symbol && logical-and symbol || logical-or symbol ? query symbol : colon
//symbol = assign symbol *= star-assign symbol /= slash-assign
//symbol %= percent-assign symbol += plus-assign symbol -= minus-assign
//symbol <<= lshift-assign symbol >>= rshift-assign symbol &= ampersand-assign
//symbol ^= caret-assign symbol |= pipe-assign symbol , comma symbol ; semicolon

//keyword auto keyword break keyword case keyword const keyword continue
//keyword default keyword do keyword else keyword enum keyword extern keyword for
//keyword goto keyword if keyword register keyword return keyword sizeof
//keyword static keyword struct keyword switch keyword typedef keyword union
//keyword volatile keyword while
//keyword char keyword double keyword float keyword int keyword long keyword short
//keyword signed keyword unsigned keyword void
//keyword inline keyword restrict
//keyword _Static_assert keyword _Thread_local keyword _Noreturn keyword _Bool
//keyword _Complex keyword _Atomic keyword _Alignas keyword asm keyword _Generic
//keyword __extension__ keyword __thread keyword __auto_type keyword _Decimal32
//keyword _Decimal64 keyword _Decimal128 keyword _Fract keyword _Accum keyword _Sat
//keyword typeof keyword __attribute__ keyword __label__ keyword __alignof__
//keyword __real__ keyword __imag__ keyword __func__ keyword __FUNCTION__
//keyword __PRETTY_FUNCTION__ keyword __builtin_va_arg keyword __builtin_offsetof
//keyword __builtin_choose_expr keyword __builtin_types_compatible_p
//keyword __builtin_complex keyword __builtin_shuffle

%%
"#"[^\r\n]*([\r\n]|[\r]|[\n]) {}
[ \t\r\n]* {}
\( { printf( "lp" ); return lparen; }
\) { printf( "rp" ); return rparen; }
\{ { return lbrace; }
\} { return rbrace; }
\[ { return lbracket; }
\] { return rbracket; }
"\.\.\." { return ellipsis; }
"\." { return dot; }
"\->" { return arrow; }
"\-\-" { return decr; }
"\+\+" { return incr; }
"<<=" { return lshift-assign; }
">>=" { return rshift-assign; }
"<<" { return lshift; }
">>" { return rshift; }
"<=" { return le; }
">=" { return ge; }
"==" { return eq; }
"!=" { return ne; }
"&&" { return logical-and; }
"\|\|" { return logical-or; }
"\*=" { return star-assign; }
"/=" { return slash-assign; }
"\%=" { return percent-assign; }
"\+=" { return plus-assign; }
"\-=" { return minus-assign; }
"&=" { return ampersand-assign; }
"\^=" { return caret-assign; }
"\|=" { return pipe-assign; }
[,] { return comma; }
[;] { return semicolon; }
"&" { return ampersand; }
"\*" { return star; }
"\+" { return plus; }
"\-" { return minus; }
"\~" { return tilde; }
"\!" { return bang; }
"/" { return slash; }
"\%" { return percent; }
"<" { return lt; }
">" { return gt; }
"\^" { return caret; }
"\|" { return pipe; }
"\?" { return query; }
":" { return colon; }
"=" { return assign; }

 auto
 break
 case
 const
 continue
 default
 do
 else
 enum
 extern
 for
 goto
 if
 register
 return
 sizeof
 static
 struct
 switch
 typedef
 union
 volatile
 while
 char
 double
 float
 int
 long
 short
 signed
 unsigned
 void
 inline
 restrict
 _Static_assert
 _Thread_local
 _Noreturn
 _Bool
 _Complex
 _Atomic
 _Alignas
 asm
 _Generic
 __extension__
 __thread
 __auto_type
 _Decimal32
 _Decimal64
 _Decimal128
 _Fract
 _Accum
 _Sat
 typeof 
 __attribute__
 __label__
 __alignof__
 __real__
 __imag__
 __func__
 __FUNCTION__
 __PRETTY_FUNCTION__
 __builtin_va_arg
 __builtin_offsetof
 __builtin_choose_expr
 __builtin_types_compatible_p
 __builtin_complex
 __builtin_shuffle


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

//\([,a-zA-Z0-9\*_ \t\r\n]*\) { return identy; }  // see flex manual faq questions
[6~//\([,a-zA-Z0-9\*_ \t\r\n]*\)([ \r\n\t])*; { return identy; }

int newtype_traverse( char* candidate )
{
   struct newtype_t* bah = newtype_top;
   printf( "Z" );
   for(; bah != NULL; bah = bah -> next )
   {
      if( 0 == strcmp( candidate, bah -> types ) )
         return 0;
   }
   return 1;
}

int yywrap() // it's in -lfl
{
   return 1;
}

//int main( int argc, char* argv ) // it's in the yacc input
//{
//   while( yylex() );
//   return 0;
//}

parameters :
	   | lparen parameter_list rparen
	   ;

sis : lparen pointer declarator parameters rparen
    | lparen pointer declarator sis parameters rparen
    ;

boom : type parameters
     | type boom parameters
     | type declarator parameters
     | type declarator boom parameters
     ;

bah : boom semicolon
    ;



type
lparen pointer declarator __optional__ lparen __parameters__ rparen rparen
lparen parameters rparen semicolon
type declarator lparen parameters rparen semicolon
*/



