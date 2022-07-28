source -> program .

# *** DECLARATIONS ***

program -> program_header label_part const_part type_part var_part subprg_part compound_stmt DOT .

program_header -> PROGRAM IDENTIFIER files SEMIC .

files -> .
files -> LPARENT file_names RPARENT .

file_names -> IDENTIFIER file_names' .
  file_names' -> COMMA IDENTIFIER file_names' .
  file_names' -> .

label_part -> .
label_part -> LABEL label_decls SEMIC .

label_decls -> label_decl label_decls' .
  label_decls' -> COMMA label_decl label_decls' .
  label_decls' -> .

label_decl -> INTEGERCONST .

const_part -> .
const_part -> CONST const_decls .

const_decls -> const_decl const_decls' .
  const_decls' -> const_decl const_decls' .
  const_decls' -> .

const_decl -> IDENTIFIER EQU constant SEMIC .

type_part -> .
type_part -> TYPE type_decls .

type_decls -> type_decl type_decls' .
  type_decls' -> type_decl type_decls' .
  type_decls' -> .

type_decl -> IDENTIFIER EQU type SEMIC .

var_part -> .
var_part -> VAR var_decls .

var_decls -> var_decl var_decls' .
  var_decls' -> var_decl var_decls' .
  var_decls' -> .

var_decl -> var_names COLON type SEMIC .

var_names -> IDENTIFIER var_names' .
  var_names' -> COMMA IDENTIFIER var_names' .
  var_names' -> .

subprg_part -> .
subprg_part -> subprg_decls .

subprg_decls -> subprg_decl subprg_decls' .
  subprg_decls' -> subprg_decl subprg_decls' .
  subprg_decls' -> .

subprg_decl -> procedure_header subprg_decl' .
  subprg_decl' -> label_part const_part type_part var_part subprg_part compound_stmt SEMIC .
  subprg_decl' -> EXTERNAL SEMIC .
  subprg_decl' -> FORWARD SEMIC .
subprg_decl -> function_header subprg_decl'' .
  subprg_decl'' -> label_part const_part type_part var_part subprg_part compound_stmt SEMIC .
  subprg_decl'' -> EXTERNAL SEMIC .
  subprg_decl'' -> FORWARD SEMIC .

procedure_header -> PROCEDURE IDENTIFIER par_part SEMIC .

function_header -> FUNCTION IDENTIFIER par_part COLON type SEMIC .

par_part -> .
par_part -> LPARENT par_decls RPARENT .

par_decls -> par_decl par_decls' .
  par_decls' -> SEMIC par_decl par_decls' .
  par_decls' -> .

par_decl -> pars COLON type .
par_decl -> VAR pars COLON type .
par_decl -> PROCEDURE IDENTIFIER par_part .
par_decl -> FUNCTION IDENTIFIER par_part COLON type .

pars -> IDENTIFIER pars' .
  pars' -> COMMA IDENTIFIER pars' .
  pars' -> .

# *** STATEMENTS ***

labeled_stmts -> labeled_stmt labeled_stmts' .
  labeled_stmts' -> SEMIC labeled_stmt labeled_stmts' .
  labeled_stmts' -> .

labeled_stmt -> .
labeled_stmt -> stmt .
labeled_stmt -> INTEGERCONST COLON labeled_stmt' .
  labeled_stmt' -> .
  labeled_stmt' -> stmt .

stmt -> assign_proc_stmt .
stmt -> case_stmt .
stmt -> compound_stmt .
stmt -> for_stmt .
stmt -> goto_stmt .
stmt -> if_stmt .
stmt -> repeat_stmt .
stmt -> while_stmt .
stmt -> with_stmt .

assign_proc_stmt -> IDENTIFIER assign_proc_stmt' .
  assign_proc_stmt' -> .
  assign_proc_stmt' -> LPARENT args RPARENT .
  assign_proc_stmt' -> var ASSIGN expr .

case_stmt -> CASE expr OF case_branches END .

case_branches -> case_branch case_branches' .
  case_branches' -> SEMIC case_branch case_branches' .
  case_branches' -> .

case_branch -> .
case_branch -> exprs COLON labeled_stmt .

compound_stmt -> BEGIN labeled_stmts END .

for_stmt -> FOR IDENTIFIER ASSIGN expr for_stmt' .
  for_stmt' -> TO expr for_stmt'' DO labeled_stmt .
  for_stmt' -> DOWNTO expr for_stmt'' DO labeled_stmt .
  for_stmt'' -> .
  for_stmt'' -> STEP expr .

goto_stmt -> GOTO INTEGERCONST .

if_stmt -> IF expr THEN labeled_stmt ELSE labeled_stmt .

repeat_stmt -> REPEAT labeled_stmts UNTIL expr .

while_stmt -> WHILE expr DO labeled_stmt .

with_stmt -> WITH vars DO labeled_stmt .

vars -> IDENTIFIER var vars' .
  vars' -> COMMA IDENTIFIER var vars' .
  vars' -> .

var -> LBRACKET exprs RBRACKET var .
var -> DOT IDENTIFIER var .
var -> PTR var .
var -> .

# *** EXPRESSIONS ***

exprs -> expr exprs' .
  exprs' -> COMMA expr exprs' .
  exprs' -> .

expr -> rel_expr .

rel_expr -> add_expr rel_expr' .
  rel_expr' -> .
  rel_expr' -> EQU add_expr .
  rel_expr' -> NEQ add_expr .
  rel_expr' -> LTH add_expr .
  rel_expr' -> GTH add_expr .
  rel_expr' -> LEQ add_expr .
  rel_expr' -> GEQ add_expr .
  rel_expr' -> IN add_expr .

add_expr -> mul_expr add_expr' .
  add_expr' -> ADD mul_expr add_expr' .
  add_expr' -> SUB mul_expr add_expr' .
  add_expr' -> OR mul_expr add_expr' .
  add_expr' -> .

mul_expr -> pfx_expr mul_expr' .
  mul_expr' -> MUL pfx_expr mul_expr' .
  mul_expr' -> DIV pfx_expr mul_expr' .
  mul_expr' -> IDIV pfx_expr mul_expr' .
  mul_expr' -> IMOD pfx_expr mul_expr' .
  mul_expr' -> AND pfx_expr mul_expr' .
  mul_expr' -> .

pfx_expr -> sfx_expr .
pfx_expr -> ADD sfx_expr .
pfx_expr -> SUB sfx_expr .
pfx_expr -> NOT sfx_expr .
pfx_expr -> PTR sfx_expr .

sfx_expr -> atom_expr sfx_expr' .
  sfx_expr' -> LBRACKET exprs RBRACKET sfx_expr' .
  sfx_expr' -> DOT IDENTIFIER sfx_expr' .
  sfx_expr' -> PTR sfx_expr' .
  sfx_expr' -> .

atom_expr -> STRINGCONST .
atom_expr -> REALCONST .
atom_expr -> INTEGERCONST .
atom_expr -> BOOLEANCONST .
atom_expr -> NIL .
atom_expr -> LPARENT expr RPARENT .
atom_expr -> IDENTIFIER atom_expr' .
  atom_expr' -> .
  atom_expr' -> LPARENT args RPARENT .

args -> arg args' .
  args' -> COMMA arg args' .
  args' -> .

arg -> expr arg' .
  arg' -> .
  arg' -> COLON INTEGERCONST arg'' .
  arg'' -> .
  arg'' -> COLON INTEGERCONST .

constants -> constant constants' .
constants -> IDENTIFIER constants' .
  constants' -> COMMA constant constants' .
  constants' -> COMMA IDENTIFIER constants' .
  constants' -> .

constant -> unsigned_constant .
constant -> ADD constant' .
constant -> SUB constant' .
  constant' -> IDENTIFIER .
  constant' -> unsigned_constant .

unsigned_constant -> STRINGCONST .
unsigned_constant -> REALCONST .
unsigned_constant -> INTEGERCONST .
unsigned_constant -> BOOLEANCONST .

# *** TYPES ***

type -> simple_type .
type -> pointer_type .
type -> struct_type .

simple_type -> atom_type .
simple_type -> scalar_type .
simple_type -> named_subrange_type .

atom_type -> CHAR .
atom_type -> REAL .
atom_type -> INTEGER .
atom_type -> BOOLEAN .

scalar_type -> LPARENT enums RPARENT .

enums -> IDENTIFIER enums' .
  enums' -> COMMA IDENTIFIER enums' .
  enums' -> .

named_subrange_type -> IDENTIFIER named_subrange_type' .
named_subrange_type -> constant INTERVAL named_subrange_type'' .
  named_subrange_type' -> .
  named_subrange_type' -> INTERVAL named_subrange_type'' .
  named_subrange_type'' -> INTEGER .
  named_subrange_type'' -> constant .

pointer_type -> PTR type .

struct_type -> array_type .
struct_type -> file_type .
struct_type -> set_type .
struct_type -> record_type .
struct_type -> PACKED struct_type' .
  struct_type' -> array_type .
  struct_type' -> file_type .
  struct_type' -> set_type .
  struct_type' -> record_type .

array_type -> ARRAY LBRACKET simple_types RBRACKET OF type .

simple_types -> simple_type simple_types' .
  simple_types' -> COMMA simple_type simple_types' .
  simple_types' -> .

file_type -> FILE OF type .

set_type -> SET OF type .

record_type -> RECORD rec_fields END .

rec_fields -> .
rec_fields -> CASE rec_fields' .
rec_fields -> rec_comp rec_fields'' .
  rec_fields' -> type OF rec_case_branches .
  rec_fields' -> IDENTIFIER COLON type OF rec_case_branches .
  rec_fields'' -> .
  rec_fields'' -> SEMIC rec_fields .

rec_comp -> SEMIC .
rec_comp -> comps COLON type .

comps -> IDENTIFIER comps' .
  comps' -> COMMA IDENTIFIER comps' .
  comps' -> .

rec_case_branches -> .
rec_case_branches -> rec_case_branch rec_case_branches' .
  rec_case_branches' -> .
  rec_case_branches' -> SEMIC rec_case_branches .

rec_case_branch -> SEMIC .
rec_case_branch -> constants COLON LPARENT rec_fields RPARENT .
