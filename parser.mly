%{
(* open Syntax *)
module State = Map.Make(String)
type state = int State.t
type operators = ((int list) -> (int list)) State.t
let (gl_st:state) = State.empty
type item =
  | Item of string
  | Num of int
type command = {
    src : item list;
    dest : string list;
    ope : string
  }
let consume k m =
  let v = State.find k m in
  let m' = State.remove k m in
  (v,m')
let create (k,v) m =
  try
    let _ = State.find k m in
    raise @@ Failure ("create:name "^k^" is used")
  with Not_found ->
    let m' = State.add k v m in
    m'
let rec consumes l m =
  match l with
  | [] -> ([],m)
  | (Item i)::tl ->
    let (v,m') = consume i m in
    let (l',m'') = consumes tl m' in
    (v::l',m'')
  | (Num n)::tl ->
    let (l',m') = consumes tl m in
    (n::l',m')
let rec creates l m =
  match l with
  | [] -> m
  | (k,v)::tl ->
    let m' = creates tl m in
    let m'' = create (k,v) m' in
    m''
let ax_ope = [
  "warp"  (* \ ??A->num,??A *);
  "intr"  (* ??A->??B,??A \ ??B *);
  "id"  (* ??A \ ??A *)
]
let exec_command c st =
  let (src,dst,ope) = (c.src,c.dest,c.ope) in
  let (l,_) = consumes src st in
  let _ = creates (List.map (fun x -> (x,0)) dst) st in
  1
let print_st st =
  State.iter
    (fun k v ->
      print_string ("?"^k^"="^(string_of_int v)^",");
      flush stdout )
    st 
%}

%token L_SLASH R_SLASH COLON SCOLON COMMA
%token <string> ITEM OPERATOR
%token <int> NUM
%token EOF

%start buffer
%type <int> buffer
%%
buffer:
  | command EOF {
      let _ = exec_command $1 gl_st in
      let _ = print_st gl_st in
      1 }
  ;
command:
  | L_SLASH COLON operator SCOLON { {src=[];dest=[];ope=$3} }
  | operator COLON R_SLASH SCOLON {  {src=[];dest=[];ope=$1} }
  | L_SLASH dest_list COLON operator SCOLON { {src=[];dest=$2;ope=$4} }
  | src_list L_SLASH COLON operator SCOLON {  {src=$1;dest=[];ope=$4} }
  | operator COLON R_SLASH src_list SCOLON {  {src=$4;dest=[];ope=$1} }
  | operator COLON dest_list R_SLASH SCOLON {  {src=[];dest=$3;ope=$1} }
  | src_list L_SLASH dest_list COLON operator SCOLON
      {  {src=$1;dest=$3;ope=$5} }
  | operator COLON dest_list R_SLASH src_list SCOLON
      {  {src=$5;dest=$3;ope=$1} }
  ;
src_list:
  | src   { [$1] }
  | src COMMA src_list { ($1::$3) }
  ;
dest_list:
  | ITEM  { [$1] }
  | ITEM COMMA dest_list { $1::$3 }
  ;
src:
  | ITEM   { (Item $1) }
  | NUM   { (Num $1) }
  ;
operator:
  | OPERATOR { $1 }
  ;
