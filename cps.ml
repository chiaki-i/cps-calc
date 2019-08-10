open Source
open Target
open Gensym

exception Unsupported_syntax

(* Danvy & Filinski, 1992
 * Figure 3 : One-pass, "properly tail-recursive" CPS transformation *)
let rec standard_df_cps (exp : Source.t) : Target.t = match exp with
  | Number (n) ->
    let gen_kappa = gensym "kappa" in
    MetaLam (gen_kappa, MetaApp (Var (gen_kappa), Number (n)))
  | Bool (b) ->
    let gen_kappa = gensym "kappa" in
    MetaLam (gen_kappa, MetaApp (Var (gen_kappa), Bool (b)))
  | Var (x) ->
    let gen_kappa = gensym "kappa" in
    MetaLam (gen_kappa, MetaApp (Var (gen_kappa), Var (x)))
  | Lambda (x, e) ->
    let gen_kappa = gensym "kappa" in
    let gen_x = gensym "x" in
    let gen_k = gensym "k" in
    let e_cpsdash = standard_df_cpsdash e in
    MetaLam (gen_kappa, MetaApp (Var (gen_kappa),
                                 CPSLam (gen_x,
                                         CPSLam (gen_k, MetaApp (e_cpsdash, Var (gen_k))))))
  | App (e1, e2) ->
    let gen_kappa = gensym "kappa" in
    let gen_m = gensym "m" in
    let gen_n = gensym "n" in
    let gen_a = gensym "a" in
    let e1_cps = standard_df_cps e1 in
    let e2_cps = standard_df_cps e2 in
    MetaLam (gen_kappa, MetaApp (e1_cps,
                                 MetaLam (gen_m, MetaApp (e2_cps,
                                                                MetaLam (gen_n, CPSApp (CPSApp (e1_cps, e2_cps), CPSLam (gen_a, MetaApp (Var (gen_kappa), Var (gen_a)))))))))
  | _ -> raise Unsupported_syntax
and standard_df_cpsdash (exp : Source.t) : Target.t = match exp with
  | Number (n) ->
    let gen_k = gensym "k" in
    MetaLam (gen_k, MetaApp (Var (gen_k), Number (n)))
  | Bool (b) ->
    let gen_k = gensym "k" in
    MetaLam (gen_k, MetaApp (Var (gen_k), Bool (b)))
  | Var (x) ->
    let gen_k = gensym "k" in
    MetaLam (gen_k, MetaApp (Var (gen_k), Var (x)))
  | Lambda (x, e) ->
    let gen_k1 = gensym "k1" in
    let gen_x = gensym "x" in
    let gen_k2 = gensym "k2" in
    let e_cpsdash = standard_df_cpsdash e in
    MetaLam (gen_k1, MetaApp (Var (gen_k1),
                              CPSLam (gen_x,
                                      CPSLam (gen_k2, MetaApp (e_cpsdash, Var (gen_k2))))))
  | App (e1, e2) ->
    let gen_k = gensym "k" in
    let gen_m = gensym "m" in
    let gen_n = gensym "n" in
    let e1_cps = standard_df_cps e1 in
    let e2_cps = standard_df_cps e2 in
    MetaLam (gen_k, MetaApp (e1_cps,
                             MetaLam (gen_m, MetaApp (e2_cps,
                                                      MetaLam (gen_n, CPSApp (CPSApp (Var (gen_m), Var (gen_n)), Var (gen_k)))))))
  | _ -> raise Unsupported_syntax
