;; Although declarative module systems are nice, the implementation seems
;; significantly complex than a procedural system would be.  Maybe just
;; stick with lambdas that manage assocs (basic dependency injection).

;; So maybe don't do any of what is described here?


;; Current implementation sketch:
;; We can't maintain satisfied dependencies as a
;; gradually-extended `let` since it either prevents separate
;; compilation/expansion, or duplicates values, or forces us to include
;; irrelevant/disconnected modules.

;; instead, resolving a module looks like this:
;; _module contains:
;;   uid
;;   pre-linked lambda term representing this module's constructor
;;   definition expander, providing syntax for downstream modules to use
;;     optional, not present if no syntax names are provided
;;   assoc of provided syntax names to their parsers/transformers
;;   ordered list of provided value names
;;   ordered list of module uids to be linked with
;;   ordered list of unsatisfied dep value names
;;   ordered (topological sort) assoc of transitive dep modules
;; example, requiring definitions from modules 0 and 1, and leaving open some deps
;; uid2
;; constructor:
;; (lambda (0-a 0-b ...)
;;   (lambda (1-c 1-d ...)
;;     (lambda (open-e open-f ...)
;;       (begin (define _ ...) ...
;;         (lambda (link) (link provided-name ...))))))
;; ordered provided names: (provided-name ...)
;; linking order for satisfied deps: (uid0 uid1)
;;   e.g., we will eventually link this via: (uid1 (uid0 this))
;; unsatisfied deps: (e f ...)
;; transitive deps: (uid0-deps ... uid0 uid1-deps-unseen ... uid1-if-unseen)
;; definition expander (looks like the outer shell of constructor):
;; (lambda (0-a 0-b ...)
;;   (lambda (1-c 1-d ...)
;;     (lambda (open-e open-f ...)
;;       ;; save current env for parsers/transformers to close over
;;       ;; bind syntax defs (not the last time this will happen, but still
;;       ;;                   necessary here for recursive syntax references;
;;       ;;                   though, don't do this for plain let-syntax?)
;;       ;; extend env and hide 0-a 0-b ... 1-c 1-d ... open-e open-f ...
;;       ...hole using final env (which may hide some of the syntax we just bound)
;;       )))
;; Wouldn't be problematic to chain these for mixing multiple languages
;; except that (hidden) dependencies of a downstream language may shadow names
;; defined upstream...  So, instead of just providing a hole expander that has
;; already bound syntax and hidden deps, store the syntax defs and env they
;; should close over so they can be re-applied.  Finally, when the full
;; chain is known, bind syntax defs again, all at once in final env, and
;; provide that when filling the last hole.

;; Once we ask to build an evaluator (e.g. (module->eval root)), a let-expr is
;; built in dependency order by doing a depth-first traversal over the linking
;; order list of each module, starting with the root.
;; Actually, forget that, we can do this computation gradually as we build
;; modules, and also store an ordered list of transitive dependencies.


;; Old gradually-extended `let` idea that isn't workable:

;; _module contains:
;;   (TODO: a set of unsatisfied dependencies)
;;   assoc/set of satisfied dependencies, and itself
;;   assoc/(unique)list of provided value names
;;   definition expander (expand proc, aka language), encoding provided syntax
;;   linker expander, for extending the set of satisfied dependencies

;; structure built by expanders looks like:
;; (let ((uid-0 (begin 0-defs ... optionally use a definition-expander
;;                (lambda (link) (link 0-provided-name ...)))))
;;   (let ((uid-1 (uid-0 (lambda (0-desired-name ...)
;;                         (begin 1-defs ... optionally use a definition-expander
;;                           (lambda (link) (link 1-provided-name ...)))))))
;;     ...linker-expander))
;; module RHSs are built after a DFS resolves all dependencies of the RHS,
;; where let bindings are prepended for any that are still missing (first time
;; seen) by that point
;;
;; The definition-expander isn't clearly portrayed in this diagram, since
;; only the linker-expander hole exists at any time.  A definition-expander is
;; a one-hole context for an incomplete module RHS that is optionally used when
;; expanding a module's definitions (providing syntax via env), wrapping them
;; (they go in the one-hole).


;; Old sketch
;; module is either an env, a plain module, or a module w/ syntax?
;; languages are also modules?

(define-type language language? language-parent language-deps language-expand)

;; Create a new language using langauge-expand of language-parent:
;; (lambda (deps ...)
;;   ,(expander (lambda (env)
;;               capture env, define new parsers/transformers
;;               store captured continuation in language-expand
;;               store argument list as keys of assoc in language-deps
;;               )))

;; Create a new module using a language-expand:
;; mod:deps->link = ((language-expand l)
;;  '(lambda (deps ...)
;;     (define provided-name ...) ...
;;     (lambda (link) (link provided-name ...))))

;; Link a language/module to satisfy (some) dependencies:
;; mod:link = (upstream:link (lambda (upstream:provides ...)
;;                             (mod:deps->link desired-upstream:provides ...)))

;; TODO: hide deps in final env


;; example

(define scheme:minimal
  (module
    (letrec-syntax
      ;; add simple special forms (omit apply)
      )))

(define scheme:base:values-0
  (module
    (provide (all-defined))
    (require scheme:minimal)
    (let-syntax
      ;; primitive apply syntax
      ;; programmatically generate primitive parsers
      )))

(define scheme:base:values-1
  (module
    (provide (all-defined))
    (require scheme:minimal scheme:base:values-0)
    (let
      ;; programmatically generate primitive procedure definitions (except apply)
      )))

(define scheme:base:values-2
  (module
    (provide (all-defined) (all-from scheme:base:values-1))
    (require scheme:minimal scheme:base:values-1)
    (letrec
      ;; define cons*
      )))

(define scheme:base:values-3
  (module
    (provide (all-defined) (all-from scheme:base:values-2))
    (require scheme:minimal scheme:base:values-2)
    (let
      ;; define new apply
      )))

(define scheme:base:values
  (module
    (provide (all-defined) (all-from scheme:base-values-3))
    (require scheme:minimal scheme:base:values-3)
    (letrec  ;; could also use begin
      ;; define derived ops
      )))

(define scheme:base:syntax
  (module
    (provide (all-defined) (all-from scheme:minimal))
    (require scheme:minimal scheme:base:operators)
    (letrec-syntax
      ;; quasiquote, case, match
      )))

(define scheme:base
  (module (provide (all-from scheme:base:syntax)
                   (all-from scheme:base:values))))

(define scheme-eval (module->eval scheme:base))
