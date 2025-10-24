;; TODO: maybe this is not the way to go.  We could consolidate our effort by focusing on a single
;; register-machine IR, defining primitives in terms of the register machine, and then have fewer
;; primitive constructs (just register machine components) to express for each backend.

;; At the very least, we should be generating this code, not writing it by hand.  That way we can
;; automate the boilerplate for calling/return conventions, which is error prone to write by hand.

;; TODO: bigint, rational, f32, f64,
(define runtime.js  ; rename to prim.js, and lift out the 'prim' field as the sole return value?
  (string-append
    "(function (){
    const type_values  = -1;
    const type_symbol  = 0;
    const type_cons    = 1;
    const type_vector  = 2;
    const type_mvector = 3;
    const type_closure = 4;

    const panic = function(...args){throw ['panic'].concat(args);};
    const is_vector = function(x){return Array.isArray(x) && (x.length>0) && x[0]===type_vector;};
    const is_record = function(x){return Array.isArray(x) && (x.length>0) && is_vector(x[0]);};
    const be_record = function(x){if(is_record(x)===false) panic(false, 'not a record', x);};

    const not_implemented = function(name){return function(...args){panic(false, 'not implemented', name);};};

    return {

      // TODO: this shouldn't be part of the runtime, it should be passed as the sole argument to each procedure / continuation
      'context': {
        'fuel': ,
        'pc': , // TODO: this will point to a continuation procedure
        'argc': ,
        'sp': ,  // TODO: may not be necessary if we use stack.length instead... may need a base pointer (bp) though?
        'stack': ,  // TODO: maybe a mostly-stack machine is simpler to target than a register machine
      },

      // TODO: unless we implement flat closures, we'll also need an env, which is annoying
      // like this:
      'context': {
        'fuel': ,
        'pc': , // TODO: this will point to a continuation procedure

        // maybe we should keep this to avoid passing any real args, though
        // or almost everything is real args / vars?
        // things that should be context for sure:
        // fuel, pc, saved, env
        // things that can be real args/vars:
        // proc argc args result
        // no, we can't do just real args because we still have to trampoline
        // - control operators: (call clo argc args), (apply clo argc args), (return argc result args)
        //   - these do *not* call pc, or closure label, directly, because we have to trampoline
        //   - how does return validate argc against continuation pc though?
        //   - seems like there can't be a 'return', the pc itself has to embed such a check
        //   - btw, aside from the pcs embedded in closures, only 'return' pcs are dynamic and need any kind of checking, other pcs are static and don't receive arguments, only the env
        //     - e.g., arg signatures: (renv count args) for dynamic pcs, (renv) for static pcs
        // - we don't need argc in the context (or do we?), but we do need everything else there
        'argc': ,  // probably don't need this as a register, we can pass it as a normal argument to the 'call' or 'apply' subroutine, or have that subroutine compute it from the args list, then select the correct case-lambda pc

        'args': ,
        'proc': ,
        'env': ,
        // idea for multi-value returns: return a single value (if any) in 'result' and additional values in 'args', call pc with the 'argc', and let it decide what to do
        //'result': ,
        //'values': ,
        'saved': ,  // Just a control stack in this case, like SICP-style register machine
      },

      // TODO: this object should be passed into a program's initialization procedure (pre-execution),
      // so that the program's execution procedures can capture it.  This object should contain
      // closures that respect the normal calling convention.

      // TODO: should the context also be passed to the initialization procedure?

      'prim': {
        'panic': panic,
        'set-panic-handler!': not_implemented('set-panic-handler!'),
        'procedure-metadata': not_implemented('procedure-metadata'),
        'record?': is_record,
        'record': function(rtd,...args){return [rtd].concat(args);},
        'record-type-descriptor': function(x){be_record(x); return x[0];},
        'record-ref': function(x,i){be_record(x); return x[i+1];},
        'string->bytes':,
        'bytes->string':,

        'apply':,
        'call-with-values':,

        // TODO: primitives have to take a continuation ???
        // or values has to construct a special object
        // how do we make sure 1-value contexts don't mistaken a values object for a single value?

        'values':, function(...args){},

        'eq?': function(a,b){return a===b;},
        // TODO: bigint, rational, string, bytes
        'eqv?': function(a,b){return (a===b)||false;},
        'null?': function(x){return x===null;},
        'boolean?': function(x){return (x===true)||(x===false);},

        procedure?
        symbol?

        'string?': function(x){return (typeof x)==='string';},

        rational?
        integer?
        f32?
        f64?
        pair?

        'vector?': is_vector,

        mvector? bytes? mbytes?
        string->symbol symbol->string

        'cons': function(a,b) {return [type_pair,a,b];},

        car
        cdr

        'vector': function(...args){return [type_vector].concat(args);},

        vector-length vector-ref

        'make-mvector': function(count,x){
                          let mv=[];
                          mv.length=count+1;
                          mv[0]=type_mvector;
                          for(let i=1; i<=count; ++i){mv[i]=x;}
                          return mv;},

        mvector->vector mvector-length mvector-ref mvector-set!

        bytes bytes-length bytes-u8-ref
        make-mbytes mbytes->bytes mbytes-length
        mbytes-u8-ref mbytes-u8-set!

        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-length integer-floor-divmod

        // TODO: not implemented:
        numerator denominator

        = <= >= < > + - * /

        // TODO: not implemented:
        f32->rational rational->f32 f64->rational rational->f64
        f32->u32 u32->f32 f64->u64 u64->f64 f32->f64 f64->f32
        f32-cmp f32-floor f32-ceiling f32-truncate f32-round f32+ f32- f32* f32/
        f64-cmp f64-floor f64-ceiling f64-truncate f64-round f64+ f64- f64* f64/
        }
      }
    };})();"))

- ideally flat closures, not an env register
  - how should closures be represented? a plain function? a structure wrapping a function?
- registers for value-accumulator and procedure-to-be-called
- register for stack, which may include return-address (represented as a JS function)
- register for frame-base?
- register for argument count
- maybe some extra registers for arguments?
- values
  - null: null
  - boolean: true false
  - string: string
  - fixnum: 32-bit integer
  - arrays where field 0 contains a type header
    - symbol bigint rational f32 f64 pair vector mvector closure record
  - can bytes and mbytes go into a typed u8 array?
    - first byte is a type header indicating whether it's mutable

(define (E-compile-rkt E)

  (define address->fresh-id
    (let ((&count (box 0)))
      (lambda (a)
        (let ((name  (let ((name? (and (vector? a) (<= 1 (vector-length a)) (vector-ref a 0))))
                       (if (identifier? name?) (syntax-unwrap name?) 'X)))
              (count (unbox &count)))
          (set-box! &count (+ count 1))
          (string->symbol (string-append (symbol->string name) "." (number->string count)))))))

  ;; TODO: have to lift and simplify complex data
  (define ($rkt:quote                   x) (list 'quote x))

  ;; TODO: wrap in a function to make it behave like an expression
  (define ($rkt:if                  c t f) (list 'if c t f))

  (define ($rkt:call            proc arg*) (cons proc arg*))

  ;; TODO: implement case-lambda calling convention in JS
  (define ($rkt:case-lambda param* ^body*)
    (let* ((id*~* (map (lambda (param) (improper-list-map address->fresh-id param)) param*))
           (body* (map apply ^body* (map improper-list->list id*~*))))
      (cons 'case-lambda (map list id*~* body*))))

  (define ($rkt:letrec lhs* ^rhs*&body) (let ((id* (map address->fresh-id lhs*)))
                                          (let-values (((rhs* body) (apply ^rhs*&body id*)))
                                            (list 'letrec (map list id* rhs*) body))))

  (define cenv.empty '())
  (define (cenv-ref    cenv addr)      (cdr (assv addr cenv)))
  (define (cenv-extend cenv addr* id*) (append (map cons addr* id*) cenv))

  (let loop/env ((E E) (env cenv.empty))
    (define (loop E) (loop/env E env))
    (cond
      ((E:annotated?   E) (loop (E:annotated-E E)))
      ((E:quote?       E) ($rkt:quote (E:quote-value E)))
      ((E:ref?         E) (cenv-ref env (E:ref-address E)))
      ((E:if?          E) ($rkt:if (loop (E:if-condition E))
                                   (loop (E:if-consequent E))
                                   (loop (E:if-alternative E))))
      ((E:call?        E) ($rkt:call (loop (E:call-operator E)) (map loop (E:call-operand* E))))
      ((E:case-lambda? E) (let* ((param* (E:case-lambda-param* E))
                                 (^body* (map (lambda (addr* body)
                                                (lambda id*
                                                  (loop/env body (cenv-extend env addr* id*))))
                                              (map improper-list->list param*)
                                              (E:case-lambda-body* E))))
                            ($rkt:case-lambda param* ^body*)))
      ((E:letrec?      E) (let* ((lhs*       (E:letrec-binding-left* E))
                                 (^rhs*&body (lambda id*
                                               (let ((env (cenv-extend env lhs* id*)))
                                                 (values (map (lambda (rhs) (loop/env rhs env))
                                                              (E:letrec-binding-right* E))
                                                         (loop/env (E:letrec-body E) env))))))
                            ($rkt:letrec lhs* ^rhs*&body)))

      ;; TODO: JS primitive implementations
      ;; - how do we return multiple values? call continuation with those values as arguments
      ((E:prim?        E) (E:prim-name E))

      (else               (error "not an expression" E))))

  )
