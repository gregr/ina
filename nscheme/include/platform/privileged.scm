;; TODO:
;; Because they assume the use of raw, untagged values, these low-level primitives are going to be
;; extremely unsafe to use in normal code.  For instance, interrupts need to be carefully managed
;; to avoid garbage collection while untagged numbers and addresses are still reachable.  Limiting
;; their use to code written in a representation-type-checked sublanguage will be safer.
;(define env.primitive.low-level
;  (make-array word-count) ==> addr  ; elements are machine-words
;  (memory-u{8,16,32,64}-ref  addr offset)
;  (memory-u{8,16,32,64}-set! addr offset x)
;  (address+ addr size) => addr
;  (address- addr addr) => size  ; is result signed, or does left operand have to be larger?
;  (call-address addr arg ...)  ; may need type info to respect platform ABI w.r.t. floats
;  raw (untagged) sN,uN,fN arithmetic and bitwise ops, with carry, overflow, etc.
;  extend/truncate between different N of sN,uN
;  vectorized ops
;  )

(define package.privileged
  (cons
    '(
      native-signal-handler
      procedure-metadata
      record? record record-type-descriptor record-ref)
    (list
      native-signal-handler
      procedure-metadata
      record? record record-type-descriptor record-ref)))
