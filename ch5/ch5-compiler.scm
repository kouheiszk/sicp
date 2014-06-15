;;;;COMPILER FROM SECTION 5.5 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch5.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator's syntax procedures
;;;;  from section 4.1.2
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.

;;;;Then you can compile Scheme programs as shown in section 5.5.5

;;**implementation-dependent loading of syntax procedures
(load "./ch5-syntax.scm")			;section 4.1.2 syntax procedures


;;;SECTION 5.5.1

;;; compile手続きは、
;;; 4.1.1節のeval手続き、4.1.7節のanalyze手続き、
;;; 5.4.1節の積極制御評価器のeval-dispatch入り口に対応する
;;; compileは翻訳すべき式の構文の型すべてに対して場合分けを行い、
;;; それぞれの型に対応するコード生成器へ振り分ける

;;; compil（とそれが呼び出すコード生成器）は
;;; 翻訳すべき式と
;;; 翻訳したコードが式の値を返すレジスタと
;;; 翻訳後のコードが実行後に何処に行くかを表す接続記述を
;;; 引数に取る

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;;; 各コード生成器は命令列を返す
;;; 合成式は、それを構成する単純な式を表すコードの組み合わせで表現される
;;; 命令列を合成するときにレジスタを退避しなくてはいけない場合がある
;;; レジスタを退避にはpreservingを用いる
;;; preservingはレジスタを退避するために、命令列を解析しなくてはいけない
;;; これは複雑だし、一度解析した式を再び解析することにもなるため非効率である
;;; そこで、各命令列に予めレジスタの使い方に関する情報を対応付けておく

;;; 命令列は、
;;; 列の命令を実行する前に初期化しなければならないレジスタの集合(needs)
;;; 列の命令が値を修正するレジスタの集合(modifies)
;;; 列の実際の命令(statements)
;;; を含む

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


;;;SECTION 5.5.2

;;;linkage code

;;; 接続記述は
;;; 列の次の命令を実行する (接続記述nextで指定する)
;;; 翻訳された手続きから戻る(接続記述returnで指定する)
;;; 指名した入り口へ飛び越す(接続記述として指示したラベルを使って指定する)
;;; の3通り

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

;; 各compile手続きは
;; end-with-linkageによって
;; 変換されたコードとlinkage先を接続する

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

;; TEST
;(compile-linkage 'return)
;(compile-linkage 'next)
;(compile-linkage 'hoge-label)


;;;simple expressions

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

;; TEST
;(compile-self-evaluating 1 'x 'next)

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

;; TEST
;(compile-quoted '('quote f) 'x 'next)

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

;; TEST
;(compile-variable 'a 'x 'next)

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

;; TEST
;(compile-assignment '('set! a 1) 'x 'next)
;(compile 1 'val 'next)

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

;; TEST
;(compile-definition '('define a 1) 'x 'next)


;;;conditional expressions

;;;labels (from footnote)

;;; プログラムには複数のifがあろうから, 
;;; 下に示すようなラベルtrue-branch, false-branch
;;; およびafter-ifをそのまま使うわけにはいかない.
;;; 翻訳系はラベルを生成するのに手続き make-labelを使う. 
;;; make-label は記号を引数としてとり, 
;;; その記号で始る新しい記号を返す.
;;; 
;;; 例えば(make-label 'a)を繰り返し呼び出すとa1, a2などが返る. 
;;; make-labelは質問言語で一意的変数名を生成したのと
;;; 同様に次のように実装出来る. 

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))
;; end of footnote

;;; ifは次のように翻訳される
;;; 
;;;  ⟨標的 val, 接続 next で述語の翻訳⟩
;;;  (test (op false?) (reg val))
;;;  (branch (label false-branch))
;;; true-branch
;;;  ⟨与えられた標的と与えられた接続か  after-if の接続で帰結部の翻訳⟩
;;; false-branch
;;;  ⟨与えられた標的と接続で代替部の翻訳⟩
;;; after-if

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch)) ; ifの結果の飛び先のラベルを作る
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage))) ; 飛び先がnext以外だったら
                                                       ; そっちにする
      (let ((p-code (compile (if-predicate exp) 'val 'next)) ; それぞれの場合
                                                             ; コードを翻訳
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage))) 
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;; TEST
;(make-label 'true-branch)
;(compile (if-predicate '('if '> a 1)) 'val 'after-if1)
;(compile (if-consequent '('if '> a 1)) 'val 'next)
;(compile (if-alternative '('if '> a 1)) 'val 'next)
;(compile-if '('if '> a 1) 'x 'next)

;;; sequences

;; preservingでseqの各命令列をつなげている

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

;; TEST
;(compile '(begin (f 1) (g 2)) 'x 'next)
;(compile-sequence '((f 1) (g 2)) 'x 'next)

;;;lambda expressions

;;; lambda式の目的コードは
;;;  ⟨手続きオブジェクトを構成しそれを標的レジスタへ代入⟩
;;;  ⟨与えられた接続のコード⟩ または (goto (label after-lambda))
;;;  ⟨手続き本体の翻訳⟩
;;; after-lambda
;;; となる

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage))) ;; 接続
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry)) ;; 本体
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

;; TEST
;(compile-lambda '('lambda (+ a b) (1 2)) 'x 'next)
;(compile-lambda-body '('lambda (+ a b) (1 2)) 'entry)

;;;SECTION 5.5.3

;;;combinations

;;; 組合せのコードは
;;; ⟨標的proc, 接続nextで演算子の翻訳⟩
;;; ⟨被演算子を評価し引数リストをarglに構成する⟩
;;; ⟨与えられた標的と接続で手続き呼出しの翻訳⟩
;;; の形となる. 

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes) ; 引数のリストを作る
      (compile-procedure-call target linkage)))))

;; TEST
;(compile-application '('+ 1 3) 'x 'next)
;(operator '('+ 1 3))
;(operands '('+ 1 3))
;(compile '+ 'proc 'next)
;(compile '(1 3) 'val 'next)


;;;  引数リストを構成するコードは, 
;;; 各被演算子をvalへ評価し, 
;;; その値をargl にためられる引数リストにconsする. 
;;; 
;;; 引数はarglに順にconsするので, 
;;; 引数が結果のリストに最初から最後の順で現れるように, 
;;; 最後の引数から始め, 最初で終らなければならない. 
;;;
;;; construct-arglistは
;;; 最後の引数でarglを初期化するコードを作り出し, 
;;; 残りの引数を評価するコードを連結し, それをarglに順につなげる. 
;;; 引数を最後から最初へ処理するため, compile-applicationが渡した順から, 
;;; 被演算子コード列のリストを逆転している.

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env) ; envを退避させながら連結
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env) ; envを退避させながら連結
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;;TEST
;(construct-arglist
;  (map (lambda (operand) (compile operand 'val 'next)) '(1 3)))
;(append-instruction-sequences
;                '(() (val) ((assign val (const 3))))
;                (make-instruction-sequence '(val) '(argl)
;                 '((assign argl (op list) (reg val)))))
;(code-to-get-rest-args
;  '((() (val) ((assign val (const 3))))))


;;;applying procedures

;;; 手続き作用のコードは
;;;  (test (op primitive-procedure?) (reg proc))
;;;  (branch (label primitive-branch))
;;; compiled-branch
;;;  ⟨与えられた標的と適切な接続で翻訳された手続きに作用させるコード⟩
;;; primitive-branch
;;;  (assign ⟨標的⟩
;;;          (op apply-primitive-procedure)
;;;          (reg proc)
;;;          (reg argl))
;;;  ⟨接続⟩
;;; after-call
;;; の形になる

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;;; 基本と合成の手続きの枝は, 
;;; compile-ifの真と偽の枝のように, 逐次に実行するのではないから, 
;;; 通常のappend-instruction-sequencesではなく, 
;;; parallel-instruction-sequencesを使って連結する.

;;;applying compiled procedures

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;; TEST
;(compile-proc-appl 'val 'next)
;;> ((proc) (env proc val argl continue) 
;;>         ((assign continue (label next)) 
;;>          (assign val (op compiled-procedure-entry) (reg proc)) 
;;>          (goto (reg val))))
;(compile-proc-appl 'val 'hoge-label)
;;> ((proc) (env proc val argl continue) 
;;>         ((assign continue (label hoge-label)) 
;;>          (assign val (op compiled-procedure-entry) (reg proc)) 
;;>          (goto (reg val))))
;(compile-proc-appl 'val 'return)
;;> ((proc continue) (env proc val argl continue) 
;;>                  ((assign val (op compiled-procedure-entry) (reg proc)) 
;;>                   (goto (reg val))))
;(compile-proc-appl 'x 'next)       
;;> ((proc) (env proc val argl continue) 
;;>         ((assign continue (label proc-return96)) 
;;>          (assign val (op compiled-procedure-entry) (reg proc)) 
;;>          (goto (reg val)) 
;;>          proc-return96 
;;>          (assign x (reg val)) 
;;>          (goto (label next))))
;(compile-proc-appl 'x 'hoge-label) 
;;> ((proc) (env proc val argl continue) 
;;>         ((assign continue (label proc-return97)) 
;;>          (assign val (op compiled-procedure-entry) (reg proc)) 
;;>          (goto (reg val)) 
;;>          proc-return97 
;;>          (assign x (reg val)) 
;;>          (goto (label hoge-label))))
;(compile-proc-appl 'x 'return)     
;;> ERROR


;; footnote

;;; 変数all-regsはすべてのレジスタ名のリストに束縛されている

(define all-regs '(env proc val argl continue))


;;;SECTION 5.5.4

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

;;; 与えられた列が与えられたレジスタを必要とするか, 
;;; 修正するかを決める

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


;;; 逐次に実行される任意個の命令列をとり, 
;;; すべての列の文を連結した文を持つ命令列を返す

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))


;;; レジスタのリストregsと逐次に実行する二つの命令列seq1とseq2をとり, 
;;; seq1の文にseq2の文が続いた文を持ち, 
;;; seq1が修正するがseq2が必要とするregsにあるレジスタを守るため
;;; seq1の前後に適切なsaveとrestore命令をつけた命令列を返す. 

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2) ; 退避する必要が無いなら単純にappend
      (let ((first-reg (car regs)))
        ; 同じレジストリを使うので退避する必要があるか？
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))


;;; 手続き本体を他の列に連結するのに使う

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))


;;; テストのその時の評価により, ある枝か, 他の枝に進む.
;;; そのため第二の枝が必要とするレジスタは, 
;;; 組み合せた列では, 第一の枝で修正してもなお必要とする.

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

'(COMPILER LOADED)
