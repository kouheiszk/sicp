;;;;QUERY SYSTEM FROM SECTION 4.4.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;;Includes:
;;;;  -- supporting code from 4.1, chapter 3, and instructor's manual
;;;;  -- data base from Section 4.4.1 -- see microshaft-data-base below

;;;;This file can be loaded into Scheme as a whole.
;;;;In order to run the query system, the Scheme must support streams.

;;;;NB. PUT's are commented out and no top-level table is set up.
;;;;Instead use initialize-data-base (from manual), supplied in this file.


;;;SECTION 4.4.4.1
;;;The Driver Loop and Instantiation


;; 質問システムの駆動ループは入力の式を繰り返し読む。
;; 式がデータベースに追加すべき規則や表明であれば情報を追加する。
;; そうでなければ式は質問と考える。
;; 
;; 入力が質問であった場合、駆動ループはこの質問を
;; 単一の空フレームからなる初期のフレームストリームとともにqevalに渡す。
;; 評価の結果はデータベースで見つけた変数の値によって質問を満足させることで生成したフレームのストリームである。
;; 
;; これらのフレームを用いて、フレームのストリームから渡される値で
;; 変数を具体化した元々の質問のコピーからなる新しいストリームを形成する


;;;SECTION 4.4.4.8
;;;Frames and bindings

;; システムのextendを使わないようにここに記述
;; フレームを拡張する
(define (extend variable value frame)
  (cons (make-binding variable value) frame))



(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q) ;; (car q) が 'assert! 
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           ;; [extra newline at end] (announce-output output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp) ;; 変数だった場合
           (let ((binding (binding-in-frame exp frame))) ;; 変数が具体化できるかどうか
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)))) ;; 変数が具体化できなかった
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


;;;SECTION 4.4.4.2
;;;The Evaluator

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval))) ;; (put 'hoge 'qeval hogeproc) されているかどうか
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream)))) ;; 単純質問は simple-query で処理

;;;Simple queries

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame) ;; 拡張フレームのストリームを作る
      (delay (apply-rules query-pattern frame)))) ;; 可能な規則をすべて作用させて拡張されたフレームのもう一つのストリームを作る
   frame-stream))

;;;Compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts) ;; 残りの質問
               (qeval (first-conjunct conjuncts) ;; 1つ目の質問
                      frame-stream))))

;;(put 'and 'qeval conjoin)


(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;;(put 'or 'qeval disjoin)

;;;Filters

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands) ;; frameを質問で拡張しようと試みる
                              (singleton-stream frame)))
         (singleton-stream frame) ;; 拡張できなかったものを返す
         the-empty-stream))
   frame-stream))

;;(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v)))) ;; unbound-var-handler
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

;;(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

;;(put 'always-true 'qeval always-true)

;;;SECTION 4.4.4.3
;;;Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame))) ;; パターンとフレームに対してマッチをチェックすべき
                                                     ;; データベースの表明のすべてのストリームを得る

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

;; 既にフレームにある束縛と矛盾しない限り 
;; 現在のフレームを変数のデータへの束縛で拡張する
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))


;; 格納されている値がユニフィケーションで格納されたなら
;; パターン変数を持っている可能性がある。
;; 格納されたパターンが新しいデータに再帰的にマッチすると、
;; このパターンの変数の束縛を追加したりチェックしたりする。
;; 例えば?xが(f ?y)に束縛され, ?yは未束縛というフレームがあるとし、
;; ?xを(f b)に束縛してこのフレームを拡張したいとする。
;; 
;; 1. ?xを探しそれが(f ?y)に束縛されているのを知る
;; 2. (f ?y) はこのフレームで提案されている新しい値(f b)とマッチさせることになる
;; 3. このマッチはフレームを?yのbへの束縛を追加することで拡張する
;;
;; * ?xは(f ?y)に束縛したままである
;; * 格納されている束縛を修正することはない
;; * またある変数に一つを超える束縛を格納することもない


;;; ドット末尾を持つパターン

;; (computer . ?type) のパターンに対する
;; (computer programer suzuki) のマッチは
;; ?type を リスト (programer suzuki) にマッチする


;;;SECTION 4.4.4.4
;;;Rules and Unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
            (unify-match query-pattern
                         (conclusion clean-rule)
                         query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

;; 規則の中の変数をすべて一意的な名前に置き換える
;; -> 異る規則(仕事と住所とか)の作用で変数が互いに混乱するのを避けるため
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id)) ;; 例えば?xだったら?x-7にする
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; *** この行を除き構造はpattern-matchと同じ
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame)) ;; テスト
          ((var? val)                     ; *** varが未束縛で、valが変数
           (let ((binding (binding-in-frame val frame))) 
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; *** valが?hogeを含む式だった場合
           'failed)
          (else (extend var val frame))))) ;; 未束縛なら束縛する


;; パターン変数の値として提案された式valが
;; その変数varに依存しているかどうかテストする
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))


;;;SECTION 4.4.4.5
;;;Maintaining the Data Base

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern) ;; patternのcarがsymbolかどうか
      (get-indexed-assertions pattern) 
      (get-all-assertions))) ;; 格納されているすべての表明を返す

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream)) ;; 'hoge のassertinのストリームを取得

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream) ;; 定数の記号で始まる規則
   (get-stream '? 'rule-stream))) ;; 変数で始る規則


;; 表明と規則をデータベースに追加する
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))


;; 表明や規則を実際に格納するにはそれが添字づけ出来るかチェックする
;; 添字づけできたのであれば適切なストリームに格納する
(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream))) ;; 現在のassertion-streamを取得
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

;; 変数かsymbolであればtrue
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

;; (変数の場合の)?
;; 先頭の定数のsymbol
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

;; carがsymbolかどうか
(define (use-index? pat)
  (constant-symbol? (car pat)))


;;;SECTION 4.4.4.6
;;;Stream operations

;; ストリームに要素を追加する
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))


;; ストリームを組み合わせる
(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

;; 手続きをフレームのストリームにマップし
;; 結果のフレームのストリームを組み合せる
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

;; 単一要素からなるストリームを生成する
(define (singleton-stream x)
  (cons-stream x the-empty-stream))


;;;SECTION 4.4.4.7
;;;Query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

;; For and
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

;; For or
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

;; For not
(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

;; Rule
;; (rule conclusion rule-body)
(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule)) 

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

;; ?symbol を (? symbol) に変換する
;; (job ?x ?y) -> (job (?x) (? y))
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol))) ;; symbolをstringに変換
    (if (string=? (substring chars 0 1) "?") ;; 先頭が?だったら...
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

;; 変数は記号?で始まるリスト
(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

;; ルールごとに変数に添字をつけるためのやつ
(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?" 
     (if (number? (cadr variable))
         (string-append (symbol->string (caddr variable))
                        "-"
                        (number->string (cadr variable)))
         (symbol->string (cadr variable))))))


;;;SECTION 4.4.4.8
;;;Frames and bindings

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))


;;;;From Section 4.1

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))


;;;;Stream support from Chapter 3

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;;;;Table support from Chapter 3, Section 3.3.3 (local tables)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;;; From instructor's manual

(define get '())

(define put '())

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (put 'unique 'qeval uniquely-asserted) ;; for uniquely-asserted
  (deal-out rules-and-assertions '() '()))

;; Do following to reinit the data base from microshaft-data-base
;;  in Scheme (not in the query driver loop)
;; (initialize-data-base microshaft-data-base)

(define microshaft-data-base
  '(
;; from section 4.4.1
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(can-do-job (administration secretary)
            (administration big wheel))

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
))

(define empty-data-base '())

