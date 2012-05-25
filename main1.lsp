;;; main1.lsp


(set 'OP_NUL 0)
(set 'OP_LIT 1)
(set 'OP_STO 2)
(set 'OP_LOD 3)
(set 'OP_WRT 8)
(set 'OP_RET 9)
(set 'OP_ADD 11)
(set 'OP_MUL 13)

;;; (set 'x 1 'y "hello")
(set 'alpha "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
(set 'num "0123456789")
(set 'alnum "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")

(set 'tab (char 9))          ;;; "\t"
(set 'nl (char 10))          ;;; LF 
(set 'space (char 32))
(set 'lparen (char 40))      ;;; "("
(set 'rparen (char 41))      ;;; ")"
(set 'times (char 42))　　　　;;; "*"
(set 'plus (char 43))        ;;; "+"
(set 'semicolon (char 59))   ;;; ";"
(set 'assign (char 61))      ;;; "="
(set 'underscore (char 95))  ;;; "_"
 
(define isalpha (lambda (a) (if (find a alpha) true))) 
(define isnum (lambda (a) (if (find a num) true)))
(define isalnum (lambda (a) (if (find a alnum) true)))
(define islparen (lambda (a) (= lparen a)))
(define isrparen (lambda (a) (= rparen a))) 
(define isnl  (lambda (a) (= nl a)))
(define iswhite (lambda (a) (or (= tab a) (= space a))))
(define isassign (lambda (a) (= assign a)))
(define istimes (lambda (x) (= times x)))
(define isplus (lambda (x) (= plus x)))
(define issemicolon (lambda (a) (= semicolon a)))
(define iswrite (lambda (a) (= "write" a)))

(set 'tabcnt 0)
(set 'adrcnt 0)
(set 'table (array 16 4))    ;;; name kind val addre

(define set_table 
    (lambda (name kind) 
        (begin 
            (setf (table tabcnt 0) name) 
            (setf (table tabcnt 1) kind) 
            (setf (table tabcnt 2) 0 )
            (setf (table tabcnt 3) adrcnt) 
            (++ tabcnt)
            (++ adrcnt))))

(define isin_table 
    (lambda (name)
        (let ( (cnt 0) (num nil) )
            (while (< cnt tabcnt)
                (begin 
                    (if (= name (table cnt 0))
                        (begin
                            (println "it is in table: " (table cnt 0)" " cnt )
                            (setq num true)))
                    (++ cnt)))
            num)))

(define get_adrcnt 
    (lambda (name)
        (let ( (cnt 0) (num nil) )
            (while (< cnt tabcnt)
                (begin 
                    (if (= name (table cnt 0))
                        (begin
                            (println "it is in table: " (table cnt 0)" " cnt )
                            (setq num (table cnt 3))))
                    (++ cnt)))
            num)))

(define set_val 
    (lambda (name val)
        (setf (table (get_adrcnt name) 2) val)))

(define get_val 
    (lambda (name)
        (let ( (vcnt 0) (val nil) )
            (while (< vcnt tabcnt)
                (begin 
                    (if (= name (table vcnt 0))
                        (begin
                            (println "val: " (table vcnt 2))
                            (setq val (table vcnt 2))))
                (++ vcnt)))
            val)))

;;; (let ((sym1 [exp-init1]) [(sym2 [exp-init2]) ... ]) body)
;;; (let (sym1 exp-init1 [sym2 exp-init2 ... ]) body)
             
(define get_kind 
    (lambda (name)
        (let ( (kcnt 0) (kind nil) )
            (while (< kcnt tabcnt)
                (begin 
                    (if (= name (table kcnt 0))
                        (begin
                            (println "kind: " (table kcnt 1))
                            (setq kind (table kcnt 1))))
                (++ kcnt)))
            kind)))

(set 'path "/Your/path/to/reach/wrt.txt") 
(set 'handle (open path "read"))
(read handle buff 200)

(parse buff " ")
(set 'len (length buff))
(set 'cnt 0)

look
peec

(define dopeec (lambda () (if (< cnt len))
    (setq peec (buff cnt))))    ;;; already incremented

(define skipwhite
    (lambda ()
        (if (iswhite look)
                (while (or (iswhite look) (isnl look))
                   (dolook)))))

(define dolook
    (lambda ()
        (if (< cnt len)
            (begin 
                (setf look (buff cnt))
                (inc cnt)
                look))))

(dolook)    ;;; starting look

(set 'code (array 16 2))
(set 'codcnt 0)

(define genlit (lambda(n) 
    (setf (code codcnt 0) OP_LIT) (setf (code codcnt 1) n) (inc codcnt)))

;;; get the adr from name table
(define gensto (lambda(adr) 
    (setf (code codcnt 0) OP_STO) (setf (code codcnt 1) adr) (inc codcnt)))

(define genlod (lambda(adr) 
    (setf (code codcnt 0) OP_LOD) (setf (code codcnt 1) adr) (inc codcnt)))

(define genwrt (lambda() 
    (setf (code codcnt 0) OP_WRT) (setf (code codcnt 1) 0) (inc codcnt)))

(define genret (lambda() 
    (setf (code codcnt 0) OP_RET) (setf (code codcnt 1) 0) (inc codcnt)))


(define factors 
    (lambda ()
        (let ((strnum) (myData2)  )
        (println "\nfactors called:")
        (println "look: " look)
        (if 
            (isassign look) 
                (begin 
                    (println "assign operator found: ")
                    (dolook)))
      ;          (println "look after dolook: " look)))   ;;; 5

        (if 
            (isnum look)
                (begin
                    (println "number found: ")
                    (while (isnum look) 
                        (begin
                            (push look strnum -1) 
                            (dolook)))
                    (genlit (int (join strnum)))
                    (println "strnum: " (int (join strnum)))))

        (if
            (isalpha look) 
                (begin
                    (println "alpha found: ") 
                    (while (isalnum look)
                        (begin
                            (push look myData2 -1) 
                            (dolook)))
             
                    (println "myData2: " myData2)
                    (set 'mydata3 (join myData2))
                    (println "mydata3: " mydata3)
                    (if (isin_table mydata3) 
                        (begin
                            (println mydata3 " is in table")
                            (genlod (get_adrcnt mydata3)))))))))

(define terms (lambda ()
    (println "\nterms called:")
    (factors)
    (println "\nafter back from factors in terms: look: " look)
    (while (istimes look) 
        (begin
            (dolook)
            (factors)
            (gen OP_MUL 0)))))

(define expression (lambda ()
    (println "\nexpression called:")
    (terms)
    (println "\nafter back from terms in expression: look: " look)
    (while (isplus look) 
        (begin
            (dolook)
            (terms)
            (gen OP_ADD 0)))))

(define paren_expre
    (lambda ()
        (if (islparen look) (dolook))
        (expression)
        (println "look: after back from expresion paren_expre: " look) ;;; ')'
        (if (isrparen look) (dolook))))

(define write_stmnt
    (lambda ()
        (println "\nwrite_stmnt called: ")
        (println "look in top of write_stmnt: " look)
        (paren_expre)
        (println "look after back from paren_expre in write stmnt: " look)
        (genwrt)))

(define get_gstr
    (lambda ()
        (let (mydata)
            (if (isalpha look) 
                (while (isalnum look)
                    (begin
                        (push look mydata -1)
                        (dolook))))
            (set 'gstr (join mydata))
            gstr)))

(define statement 
    (lambda ()                   ;;; 2
        (println "statement called")
        (skipwhite)
        (println "look: at top of stmnt: " look) 
        (get_gstr)
        (if 
            (iswrite gstr) (write_stmnt)
            (if-not (isin_table gstr)
                (begin
                    (println "not in the table will set here")
                    (set_table gstr) 
                    (println "gstr len: " (length gstr)) ;;; 1st 1
                    (expression)
                    (println "back from expression look in stmnt: " look)
                    (if (issemicolon look) (dolook))
                    (println "whats the gstr: " gstr) ;;; still 'a' 
                    (gensto (get_adrcnt gstr)))

                (begin    ;;;  sort of else part
                    (println "it is in the table in else: ")
                    (begin
                        (dolook)
                        (expression)))))))

(define main (lambda ()
    (while (< cnt len) 
        (if 
            (isalpha look) (statement)
            (dolook)))
    (genret)))

(define prtcode 
    (lambda (n1 n2 n3)
        (case n1
            (1   (println n2 " lit: " n3)) 
            (2   (println n2 " sto: " n3)) 
            (3   (println n2 " lod: " n3))  
            (8   (println n2 " wrt: " n3)) 
            (9   (println n2 " ret: " n3)))))
            

(set 'codlen (length code))

(define prtloop
    (lambda ()
        (while (< $idx codlen)
            (prtcode (code $idx 0) $idx (code $idx 1)))))

(main)
(prtloop)

