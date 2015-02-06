;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ###################### PEG-SOLITAIRE ######################
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROJETO PROCURA E PLANEAMENTEO, 2014/15 
;;; 
;;;  GRUPO 09:
;;;                   ANDRE SANTOS 82118 
;;;                   RUBEN SANTOS 82252
;;;
;;; ENTREGA: 19 DEZEMBRO 2014
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PEG-SOLITAIRE Usando um tabuleiro 9x9
;;;

(in-package :user)

;estado
(defstruct estado
  	npegs				
  	tabuleiro
  	ultimo-mov
  	profundidade					
)

;contar nos
(defvar nos-expandidos 0)
(defvar nos-gerados 0)

;tempo limite
(defvar tempo-limite 295)

;; posicao das peças nos cantos
(defparameter pos-cantos  '((0 . 2)(0 . 6)(2 . 0)(2 . 8)(6 . 0)(6 . 8)(8 . 2)(8 . 6)))

;cria estado
(defun makestate (npegs tabuleiro ultimo-mov profundidade)
	(let ((init
		(make-estado
  			:npegs npegs
  			:tabuleiro tabuleiro
  			:ultimo-mov ultimo-mov
  			:profundidade profundidade
  		)
	))
	init
))

;converte lista para array
;recebe lista e tamanho lista
(defun toArray (lst x)
	(make-array (list x x) :initial-contents lst)
)

;recebe tabulerio e seu tamanho
;retorna numero pecas
(defun getnpegs (tabuleiro x)
	(let ((number 0))
		(dotimes (i x)
			(dotimes (j x)
				(if (equal (aref tabuleiro i j) "O")
					(incf number)
				)
			)
		)
		number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SUCESSORES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;brute-force sucessores
;recebe estado e procura no seu tabuleiroulerio com 2 for por 'O'
;SE ENCONTRA CHAMA FUNCAO auxiliar-sucessores auxiliar-sucessores que cria novo estado
;retorna lista estados
(defun sucessores (estado)
	(let ((size (array-dimension (estado-tabuleiro estado) 0)) 
		  (tabuleiro (estado-tabuleiro estado)) 
		  (npegss (estado-npegs estado))
		  (profundidade (estado-profundidade estado))
		  (ultimo-mov (estado-ultimo-mov estado))
		  (sucessores nil))
		(dotimes (i size)
			(dotimes (j size)
				(if (equal (aref tabuleiro i j) "O")
					(progn 
						(setf sucessores (append (auxiliar-sucessores tabuleiro i j size npegss profundidade ultimo-mov) sucessores))
					)
				)
			)
		)
		sucessores))

;chamada por sucessores
;recebe tabuleiroulerio, posicao "O", tamanho tabuleiroulerio, numero peças estado, profundidade estado
;cria novos estados ja com peca comida
;retorna lista sucessores
(defun auxiliar-sucessores (tabuleiro i j size npegs profundidade ultimo-mov)
	(let ((sucessores nil))
		;CIMA
		(if (and (peg-valida (- i 1) j size) (equal (aref tabuleiro (- i 1) j) "O"))
			(if (and (peg-valida (- i 2) j size) (equal (aref tabuleiro (- i 2) j) nil))
				(progn 
					(let ((copytabuleiro (copy-array tabuleiro)))
						(setf (aref copytabuleiro i j) nil)
						(setf (aref copytabuleiro (- i 1) j) nil)
						(setf (aref copytabuleiro (- i 2) j) "O")
						(setf sucessores (append (list 
							(makestate 
								(- npegs 1) 
								copytabuleiro
								(append ultimo-mov (list (cons (cons i j) (cons (- i 2) j))))
								 
								(1+ profundidade))
							) 
							sucessores))
					)
				)
			)
		)
		;BAIXO
		(if (and (peg-valida (+ i 1) j size) (equal (aref tabuleiro (+ i 1) j) "O"))
			(if (and (peg-valida (+ i 2) j size) (equal (aref tabuleiro (+ i 2) j) nil))
				(progn 
					;(format t "~A ~A ~%" (+ i 2) j)
					(let ((copytabuleiro (copy-array tabuleiro)))
						(setf (aref copytabuleiro i j) nil)
						(setf (aref copytabuleiro (+ i 1) j) nil)
						(setf (aref copytabuleiro (+ i 2) j) "O")
						(setf sucessores (append (list 
							(makestate 
								(- npegs 1) 
								copytabuleiro
								(append ultimo-mov (list (cons (cons i j) (cons (+ i 2) j))))
								 
								(1+ profundidade))
							) 
							sucessores))
					)
				)
			)
		)
		;DIREITA
		(if (and (peg-valida i (+ j 1) size) (equal (aref tabuleiro i (+ j 1)) "O"))
			(if (and (peg-valida i (+ j 2) size) (equal (aref tabuleiro i (+ j 2)) nil))
				(progn 
					;(format t "~A ~A ~%" i (+ j 2))
					(let ((copytabuleiro (copy-array tabuleiro)))
						(setf (aref copytabuleiro i j) nil)
						(setf (aref copytabuleiro i (+ j 1)) nil)
						(setf (aref copytabuleiro i (+ j 2)) "O")
						(setf sucessores (append (list 
							(makestate 
								(- npegs 1) 
								copytabuleiro
								(append ultimo-mov (list (cons (cons i j) (cons i (+ j 2)))))
								 
								(1+ profundidade))
							) 
							sucessores))
					)
				)				
			)
		)
		;ESQUERDA
		(if (and (peg-valida i (- j 1) size) (equal (aref tabuleiro i (- j 1)) "O"))
			(if (and (peg-valida i (- j 2) size) (equal (aref tabuleiro i (- j 2)) nil))
				(progn 
					;(format t "~A ~A ~%" i (- j 2))
					(let ((copytabuleiro (copy-array tabuleiro)))
						(setf (aref copytabuleiro i j) nil)
						(setf (aref copytabuleiro i (- j 1)) nil)
						(setf (aref copytabuleiro i (- j 2)) "O")
						(setf sucessores (append (list 
							(makestate 
								(- npegs 1) 
								copytabuleiro
								(append ultimo-mov (list (cons (cons i j) (cons i (- j 2)))))
								 
								(1+ profundidade))
							) 
							sucessores))
					)
				)
			)
		)
		sucessores
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCAO OBJECTIVO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;funcao objectivo
;se estado nao tiver sucessores
(defun peg-goal (estado)
	(if (equal (peg-h estado) 0)
		t
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEURISTICA1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; >
;heuristica1
;numero sucessores estado
(defun peg-h (estado)
	(length (sucessores estado))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEURISTICA2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <
;; funcao para verificar se um elemento existe na lista de lista
;; retornando true ou false
;; recebe uma lista e o valor a procurar na mesma
;; ex: (find-element-list nomedalista '(3 . 0))
(defun find-element-list (lista value)
	(let ((result ()))
    	(loop for x from 0 to (list-length lista) do  
      		(if (equal value (nth x lista))            
      			(setf result (equal value (nth x lista)))
      		) ;; fim do if               
    	) ;; fim do loop
    	result
    ) ;; fim do let, retorna result no fim (T ou NIL)
)

;;; função para converter uma lista num array 2d
;;; criou-se esta função, uma vez que trabalhamos sempre com arrays
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

;;; o contrário da função anterior, converte um array 2d para uma lista
(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

;numero de peças nos cantos
(defun peg-h2 (estado)
	(let ((resultado 0))
		(loop for x in (find-anything  (estado-tabuleiro estado) "O") do
			(if (find-element-list pos-cantos x) 
				(incf resultado)
			)
		)
	resultado
	)
)

;recebe array e valor
;retorna lista todas posicoes onde esta o valor
(defun find-anything (array value)
  (let ((result ()))
    (loop for x from 0 to (- (car (array-dimensions array)) 1) do  
      (loop for y from 0 to (- (car (cdr (array-dimensions array))) 1) do
            (if (equal value (aref array x y))               
              (setf result (append result (list (cons x y))))) 
    ))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HEURISTICA3;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <
;se peca esta isolada

(defun peg-h3 (estado)
	(let ((number 0) (size (array-dimension (estado-tabuleiro estado) 0)) (tabuleiro (estado-tabuleiro estado)))
		(dotimes (y size)
			(dotimes (x size)
				(if (equal (aref tabuleiro y x) "O")
					(progn
					;cima
					(if (peg-valida (+ y 1) x size)
						(if (equal (aref tabuleiro (+ y 1) x) "O")
							(incf number)
						)
					)
					;baixo
					(if (peg-valida (- y 1) x size)
						(if (equal (aref tabuleiro (- y 1) x) "O")
							(incf number)
						)
					)
					;direita
					(if (peg-valida y (+ 1 x) size)
						(if (equal (aref tabuleiro y (+ 1 x)) "O")
							(incf number)
						)
					)
					;esqu
					(if (peg-valida y (- 1 x) size)
						(if (equal (aref tabuleiro y (- 1 x)) "O")
							(incf number)
						)
					)
					)
				)
			)
		)
		number
	)
)

;ve se determinada posicao e valida
;recebe posicao e tamanho tabuleiroulerio
(defun peg-valida (x y size)
	(and (>= x 0) (< x size) (>= y 0) (< y size))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SONDAGEM ITERATIVA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;gera sucesores e escolhe aleatoriamente
;recebe estado
(defun randomsucessores (estado)
	(let ((sucessores (sucessores estado)))
		(setq nos-gerados (+ nos-gerados (list-length sucessores)) )
		(if (equal sucessores nil)
			nil
			(nth (random (length sucessores)) sucessores)
		)
 	)
)

;retorna folha
;recebe estado
(defun sondagem-iterativa (estado)
	(let ((actual 1) (previousactual estado) (father nil))
		;enquanto conseguir gerar sucessores
  		(loop while (not (equal actual nil))
   			do(setf actual (randomsucessores previousactual))
   			do(incf nos-expandidos)
			do(setf father previousactual)
   			do(setf previousactual actual)
  		)
  	father
  	)
)

;sondagem iterativa com tempo
;executa varias vezes sondagem iterativa e retorna a melhor dentro tempo limite
;recebe input prof e tempo limite
(defun sondagem-iterativa-tempo (lst time)
	(let ((start (get-universal-time)) 
		(profundidade 0) 
		(best (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0));MELHOR TODOS
		(tempo time);TEMPO LIMITE
		(mlocal nil));MELHOR LOCAL
		(loop while (< (- (get-universal-time) start) tempo)
			do(setf mlocal (sondagem-iterativa (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil (1+ profundidade))));atencao com ruben
			do(if (< (estado-npegs mlocal) (estado-npegs best))
				(setf best mlocal)
			)
		)
		best
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ORDENAR;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;funcao que escolhe melhor de 2 estados de acordo heuristica
;vai ser usado para ordenar lista sucessores
(defun sort2Sactuals (estado1 estado2);meter tipo funcao heuristica
	(let ( (sucessores1 (peg-h estado1)) 
		   (sucessores2 (peg-h estado2)))
		(if (> sucessores1 sucessores2)
			t 
			nil
		)
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PROCURA GANANCIOSA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;recebe estado
;retorna estado com maior valor heuristica
(defun bestsucessor (estado)
	(let ((sucessores (sucessores estado)))
		(let ((best (nth 0 sucessores)) (local nil) (nsucessores (length sucessores)))
			(setq nos-gerados (+ nos-gerados nsucessores))
			(dotimes (i nsucessores)
				(setf local (nth i sucessores))
				(if (> (peg-h local) (peg-h best))
					(setf best local)
				)
			)
			best
		)
	)
)

;recebe estado
;devolve sucessor com maior heuristica
(defun cortesucessores (estado)
	(let ((sucessoresm (sucessores estado)) (best (nth 0 (sucessores estado))) (local nil))
		(cond  ((equal (list-length sucessoresm) 0) (setf best nil))
				((equal (list-length sucessoresm) 1) best)
				((> (list-length sucessoresm) 1) 
					(dotimes (i (length sucessoresm))
						(setf local (nth i sucessoresm))
						(if (> (peg-h local) (peg-h best))
							(setf best local)
						)
					)
				)
		)
	(list best)
)
)

;recebe estado
;retorna estado com maior valor heuristica
;ordenar sucessores
;mais lento
(defun firstsucessores (estado)
	(let ((sucessores (sucessores estado)))
		(if (equal (length sucessores) 1)
			(nth 0 sucessores)
			(progn 		
				(sort sucessores #'sort2Sactuals)
				(if (equal sucessores nil)
					nil
					(nth 0 sucessores)
				)
			)
		)
	)
)

;recebe estado e retorna estado
;temos 2 formas de obter maximo estado heuristico
;ordenar firstsucessores
;obter maior bestsucessor
(defun gananciosa (estado)
	(let ((actual 1) (previousactual estado) (father estado))
		;enquanto conseguir gerar sucessores
		(loop while (not (equal actual nil))
			do(setf actual (bestsucessor previousactual))
			do(incf nos-expandidos)
			do(setf father previousactual)
			do(setf previousactual actual)
		)
		father
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;procura hibrida;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;usado no hibrido com limite profundidade
;executa sondagem iterativa ate profundidade
;retorna estado
(defun sondagem-iterativa-profundidade (estado profundidade)
	(let ((actual 1) (previousactual estado) (father estado))
		(loop while (and 
			(not (equal actual nil)) 
			(not (equal profundidade (estado-profundidade father)))
			)
			do(setf actual (randomsucessores previousactual))
			do(incf nos-expandidos)
			do(setf father previousactual)
			do(setf previousactual actual)
		)
		father
	)
)

;recebe estado
;retorna estado
;executa sondagem iterativa ate profundidade calculada
;depois continua procura com gananciosa
;profundidade e calculada sendo metade numero pecas tabuleiro
(defun hibrido (estado)
	(let ((n (getnpegs (estado-tabuleiro estado) 9)) (profundidade nil))
		(if (equal n 1)
			(setf profundidade 1);para nao rebentar
			(setf profundidade (/ n 2))
		)
		(let ((profundidade (/ n 2)) (continuar nil))
			(setf continuar (sondagem-iterativa-profundidade estado profundidade))
			(gananciosa continuar)
		)
	)
)

;executa varias vezes o hibrido dentro tempo limite e retorna melhor
(defun hibrido-tempo (lst time)
	(let ((start (get-universal-time)) (profundidade 0) 
		(best (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0));MELHOR TODOS
		(tempo time);TEMPO LIMITE
		(mlocal nil));MELHOR LOCAL
		(loop while (< (- (get-universal-time) start) tempo)
			;primeiro ultimo-mov é nil
			do(setf mlocal (hibrido (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil profundidade)));atencao com ruben
			do(if (< (estado-npegs mlocal) (estado-npegs best))
				(progn
					(setf best mlocal)
				)
			)
		)
		;(list best nos-expandidos nos-gerados)
		best
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DDS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gerar todos nos ate profundidade dada
;filtrar estados gerados por profundidade dada
(defun filtrarestados (lst profundidade)
	(let ((todos (first (procura 
		(cria-problema 
			(makeState (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0)
			(list #'sucessores)
			:estado= #'equal
			:objectivo? #'peg-goal
			:heuristica #'peg-h)
		"obter" profundidade T))) (result nil))
	
	(dolist (x todos)
	(if (equal (estado-profundidade x) profundidade)
			(setf result (append result (list x)))
		)
	)
	result
	)
)

;recebe estado
;gera sucessores e atravez logica escolhemos valor que contraria heuristica
(defun contrariar-heuristica (estado)
	(let ((sucessores (sucessores estado)) (nrandom 0) (sucessor nil))
		(incf nos-expandidos)
		(incf nos-gerados (length sucessores))
		(cond 
			(( equal (length sucessores) 0 ) ( setf sucessor estado ))
			(( equal (length sucessores) 1) (setf sucessor (nth 0 sucessores)))
			((> (length sucessores) 1) 
				(progn
					(let ((local2 nil) (local1 nil))
						(setf local2 (nth 1 sucessores))
						(setf local1 (nth 0 sucessores))
						(if (< (peg-h local2) (peg-h local1))
							(setf sucessor local2)
							(setf sucessor local1)
						)
					)
			))
		)
		sucessor
	)
)

;usado no DDS quando iteracao e maior que 2
;recebe lista iteracao tempo inicio e tempo limite
(defun outra-iteracao (lst iteracao start time)
	(let ((estados (filtrarestados lst iteracao)) (best nil) (local nil))
		(if (> (length estados) 0)
			(progn 
				;so para iniciar
				(setf best (nth 0 estados))
				;percorrer todos a determinada profundidade
				(dolist (x estados)
					;tempo
					(loop while (< (- (get-universal-time) start) time)
						do(if (equal (estado-profundidade x) iteracao)
							(incf nos-gerados)
							(progn 
								(setf local (gananciosa (contrariar-heuristica x)))
								(incf nos-expandidos)
								;retornar sempre melhor
								(if (< (estado-npegs local) (estado-npegs best))
									(setf best local)
								)
							)
						)
					)
				)
			)
			nil
		)
		best
	)
)

;recebe lista iteracao tempo inicio e tempo limite
;executa DDS cde acordo iteracao
(defun dds-iteracao (lst iteracao start time)
	(cond
		((equal iteracao 0) (progn 
			(gananciosa (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0))))
		((equal iteracao 1) (progn 
			(gananciosa (contrariar-heuristica (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0)))))
		(t (outra-iteracao lst iteracao start time))
	)
)

;recebe lista e tempo limite
;executa DDS dentro tempo limite e nivel discrepancia
;iteracao e nivel discrepancia
(defun dds-tempo (lst time)
	(let (
		(start (get-universal-time)) 
		(iteracao 0) 
		(best (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0))
		(mlocal nil));MELHOR LOCAL
		(loop while (< (- (get-universal-time) start) time)
			do(setf mlocal (dds-iteracao lst iteracao start time))
			do(incf iteracao)
			(if (not (equal mlocal nil))
				(progn
					(if (< (estado-npegs mlocal) (estado-npegs best))
						(setf best mlocal)
					)
				)
				(return best);exit
			)
		)
		;(list best nos-gerados nos-expandidos)
		best
	)
)

(defun resolve-solitario (lst tipo)
(cond 
	((equal "melhor.abordagem" tipo) (imprimir (hibrido-tempo lst tempo-limite)))

	((equal "a*.melhor.heuristica" tipo) ;; CONDICAO 01
 		(imprimir (first (first (procura2
		(cria-problema 
			(makeState (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0)
			(list #'sucessores)
			:estado= #'equal
			:objectivo? #'peg-goal
			:heuristica #'peg-h)
		"a*" 999 T))))
	)

	((equal "a*.melhor.heuristica.alternativa" tipo) ;; CONDICAO 02
 		(imprimir (first (first (procura2
		(cria-problema 
			(makeState (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0)
			(list #'sucessores)
			:estado= #'equal
			:objectivo? #'peg-goal
			:heuristica #'peg-h2)
		"a*" 999 T))))
	)
	
	((equal "sondagem.iterativa" tipo) (imprimir (sondagem-iterativa-tempo lst tempo-limite)))

  	((equal "DDS" tipo) (imprimir (dds-tempo lst tempo-limite)))

  	((equal "abordagem.alternativa" tipo) (imprimir (gananciosa (makestate (getnpegs (toArray lst 9) 9) (toArray lst 9) nil 0))))
	);; FIM DO COND
)

;; ############################### PROCURA ###############################
(defparameter resultado2 (list ))

;serve para obter nos a uma determinada profndidade
(defun obter-estados-prof (problema profundidade-maxima) 
(let ((cenas ))
  (let ((estado= (problema-estado= problema))
 (objectivo? (problema-objectivo? problema))
  (resultado))
    (labels ((esta-no-caminho? (estado caminho)
        (member estado caminho :test estado=))
      (procura-prof (estado caminho prof-actual)
        (block procura-prof
   ;; base da recursao:
   ;; 1. quando comecamos a repetir estados pelos quais ja
   ;;    passamos no caminho que esta a ser percorrido
   ;;    (para evitar caminhos infinitos)
   ;; 2. quando atingimos o objectivo
   ;; 3. quando ultrapassamos a profundidade limite ate
   ;;    onde se deve efectuar a procura
   (cond ((funcall objectivo? estado) (list estado))
         ((= prof-actual profundidade-maxima) nil)
         ((esta-no-caminho? estado caminho) nil)
         (t 
   (dolist (suc (problema-gera-sucessores problema
              estado))
     ;; avancamos recursivamente, em profundidade,
     ;; para cada sucessor
     (let ((
          solucao (procura-prof suc  (cons estado caminho) (1+ prof-actual))
          ))
        (push  suc resultado2)      
))
      )))))
      (procura-prof (problema-estado-inicial problema) nil 0) )
  )
resultado2)
)

;usado na invocacao da funcao acima definida
(defun procura (problema tipo-procura profundidade-maxima espaco-em-arvore? )
  (flet ((faz-a-procura (problema tipo-procura 
       profundidade-maxima espaco-em-arvore?)
    (cond 
		((string-equal tipo-procura "largura") (largura-primeiro problema :espaco-em-arvore? espaco-em-arvore?))
		((string-equal tipo-procura "obter") (obter-estados-prof problema profundidade-maxima))
		((string-equal tipo-procura "profundidade") (profundidade-primeiro problema profundidade-maxima))
		((string-equal tipo-procura "profundidade-iterativa") (profundidade-iterativa problema profundidade-maxima))
		((string-equal tipo-procura "a*") (a* problema :espaco-em-arvore? espaco-em-arvore?))
		((string-equal tipo-procura "ida*") (ida* problema :espaco-em-arvore? espaco-em-arvore?)))))

    (let ((*nos-gerados* 0) (*nos-expandidos* 0) (tempo-inicio (get-internal-run-time)))
		(let ((solucao (faz-a-procura problema tipo-procura
		    profundidade-maxima
		    espaco-em-arvore?)))
  	(list solucao)
  	)))
)

(defun procura2 (problema tipo-procura profundidade-maxima espaco-em-arvore? )
  (flet ((faz-a-procura (problema tipo-procura 
       profundidade-maxima espaco-em-arvore?)
    (cond 
		((string-equal tipo-procura "largura") (largura-primeiro problema :espaco-em-arvore? espaco-em-arvore?))
		((string-equal tipo-procura "obter") (obter-estados-prof problema profundidade-maxima))
		((string-equal tipo-procura "profundidade") (profundidade-primeiro problema profundidade-maxima))
		((string-equal tipo-procura "profundidade-iterativa") (profundidade-iterativa problema profundidade-maxima))
		((string-equal tipo-procura "a*") (a* problema :espaco-em-arvore? espaco-em-arvore?))
		((string-equal tipo-procura "ida*") (ida* problema :espaco-em-arvore? espaco-em-arvore?)))))

    (let ((*nos-gerados* 0) (*nos-expandidos* 0) (tempo-inicio (get-internal-run-time)))
		(let ((solucao (faz-a-procura problema tipo-procura
		    profundidade-maxima
		    espaco-em-arvore?)))
  	(list (last solucao))
  	)))
)

;reconhecer nome heuristica
(defvar *nome-da-heuristica*)
(defvar *heuristica1*)
(setf heuristica1 #'peg-h)
(defvar *heuristica2*)
(setf heuristica2 #'peg-h2)

(defun cria-problema (estado-inicial operadores 
          &key estado-final
         objectivo?
         custo
         heuristica
         (hash #'sxhash)
         (estado= #'eql))
  (setf nome-da-heuristica heuristica)
  (let ((obj? (cond ((functionp objectivo?) objectivo?)
        (estado-final
         #'(lambda (estado) 
       (funcall estado= estado estado-final)))
        (t (always t)))))
  (make-problema :estado-inicial estado-inicial
     :operadores operadores
     :objectivo? obj?
     :custo (or custo (always 1))
     :heuristica (or heuristica (always 0))
     :hash hash
     :estado= estado=)))


(defun junta-ordenados (abertos nos-a*)
(cond 
( (eq nome-da-heuristica heuristica2)    ;; CONDICAO 1
  (flet ((menor (n1 n2) ;; RESULTADO 1
     (< n1 n2)))
    (merge 'list (sort nos-a* #'menor :key #'no-a*-f) abertos
     #'menor :key #'no-a*-f))
)
( (eq nome-da-heuristica heuristica1)    ;; CONDICAO 1
  (flet ((maior (n1 n2) ;; RESULTADO 1
     (> n1 n2)))
    (merge 'list (sort nos-a* #'maior :key #'no-a*-f) abertos
     #'maior :key #'no-a*-f))
)))

(defun procura-com-espaco (problema espaco)
  
  (let ((objectivo? (problema-objectivo? problema)) (start (get-universal-time)))
  	;(loop while (< (- (get-universal-time) start) time)

   (loop while (< (- (get-universal-time) start) tempo-limite)
	      ;; Quando nao temos mais nos e porque ja exploramos todo o
	      ;; espaco e nao encontramos a solucao (nao existe)
	      do(when (espaco-vazio? espaco)
	      	(return nil)
	      )
	      ;; Vamos considerar o no gerado mais antigo para termos uma
	      ;; procura em largura primeiro
	      do(let ((proximo-no (espaco-proximo-no espaco)))
	      	;; Se atingimos a solucao paramos e devolvemos os estados no
			;; caminho 
			(when (funcall objectivo? (no-estado proximo-no))
				(return (da-caminho proximo-no))
			)
			;; Caso contrario, devemos expandir o no
			(espaco-expande-no espaco proximo-no)
			)
		)
	)
)

;########################################IMPRIMIR CORRECTAMENTE###############################################
;;; -------------------------------------------------------------------------
;;; Função imprimir-lista, uma alternativa ao imprimir-peg
(defun imprimir-lista (lista)
    (loop for x from 0 to (1- (list-length lista)) do  
       (print (nth x lista))
    )
   (values)
)

(defun imprimir (estado)
	;(imprimir-lista (2d-array-to-list (estado-tabuleiro estado)))
	;(format t "~%~a" (estado-ultimo-mov estado))
	;(values)
	(if (equal estado nil)
		nil
		(list (2d-array-to-list (estado-tabuleiro estado)) (estado-ultimo-mov estado))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXEMPLOS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Melhor Abordagem
;(resolve-solitario '
;(("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" NIL "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X"))
; "melhor.abordagem")

;A* heuristica 1
;(resolve-solitario '
;(("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" NIL "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X"))
; "a*.melhor.heuristica")

;A* heuristica 2
;(resolve-solitario '
;(("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" NIL "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X"))
; "a*.melhor.heuristica.alternativa")

;Sondagem Iterativa
;(resolve-solitario '
;(("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" NIL "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X"))
; "sondagem.iterativa")

;DDS
;(resolve-solitario '
;(("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" NIL "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X"))
; "DDS")

;Abordagem Alternativa
;(resolve-solitario '
;(("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" NIL "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("O" "O" "O" "O" "O" "O" "O" "O" "O")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X")
; ("X" "X" "O" "O" "O" "O" "O" "X" "X"))
; "abordagem.alternativa")

