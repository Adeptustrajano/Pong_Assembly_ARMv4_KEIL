AREA datos,DATA
;vuestras variables y constantes
VICVectAddr0	EQU	0xFFFFF100
VICIntEnable	EQU	0xFFFFF010
VICIntEnClr		EQU	0xFFFFF014
VICVectAddr		EQU	0xFFFFF030
T0_IR			EQU	0xE0004000
UART1			EQU	0xE0010000
I_Bit			EQU 0x80 ; bit 7 de la CPSR
N				EQU	16 ; filas
M				EQU	32 ;columnas
		ALIGN 4
crono 			DCD 0	;contador de centesimas de segundo
max 			DCD 8 	;velocidad de movimiento (en centesimas s.)
next 			DCD 0 	;instante siguiente movimiento
pelota          DCD 0   ;posicion pelota
		ALIGN 4
MarcadorIzq     DCB 0   ; marcador del jugador izquierdo
MarcadorDch     DCB 0   ; marcador del jugador derecho
		ALIGN 4
raquetaIzq		SPACE	20 ; Vector [5] donde se guardan las posiciones de las X de la raqueta izquierda
		ALIGN 4
raquetaDch		SPACE	20 ; Vector [5] donde se guardan las posiciones de las X de la raqueta derecha
		ALIGN 4
tecla			DCB 0	; tecla pulsada en ASCII
dir1 			DCB 0 	;mov. vertical raqueta izda (-1 arriba,0 stop,1 abajo)
dir2 			DCB 0 	;mov. vertical raqueta dcha (-1 arriba,0 stop,1 abajo)
dir3 			DCB 0 	;mov. pelota (a decidir por el alumno)
fin 			DCB 0 	;indicador fin de programa (si vale 1)
		ALIGN 4


		AREA codigo,CODE
		EXPORT inicio			; forma de enlazar con el startup.s
		IMPORT srand			; para poder invocar SBR srand
		IMPORT rand				; para poder invocar SBR rand
		; se recomienda poner punto de parada (breakpoint) en la primera
		; instruccion de c digo para poder ejecutar todo el Startup de golpe
		;actualizar el vector de interrupciones para que apunte a las RSI
		
inicio	
		LDR r0, =VICVectAddr0 ;accedo a la @ del VICVectAddr0
		LDR r1, =RSI_teclado ;accedo a la @ de la subrutina del teclado
		mov r2, #7 ;mLuevo un 7 a r1 ya que es la IRQ7
		str r1, [r0, r2, LSL #2] ;guardo la @ de la subrutina del teclado en la posicion 7 del VICVectAddr0
		LDR r1, =RSI_reloj ;accedo a la @ de la subrutina del TIMER
		mov r2, #4 ;muevo un 4 a r1 ya que es la IRQ4
		str r1, [r0, r2, LSL #2] ;guardo la @ de la subrutina de la TIMER en la posicion 4 del VICVectAddr0
		;una vez programado el vector con las @ de las subrutinas ya se pueden habilitar las interrupciones en el VIC
		LDR r0, =VICIntEnable
		mov r1, #0x90 ; muevo a r1 la mascara que tiene a 1 los bits 7 y 4
		str r1, [r0] ;guardo la m scara en VICIntEnable
		
		;poner un valor a variables dir1,2 en 0
		LDR r0, =dir1
		MOV r1, #0         ; Inicializar dir1 a 0 (detenido)
		STRB r1, [r0]

		LDR r0, =dir2
		MOV r1, #0         ; Inicializar dir2 a 0 (detenido)
		STRB r1, [r0]
		
		;DEPURACION QUE INIZIALIZA RAQUETAS EN AREA DATOS VECTOR PARA NO DAR ERROR VIOLACION DE ACCESO
		;direcciones v?lidas dentro del rango del tablero.
		
		; Inicializar raqueta izquierda
        LDR r2, =raquetaIzq         ; Direcci?n base del array raquetaIzq
        LDR r3, =0x40007E00         ; Direcci?n inicial del tablero (columna 1)
        MOV r4, #5                  ; N?mero de posiciones a inicializar (5 filas)

bucle_i_raquetaIzq
        STR r3, [r2], #4            ; Guardar direcci?n en raquetaIzq y avanzar al siguiente ?ndice
        ADD r3, r3, #32             ; Avanzar a la siguiente fila en el tablero
        SUBS r4, r4, #1             ; Decrementar el contador usando la flag z
        BNE bucle_i_raquetaIzq   ; Si no se ha llegado a 0, continuar el bucle

        ; Inicializar raqueta derecha
        LDR r2, =raquetaDch         ; Direcci?n base del array raquetaDch
        LDR r3, =0x40007E1D         ; Direcci?n inicial del tablero (columna 29)
        MOV r4, #5                  ; N?mero de posiciones a inicializar (5 filas)

bucle_i_raquetaDch
        STR r3, [r2], #4            ; Guardar direcci?n en raquetaDch y avanzar al siguiente ?ndice
        ADD r3, r3, #32             ; Avanzar a la siguiente fila en el tablero
        SUBS r4, r4, #1             ; Decrementar el contador usando las flag z de 0
        BNE bucle_i_raquetaDch   ; Si no se ha llegado a 0, continuar el bucle
		
LTORG   ; Aqu? se generan los literales en el literalpool
pintar_pantalla
		LDR r0, =0x40007E00
		mov r1, #'X'
		mov r2, #'*'
		mov r3, #0x20 					; espacio
		mov r4, #0 						; i=0
		mov r5, #N 						; filas
		mov r6, #M 						; columnas
		mul r5, r6, r5 					; tamannio de la pantalla
		cmp r4, r5
		bge fin_buc_espacios
ini_bus_espacios
		strb r3, [r0, r4] 				; poner toda la pantalla con espacio con preindexado para conservar la posi n inicial
		add r4, r4, #1
		cmp r4, r5
		blt ini_bus_espacios
fin_buc_espacios	
		;colocar las raquetas
		;usar crono para semilla aleatoria 
		LDR r6, =crono
		push {r6}
		bl srand
		bl rand
		pop {r6} 						; n  random
;calcular n  random mod 12, ya que las filas v lidas para pintar la raqueta son de la 0 a la 11, sino se sale

;hay que comprobar que r7, y r8 estan dentro de parametros de tablero porque sale error:
;error 65: access violation at 0x00000000 : no 'write' permission: indica un intento de escribir en una direcci?n de memoria inv?lida o no mapeada
;Asegurar que r6 est? entre 0 y 11 (rango v?lido para las raquetas)
		AND r6, r6, #0xF            ; Limitar r6 a un m?ximo de 15 (4 bits)
		CMP r6, #12
		BLT fin_buc_mod
		SUB r6, r6, #12             ; Ajustar al rango 0-11 si es mayor
fin_buc_mod
		LDR r2, =raquetaIzq
		LDR r3, =raquetaDch
		mov r4, #0 						; i = 0
		mov r5, #32						; tama?o fila
pintar_raquetas
		mul r7, r6, r5 					; r7 = fila *32
		mov r8, r7
		add r7, r7, #1					; offset columna 1
		add r7, r0, r7 					; direccion inicio raq izq
		strb r1, [r7]					; pintar X
		str r7, [r2], #4				; guardar @ de la pos de X raq izq
		
		add r8, r8, #29					; offset columna 29
		add r8, r0, r8 					; direcci n inicio raq izq
		strb r1, [r8]					; pintar X
		str r8, [r3], #4				; guardar @ de la pos de X raq dch
		
		add r6, r6, #1 					; siguiente fila
		add r4, r4, #1
		cmp r4, #5
		blt pintar_raquetas						; tama?o fila
		
pintar_pelota
        LDR r0, =0x40007E00       ; @ inicio de tablero
        MOV r1, #8                ; fila media
        MOV r2, #16               ; columna media
        MOV r3, #32				  ; tama?o fila
        ; Generar fila aleatoria (0 a 15) usando la semilla crono
        LDR r6, =crono            ; r6 contiene la direcci?n de crono (semilla)
        LDR r6, [r6]              ; r6 = valor actual del crono (semilla para rand)
        PUSH {r6}                 ; guardar r6 en la pila como semilla
        BL srand                  ; inicializar rand con la semilla
        BL rand                   ; 
        POP {r6}                  ; restaurar r6
        AND r6, r6, #0xF          ; limitar valor aleatorio a 4 bits (0 a 15) mascara
        MOV r1, r6                ; r1 = fila aleatoria (entre 0 y 15)

        ; Calcular posici?n de la pelota en la columna central pero fila segun crono rand
        MUL r4, r1, r3            ; r4 = fila * tama?o fila
        ADD r4, r4, r2            ; r4 = fila * tama?o fila + columna
        ADD r4, r0, r4            ; posici?n inicial en memoria del tablero + r4 que es posicion en tablero
		
		;Pintar la pelota
        MOV r5, #'.'
        STRB r5, [r4]             ; pintar la pelota en pantalla en la posici?n calculada aleatoria

		;guardar posicion pelota
        LDR r6, =pelota
        STR r4, [r6]              ; guardar posicion en variable pelota la inicial mitad del campo

        LDR r6, =crono       	  ; r6  direcci?n de crono
        LDR r6, [r6]        	  ; r6  valor actual del crono (semilla)
        PUSH {r6}             
        BL srand             	  ; inicializa rand con la semilla
        BL rand              	  ; r0 <- n?mero aleatorio
        POP {r6}             	  ; restaurar r6 (opcional)

        AND r6, r6, #0x3    	  ; mascara de los dos ?ltimos bits con AND y el nuemro RAND para opciones de 0,1,2,3
        LDR r1, =dir3        	  ; direcci?n donde se guarda la direcci?n de movimiento
        STRB r6, [r1]       	  ; guardar direcci?n aleatoria en dir3

     
;se inicia pelota en mitad del campo y se pone direccion de mov inicial con una mascara AND de 2 bits en 1 de 4 op.
;una vez pintada guardar posici?n pelota en pelota en memoria DATA y direccion en dir3
		
;se inicia la partida
jugar
		LDR r4, =crono ;accedo a la @ del TIMER
		LDR r5, =next
		LDR r6, =max
		ldr r4, [r4]					;guardo en r4 el valor del TIMER
		ldr r5, [r5]					;guardo en r5 el valor del contador en el  ltimo movimiento
		ldr r6, [r6]					;guardo en r6 el valor de max
		add r5, r5, r6					;contador + max
		; Ha pasado el tiempo suficiente desde la acci n anterior?
		cmp r4, r5						;while(reloj < contador + max) 
		bge siguiente_mov 				;if(reloj <= contador + max) -> realizar nuevos movimientos de la raqueta y la pelota
		
		;recuperar valor de la tecla pulsada y analizar a que  accion se corresponde
		LDR r4, =tecla
		ldrb r4, [r4]
		
		;Analizar el movimiento de las raquetas
		cmp r4, #'q'
		ldreq r5, =dir1
		ldreqsb r6, [r5]
		cmp r4, #'a'
		ldreq r5, =dir1
		ldreqsb r6, [r5]
		cmp r4, #'o'
		ldreq r5, =dir2
		ldreqsb r6, [r5]
		cmp r4, #'l'
		ldreq r5, =dir2
		ldreqsb r6, [r5]
		
		cmp r4, #'q'
		beq caso_q_o
		cmp r4, #'o'
		beq caso_q_o
		cmp r4, #'a'
		beq caso_a_l
		cmp r4, #'l'
		beq caso_a_l
		
		bne pass
		
caso_q_o
		cmp r6, #-1; si dir =-1, hacia arriba
		moveq r7, #0;detener
		cmp r6, #1;si dir=1, abajo
		moveq r7, #-1;subir
		cmp r6, #0; si dir= 0, cambia hacia arriba
		moveq r7, #-1;
		
		strb r7, [r5]
		
		LDR r4, =tecla
		mov r5, #0
		strb r5, [r4]
		b pass
		
caso_a_l
		cmp r6, #1
		moveq r7, #0
		cmp r6, #-1
		moveq r7, #1
		cmp r6, #0
		moveq r7, #1
		
		strb r7, [r5]
		
		LDR r4, =tecla
		mov r5, #0
		strb r5, [r4]

pass		
		;subir o bajar la valocidad
		LDR r5, =max
		ldr r6, [r5]					;guardo en r6 el valor de max
aumentar
		cmp r4, #'+'					;comparo el valor de la variable tecla con el + (que indica aumento de la velocidad)
		bne disminuir					;si no es igual salto a la comparaci n con la tecla -
		cmp r6, #1						;
		movgt r6, r6, LSR #1			; if(tecla == '+' && periodo > 1) periodo = periodo/2
		str r6, [r5]
		
		LDR r4, =tecla
		mov r5, #0
		strb r5, [r4]
disminuir	
		cmp r4, #'-'					;comparo el valor de la variable tecla con el - (que indica disminuci n de la velocidad)
		bne salir
		cmp r6, #128					
		movlt r6, r6, LSL #1			; else if(tecla == '-' && periodo < 128) periodo = periodo*2
		str r6, [r5]
		LDR r4, =tecla
		mov r5, #0
		strb r5, [r4]
		
salir		
		LDR r5, =fin
		ldrb r6, [r5]
		cmp r4, #'6'
		moveq r6, #1					; if(tecla == '6') fin = 1
		streqb r6, [r5]
		beq terminar					; salimos del while
		
		;Esto resetea el valor de la tecla. Sirve en el caso de que no se pulse ninguna tecla en el perif rico 
		;Hace falta hacer esto???

		
		b jugar 						;si no se ha pulsado la letra 6 volvemos al principio


siguiente_mov
		mov r0, #0x20 ;hueco blanco
		mov r1, #'X'  ;raqueta
		LDR r2, =next
		str r5, [r2] ; actualizar next
		
		LDR r6, =0x40007E00
		LDR r7, =0x40007FFF
		
mov_raq_izq
		LDR r2,=raquetaIzq
		LDR r3, =dir1
		ldrsb r3, [r3] ; valor de dir1 (-1,1,0)
		mov r4, #0
bucle_raq_izq
		ldr r5, [r2]; la posicion de la X en pantalla
		strb r0, [r5]
		cmp r3, #0          ; Compara dir1 con 0
		beq else_izq ; Si dir1 == 0, no hace nada
		
		addgt r5, r5, #32   ; Si dir1 > 0, pos = pos + 32
		sublt r5, r5, #32   ; Si dir1 < 0, pos = pos - 32
		
		addgt r8, r6, #1
		sublt r8, r7, #30
		
		cmp r5, r7 ; new_pos > 0x40007FFF
		movgt r5, r8
		cmp r5, r6; new_pos < 0x40007E00
		movlt r5, r8
		
else_izq		
		strb r1, [r5]
		str r5, [r2], #4
		
		add r4, r4, #1
		cmp r4, #5
		blt bucle_raq_izq
		
mov_raq_dcha
		LDR r2,=raquetaDch
		LDR r3, =dir2
		ldrsb r3, [r3] ; valor de dir2 (-1,1,0)
		mov r4, #0
bucle_raq_dch
		ldr r5, [r2]; la posicion de la X en pantalla
		
		;TODO -Corregirlo
		cmp r3, #0
		cmpgt r4, #0
		streqb r0, [r5]
		
		cmp r3, #0
		cmplt r4, #4
		streqb r0, [r5]

next_mov
		cmp r3, #0          ; Compara dir2 con 0
		beq else_dech ; Si dir1 == 0, no hace nada
		
		addgt r5, r5, #32   ; Si dir2 > 0, pos = pos + 32
		sublt r5, r5, #32   ; Si dir2 < 0, pos = pos - 32
		
		addgt r8, r6, #29 ;Se sobrepasa por abajo y la siguiente posición es arriba
		sublt r8, r7, #2 ;se sobrepasa por arriba y la siguiente posición es abajo
		
		cmp r5, r7 ; new_pos > 0x40007FFF
		movgt r5, r8
		cmp r5, r6; new_pos < 0x40007E00
		movlt r5, r8
		
else_dech
		strb r1, [r5]
		str r5, [r2], #4
		
		add r4, r4, #1
		cmp r4, #5
		blt bucle_raq_dch
	

mover_pelota
        LDR r0, =pelota       ;posicion
        LDR r1, [r0]          ; r1 = posicion actual pelota
        MOV r2, #0x20         ; espacio para borrar la pelota
        STRB r2, [r1]         ; borrar la pelota anterior con espacio en blanco

        ; calcular fila y columna actuales con =pelota 
        LDR r3, =0x40007E00
        SUB r4, r1, r3        ; r4 = offset pelota = fila*32 + col
        MOV r5, #32
        MOV r6, r4, lsr#5      ; fila = offset / 32 con movimiento de 5 bits derecha
		AND r7, r4, #31       ; col = mascara de bits con AND para obtener el resto  de la division para representar la columna dentro de la fila 

comprobar_bucle
        ; cargar direcci?n actual de la pelota 0,1,2,3
        LDR r8, =dir3
        LDRB r8, [r8]

        ; calcular nueva fila y columna segun direccion
        CMP r8, #0            ;0 es arriba izq
        BEQ mov_arr_izq
        CMP r8, #1            ;1 es abajo izq
        BEQ mov_aba_izq
        CMP r8, #2            ;2 es arriba dch
        BEQ mov_arr_dch
        CMP r8, #3            ;3 es abajo dch
        BEQ mov_aba_dch

mov_arr_izq
        SUB r6, r6, #1       ; fila--
        SUB r7, r7, #1       ; col--
        B comprobar
mov_aba_izq
        ADD r6, r6, #1       ; fila++
        SUB r7, r7, #1       ; col--
        B comprobar
mov_arr_dch
        SUB r6, r6, #1       ; fila--
        ADD r7, r7, #1       ; col++
        B comprobar
mov_aba_dch
        ADD r6, r6, #1       ; fila++
        ADD r7, r7, #1       ; col++
        B comprobar

;comprobar todos los posibles casos si ha colisionado
comprobar

		; gol en columna 0 o 31
        CMP r7, #0
        BEQ gol_dch
        CMP r7, #31
        BEQ gol_izq
		
        ; rebote con techo o suelo
        CMP r6, #0
        BEQ rebote_vertical
        CMP r6, #15
        BEQ rebote_vertical

        ; rebote con raquetas en la columna justo de antes
        CMP r7, #2
        BEQ comprobar_raq_izq
        CMP r7, #29
        BEQ comprobar_raq_dch
		
		;actualizar posicion de la pelota en memoria
		LDR r3, =0x40007E00
		MOV r5, #32
		mov r9, #'.'
        MUL r4, r6, r5            ; r4 = fila * 32
        ADD r4, r4, r7            ; r4 = fila * 32 + columna
        ADD r4, r3, r4            ; posicion pelota actual 
		strb r9, [r4]
		str r4, [r0]
		
		B jugar
        ;saltar a jugar 

rebote_vertical
        ; invertir el bit vertical de dir3 (bit 1)
		; invertir el bit vertical (bit 1), cambiando la direcci?n entre "arriba" y "abajo".
        LDR r9, =dir3
        LDRB r10, [r9]
        EOR r10, r10, #0x1    ; usar una mascara con eor para invertir el bit 1 osea ir para arriba o para abajo intercambiar
        STRB r10, [r9]
		b comprobar_bucle

;rebote raquetas: verificar la columna 1 en lugar de la columna actual (2):
comprobar_raq_izq
        ; comprobar si r6 (fila) coincide con alguna fila de raquetaIzq
        LDR r9, =raquetaIzq          
        MOV r10, #0                  
comprobar_bucle_izq
        LDR r11, [r9, r10, LSL #2]   ; posici?n de raqueta (memoria)
        SUB r11, r11, r3             ; offset = pos_raqueta - inicio_tablero
        MOV r12, r11, LSR #5         ; Fila de la raqueta (offset / 32) de cada BYTE de los 5
        CMP r12, r6                  ; Comparar fila de raqueta con fila de la pelota
        BEQ rebote_horizontal        
        ADD r10, r10, #1             ; raqueta++
        CMP r10, #5                  ; ?Se han comprobado todas las raquetas?
        BLT comprobar_bucle_izq      
        B comprobar_bucle            
		
comprobar_raq_dch
        ; comprobar si r6 (fila) coincide con alguna fila de raquetaDch
        LDR r9, =raquetaDch          
        MOV r10, #0                  
comprobar_bucle_dch
        LDR r11, [r9, r10, LSL #2]   ; posici?n de raqueta (memoria)
        SUB r11, r11, r3             ; offset = pos_raqueta - inicio_tablero
        MOV r12, r11, LSR #5         ; Fila de la raqueta (offset / 32) de cada BYTE de los 5
        CMP r12, r6                  ; Comparar fila de raqueta con fila de la pelota
        BEQ rebote_horizontal        
        ADD r10, r10, #1             ; raqueta++
        CMP r10, #5                  ; ?Se han comprobado todas las raquetas?
        BLT comprobar_bucle_dch      
        B comprobar_bucle            

rebote_horizontal
        ; invertir el bit horizontal de dir3 (bit 0)
		;invertir el bit horizontal (bit 0), cambiando la direcci?n entre "izquierda" y "derecha".
        LDR r9, =dir3
        LDRB r10, [r9]
        EOR r10, r10, #0x2
        STRB r10, [r9]
        B comprobar_bucle

gol_dch
        ; Incrementar marcador del jugador derecho
        LDR r0, =MarcadorDch
        LDRB r1, [r0]
        ADD r1, r1, #1             ; MarcadorDch++
        STRB r1, [r0]

        ; Comprobar si el marcador alcanza 10 puntos
        CMP r1, #10
        BEQ terminar               ; Si MarcadorDch == 10, finalizar el juego
		
		b pintar_pantalla 

gol_izq
        ; Incrementar marcador del jugador izquierdo
        LDR r0, =MarcadorIzq
        LDRB r1, [r0]
        ADD r1, r1, #1             ; MarcadorIzq++
        STRB r1, [r0]

        ; Comprobar si el marcador alcanza 10 puntos
        CMP r1, #10
        BEQ terminar               ; Si MarcadorIzq == 10, finalizar el juego
       
		;LDR r4, =crono
		;LDR r5, =next
		;ldr r4, [r4]			; reloj
		;str r4, [r5]			; cont_viejo = reloj (actualizamos cont_viejo)
		
		b pintar_pantalla 
		
		
terminar
		LDR r0, =VICIntEnClr
		mov r1, #0x90 					;pon un 1 en el bit 4 y 7
		str r1, [r0]
		LDR r0, =VICVectAddr0
		mov r1, #4
		mov r2, #0
		str r2, [r0, r1, LSL #2]
		mov r1, #7
		str r2, [r0, r1, LSL #2]

bfin	
		b bfin
	
		
RSI_reloj	
		sub lr, lr, #4 					; corregir @ de retorno
		push {lr}
		mrs r14, spsr 					; guardar sprs
		push {r14}
		push {r0, r1, r2}
		mrs r0, cpsr 					; copia el estado actual del procesador
		bic r0, r0, #I_Bit 				; pone el bit I a 0 y habilita las interrupciones
		msr cpsr_c, r0 					; guardar el cpsr con bit7 = 0
		;ini limpiar; Limpia el flag de interrupci n del timer0, para que no se vuelva a disparar la interrupci n
		ldr r1, =T0_IR
		mov r2, #1
		str r2 , [r1]
		;fin limpiar
		LDR r1, =crono
		ldr r2, [r1] 					; contador en este momento
		add r2, r2, #1 					; cont ++
		str r2, [r1] 					; se vuelve a guardar
		mrs r1, cpsr
		orr r1, r1, #I_Bit 				; deshabilita las interrupciones
		msr cpsr_c, r1
		pop {r0, r1, r2}
		pop {r14}
		msr spsr_cxsf, r14 				; restuarar el spsr
		LDR r14, =VICVectAddr
		str r14, [r14] 					; avisar al VIC que se ha atendido la interrupci n
		pop {pc}^
		
RSI_teclado
		sub lr, lr, #4
		push {lr}
		mrs r14, spsr
		push {r14}
		push {r0, r1, r2}
		mrs r0, cpsr
		bic r0, r0, #I_Bit
		msr cpsr_c, r0
		LDR r1, =UART1
		ldr r2, [r1] 					; tecla pulsada
		LDR r1, =tecla
		strb r2, [r1] 					; guardar la tecla pulsada
		mrs r1, cpsr
		orr r1, r1, #I_Bit
		msr cpsr_c, r1
		pop {r0, r1, r2}
		pop {r14}
		msr spsr_cxsf, r14
		LDR r14, =VICVectAddr
		str r14, [r14] 					; avisar al VIC que se ha atendido la interrupcion
		pop {pc}^
		

		END
