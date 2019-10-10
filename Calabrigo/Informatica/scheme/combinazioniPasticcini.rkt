;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname combinazioniPasticcini) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;ci deve essere almeno un pasticcino per piatto, come posiziono i pasticcini?

;4 pasticcini, k=3   ->   (4 2) = 4*3/2*1 = 6
;5 pasticcini, k=3  :
;piatti:
; ***
;  *
;  *

;qui conto in quanti modi 5 pasticcini nel primo piatto, quindi (5 3)
;  (5 3) = 5*4/2 = 20/2 = 10
;  5*4*3 / 3*2*1 = 10

;oppure
; **
; **
; *
;((5 2) * (3 2))/2 = 15

;quindi 10 + 15 = 25

;Se avessimo 6 pasticcini in 3 piatti
; ****
; *
; *
;(6 4) = 15
; ***
; **
; *
;(6 3) * (3 2) = 60
; **
; **
; **
;((6 2) * (4 2) /3!)=15 ;faccio /3! xchè due disposizioni con le stesse coppie, ma in piatti diversi sono uguali, quindi divido per 3! piatti

;Quindi 15+60+15 = 90

;se ho 6 pasticcini da disporre in 3 piatti , ma uno di quelli deve essere sempre da solo allora: modi (n-1,k-1)
;se ho 6 pasticcini da disporre in 3 piatti , ma uno di quelli deve essere sempre in compagnia allora: modi k*(n-1,k)

;quindi: Modi(n,k) = Modi(n-1,k-1) + k*(Modi(n-1,k)

