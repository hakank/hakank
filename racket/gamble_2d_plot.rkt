#| 

  Plot 2d points (scatter plot) in in Racket/Gamble 

  ASCII (scatter) plot  of 2D points.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

#|

  The test points:

0.29357 |                                   *   
        |                                 *     
        |                             *         
        |                                       
        |                                       
        |                  *           *        
        |                        *              
        |                                       
        |                                       
        |                                       
        |                                       
        |                                      *
        |                                       
        |          *                            
        |                 *                     
        |                                       
        |                     *                 
        |                                       
        |                                       
        +---------------------------------------  0.94065
        0

|#
;; Example usage
(define test-points
  '((0.4588553876248004 0.22976039275297938)
    (0.45016446957229866 0.09223864697516863)
    (0.545748223903503 0.04985294196973833)
    (0.9406489428726446 0.13123195578722444)
    (0.8832388626676247 0.2935668064891118)
    (0.2827685004602764 0.0939978832731861)
    (0.8441159526296236 0.2851719116595941)
    (0.762515401142464 0.23141796261419922)
    (0.7451287927075264 0.2697663516996897)
    (0.6045360024421217 0.21228132447100143)))

(plot-2d test-points)

#|
  For x and y (normal 10 1):

12.34641 |                              *        
         |                          *   *  *     
         |                      *    ** *****    
         |                       *  **********   
         |                        ****** *****   
         |                      *  ** ** **      
         |                        *   *** **    *
         |                         * ** *  **    
         |                          *   **       
         |                                       
         |                                       
         |                                       
         |                                       
         |                                       
         |                                       
         |                                       
         |                                       
         |                                       
         |                                       
         +---------------------------------------  12.61199
         0



  For x and y (normal 0 1)
2.90935               *                         
                      |                         
                      |                         
                      *                         
             *   *    |                         
                      |                         
                      |                         
               * *  * |*   ***  *               
               *    **|** ** *                  
             *   *    |* *         *            
          * *  * *   *|*  ******                
                  **  * *                       
        *------*---**-+**---*-*-----------------  3.83532
               **     | *  * * *                
            * *  *  * | ** *   ** *             
                  * **|**   ****                
            *         * *  *  *                 
                      | *  *                   *
                  *   |                         
                      |*                        
                      0


  For x and y (poisson 10)

20 |        *                              
   |            *                          
   |                                       
   |            *        *      *          
   |        *   *    *                     
   |                   * *    *            
   |          *    *   *                   
   |            **   * * * *               
   |            ** *       *               
   |    *       ** * *     * **            
   |      *   * **   * * *    *     *     *
   |          * ** * * * * * *    *   *    
   |             * *       *  *            
   |            *  * *   * * *             
   |      *        * * *     *             
   |            *      *     *             
   |          *      *   *                 
   |                 *                     
   |                                       
   +---------------------------------------  21
   0


  For x and y (exponential 10)  

|#
(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; (define x (poisson 10))
   ;; (define y (poisson 10))
   ;; (define x (normal 10 1))
   ;; (define y (normal 10 1))   
   ;; (define x (normal 0 1))
   ;; (define y (normal 0 1))   

   ;; (define x (exponential 10))
   ;; (define y (exponential 10))   

   (define x (normal 100 15))
   (define y (normal 100 15))   


   ;; (define x (beta 10 1))
   ;; (define y (beta 10 10))   


   (list x y)

   )
)

; (plot-2d test-points)


(plot-2d (make-samples (model) 100))
(newline)
(scatter (make-samples (model) 1000))
(newline)
(show-histogram (map first (make-samples (model) 1000)))
