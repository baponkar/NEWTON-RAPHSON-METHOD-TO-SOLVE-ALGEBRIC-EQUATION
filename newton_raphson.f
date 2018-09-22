    
      !Find out the root of the equation exp(x) = 2x + 1 by Newton-Raphson method

PROGRAM NEWTON_RAPHSON
 IMPLICIT NONE

 REAL::F,X(0:100),TEMP,del_f
 INTEGER::I,J,K
 
  PRINT*,"ENTER INITIAL VALUES OF X"
   READ*,X(0)
 
  DO I=0,99
   print*,X(I)
   X(I+1)=X(I)-F(X(I))/DEL_F(X(I))
  
  END DO

   PRINT*,"THE REQUIRED ROOT IS:",X(100)

END PROGRAM NEWTON_RAPHSON
 



REAL FUNCTION F(X)
 IMPLICIT NONE
 REAL,INTENT(IN)::X
  F=EXP(X)-2*X-1
END FUNCTION F


REAL FUNCTION DEL_F(X)
 IMPLICIT NONE
 REAL,INTENT(IN)::X
  DEL_F=EXP(X)-2
END FUNCTION DEL_F
